//
//  Compiler.swift
//  InterpreterCompiler
//
//  Created by Forest Plasencia on 9/18/25.
//

import SwiftSyntax
#if canImport(SwiftParser)
import SwiftParser
#elseif canImport(SwiftSyntaxParser)
import SwiftSyntaxParser
#endif

import InterpreterModels
import Foundation
import HostSurfaceKit

@inline(__always)
private func debugLog(_ message: @autoclosure () -> String) {
#if DEBUG
    print(message())
#endif
}

private struct PendingOp {
    let symbol: String
    let node: BinaryOperatorExprSyntax
}

// MARK: - Method Signature Registry for Closure Type Inference

/// Represents a closure parameter type in a method signature
private struct ClosureSignatureInfo {
    let parameterTypes: [TypeName]  // Closure parameter types (may contain generic placeholders)
    let returnType: TypeName  // Closure return type (may be a generic placeholder)
}

/// Registry of method signatures for closure type inference
/// Built from HostSurfaceKit's ApiSurface data
private struct MethodSignatureRegistry {
    private var signatures: [String: [String: ClosureSignatureInfo]] = [:]
    // Store generic parameter names for each type: "Swift.Array" → ["Element"]
    private var typeGenericParams: [String: [String]] = [:]
    // Store property types: "Swift.String" → ["count": TypeName(Swift.Int)]
    private var propertyTypes: [String: [String: TypeName]] = [:]

    init(apiSurface: ApiSurface, genericParameterInventory: [String: [String]]) {
        // Store generic parameter names from inventory
        self.typeGenericParams = genericParameterInventory

        // Build registry from parsed host surface data
        for typeSurface in apiSurface.types {
            // Extract base type name without generic arguments
            // e.g., "Swift.Array<Swift.UInt8>" -> "Swift.Array"
            let fullTypeName = typeSurface.canonicalName
            let typeName: String
            if let genericStart = fullTypeName.firstIndex(of: "<") {
                typeName = String(fullTypeName[..<genericStart])
            } else {
                typeName = fullTypeName
            }

            for member in typeSurface.members {
                switch member {
                case .instanceMethod(let method):
                    // Index methods with closure parameters
                    let signature = method.signature
                    for param in signature.parameters {
                        if let closureSig = param.type.functionSignature {
                            // Found a closure parameter! Register it
                            let info = ClosureSignatureInfo(
                                parameterTypes: closureSig.parameters,
                                returnType: closureSig.returnType
                            )

                            if signatures[typeName] == nil {
                                signatures[typeName] = [:]
                            }
                            
                            signatures[typeName]?[signature.selector] = info
                            break  // Only handle first closure param for now
                        }
                    }

                case .staticMethod(let method):
                    // Index static methods with closure parameters
                    let signature = method.signature
                    for param in signature.parameters {
                        if let closureSig = param.type.functionSignature {
                            let info = ClosureSignatureInfo(
                                parameterTypes: closureSig.parameters,
                                returnType: closureSig.returnType
                            )

                            if signatures[typeName] == nil {
                                signatures[typeName] = [:]
                            }
                            signatures[typeName]?[signature.selector] = info
                            break
                        }
                    }

                case .initializer(let initializer):
                    // Index initializers with closure parameters
                    let signature = initializer.signature
                    for param in signature.parameters {
                        if let closureSig = param.type.functionSignature {
                            let info = ClosureSignatureInfo(
                                parameterTypes: closureSig.parameters,
                                returnType: closureSig.returnType
                            )

                            if signatures[typeName] == nil {
                                signatures[typeName] = [:]
                            }
                            signatures[typeName]?[signature.selector] = info
                            break
                        }
                    }

                case .instanceProperty(let property):
                    // Index instance properties
                    if propertyTypes[typeName] == nil {
                        propertyTypes[typeName] = [:]
                    }
                    propertyTypes[typeName]?[property.name] = property.type

                case .staticProperty(let property):
                    // Index static properties (same as instance for now)
                    if propertyTypes[typeName] == nil {
                        propertyTypes[typeName] = [:]
                    }
                    propertyTypes[typeName]?[property.name] = property.type
                }
            }
        }

        // Log summary at the end
        debugLog("[REGISTRY] Initialization complete. Total types with closure methods: \(signatures.keys.count)")
//        debugLog("[REGISTRY] Types registered: \(signatures.keys.sorted())")
    }

    func lookupClosureSignature(typeName: String, selector: String) -> ClosureSignatureInfo? {
        let baseTypeName = stripGenericArguments(typeName)
        debugLog("[REGISTRY LOOKUP] Base type: \(baseTypeName)")
        debugLog("[REGISTRY LOOKUP] Has type in registry: \(signatures[baseTypeName] != nil)")
        if let methods = signatures[baseTypeName] {
            debugLog("[REGISTRY LOOKUP] Available selectors for \(baseTypeName): \(methods.keys.sorted())")
            debugLog("[REGISTRY LOOKUP] Has selector '\(selector)': \(methods[selector] != nil)")
        }
        return signatures[baseTypeName]?[selector]
    }

    func lookupGenericParameters(typeName: String) -> [String]? {
        let baseTypeName = stripGenericArguments(typeName)
        return typeGenericParams[baseTypeName]
    }

    func lookupPropertyType(typeName: String, propertyName: String) -> TypeName? {
        let baseTypeName = stripGenericArguments(typeName)
        return propertyTypes[baseTypeName]?[propertyName]
    }

    var genericParameterInventorySnapshot: [String: [String]] {
        typeGenericParams
    }

    private func stripGenericArguments(_ typeName: String) -> String {
        if let genericStart = typeName.firstIndex(of: "<") {
            return String(typeName[..<genericStart])
        }
        return typeName
    }
}

public final class Compiler {
    public enum LocalsMode { case name, slot }
    public struct Options {
        public var mode: LocalsMode = .name
        public init(mode: LocalsMode = .name) {
            self.mode = mode
        }
    }

    private let options: Options
    private var slotResolutionEnabled: Bool = true
    // Implicit self context for method bodies
    private struct ImplicitSelfContext { let typeName: String; let fieldNames: Set<String> }
    private var implicitSelfContext: ImplicitSelfContext? = nil
    // Property wrappers metadata captured during struct collection (M1/M3)
    private struct WrapperDescriptor { let wrapperType: String, backingName: String, defaultExpr: ExprSyntax?, attrArgs: [(label: String?, expr: ExprSyntax)] }
    private var wrappersByType: [String: [String: WrapperDescriptor]] = [:]
    // Stored property defaults for non-wrapped fields (M3)
    private var storedDefaultsByType: [String: [String: ExprSyntax]] = [:]
    // Collected field order per type to support constructor synthesis (M3)
    private var collectedFieldsByType: [String: [String]] = [:]
    // Local/global wrapper bindings (M4)
    private struct LocalWrapperDescriptor { let wrapperType: String, backingName: String }
    private var wrapperLocalStack: [[String: LocalWrapperDescriptor]] = []
    // Name-mode compile-time binding tracker (for implicit self heuristics)
    private var nameBindingsStack: [Set<String>] = []
    // Minimal type environment for wrapped-property rewrites (M2)
    private enum TypeBindingKind { case value, typeAlias }
    private struct TypeBinding { let kind: TypeBindingKind; let typeName: String }
    private var typeBindingsStack: [[String: TypeBinding]] = []
    private let builtinTypeAliases: [String: String] = [
        // Integer family
        "Int": "Int", "Int8": "Int8", "Int16": "Int16", "Int32": "Int32", "Int64": "Int64",
        "UInt": "UInt", "UInt8": "UInt8", "UInt16": "UInt16", "UInt32": "UInt32", "UInt64": "UInt64",
        // Floating point
        "Float": "Float", "Double": "Double", "Float16": "Float16", "Float80": "Float80",
        // Core value types
        "Bool": "Bool", "String": "String", "Character": "Character", "Never": "Never",
        // Collections and ranges
        "Array": "Array", "Dictionary": "Dictionary", "Set": "Set", "Range": "Range", "ClosedRange": "ClosedRange",
        // Optionals and existential basics
        "Optional": "Optional", "Any": "Any", "AnyObject": "AnyObject"
    ]

    private struct CallArgument {
        let label: String?
        let expression: ExprSyntax
        let isTrailing: Bool
    }
    private func startsWithUppercase(_ s: String) -> Bool {
        guard let u = s.unicodeScalars.first else { return false }
        return CharacterSet.uppercaseLetters.contains(u)
    }
    // Slot-mode symbol table (scaffold): only used when options.mode == .slot
    final class SlotResolver {
        struct Scope { var map: [String: (slot: Int, isConst: Bool)]; var nextSlot: Int; let total: Int }
        private(set) var scopes: [Scope] = []
        var hasActiveScope: Bool { !scopes.isEmpty }
        func push(total: Int) { scopes.append(.init(map: [:], nextSlot: 0, total: total)) }
        func pop() { _ = scopes.popLast() }
        func declare(_ name: String, isConst: Bool) throws -> Int {
            guard var top = scopes.popLast() else { throw CompileError.message("declare outside of slot scope") }
            if top.map[name] != nil { throw CompileError.message("redeclaration of \(name) in same scope") }
            let slot = top.nextSlot
            top.nextSlot += 1
            top.map[name] = (slot, isConst)
            scopes.append(top)
            return slot
        }
        // Returns (depth, slot, isConst)
        func resolve(_ name: String) -> (depth: Int, slot: Int, isConst: Bool)? {
            // depth 0 = current slot scope
            var d = 0
            for scope in scopes.reversed() {
                if let info = scope.map[name] { return (depth: d, slot: info.slot, isConst: info.isConst) }
                d += 1
            }
            return nil
        }
    }
    private var slot: SlotResolver? = nil
    // Method signature registry for closure type inference (built from host surface data)
    private let methodSignatureRegistry: MethodSignatureRegistry
    // Type environment for tracking variable types (B3.5: receiver type tracking)
    // Maps variable names to their inferred TypeName (includes generics)
    private var typeEnvironmentStack: [[String: TypeName]] = []

    public init(
        options: Options = .init(),
        hostSurface: ApiSurface? = nil,
        genericParameterInventory: [String: [String]]? = nil
    ) {
        self.options = options
        // Build registry from host surface if provided, otherwise use empty registry
        if let surface = hostSurface {
            let inventory = genericParameterInventory ?? [:]
            self.methodSignatureRegistry = MethodSignatureRegistry(
                apiSurface: surface,
                genericParameterInventory: inventory
            )
        } else {
            // Fallback: create empty registry (no type inference available)
            self.methodSignatureRegistry = MethodSignatureRegistry(
                apiSurface: ApiSurface(types: []),
                genericParameterInventory: [:]
            )
        }
    }
    
    public func compileProgram(source: String, fileName: String = "input.swift") throws -> Program {
#if canImport(SwiftParser)
        let root = Parser.parse(source: source)
#elseif canImport(SwiftSyntaxParser)
        let root = try SyntaxParser.parse(source: source)
#else
        throw CompileError.message("No SwiftParser/SwiftSyntaxParser available for parsing.")
#endif
        guard let sf = root.as(SourceFileSyntax.self) else {
            throw CompileError.message("Expected SourceFileSyntax root.")
        }
        // Reset per-compilation state
        wrappersByType.removeAll()
        storedDefaultsByType.removeAll()
        collectedFieldsByType.removeAll()
        wrapperLocalStack.removeAll(); wrapperLocalStack.append([:])
        // Initialize slot-mode resolver if needed
        if options.mode == .slot { self.slot = SlotResolver() }
        nameBindingsStack.removeAll()
        typeBindingsStack.removeAll()
        typeBindingsStack.append([:]) // root frame
        seedBuiltinTypeBindings()
        // B3.5: Initialize type environment for tracking variable types
        typeEnvironmentStack.removeAll()
        pushTypeEnvironment()

        let converter = SourceLocationConverter(file: fileName, tree: sf)
        let b = BytecodeBuilder(fileName: fileName, converter: converter)
        
        func locFor(_ node: SyntaxProtocol) -> SourceLoc {
            let start = converter.location(for: node.positionAfterSkippingLeadingTrivia)
            let end   = converter.location(for: node.endPositionBeforeTrailingTrivia)
            return .init(
                file: start.file ?? fileName,
                line: start.line ?? 0, column: start.column ?? 0,
                endLine: end.line ?? (start.line ?? 0), endColumn: end.column ?? (start.column ?? 0)
            )
        }
        
        // ----- Pass 1: collect structs -----
        var typesToDefine: [(name: String, fields: [String], decl: StructDeclSyntax)] = []
        var fieldsByType: [String: Set<String>] = [:]
        // Collect struct methods: (type, funcDecl)
        struct StructMethodDecl { let typeName: String; let decl: FunctionDeclSyntax }
        var structMethods: [StructMethodDecl] = []
        for item in sf.statements {
            if let decl = item.item.as(StructDeclSyntax.self),
               let t = try collectStruct(decl) {
                typesToDefine.append((t.name, t.fields, decl))
                fieldsByType[t.name] = Set(t.fields)
                // Collect function members as methods
                for member in decl.memberBlock.members {
                    if let fd = member.decl.as(FunctionDeclSyntax.self), fd.body != nil {
                        structMethods.append(.init(typeName: t.name, decl: fd))
                    }
                }
            }
        }
        // Persist field order per type for constructor synthesis
        self.collectedFieldsByType = Dictionary(uniqueKeysWithValues: typesToDefine.map { ($0.name, $0.fields) })
        let knownTypes = Set(typesToDefine.map(\.name))
        
        // ----- Pass 1b: collect functions -----
        struct Fn { let name: String; let params: [String]; let body: CodeBlockSyntax; let decl: FunctionDeclSyntax }
        var functions: [Fn] = []
        for item in sf.statements {
            guard let f = item.item.as(FunctionDeclSyntax.self),
                  let body = f.body else { continue }
            let name = f.name.text
            let params = f.signature.parameterClause.parameters.enumerated().map { paramLocalName($0, $1) }
            functions.append(.init(name: name, params: params, body: body, decl: f))
        }
        let knownFunctions = Set(functions.map(\.name))
        
        // ----- Codegen -----
        
        // Slot-mode: prepare global slot scope before compiling functions so functions can resolve globals
        var globalBindingNames: [String] = []
        var nTopSlots: Int = 0
        var topEnterIndex: Int? = nil
        if options.mode == .slot {
            for item in sf.statements {
                if let vd = item.item.as(VariableDeclSyntax.self) {
                    let spec = vd.bindingSpecifier.text
                    if spec == "var" || spec == "let" {
                        // Detect wrapper attribute textually to add backing names for top-level slots
                        let text = vd.description
                        var wrapperType: String? = nil
                        if let at = text.firstIndex(of: "@") {
                            var i = text.index(after: at); var nameBuf = ""
                            while i < text.endIndex {
                                let ch = text[i]
                                if ch.isLetter { nameBuf.append(ch); i = text.index(after: i); continue }
                                break
                            }
                            if !nameBuf.isEmpty { wrapperType = nameBuf }
                        }
                        for binding in vd.bindings {
                            if let id = binding.pattern.as(IdentifierPatternSyntax.self) {
                                if wrapperType != nil { globalBindingNames.append("_" + id.identifier.text) }
                                else { globalBindingNames.append(id.identifier.text) }
                            }
                        }
                    }
                }
            }
            if !globalBindingNames.isEmpty {
                slot?.push(total: globalBindingNames.count)
                for name in globalBindingNames { _ = try? slot?.declare(name, isConst: false) }
            }
            nTopSlots = globalBindingNames.count
            if nTopSlots > 0 {
                // Ensure the runtime frame exists before any top-level expression executes (e.g., closure captures)
                topEnterIndex = b.emit(.enterScope(nLocals: nTopSlots), at: sf)
            }
        }

        // Jump over function bodies at runtime
        let jumpIndex = b.emit(.jump(0), at: sf)
        
        // Emit function bodies (each ends with implicit return true if none)
        var functionEntries: [(name: String, params: [String], entry: Int, decl: FunctionDeclSyntax, localCount: Int)] = []
        // Emit struct method bodies similarly, with implicit first param 'self'
        struct CompiledMethod { let typeName: String; let selector: String; let params: [String]; let entry: Int; let localCount: Int; let decl: FunctionDeclSyntax; let isMutating: Bool }
        var compiledMethods: [CompiledMethod] = []
        for fn in functions {
            let compiled = try compileCallableBody(params: fn.params, body: fn.body, kind: "function", name: fn.name, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b, noteName: fn.name)
            functionEntries.append((fn.name, fn.params, compiled.entry, fn.decl, compiled.localCount))
        }

        // Compile struct methods via unified callable path
        for m in structMethods {
            guard let body = m.decl.body else { continue }
            let name = m.decl.name.text
            // External labels for canonical selector
            let labels: [String] = m.decl.signature.parameterClause.parameters.map { p in
                let label = p.firstName.text
                return label.isEmpty ? "_" : label
            }
            func selectorString(_ name: String, labels: [String]) -> String {
                if labels.isEmpty { return name + "()" }
                return name + "(" + labels.map { $0 + ":" }.joined() + ")"
            }
            let selector = selectorString(name, labels: labels)
            // Collision rule: forbid property `p` and zero-arg method `p()` on the same type
            if labels.isEmpty, let fields = fieldsByType[m.typeName], fields.contains(name) {
                throw CompileError.message("Collision: property '\(name)' and zero-arg method '\(name)()' on type \(m.typeName)")
            }
            // Local parameter names (including implicit self first)
            var params: [String] = ["self"]
            params.append(contentsOf: m.decl.signature.parameterClause.parameters.enumerated().map { paramLocalName($0, $1) })
            let note = "\(m.typeName).\(selector)"
            let compiled = try compileCallableBody(params: params, body: body, kind: "method", name: note, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b, noteName: note, implicitSelf: .init(typeName: m.typeName, fieldNames: fieldsByType[m.typeName] ?? []))
            let mut = detectSelfMutation(in: body)
            compiledMethods.append(.init(typeName: m.typeName, selector: selector, params: params, entry: compiled.entry, localCount: compiled.localCount, decl: m.decl, isMutating: mut))
        }
        
        // Patch the prologue jump to land just after function bodies
        let afterFuncs = b.count
        b.patch(jumpIndex, to: .jump(afterFuncs - (jumpIndex + 1)))
        
        // Define types & functions
        for t in typesToDefine {
            _ = b.internType(t.name) // F1d: ensure user type name is interned for stable TypeID
            b.emit(.defineType(name: t.name, fields: t.fields), at: t.decl)
        }
        for fe in functionEntries {
            b.emit(.defineFunction(name: fe.name, params: fe.params, entry: fe.entry, localCount: fe.localCount), at: fe.decl)
        }
        // Build method table entries in builder
        for cm in compiledMethods {
            b.addUserMethod(typeName: cm.typeName, selector: cm.selector, entry: cm.entry, params: cm.params, localCount: cm.localCount, mutating: cm.isMutating)
        }
        
        // Top-level statements (skip struct/func bodies)
        if options.mode == .slot {
            let nTop = nTopSlots
            // enterScope already emitted above if needed
            for item in sf.statements {
                let node = item.item
                if node.is(StructDeclSyntax.self) || node.is(FunctionDeclSyntax.self) { continue }
                try compileStatementNode(node, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            }
            if nTop > 0 {
                let leaveIdx = b.emit(.leaveScope, at: sf)
                if let enterIdx = topEnterIndex, let slotRes = slot, let scope = slotRes.scopes.last {
                    var bySlot: [Int:String] = [:]
                    for (name, info) in scope.map {
                        bySlot[info.slot] = name
                    }
                    b.addLocalsMap(start: enterIdx, end: leaveIdx, depth: 0, namesBySlot: bySlot)
                    b.addScopeNote(start: enterIdx, end: leaveIdx, kind: "topLevel", name: nil)
                }
                slot?.pop()
            }
        } else {
            for item in sf.statements {
                let node = item.item
                if node.is(StructDeclSyntax.self) || node.is(FunctionDeclSyntax.self) { continue }
                try compileStatementNode(node, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            }
        }
        
        let sidecar = DebugSidecar(scopeNotes: b.scopeNotes, localsMaps: b.localsMaps)
        // Generate a stable-ish program id via FNV-1a 64-bit over file+source+counts
        func fnv1a64(_ data: Data) -> UInt64 {
            var hash: UInt64 = 0xcbf29ce484222325
            let prime: UInt64 = 0x100000001b3
            for byte in data { hash ^= UInt64(byte); hash = hash &* prime }
            return hash
        }
        var hasherInput = Data()
        hasherInput.append(fileName.data(using: .utf8) ?? Data())
        hasherInput.append(source.data(using: .utf8) ?? Data())
        withUnsafeBytes(of: UInt64(b.ops.count)) { hasherInput.append(contentsOf: $0) }
        withUnsafeBytes(of: UInt64(b.selectorTable.count)) { hasherInput.append(contentsOf: $0) }
        withUnsafeBytes(of: UInt64(b.typeTable.count)) { hasherInput.append(contentsOf: $0) }
        withUnsafeBytes(of: UInt64(b.propertyTable.count)) { hasherInput.append(contentsOf: $0) }
        let pid = fnv1a64(hasherInput)
        return Program(ops: b.ops,
                       locs: b.locs,
                       source: source,
                       file: fileName,
                       debug: sidecar,
                       typeTable: b.typeTable,
                       selectorTable: b.selectorTable,
                       propertyTable: b.propertyTable,
                       id: pid,
                       userMethods: b.userMethods)
    }
    
//    public func compile(source: String) throws -> [OpCode] {
//        
//    }
//    
    private func paramLocalName(_ index: Int, _ p: FunctionParameterSyntax) -> String {
        // Prefer secondName (internal), else firstName; replace '_' with a generated name
        if let second = p.secondName, second.text != "_" { return second.text }
        let first = p.firstName.text
        return first == "_" ? "arg\(index)" : first
    }

    // Unified compilation path for functions and methods
    private func compileCallableBody(
        params: [String],
        body: CodeBlockSyntax,
        kind: String,                // "function" | "method"
        name: String?,               // for scope note
        knownTypes: Set<String>,
        knownFunctions: Set<String>,
        builder b: BytecodeBuilder,
        noteName: String?,
        implicitSelf: ImplicitSelfContext? = nil
    ) throws -> (entry: Int, localCount: Int) {
        let prevImplicit = implicitSelfContext
        implicitSelfContext = implicitSelf
        defer { implicitSelfContext = prevImplicit }
        // Push initial name-mode bindings (params)
        if options.mode == .name {
            nameBindingsStack.append(Set(params))
        }
        // Push type env frame; for methods, seed 'self'
        typeBindingsStack.append([:])
        if let ctx = implicitSelf {
            if let first = params.first, first == "self" {
                typeBindingsStack[typeBindingsStack.count - 1]["self"] = TypeBinding(kind: .value, typeName: ctx.typeName)
            }
        }
        // Push wrapper locals frame
        wrapperLocalStack.append([:])
        defer {
            if options.mode == .name { _ = nameBindingsStack.popLast() }
            _ = typeBindingsStack.popLast()
            _ = wrapperLocalStack.popLast()
        }
        // Compute locals in slot-mode
        let topBindings = (options.mode == .slot) ? countBindings(body) : 0
        var localCount = (options.mode == .slot) ? (params.count + topBindings) : 0
        if options.mode == .slot && localCount == 0 {
            // Ensure slot-mode functions still allocate a locals frame so
            // captured globals resolve at the expected depth even when the body
            // introduces no new bindings.
            localCount = 1
        }
        if options.mode == .slot, let slotRes = slot {
            slotRes.push(total: localCount)
            // Predeclare params in slots [0 ..< params.count]
            for p in params { _ = try? slotRes.declare(p, isConst: false) }
        }
        let entry = b.count
        let prev = slotResolutionEnabled
        if options.mode == .slot { slotResolutionEnabled = true }
        try compileCodeBlock(body, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b, treatAsTopScope: true)
        slotResolutionEnabled = prev
        b.emit(.loadConst(.bool(true)), at: body)
        b.emit(.ret, at: body)
        if options.mode == .slot && localCount > 0, let slotRes = slot, let scope = slotRes.scopes.last {
            let endIP = b.count - 1
            var bySlot: [Int: String] = [:]
            for (name, info) in scope.map { bySlot[info.slot] = name }
            b.addLocalsMap(start: entry, end: endIP, depth: 0, namesBySlot: bySlot)
            b.addScopeNote(start: entry, end: endIP, kind: kind, name: noteName)
        }
        if options.mode == .slot { slot?.pop() }
        return (entry, localCount)
    }

    // Simple mutability detector: returns true if body contains assignment to self.* on LHS
    private func detectSelfMutation(in body: CodeBlockSyntax) -> Bool {
        func assignsSelf(_ node: Syntax) -> Bool {
            if let infix = node.as(InfixOperatorExprSyntax.self), infix.operator.is(AssignmentExprSyntax.self) {
                // LHS could be member access with base 'self'
                if let mem = infix.leftOperand.as(MemberAccessExprSyntax.self) {
                    if let baseIdent = mem.base?.as(IdentifierExprSyntax.self), baseIdent.identifier.text == "self" { return true }
                    if let baseDecl = mem.base?.as(DeclReferenceExprSyntax.self), baseDecl.baseName.text == "self" { return true }
                }
            }
            if let seq = node.as(SequenceExprSyntax.self) {
                let elems = Array(seq.elements)
                if elems.count >= 3, elems[1].is(AssignmentExprSyntax.self) {
                    if let mem = elems[0].as(MemberAccessExprSyntax.self) {
                        if let baseIdent = mem.base?.as(IdentifierExprSyntax.self), baseIdent.identifier.text == "self" { return true }
                        if let baseDecl = mem.base?.as(DeclReferenceExprSyntax.self), baseDecl.baseName.text == "self" { return true }
                    }
                }
            }
            return false
        }
        for item in body.statements {
            let s = Syntax(item.item)
            if assignsSelf(s) { return true }
            // shallow scan inside expression statements and if/while bodies
            if let es = s.as(ExpressionStmtSyntax.self) {
                if assignsSelf(Syntax(es.expression)) { return true }
            }
            if let ife = s.as(IfExprSyntax.self) {
                if assignsSelf(Syntax(ife.body)) { return true }
                if let eb = ife.elseBody?.as(CodeBlockSyntax.self), assignsSelf(Syntax(eb)) { return true }
            }
            if let ws = s.as(WhileStmtSyntax.self) {
                if assignsSelf(Syntax(ws.body)) { return true }
            }
        }
        return false
    }
    
    // MARK: - Collect struct(name, fields)
    
    private func collectStruct(_ decl: StructDeclSyntax) throws -> (name: String, fields: [String])? {
        let name = decl.name.text
        var fields: [String] = []
        
        // Look for stored properties (`var` or `let`) at top-level of the struct.
        // Skip computed properties that have an accessor block (get/set) — only include actual stored fields.
        for member in decl.memberBlock.members {
            guard let vd = member.decl.as(VariableDeclSyntax.self) else { continue }
            let spec = vd.bindingSpecifier.text
            guard spec == "var" || spec == "let" else { continue }

            // Detect a wrapper attribute on this variable declaration (applies to all its bindings for M1/M5)
            let wrapperType: String? = {
                // Prefer structured attribute, fall back to textual scan for resilience across SwiftSyntax variants
                // Textual scan: look for @Name before the binding specifier
                let text = vd.description
                if let at = text.firstIndex(of: "@") {
                    var i = text.index(after: at)
                    var nameBuf = ""
                    while i < text.endIndex {
                        let ch = text[i]
                        if ch.isLetter { nameBuf.append(ch) ; i = text.index(after: i); continue }
                        break
                    }
                    if !nameBuf.isEmpty { return nameBuf }
                }
                return nil
            }()
            if let w = wrapperType, spec == "let" {
                throw CompileError.message("Property wrapper '@\(w)' requires 'var' storage on type \(name)")
            }
            // Parse wrapper attribute argument list if present
            let attrArgs: [(label: String?, expr: ExprSyntax)] = {
                guard let w = wrapperType else { return [] }
                return extractWrapperAttributeArgsPairs(from: vd, wrapperName: w) ?? []
            }()

            for b in vd.bindings {
                // Computed property: has accessor block (getter/setter); not a stored field.
                if b.accessorBlock != nil { continue }
                if let id = b.pattern.as(IdentifierPatternSyntax.self) {
                    let propName = id.identifier.text
                    if let w = wrapperType {
                        let backing = "_" + propName
                        fields.append(backing)
                        var byProp = wrappersByType[name] ?? [:]
                        byProp[propName] = WrapperDescriptor(wrapperType: w, backingName: backing, defaultExpr: b.initializer?.value, attrArgs: attrArgs)
                        wrappersByType[name] = byProp
                    } else {
                        fields.append(propName)
                        if let def = b.initializer?.value {
                            var map = storedDefaultsByType[name] ?? [:]
                            map[propName] = def
                            storedDefaultsByType[name] = map
                        }
                    }
                }
            }
        }
        
        // Only register if we found at least one field (you could allow empty as well)
        return fields.isEmpty ? nil : (name, fields)
    }
    
    // MARK: - var x = expr
    
    private func compileVariableDecl(
        _ decl: VariableDeclSyntax,
        knownTypes: Set<String>,
        knownFunctions: Set<String>,
        builder b: BytecodeBuilder
    ) throws {
        let spec = decl.bindingSpecifier.text
        guard spec == "var" || spec == "let" else {
            throw CompileError.unsupported("Only 'var' and 'let' are supported in variable declarations")
        }
        // Detect wrapper attribute applied to this declaration (all bindings)
        let declWrapperType: String? = {
            let text = decl.description
            if let at = text.firstIndex(of: "@") {
                var i = text.index(after: at); var buf = ""
                while i < text.endIndex {
                    let ch = text[i]
                    if ch.isLetter { buf.append(ch); i = text.index(after: i); continue }
                    break
                }
                if !buf.isEmpty { return buf }
            }
            return nil
        }()
        if let w = declWrapperType, spec == "let" {
            throw CompileError.message("Property wrapper '@\(w)' requires 'var' storage")
        }
        for binding in decl.bindings {
            // Support discard binding: `let _ = expr` or `var _ = expr` and wildcard pattern
            if let wildcard = binding.pattern.as(WildcardPatternSyntax.self) {
                guard let initClause = binding.initializer else {
                    throw CompileError.unsupported("discard binding requires initializer")
                }
                try compileExpr(initClause.value, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                b.emit(.pop, at: wildcard)
                continue
            }
            guard let pattern = binding.pattern.as(IdentifierPatternSyntax.self) else {
                throw CompileError.unsupported("Only simple identifiers are supported in var decl")
            }
            let name = pattern.identifier.text
            if name == "_" {
                guard let initClause = binding.initializer else {
                    throw CompileError.unsupported("discard binding requires initializer")
                }
                try compileExpr(initClause.value, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                b.emit(.pop, at: pattern)
                continue
            }
            // Disallow shadowing 'self' inside method bodies
            if implicitSelfContext != nil && name == "self" {
                throw CompileError.message("cannot redeclare 'self' in method scope")
            }
            // Handle local/global wrapper variable: @Wrapper var x = expr
            if let w = declWrapperType {
                guard let initClause = binding.initializer else {
                    throw CompileError.unsupported("@\(w) wrapped variable \(name) requires initializer")
                }
                let tid = b.internType(w)
                b.emit(.loadConst(.metatype(tid)), at: initClause)
                let args = extractWrapperAttributeArgsPairs(from: decl, wrapperName: w) ?? []
                let selString = buildWrapperInitSelectorString(from: args)
                let sel = b.internSelector(selString)
                for pair in args { try compileExpr(pair.expr, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b) }
                try compileExpr(initClause.value, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                b.emit(.callMethod(selector: sel, argc: 1 + args.count + 1), at: initClause)
                let backing = "_" + name
                if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, slotRes.hasActiveScope {
                    if let hit = slotRes.resolve(backing), hit.depth == 0 {
                        b.emit(.defLocal(slot: hit.slot, isConst: false), at: pattern)
                    } else {
                        let s = try slotRes.declare(backing, isConst: false)
                        b.emit(.defLocal(slot: s, isConst: false), at: pattern)
                    }
                } else {
                    b.emit(.defVar(backing), at: pattern)
                    if options.mode == .name {
                        if var top = nameBindingsStack.popLast() { top.insert(backing); nameBindingsStack.append(top) }
                    }
                }
                if var top = wrapperLocalStack.popLast() {
                    top[name] = LocalWrapperDescriptor(wrapperType: w, backingName: backing)
                    wrapperLocalStack.append(top)
                }
                continue
            }
            guard let initClause = binding.initializer else {
                throw CompileError.unsupported("\(spec) \(name) requires initializer")
            }
            // Compile initializer and record simple type info for wrapped-property rewrites
            try compileExpr(initClause.value, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)

            // B3.5: Track variable type for type inference
            if let inferredType = inferExpressionType(initClause.value, knownTypes: knownTypes) {
                registerVariableType(name: name, type: inferredType)
            }

            if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, slotRes.hasActiveScope {
                if let hit = slotRes.resolve(name), hit.depth == 0 {
                    // Predeclared (e.g., global or function param mapped); just initialize
                    b.emit(.defLocal(slot: hit.slot, isConst: spec == "let"), at: pattern)
                } else {
                    let s = try slotRes.declare(name, isConst: spec == "let")
                    b.emit(.defLocal(slot: s, isConst: spec == "let"), at: pattern)
                }
            } else {
                if spec == "var" { b.emit(.defVar(name), at: pattern) }
                else { b.emit(.defLet(name), at: pattern) }
                if options.mode == .name {
                    if var top = nameBindingsStack.popLast() { top.insert(name); nameBindingsStack.append(top) }
                }
                // Record type for this binding if RHS is a known-type constructor call
                if let tname = inferInitializerTypeName(initClause.value, knownTypes: knownTypes) {
                    if var top = typeBindingsStack.popLast() {
                        top[name] = TypeBinding(kind: .value, typeName: tname)
                        typeBindingsStack.append(top)
                    }
                }
            }
        }
    }

    // Infer a simple type name from an initializer expression (e.g., TypeName(...))
    private func inferInitializerTypeName(_ expr: ExprSyntax, knownTypes: Set<String>) -> String? {
        if let call = expr.as(FunctionCallExprSyntax.self) {
            if let base = calleeBaseName(from: call.calledExpression), knownTypes.contains(base) { return base }
            if let typeExpr = call.calledExpression.as(TypeExprSyntax.self),
               let identType = typeExpr.type.as(IdentifierTypeSyntax.self) {
                let n = identType.name.text
                if knownTypes.contains(n) { return n }
            }
        }
        return nil
    }

    // Infer a base expression's type name using local type bindings and simple constructor patterns
    private func inferExprTypeName(_ expr: ExprSyntax, knownTypes: Set<String>) -> String? {
        if let ident = expr.as(IdentifierExprSyntax.self) { return resolveValueTypeBinding(ident.identifier.text) }
        if let ref = expr.as(DeclReferenceExprSyntax.self) { return resolveValueTypeBinding(ref.baseName.text) }
        if let opt = expr.as(OptionalChainingExprSyntax.self) { return inferExprTypeName(opt.expression, knownTypes: knownTypes) }
        if let call = expr.as(FunctionCallExprSyntax.self) {
            if let base = calleeBaseName(from: call.calledExpression), knownTypes.contains(base) { return base }
        }
        return nil
    }

    private func hasNameBindingInScope(_ name: String) -> Bool {
        if options.mode == .slot, slotResolutionEnabled, let slotRes = slot, slotRes.resolve(name) != nil {
            return true
        }
        if options.mode == .name {
            for set in nameBindingsStack.reversed() {
                if set.contains(name) { return true }
            }
        }
        return false
    }

    private func canonicalTypeCandidate(for rawName: String, knownTypes: Set<String>?) -> String? {
        if rawName == "_" { return nil }
        if rawName == "self" { return nil }
        if let alias = resolveTypeAliasBinding(rawName) { return alias }
        if let knownTypes, knownTypes.contains(rawName) { return rawName }
        if startsWithUppercase(rawName) && !hasNameBindingInScope(rawName) {
            return rawName
        }
        return nil
    }

    private func canonicalTypeName(from typeSyntax: TypeSyntax) -> String {
        let raw = typeSyntax.description
        let collapsed = raw.split(whereSeparator: { $0.isWhitespace }).joined()
        if !collapsed.isEmpty { return collapsed }
        return raw.trimmingCharacters(in: .whitespacesAndNewlines)
    }

    private func seedBuiltinTypeBindings() {
        guard var root = typeBindingsStack.popLast() else { return }
        for (alias, canonical) in builtinTypeAliases {
            if root[alias] == nil {
                root[alias] = TypeBinding(kind: .typeAlias, typeName: canonical)
            }
        }
        typeBindingsStack.append(root)
    }

    private func typeNameIfMetatypeExpr(_ expr: ExprSyntax, knownTypes: Set<String>?) -> String? {
        if let typeExpr = expr.as(TypeExprSyntax.self) {
            return canonicalTypeName(from: typeExpr.type)
        }
        if let ident = expr.as(IdentifierExprSyntax.self) {
            return canonicalTypeCandidate(for: ident.identifier.text, knownTypes: knownTypes)
        }
        if let ref = expr.as(DeclReferenceExprSyntax.self) {
            return canonicalTypeCandidate(for: ref.baseName.text, knownTypes: knownTypes)
        }
        return nil
    }

    private func emitMetatypeIfTypeLike(_ expr: ExprSyntax,
                                        knownTypes: Set<String>?,
                                        builder b: BytecodeBuilder) -> Bool {
        guard let typeName = typeNameIfMetatypeExpr(expr, knownTypes: knownTypes) else { return false }
        let tid = b.internType(typeName)
        b.emit(.loadConst(.metatype(tid)), at: expr)
        return true
    }

    private func collectCallArguments(_ call: FunctionCallExprSyntax) -> [CallArgument] {
        var result: [CallArgument] = call.arguments.map {
            CallArgument(label: $0.label?.text, expression: ExprSyntax($0.expression), isTrailing: false)
        }
        if let trailing = call.trailingClosure {
            result.append(CallArgument(label: nil, expression: ExprSyntax(trailing), isTrailing: true))
        }
        let additional = call.additionalTrailingClosures
        for element in additional {
            result.append(CallArgument(label: element.label.text, expression: ExprSyntax(element.closure), isTrailing: true))
        }
        
        return result
    }

    private func canonicalConstantTypeName(for raw: String) -> String {
        if let alias = builtinTypeAliases[raw] { return alias }
        if let last = raw.split(separator: ".").last {
            let lastString = String(last)
            return builtinTypeAliases[lastString] ?? lastString
        }
        return raw
    }

    private func metatypeStaticConstant(typeName: String, member: String) -> Value? {
        let canonical = canonicalConstantTypeName(for: typeName)
        switch canonical {
        case "Int":
            switch member {
            case "max": return .int(Int.max)
            case "min": return .int(Int.min)
            case "bitWidth": return .int(Int.bitWidth)
            case "zero": return .int(0)
            case "isSigned": return .bool(Int.isSigned)
            default: return nil
            }
        case "UInt":
            switch member {
            case "max": return .uint(UInt64(UInt.max))
            case "min": return .uint(UInt64(UInt.min))
            case "bitWidth": return .int(UInt.bitWidth)
            case "zero": return .uint(0)
            case "isSigned": return .bool(false)
            default: return nil
            }
        case "UInt8":
            switch member {
            case "max": return .uint(UInt64(UInt8.max))
            case "min": return .uint(UInt64(UInt8.min))
            case "bitWidth": return .int(UInt8.bitWidth)
            case "zero": return .uint(0)
            case "isSigned": return .bool(false)
            default: return nil
            }
        case "UInt16":
            switch member {
            case "max": return .uint(UInt64(UInt16.max))
            case "min": return .uint(UInt64(UInt16.min))
            case "bitWidth": return .int(UInt16.bitWidth)
            case "zero": return .uint(0)
            case "isSigned": return .bool(false)
            default: return nil
            }
        case "UInt32":
            switch member {
            case "max": return .uint(UInt64(UInt32.max))
            case "min": return .uint(UInt64(UInt32.min))
            case "bitWidth": return .int(UInt32.bitWidth)
            case "zero": return .uint(0)
            case "isSigned": return .bool(false)
            default: return nil
            }
        case "UInt64":
            switch member {
            case "max": return .uint(UInt64.max)
            case "min": return .uint(UInt64.min)
            case "bitWidth": return .int(UInt64.bitWidth)
            case "zero": return .uint(0)
            case "isSigned": return .bool(false)
            default: return nil
            }
        case "Double":
            switch member {
            case "greatestFiniteMagnitude": return .double(Double.greatestFiniteMagnitude)
            case "leastNormalMagnitude": return .double(Double.leastNormalMagnitude)
            case "leastNonzeroMagnitude": return .double(Double.leastNonzeroMagnitude)
            case "infinity": return .double(Double.infinity)
            case "pi": return .double(Double.pi)
            default: return nil
            }
        case "Float":
            switch member {
            case "greatestFiniteMagnitude": return .double(Double(Float.greatestFiniteMagnitude))
            case "leastNormalMagnitude": return .double(Double(Float.leastNormalMagnitude))
            case "leastNonzeroMagnitude": return .double(Double(Float.leastNonzeroMagnitude))
            case "infinity": return .double(Double(Float.infinity))
            case "pi": return .double(Double(Float.pi))
            default: return nil
            }
        case "Float16":
            switch member {
            case "greatestFiniteMagnitude": return .double(Double(Float16.greatestFiniteMagnitude))
            case "leastNormalMagnitude": return .double(Double(Float16.leastNormalMagnitude))
            case "leastNonzeroMagnitude": return .double(Double(Float16.leastNonzeroMagnitude))
            case "infinity": return .double(Double(Float16.infinity))
            case "pi": return .double(Double(Float16.pi))
            default: return nil
            }
        default:
            return nil
        }
    }

    private func emitMetatypeStaticConstantIfPossible(_ baseExpr: ExprSyntax,
                                                      propertyName: String,
                                                      knownTypes: Set<String>?,
                                                      builder b: BytecodeBuilder,
                                                      at node: MemberAccessExprSyntax) -> Bool {
        guard let typeName = typeNameIfMetatypeExpr(baseExpr, knownTypes: knownTypes) else { return false }
        guard let value = metatypeStaticConstant(typeName: typeName, member: propertyName) else { return false }
        b.emit(.loadConst(value), at: node)
        return true
    }
    private func resolveBinding(_ name: String, expectedKind: TypeBindingKind? = nil) -> TypeBinding? {
        for frame in typeBindingsStack.reversed() {
            if let binding = frame[name] {
                if let expected = expectedKind, binding.kind != expected { continue }
                return binding
            }
        }
        return nil
    }
    private func resolveValueTypeBinding(_ name: String) -> String? {
        resolveBinding(name, expectedKind: .value)?.typeName
    }
    private func resolveTypeAliasBinding(_ name: String) -> String? {
        resolveBinding(name, expectedKind: .typeAlias)?.typeName
    }
    private func resolveWrapperLocal(_ name: String) -> LocalWrapperDescriptor? {
        for frame in wrapperLocalStack.reversed() { if let d = frame[name] { return d } }
        return nil
    }

    // Extract wrapper attribute argument pairs from a variable declaration's textual form.
    // Returns nil if parsing fails; on success returns [(label?, expr)].
    private func extractWrapperAttributeArgsPairs(from decl: VariableDeclSyntax, wrapperName: String) -> [(label: String?, expr: ExprSyntax)]? {
        let text = decl.description
        guard let atRange = text.range(of: "@" + wrapperName) else { return [] }
        var i = atRange.upperBound
        // Skip whitespace
        while i < text.endIndex, text[i].isWhitespace { i = text.index(after: i) }
        guard i < text.endIndex, text[i] == "(" else { return [] }
        // Find matching ')'
        var depth = 0
        var j = i
        while j < text.endIndex {
            let ch = text[j]
            if ch == "(" { depth += 1 }
            if ch == ")" { depth -= 1; if depth == 0 { break } }
            j = text.index(after: j)
        }
        guard j < text.endIndex else { return [] }
        let argsText = String(text[text.index(after: i)..<j])
        // Parse argsText into a tuple via SwiftParser, then extract labeled elements
        #if canImport(SwiftParser)
        let src = "let __pw = (" + argsText + ")\n"
        let root = Parser.parse(source: src)
        guard let sf = root.as(SourceFileSyntax.self) else { return [] }
        for item in sf.statements {
            if let vd = item.item.as(VariableDeclSyntax.self) {
                for b in vd.bindings {
                    if let initClause = b.initializer, let tuple = initClause.value.as(TupleExprSyntax.self) {
                        var out: [(String?, ExprSyntax)] = []
                        for el in tuple.elements { out.append((el.label?.text, el.expression)) }
                        return out
                    }
                }
            }
        }
        return []
        #else
        return []
        #endif
    }

    // Build a selector for wrapper init given attribute arg labels
    private func buildWrapperInitSelectorString(from args: [(label: String?, expr: ExprSyntax)]) -> String {
        let labels: [String] = args.map { $0.label ?? "_" } + ["wrappedValue"]
        return "init(" + labels.map { $0 + ":" }.joined() + ")"
    }
    
    // MARK: - Expressions
    
    // MARK: - Expressions

    private func compileExpr(
        _ expr: ExprSyntax,
        knownTypes: Set<String>?,
        knownFunctions: Set<String>?,
        builder b: BytecodeBuilder
    ) throws {
        // literals / identifiers unchanged...
        if let intLit = expr.as(IntegerLiteralExprSyntax.self) {
            guard let val = Int(intLit.literal.text) else {
                throw CompileError.message("Bad integer literal: \(intLit.literal.text)")
            }
            b.emit(.loadConst(.int(val)), at: intLit)
            return
        }

        // String literal (with interpolation support)
        if let strLit = expr.as(StringLiteralExprSyntax.self) {
            let segments = Array(strLit.segments)
            // Fast-path: no interpolation segments, emit a single constant
            let allPlain = segments.allSatisfy { $0.is(StringSegmentSyntax.self) }
            if allPlain {
                let s = segments.reduce(into: "") { acc, seg in
                    if let cont = seg.as(StringSegmentSyntax.self) { acc += cont.content.text }
                }
                b.emit(.loadConst(.string(s)), at: strLit)
                return
            }

            // General case: fold segments via string concatenation.
            // Start with empty string accumulator to force string context.
            b.emit(.loadConst(.string("")), at: strLit)
            for seg in segments {
                if let cont = seg.as(StringSegmentSyntax.self) {
                    let text = cont.content.text
                    if !text.isEmpty {
                        b.emit(.loadConst(.string(text)), at: cont)
                        b.emit(.add, at: cont)
                    }
                    continue
                }
                // Expression segment: compile inner expression then add
                if let exprSeg = seg.as(ExpressionSegmentSyntax.self) {
                    // Prefer the primary interpolation expression from expressions list; fall back to widest descendant.
                    let args = Array(exprSeg.expressions)
                    let chosen: ExprSyntax? = args.first?.expression
                    if let e = chosen ?? largestExprDescendant(of: Syntax(exprSeg)) {
                        try compileExpr(e, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    } else {
                        // Defensive: treat as empty
                        b.emit(.loadConst(.string("")), at: exprSeg)
                    }
                    b.emit(.add, at: exprSeg)
                    continue
                }
                // Unknown segment kind: best-effort—if it contains an expression child, use it; else treat as empty
                if let e = largestExprDescendant(of: Syntax(seg)) {
                    try compileExpr(e, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    b.emit(.add, at: seg)
                }
            }
            return
        }

        // Array literal: [e1, e2, ...]
        if let arr = expr.as(ArrayExprSyntax.self) {
            let elements = Array(arr.elements)
            for el in elements {
                try compileExpr(el.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            }
            b.emit(.makeArray(count: elements.count), at: arr)
            return
        }

        // Dictionary literal: [k1: v1, k2: v2, ...] or [:]
        if let dict = expr.as(DictionaryExprSyntax.self) {
            switch dict.content {
            case .elements(let list):
                let elements = Array(list)
                for el in elements {
                    // compile key then value; keys must evaluate to String at runtime
                    try compileExpr(el.key, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    try compileExpr(el.value, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                }
                b.emit(.makeDict(count: elements.count), at: dict)
                return
            case .colon:
                // empty dictionary [:]
                b.emit(.makeDict(count: 0), at: dict)
                return
            @unknown default:
                throw CompileError.unsupported("Unknown dictionary literal form")
            }
        }

        // Boolean literal
        if let boolLit = expr.as(BooleanLiteralExprSyntax.self) {
            b.emit(.loadConst(.bool(boolLit.literal.text == "true")), at: boolLit)
            return
        }

        // nil literal
        if expr.is(NilLiteralExprSyntax.self) {
            b.emit(.loadConst(.nilValue), at: expr)
            return
        }

        if let (lowerExpr, isClosed, upperExpr) = parseRangeExpr(expr) {
            try compileRangeLiteral(
                lowerExpr: lowerExpr,
                isClosed: isClosed,
                upperExpr: upperExpr,
                node: expr,
                knownTypes: knownTypes,
                knownFunctions: knownFunctions,
                builder: b
            )
            return
        }

        // Closure literal: { params in ... }
        if let clo = expr.as(ClosureExprSyntax.self) {
            try compileClosureExpr(clo, knownTypes: knownTypes ?? [], knownFunctions: knownFunctions ?? [], builder: b)
            return
        }

        // Prefix operators: !, -, +
        if let pre = expr.as(PrefixOperatorExprSyntax.self) {
            let opText = pre.operatorToken.text.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
            if opText == "!" {
                try compileExpr(pre.postfixExpression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                b.emit(.logicalNot, at: pre)
                return
            } else if opText == "-" {
                try compileExpr(pre.postfixExpression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                b.emit(.neg, at: pre)
                return
            } else if opText == "+" {
                // unary plus is a no-op
                try compileExpr(pre.postfixExpression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                return
            }
            throw CompileError.unsupported("Prefix operator '\(opText)' not supported")
        }

        // Parenthesized expression: (expr) comes as TupleExprSyntax with 1 element.
        if let tuple = expr.as(TupleExprSyntax.self) {
            if tuple.elements.count == 1 {
                try compileExpr(tuple.elements.first!.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                return
            } else {
                throw CompileError.unsupported("Tuple expressions with arity != 1 are not supported")
            }
        }

        // Ternary conditional expression: cond ? then : else
        if let tern = expr.as(TernaryExprSyntax.self) {
            try compileTernaryExpr(tern, knownTypes: knownTypes ?? [], knownFunctions: knownFunctions ?? [], builder: b)
            return
        }
        // Unresolved ternary fallback across SwiftSyntax versions
        if Syntax(expr).kind == .unresolvedTernaryExpr {
            let sx = Syntax(expr)
            let parts = exprChildren(of: sx)
            if parts.count == 3 {
                // cond, then, else all present as children
                try compileExpr(parts[0], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                let jif = b.emit(.jumpIfFalse(0), at: parts[0])
                try compileExpr(parts[1], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                let jmpEnd = b.emit(.jump(0), at: expr)
                let toElse = b.count - (jif + 1)
                b.patch(jif, to: .jumpIfFalse(toElse))
                try compileExpr(parts[2], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                let toEnd = b.count - (jmpEnd + 1)
                b.patch(jmpEnd, to: .jump(toEnd))
                return
            } else if parts.count == 2 {
                // then/else are children; condition is likely previous element in parent sequence
                if let parentSeq = sx.parent?.as(SequenceExprSyntax.self) {
                    let elems = Array(parentSeq.elements)
                    if let pos = elems.firstIndex(where: { Syntax($0).id == sx.id }), pos > 0 {
                        let cond = elems[pos - 1]
                        try compileExpr(cond, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                        let jif = b.emit(.jumpIfFalse(0), at: cond)
                        try compileExpr(parts[0], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                        let jmpEnd = b.emit(.jump(0), at: expr)
                        let toElse = b.count - (jif + 1)
                        b.patch(jif, to: .jumpIfFalse(toElse))
                        try compileExpr(parts[1], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                        let toEnd = b.count - (jmpEnd + 1)
                        b.patch(jmpEnd, to: .jump(toEnd))
                        return
                    }
                }
            } else if parts.count == 1 {
                // only 'then' present; cond from parent previous, else from parent next
                if let parentSeq = sx.parent?.as(SequenceExprSyntax.self) {
                    let elems = Array(parentSeq.elements)
                    if let pos = elems.firstIndex(where: { Syntax($0).id == sx.id }), pos > 0, pos + 1 < elems.count {
                        let cond = elems[pos - 1]
                        let elseE = elems[pos + 1]
                        try compileExpr(cond, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                        let jif = b.emit(.jumpIfFalse(0), at: cond)
                        try compileExpr(parts[0], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                        let jmpEnd = b.emit(.jump(0), at: expr)
                        let toElse = b.count - (jif + 1)
                        b.patch(jif, to: .jumpIfFalse(toElse))
                        try compileExpr(elseE, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                        let toEnd = b.count - (jmpEnd + 1)
                        b.patch(jmpEnd, to: .jump(toEnd))
                        return
                    }
                }
            }
        }
        // Unresolved ternary: represented in some toolchains as a sequence element where the condition is the previous element
        if Syntax(expr).kind == .unresolvedTernaryExpr {
            let sx = Syntax(expr)
            let branches = exprChildren(of: sx)
            if let parentSeq = sx.parent?.as(SequenceExprSyntax.self) {
                let elems = Array(parentSeq.elements)
                if let pos = elems.firstIndex(where: { Syntax($0).id == sx.id }), pos > 0, branches.count >= 2 {
                    let cond = elems[pos - 1]
                    try compileExpr(cond, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    let jif = b.emit(.jumpIfFalse(0), at: cond)
                    try compileExpr(branches[0], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    let jmpEnd = b.emit(.jump(0), at: expr)
                    let toElse = b.count - (jif + 1)
                    b.patch(jif, to: .jumpIfFalse(toElse))
                    try compileExpr(branches[1], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    let toEnd = b.count - (jmpEnd + 1)
                    b.patch(jmpEnd, to: .jump(toEnd))
                    return
                }
            }
        }

        // Early textual fallback for string interpolation across SwiftSyntax variants.
        // Only trigger when the expression itself appears to be a string literal (starts with a quote),
        // to avoid matching surrounding nodes like function calls: print("...\(x)...").
        do {
            let text = expr.description
            let trimmed = text.trimmingCharacters(in: .whitespacesAndNewlines)
            if trimmed.hasPrefix("\"") && trimmed.contains("\\(") {
                // Emit: "" (+ prefix)? (+ innerExpr)?
                b.emit(.loadConst(.string("")), at: expr)
                // Try to extract a literal prefix between the opening quote and first \(
                if let q = trimmed.firstIndex(of: "\""),
                   let range = trimmed.range(of: "\\(", range: trimmed.index(after: q)..<trimmed.endIndex) {
                    let prefix = String(trimmed[trimmed.index(after: q)..<range.lowerBound])
                    if !prefix.isEmpty {
                        b.emit(.loadConst(.string(prefix)), at: expr)
                        b.emit(.add, at: expr)
                    }
                }
                // Compile first inner expression we can find under this node
                if let inner = firstExprDescendant(of: Syntax(expr)) {
                    try compileExpr(inner, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    b.emit(.add, at: expr)
                }
                return
            }
        }

        // Double literal
        if let floatLit = expr.as(FloatLiteralExprSyntax.self) {
            let raw = floatLit.literal.text.replacingOccurrences(of: "_", with: "")
            guard let d = Double(raw) else { throw CompileError.message("Bad float literal: \(floatLit.literal.text)") }
            b.emit(.loadConst(.double(d)), at: floatLit)
            return
        }

        // Identifier reference (with implicit self support inside methods)
        if let ident = expr.as(IdentifierExprSyntax.self) {
            let name = ident.identifier.text
            // Local/global wrapper variable read: x => _x.wrappedValue
            if let wd = resolveWrapperLocal(name) {
                let noteStart = b.count
                // Load backing then get wrappedValue
                if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let hit = slotRes.resolve(wd.backingName) {
                    b.emit(.loadLocal(depth: hit.depth, slot: hit.slot), at: ident)
                } else {
                    b.emit(.loadVar(wd.backingName), at: ident)
                }
                let pidWV = b.internProperty("wrappedValue")
                b.emit(.getProp(pidWV), at: ident)
                let noteEnd = b.count - 1
                b.addScopeNote(start: noteStart, end: noteEnd, kind: "wrapperAccessLocal", name: name)
                return
            }
            // Support bare $prop projection inside methods: rewrite to self._prop.projectedValue
            if name.hasPrefix("$"), let ctx = implicitSelfContext {
                let prop = String(name.dropFirst())
                if let wd = wrappersByType[ctx.typeName]?[prop] {
                    if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let selfHit = slotRes.resolve("self") {
                        b.emit(.loadLocal(depth: selfHit.depth, slot: selfHit.slot), at: ident)
                    } else {
                        b.emit(.loadVar("self"), at: ident)
                    }
                    let pidBacking = b.internProperty(wd.backingName)
                    b.emit(.getProp(pidBacking), at: ident)
                    let pidProj = b.internProperty("projectedValue")
                    b.emit(.getProp(pidProj), at: ident)
                    return
                }
                // Local/global wrapper variable: $x => _x.projectedValue
                if let wd = resolveWrapperLocal(prop) {
                    let noteStart = b.count
                    if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let hit = slotRes.resolve(wd.backingName) {
                        b.emit(.loadLocal(depth: hit.depth, slot: hit.slot), at: ident)
                    } else {
                        b.emit(.loadVar(wd.backingName), at: ident)
                    }
                    let pidProj = b.internProperty("projectedValue")
                    b.emit(.getProp(pidProj), at: ident)
                    let noteEnd = b.count - 1
                    b.addScopeNote(start: noteStart, end: noteEnd, kind: "wrapperProjectLocal", name: prop)
                    return
                }
            }
            if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let hit = slotRes.resolve(name) {
                b.emit(.loadLocal(depth: hit.depth, slot: hit.slot), at: ident)
                return
            }
            // Implicit self for fields when unresolved and not shadowed by a local binding
            let isShadowedInNameMode: Bool = {
                if options.mode == .name {
                    for set in nameBindingsStack.reversed() { if set.contains(name) { return true } }
                }
                return false
            }()
            if let ctx = implicitSelfContext, ctx.fieldNames.contains(name) && name != "self" && !isShadowedInNameMode {
                // load self
                if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let selfHit = slotRes.resolve("self") {
                    b.emit(.loadLocal(depth: selfHit.depth, slot: selfHit.slot), at: ident)
                } else {
                    b.emit(.loadVar("self"), at: ident)
                }
                let pid = b.internProperty(name)
                b.emit(.getProp(pid), at: ident)
                return
            }
            b.emit(.loadVar(name), at: ident)
            return
        }
        if let ref = expr.as(DeclReferenceExprSyntax.self) {
            // bare reference to a decl; support implicit self for fields
            let name = ref.baseName.text
            // Local/global wrapper variable read
            if let wd = resolveWrapperLocal(name) {
                let noteStart = b.count
                if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let hit = slotRes.resolve(wd.backingName) {
                    b.emit(.loadLocal(depth: hit.depth, slot: hit.slot), at: ref)
                } else {
                    b.emit(.loadVar(wd.backingName), at: ref)
                }
                let pidWV = b.internProperty("wrappedValue")
                b.emit(.getProp(pidWV), at: ref)
                let noteEnd = b.count - 1
                b.addScopeNote(start: noteStart, end: noteEnd, kind: "wrapperAccessLocal", name: name)
                return
            }
            // Support bare $prop projection inside methods
            if name.hasPrefix("$"), let ctx = implicitSelfContext {
                let prop = String(name.dropFirst())
                if let wd = wrappersByType[ctx.typeName]?[prop] {
                    if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let selfHit = slotRes.resolve("self") {
                        b.emit(.loadLocal(depth: selfHit.depth, slot: selfHit.slot), at: ref)
                    } else {
                        b.emit(.loadVar("self"), at: ref)
                    }
                    let pidBacking = b.internProperty(wd.backingName)
                    b.emit(.getProp(pidBacking), at: ref)
                    let pidProj = b.internProperty("projectedValue")
                    b.emit(.getProp(pidProj), at: ref)
                    return
                }
                // Local/global wrapper variable: $x
                if let wd = resolveWrapperLocal(prop) {
                    let noteStart = b.count
                    if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let hit = slotRes.resolve(wd.backingName) {
                        b.emit(.loadLocal(depth: hit.depth, slot: hit.slot), at: ref)
                    } else {
                        b.emit(.loadVar(wd.backingName), at: ref)
                    }
                    let pidProj = b.internProperty("projectedValue")
                    b.emit(.getProp(pidProj), at: ref)
                    let noteEnd = b.count - 1
                    b.addScopeNote(start: noteStart, end: noteEnd, kind: "wrapperProjectLocal", name: prop)
                    return
                }
            }
            if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let hit = slotRes.resolve(name) {
                b.emit(.loadLocal(depth: hit.depth, slot: hit.slot), at: ref)
                return
            }
            // Implicit self for fields when unresolved and not shadowed by a local binding (name-mode)
            let isShadowedInNameMode: Bool = {
                if options.mode == .name {
                    for set in nameBindingsStack.reversed() { if set.contains(name) { return true } }
                }
                return false
            }()
            if let ctx = implicitSelfContext, ctx.fieldNames.contains(name) && name != "self" && !isShadowedInNameMode {
                // load self then get property
                if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let selfHit = slotRes.resolve("self") {
                    b.emit(.loadLocal(depth: selfHit.depth, slot: selfHit.slot), at: ref)
                } else {
                    b.emit(.loadVar("self"), at: ref)
                }
                let pid = b.internProperty(name)
                b.emit(.getProp(pid), at: ref)
                return
            }
            b.emit(.loadVar(name), at: ref)
            return
        }

        // Member access: base.prop (with optional chaining support)
        if let member = expr.as(MemberAccessExprSyntax.self) {
            guard let baseAny = member.base else {
                throw CompileError.unsupported("Member access without base not supported")
            }
            let rawProp = member.declName.baseName.text
            // Support $projection: strip leading '$' and mark as projected access
            let isProjection = rawProp.hasPrefix("$")
            let prop = isProjection ? String(rawProp.dropFirst()) : rawProp
            if !isProjection,
               emitMetatypeStaticConstantIfPossible(baseAny,
                                                    propertyName: prop,
                                                    knownTypes: knownTypes,
                                                    builder: b,
                                                    at: member) {
                return
            }
            // Resolve wrapper info when possible
            var wrapper: WrapperDescriptor? = nil
            if let knownTypes = knownTypes {
                if let baseType = inferExprTypeName(baseAny, knownTypes: knownTypes) {
                    wrapper = wrappersByType[baseType]?[prop]
                }
                // Self within methods: if base is 'self', use implicitSelfContext
                if wrapper == nil, let ctx = implicitSelfContext {
                    if let baseIdent = baseAny.as(IdentifierExprSyntax.self), baseIdent.identifier.text == "self" {
                        wrapper = wrappersByType[ctx.typeName]?[prop]
                    } else if let baseDecl = baseAny.as(DeclReferenceExprSyntax.self), baseDecl.baseName.text == "self" {
                        wrapper = wrappersByType[ctx.typeName]?[prop]
                    }
                }
            }
            if let optBase = baseAny.as(OptionalChainingExprSyntax.self) {
                // Lower: base; dup; jumpIfNil Lnil; getProp; jump Lend; Lnil: pop; nil; Lend
                let optInner = optBase.expression
                if !emitMetatypeIfTypeLike(optInner, knownTypes: knownTypes, builder: b) {
                    try compileExpr(optInner, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                }
                b.emit(.dup, at: member)
                let jf = b.emit(.jumpIfNil(0), at: member)
                if let wd = wrapper {
                    let noteStart = b.count
                    let pidBacking = b.internProperty(wd.backingName)
                    b.emit(.getProp(pidBacking), at: member)
                    let pidWV = b.internProperty(isProjection ? "projectedValue" : "wrappedValue")
                    b.emit(.getProp(pidWV), at: member)
                    let noteEnd = b.count - 1
                    b.addScopeNote(start: noteStart, end: noteEnd, kind: isProjection ? "wrapperProject" : "wrapperAccess", name: prop)
                } else {
                    let pid = b.internProperty(prop)
                    b.emit(.getProp(pid), at: member)
                }
                let jEnd = b.emit(.jump(0), at: member)
                let toNil = b.count - (jf + 1)
                b.patch(jf, to: .jumpIfNil(toNil))
                b.emit(.pop, at: member)
                b.emit(.loadConst(.nilValue), at: member)
                let toEnd = b.count - (jEnd + 1)
                b.patch(jEnd, to: .jump(toEnd))
                return
            } else {
                let emittedMetatype = emitMetatypeIfTypeLike(baseAny, knownTypes: knownTypes, builder: b)
                if !emittedMetatype {
                    try compileExpr(baseAny, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                }
                if let wd = wrapper {
                    let noteStart = b.count
                    let pidBacking = b.internProperty(wd.backingName)
                    b.emit(.getProp(pidBacking), at: member)
                    let pidWV = b.internProperty(isProjection ? "projectedValue" : "wrappedValue")
                    b.emit(.getProp(pidWV), at: member)
                    let noteEnd = b.count - 1
                    b.addScopeNote(start: noteStart, end: noteEnd, kind: isProjection ? "wrapperProject" : "wrapperAccess", name: prop)
                } else {
                    let pid = b.internProperty(prop)
                    b.emit(.getProp(pid), at: member)
                }
                return
            }
        }

        if let force = expr.as(ForceUnwrapExprSyntax.self) {
            try compileExpr(force.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            b.emit(.dup, at: force)
            let nilJump = b.emit(.jumpIfNil(0), at: force)
            let skipTrap = b.emit(.jump(0), at: force)
            let trapStart = b.count
            let toTrap = trapStart - (nilJump + 1)
            b.patch(nilJump, to: .jumpIfNil(toTrap))
            b.emit(.trap("Unexpectedly found nil while unwrapping an Optional value"), at: force)
            let toAfterTrap = b.count - (skipTrap + 1)
            b.patch(skipTrap, to: .jump(toAfterTrap))
            return
        }

        // Subscript read: base[index]
        if let sub = expr.as(SubscriptCallExprSyntax.self) {
            let base = sub.calledExpression
            let args = Array(sub.arguments)
            guard let first = args.first else { throw CompileError.unsupported("Subscript requires an index") }
            if let optBase = base.as(OptionalChainingExprSyntax.self) {
                try compileExpr(optBase.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                b.emit(.dup, at: sub)
                let jf = b.emit(.jumpIfNil(0), at: sub)
                try compileExpr(first.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                b.emit(.getIndex, at: sub)
                let jEnd = b.emit(.jump(0), at: sub)
                let toNil = b.count - (jf + 1)
                b.patch(jf, to: .jumpIfNil(toNil))
                b.emit(.pop, at: sub)
                b.emit(.loadConst(.nilValue), at: sub)
                let toEnd = b.count - (jEnd + 1)
                b.patch(jEnd, to: .jump(toEnd))
                return
            } else {
                try compileExpr(base, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                try compileExpr(first.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                b.emit(.getIndex, at: sub)
                return
            }
        }

        // If-expression (SwiftSyntax 510+ models 'if' as IfExprSyntax)
        if let ifExpr = expr.as(IfExprSyntax.self) {
            try compileIfExpr(ifExpr, knownTypes: knownTypes ?? [], knownFunctions: knownFunctions ?? [], builder: b)
            return
        }

        // Function call: print / constructor / user function / member call / dynamic callee (closure)
        if let call = expr.as(FunctionCallExprSyntax.self) {
            let callArgs = collectCallArguments(call)
            // Member call lowering: base.method(args) → push base, args; callMethod(selectorID, argc: 1+N)
            if let member = call.calledExpression.as(MemberAccessExprSyntax.self) {
                // Helper to canonicalize selector name
                func selectorString(_ name: String, labels: [String]) -> String {
                    if labels.isEmpty { return name + "()" }
                    return name + "(" + labels.map { $0 + ":" }.joined() + ")"
                }
                let labels: [String] = callArgs.map { $0.label ?? "_" }
                let methodName = member.declName.baseName.text
                // Handle Type.init(...) synthesized as host initializer if type is not a known user type
                if methodName == "init", let base = member.base?.as(TypeExprSyntax.self),
                   let identType = base.type.as(IdentifierTypeSyntax.self) {
                    let typeName = identType.name.text
                    if !(knownTypes?.contains(typeName) ?? false) {
                        let sel = b.internSelector(selectorString("init", labels: labels))
                        let tid = b.internType(typeName)
                        b.emit(.loadConst(.metatype(tid)), at: base)
                        for arg in callArgs { try compileExpr(arg.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b) }
                        b.emit(.callMethod(selector: sel, argc: 1 + callArgs.count), at: call)
                        return
                    }
                }
                // Regular instance method on receiver; optional chaining lowered via dup/jumpIfNil/pop
                if let base = member.base, let opt = base.as(OptionalChainingExprSyntax.self) {
                    try compileExpr(opt.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    b.emit(.dup, at: call)
                    let jf = b.emit(.jumpIfNil(0), at: call)
                    for arg in callArgs { try compileExpr(arg.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b) }
                    let sel = b.internSelector(selectorString(methodName, labels: labels))
                    b.emit(.callMethod(selector: sel, argc: 1 + callArgs.count), at: call)
                    let jEnd = b.emit(.jump(0), at: call)
                    let toNil = b.count - (jf + 1)
                    b.patch(jf, to: .jumpIfNil(toNil))
                    b.emit(.pop, at: call)
                    b.emit(.loadConst(.nilValue), at: call)
                    let toEnd = b.count - (jEnd + 1)
                    b.patch(jEnd, to: .jump(toEnd))
                } else {
                    // Compile receiver (base)
                    if let base = member.base { try compileExpr(base, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b) }

                    // B2: Detect closure arguments and perform type inference
                    let selector = selectorString(methodName, labels: labels)

                    // B3.5.4: Look up receiver type from type environment
                    var receiverType: TypeName? = nil
                    if let base = member.base {
                        receiverType = inferExpressionType(base, knownTypes: knownTypes)
                        debugLog("[COMPILER] Inferred receiver type: \(receiverType?.canonicalDescription() ?? "nil")")
                    }

                    // Try to look up closure signature for this method
                    var closureSignatureInfo: ClosureSignatureInfo? = nil
                    if let recType = receiverType {
                        // Look up using actual receiver type
                        let canonicalTypeName = recType.path.joined(separator: ".")
                        debugLog("[COMPILER] Looking up closure signature for type: \(canonicalTypeName), selector: \(selector)")
                        closureSignatureInfo = methodSignatureRegistry.lookupClosureSignature(
                            typeName: canonicalTypeName,
                            selector: selector
                        )
                        debugLog("[COMPILER] Found signature: \(closureSignatureInfo != nil)")
                    }


                    // Compile arguments, with special handling for closures
                    for (idx, arg) in callArgs.enumerated() {
                        if let closureExpr = arg.expression.as(ClosureExprSyntax.self),
                           idx == 0,  // First argument (typically the closure parameter)
                           let sigInfo = closureSignatureInfo {
                            // B3.5.5: Extract generic substitutions from receiver type
                           let substitutions: [String: TypeName]
                            if let recType = receiverType {
                                substitutions = extractGenericSubstitutions(from: recType)
                                debugLog("[COMPILER] Extracted substitutions: \(substitutions.mapValues { $0.canonicalDescription() })")
                            } else {
                                // No receiver type available - can't substitute
                                substitutions = [:]
                                debugLog("[COMPILER] No receiver type - empty substitutions")
                            }

                            let substitutedParamTypes = sigInfo.parameterTypes.map {
                                substituteGenericPlaceholders($0, substitutions: substitutions)
                            }
                            let substitutedReturnType = substituteGenericPlaceholders(
                                sigInfo.returnType,
                                substitutions: substitutions
                            )

                            debugLog("[COMPILER] Original param types: \(sigInfo.parameterTypes.map { $0.canonicalDescription() })")
                            debugLog("[COMPILER] Substituted param types: \(substitutedParamTypes.map { $0.canonicalDescription() })")
                            debugLog("[COMPILER] Original return type: \(sigInfo.returnType.canonicalDescription())")
                            debugLog("[COMPILER] Substituted return type: \(substitutedReturnType.canonicalDescription())")

                            // Convert TypeName to canonical string for bytecode
                            let paramTypeStrings = substitutedParamTypes.map { $0.canonicalDescription() }
                            let firstParamString = paramTypeStrings.first

                            // B4: Infer return type from closure body
                            let inferredReturn = inferClosureReturnType(
                                closureExpr,
                                parameterType: firstParamString,
                                knownTypes: knownTypes
                            )
                            debugLog("[COMPILER] Inferred return type from closure body: \(inferredReturn ?? "nil")")

                            // B5 & B6: Pass inferred types to closure compilation
                            try compileClosureExpr(
                                closureExpr,
                                knownTypes: knownTypes ?? [],
                                knownFunctions: knownFunctions ?? [],
                                builder: b,
                                inferredParamTypes: paramTypeStrings,
                                inferredReturnType: inferredReturn
                            )
                        } else {
                            // Regular argument compilation
                            try compileExpr(arg.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                        }
                    }

                    let sel = b.internSelector(selector)
                    b.emit(.callMethod(selector: sel, argc: 1 + callArgs.count), at: call)
                }
                return
            }
            if let name = calleeBaseName(from: call.calledExpression) {
                // print(...)
                if name == "print" {
                    if let firstArg = callArgs.first?.expression {
                        try compileExpr(firstArg, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    } else {
                        b.emit(.loadConst(.string("")), at: call)
                    }
                    b.emit(.printTop, at: call)
                    return
                }

                // Constructor call if name matches a known type
                if let types = knownTypes, types.contains(name) {
                    var origLabels: [String] = callArgs.map { $0.label ?? "" }
                    if origLabels.contains("") {
                        throw CompileError.unsupported("\(name) initializer requires labeled arguments")
                    }
                    // Rewrite for wrapped fields (M1): for each arg whose label is a wrapped property,
                    // construct the wrapper object and change the label to the backing name.
                    let wrappers = wrappersByType[name] ?? [:]
                    var pushedLabels: [String] = []
                    var providedSet: Set<String> = []
                    for arg in callArgs {
                        let label = arg.label ?? ""
                        if let wd = wrappers[label] {
                            // Build: load metatype(Wrapper), compile attribute args, then wrapped value expr, call init(..., wrappedValue:)
                            let tid = b.internType(wd.wrapperType)
                            b.emit(.loadConst(.metatype(tid)), at: call)
                            let selString = buildWrapperInitSelectorString(from: wd.attrArgs)
                            let sel = b.internSelector(selString)
                            for pair in wd.attrArgs { try compileExpr(pair.expr, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b) }
                            try compileExpr(arg.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                            b.emit(.callMethod(selector: sel, argc: 1 + wd.attrArgs.count + 1), at: call)
                            pushedLabels.append(wd.backingName)
                            providedSet.insert(wd.backingName)
                        } else {
                            try compileExpr(arg.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                            pushedLabels.append(label)
                            providedSet.insert(label)
                        }
                    }
                    // M3: Fill missing fields with defaults where available
                    let allFields = collectedFieldsByType[name] ?? []
                    let defaultsPlain = storedDefaultsByType[name] ?? [:]
                    for field in allFields where !providedSet.contains(field) {
                        // Determine if this field is a wrapped backing
                        if let wd = wrappers.values.first(where: { $0.backingName == field }) {
                            if let def = wd.defaultExpr {
                                let tid = b.internType(wd.wrapperType)
                                b.emit(.loadConst(.metatype(tid)), at: def)
                                let selString = buildWrapperInitSelectorString(from: wd.attrArgs)
                                let sel = b.internSelector(selString)
                                for pair in wd.attrArgs { try compileExpr(pair.expr, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b) }
                                try compileExpr(def, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                                b.emit(.callMethod(selector: sel, argc: 1 + wd.attrArgs.count + 1), at: def)
                                pushedLabels.append(field)
                                continue
                            } else {
                                throw CompileError.message("Missing argument for wrapped property '\(field)' in \(name) initializer and no default is available")
                            }
                        } else {
                            if let def = defaultsPlain[field] {
                                try compileExpr(def, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                                pushedLabels.append(field)
                                continue
                            } else {
                                throw CompileError.message("Missing argument for property '\(field)' in \(name) initializer and no default is available")
                            }
                        }
                    }
                    b.emit(.newObject(typeName: name, argLabels: pushedLabels), at: call)
                    return
                }
                // user function
                if let fns = knownFunctions, fns.contains(name) {
                    for arg in callArgs {
                        try compileExpr(arg.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    }
                    b.emit(.callFunc(name: name, argc: callArgs.count), at: call)
                    return
                }
                // Implicit self method call inside methods: name(...) → self.name(...)
                if implicitSelfContext != nil {
                    // Treat as a method on self
                    // If name is a visible binding (name-mode or slot-mode), prefer dynamic callee rather than implicit self.
                    var isShadowed = false
                    if options.mode == .slot, let slotRes = slot, slotRes.resolve(name) != nil { isShadowed = true }
                    if options.mode == .name {
                        for set in nameBindingsStack.reversed() { if set.contains(name) { isShadowed = true; break } }
                    }
                    if isShadowed {
                        // fall through to dynamic callee at end of this branch
                    } else {
                        // push self, args; callMethod(selector, argc: 1+N)
                        if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let selfHit = slotRes.resolve("self") {
                            b.emit(.loadLocal(depth: selfHit.depth, slot: selfHit.slot), at: call)
                        } else {
                            b.emit(.loadVar("self"), at: call)
                        }
                        for arg in callArgs { try compileExpr(arg.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b) }
                        let labels: [String] = callArgs.map { $0.label ?? "_" }
                        let selString = labels.isEmpty ? name + "()" : (name + "(" + labels.map { $0 + ":" }.joined() + ")")
                        let sel = b.internSelector(selString)
                        b.emit(.callMethod(selector: sel, argc: 1 + callArgs.count), at: call)
                        return
                    }
                }

                // Host initializer fallback for bare identifier callee not known as user type/function
                // IMPORTANT: Only treat as a type initializer when the name looks like a Type
                // (starts with an uppercase letter). This prevents miscompiling calls like `f()`
                // where `f` is a closure variable.
                if !(knownTypes?.contains(name) ?? false)
                    && !(knownFunctions?.contains(name) ?? false)
                    && startsWithUppercase(name) {
                    let labels: [String] = callArgs.map { $0.label ?? "_" }
                    let selString = labels.isEmpty ? "init()" : ("init(" + labels.map { $0 + ":" }.joined() + ")")
                    let sel = b.internSelector(selString)
                    let tid = b.internType(name)
                    b.emit(.loadConst(.metatype(tid)), at: call.calledExpression)
                    for arg in callArgs { try compileExpr(arg.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b) }
                    b.emit(.callMethod(selector: sel, argc: 1 + callArgs.count), at: call)
                    return
                }
            }
            // Host initializer when callee is a TypeExpr name not in known user-defined types
            if let typeExpr = call.calledExpression.as(TypeExprSyntax.self),
               let identType = typeExpr.type.as(IdentifierTypeSyntax.self) {
                let typeName = identType.name.text
                if !(knownTypes?.contains(typeName) ?? false) {
                    let labels: [String] = callArgs.map { $0.label ?? "_" }
                    let sel = b.internSelector(labels.isEmpty ? "init()" : ("init(" + labels.map { $0 + ":" }.joined() + ")"))
                    let tid = b.internType(typeName)
                    b.emit(.loadConst(.metatype(tid)), at: typeExpr)
                    for arg in callArgs { try compileExpr(arg.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b) }
                    b.emit(.callMethod(selector: sel, argc: 1 + callArgs.count), at: call)
                    return
                }
            }
            // Dynamic callee: evaluate callee expr → push closure, then args, then callValue
            try compileExpr(call.calledExpression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            for arg in callArgs {
                try compileExpr(arg.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            }
            b.emit(.callValue(argc: callArgs.count), at: call)
            return
        }

        // assignment, binary ops, sequence handling — unchanged except threading knownFunctions
        if let infix = expr.as(InfixOperatorExprSyntax.self),
           infix.operator.is(AssignmentExprSyntax.self) {
            if let member = infix.leftOperand.as(MemberAccessExprSyntax.self) {
                guard let base = member.base else { throw CompileError.unsupported("Member assignment without base") }
                let rawProp = member.declName.baseName.text
                let isProjection = rawProp.hasPrefix("$")
                let prop = isProjection ? String(rawProp.dropFirst()) : rawProp
                var wrapper: WrapperDescriptor? = nil
                if let knownTypes = knownTypes {
                    if let baseType = inferExprTypeName(base, knownTypes: knownTypes) {
                        wrapper = wrappersByType[baseType]?[prop]
                    }
                    if wrapper == nil, let ctx = implicitSelfContext {
                        if let baseIdent = base.as(IdentifierExprSyntax.self), baseIdent.identifier.text == "self" {
                            wrapper = wrappersByType[ctx.typeName]?[prop]
                        } else if let baseDecl = base.as(DeclReferenceExprSyntax.self), baseDecl.baseName.text == "self" {
                            wrapper = wrappersByType[ctx.typeName]?[prop]
                        }
                    }
                }
                try compileExpr(base, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                if let wd = wrapper {
                    // base._prop.wrappedValue = rhs
                    let noteStart = b.count
                    let pidBacking = b.internProperty(wd.backingName)
                    b.emit(.getProp(pidBacking), at: member)
                    try compileExpr(infix.rightOperand, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    let pidWV = b.internProperty("wrappedValue")
                    b.emit(.setProp(pidWV), at: member)
                    let noteEnd = b.count - 1
                    b.addScopeNote(start: noteStart, end: noteEnd, kind: "wrapperAssign", name: prop)
                } else {
                    try compileExpr(infix.rightOperand, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    let pid = b.internProperty(prop)
                    b.emit(.setProp(pid), at: member)
                }
                return
            }
            if let sub = infix.leftOperand.as(SubscriptCallExprSyntax.self) {
                let base = sub.calledExpression
                let args = Array(sub.arguments)
                guard let first = args.first else { throw CompileError.unsupported("Subscript requires an index") }
                // Stack: base, index, value → setIndex
                try compileExpr(base, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                try compileExpr(first.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                try compileExpr(infix.rightOperand, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                b.emit(.setIndex, at: sub)
                return
            }
            if let ident = infix.leftOperand.as(IdentifierExprSyntax.self) {
                let name = ident.identifier.text
                // Implicit self property set when applicable
                if let ctx = implicitSelfContext, ctx.fieldNames.contains(name) && name != "self" {
                    if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let selfHit = slotRes.resolve("self") {
                        b.emit(.loadLocal(depth: selfHit.depth, slot: selfHit.slot), at: ident)
                    } else {
                        b.emit(.loadVar("self"), at: ident)
                    }
                    try compileExpr(infix.rightOperand, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    let pid = b.internProperty(name)
                    b.emit(.setProp(pid), at: ident)
                    return
                }
                // Regular variable assignment
                try compileExpr(infix.rightOperand, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let hit = slotRes.resolve(name) {
                    b.emit(.setLocal(depth: hit.depth, slot: hit.slot), at: ident)
                } else {
                    b.emit(.setVar(name), at: ident)
                }
                return
            }
            throw CompileError.unsupported("Unsupported assignment lhs: \(infix.leftOperand.kind)")
        }

        if let infix = expr.as(InfixOperatorExprSyntax.self),
           let binop = infix.operator.as(BinaryOperatorExprSyntax.self) {
            let opText = binop.operatorToken.text.trimmingCharacters(in: .whitespacesAndNewlines)
            if opText == "&&" || opText == "||" {
                try compileLogicalInfix(
                    lhs: infix.leftOperand, opSymbol: opText, rhs: infix.rightOperand,
                    knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b
                )
                return
            }
            try compileExpr(infix.leftOperand,  knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            try compileExpr(infix.rightOperand, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            try emitBinaryOp(opText, at: binop, builder: b)
            return
        }

        // Older/newer SwiftSyntax sometimes produces SequenceExpr for binary ops & assignments
        if let seq = expr.as(SequenceExprSyntax.self) {
            try compileSequenceExpr(seq, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            return
        }

        throw CompileError.unsupported("Expr kind: \(expr.kind)")
    }

    // MARK: - Type Environment Management (B3.5)

    /// Push a new type environment scope
    private func pushTypeEnvironment() {
        typeEnvironmentStack.append([:])
    }

    /// Pop the current type environment scope
    private func popTypeEnvironment() {
        _ = typeEnvironmentStack.popLast()
    }

    /// Register a variable's inferred type
    private func registerVariableType(name: String, type: TypeName) {
        guard !typeEnvironmentStack.isEmpty else { return }
        typeEnvironmentStack[typeEnvironmentStack.count - 1][name] = type
    }

    /// Look up a variable's type from the environment
    private func lookupVariableType(name: String) -> TypeName? {
        for scope in typeEnvironmentStack.reversed() {
            if let type = scope[name] {
                return type
            }
        }
        return nil
    }

    /// Extract generic argument substitutions from a receiver type
    /// Example: Array<String> with placeholder "Element" → ["Element": Swift.String]
    /// Uses parsed generic parameter names from HostSurfaceKit - works for ANY generic type!
    private func extractGenericSubstitutions(from receiverType: TypeName) -> [String: TypeName] {
        var substitutions: [String: TypeName] = [:]

        // Look up generic parameter names from registry (parsed from stdlib definitions)
        let canonicalTypeName = receiverType.path.joined(separator: ".")
        guard let genericParamNames = methodSignatureRegistry.lookupGenericParameters(typeName: canonicalTypeName) else {
            // Type has no generic parameters or not in registry
            return substitutions
        }

        // Map generic parameter names to concrete types in order
        // Example: Array has ["Element"], so genericArguments[0] → "Element"
        //          Dictionary has ["Key", "Value"], so genericArguments[0] → "Key", [1] → "Value"
        for (index, paramName) in genericParamNames.enumerated() {
            if index < receiverType.genericArguments.count {
                substitutions[paramName] = receiverType.genericArguments[index]
            }
        }

        return substitutions
    }

    private func inferBinaryOperatorResult(symbol rawSymbol: String,
                                           lhsType: TypeName?,
                                           rhsType: TypeName?) -> TypeName? {
        let symbol = rawSymbol.trimmingCharacters(in: .whitespacesAndNewlines)
        switch symbol {
        case "+", "-", "*", "/", "%":
            if let lhsType, let rhsType {
                if lhsType.canonicalDescription() == rhsType.canonicalDescription() {
                    return lhsType
                }
                return lhsType ?? rhsType
            }
            return lhsType ?? rhsType
        case "==", "!=", "<", "<=", ">", ">=":
            return TypeName(path: ["Swift", "Bool"])
        case "&&", "||":
            return TypeName(path: ["Swift", "Bool"])
        default:
            return nil
        }
    }

    /// Infer type from an expression (simplified - handles common cases)
    private func inferExpressionType(_ expr: ExprSyntax, knownTypes: Set<String>?) -> TypeName? {
        // Array literal: ["a", "b"] → Array<String>
        if let arrayExpr = expr.as(ArrayExprSyntax.self) {
            return inferArrayLiteralType(arrayExpr, knownTypes: knownTypes)
        }

        // Dictionary literal: ["a": 1] → Dictionary<String, Int>
        if let dictExpr = expr.as(DictionaryExprSyntax.self) {
            return inferDictionaryLiteralType(dictExpr, knownTypes: knownTypes)
        }

        // Variable reference: look up in environment
        if let declRef = expr.as(DeclReferenceExprSyntax.self) {
            return lookupVariableType(name: declRef.baseName.text)
        }

        // String literal → Swift.String
        if expr.is(StringLiteralExprSyntax.self) {
            return TypeName(path: ["Swift", "String"])
        }

        // Integer literal → Swift.Int
        if expr.is(IntegerLiteralExprSyntax.self) {
            return TypeName(path: ["Swift", "Int"])
        }

        // Float literal → Swift.Double
        if expr.is(FloatLiteralExprSyntax.self) {
            return TypeName(path: ["Swift", "Double"])
        }

        // Bool literal → Swift.Bool
        if expr.is(BooleanLiteralExprSyntax.self) {
            return TypeName(path: ["Swift", "Bool"])
        }

        // Member access: base.property → look up property type
        if let member = expr.as(MemberAccessExprSyntax.self),
           let base = member.base {
            guard let baseType = inferExpressionType(base, knownTypes: knownTypes) else {
                return nil
            }

            let propertyName = member.declName.baseName.text
            let baseTypeName = baseType.path.joined(separator: ".")

            // Look up property type from registry
            guard var propertyType = methodSignatureRegistry.lookupPropertyType(
                typeName: baseTypeName,
                propertyName: propertyName
            ) else {
                // Property not found in registry - might be a custom type or not indexed
                return nil
            }

            // If the property type contains generic placeholders (e.g., Element),
            // substitute them with the base type's concrete generic arguments
            // Example: Array<String>.first has type Element? → String?
            if !baseType.genericArguments.isEmpty {
                let substitutions = extractGenericSubstitutions(from: baseType)
                propertyType = substituteGenericPlaceholders(propertyType, substitutions: substitutions)
            }

            return propertyType
        }

        if let infix = expr.as(InfixOperatorExprSyntax.self),
           let op = infix.operatorOperand.as(BinaryOperatorExprSyntax.self) {
            let symbol = op.operatorToken.text
            let leftType = inferExpressionType(infix.leftOperand, knownTypes: knownTypes)
            let rightType = inferExpressionType(infix.rightOperand, knownTypes: knownTypes)

            if let result = inferBinaryOperatorResult(symbol: symbol, lhsType: leftType, rhsType: rightType) {
                return result
            }
        }

        if let seq = expr.as(SequenceExprSyntax.self) {
            let elems = Array(seq.elements)
            guard !elems.isEmpty else { return nil }
            if elems.count == 1 {
                return inferExpressionType(elems[0], knownTypes: knownTypes)
            }

            var currentType: TypeName? = inferExpressionType(elems[0], knownTypes: knownTypes)
            var index = 1
            while index + 1 < elems.count {
                guard let opExpr = elems[index].as(BinaryOperatorExprSyntax.self) else { break }
                let rhsExpr = elems[index + 1]
                let rhsType = inferExpressionType(rhsExpr, knownTypes: knownTypes)
                if let result = inferBinaryOperatorResult(symbol: opExpr.operatorToken.text, lhsType: currentType, rhsType: rhsType) {
                    currentType = result
                } else if currentType == nil {
                    currentType = rhsType
                }
                index += 2
            }
            return currentType
        }

        // TODO: Add more expression types (function calls, etc.)
        return nil
    }

    /// Infer element type from array literal elements
    private func inferArrayLiteralType(_ arrayExpr: ArrayExprSyntax, knownTypes: Set<String>?) -> TypeName? {
        let elements = arrayExpr.elements
        guard let firstElement = elements.first else {
            // Empty array - can't infer element type
            return nil
        }

        // Infer type from first element
        guard let elementType = inferExpressionType(firstElement.expression, knownTypes: knownTypes) else {
            return nil
        }

        // Return Array<ElementType>
        return TypeName(path: ["Swift", "Array"], genericArguments: [elementType])
    }

    /// Infer key/value types from dictionary literal
    private func inferDictionaryLiteralType(_ dictExpr: DictionaryExprSyntax, knownTypes: Set<String>?) -> TypeName? {
        switch dictExpr.content {
        case .colon:
            // Empty dictionary [:]
            return nil
        case .elements(let elements):
            guard let firstElement = elements.first else { return nil }
            guard let keyType = inferExpressionType(firstElement.key, knownTypes: knownTypes),
                  let valueType = inferExpressionType(firstElement.value, knownTypes: knownTypes) else {
                return nil
            }
            return TypeName(path: ["Swift", "Dictionary"], genericArguments: [keyType, valueType])
        }
    }

    // MARK: - Closure Type Inference Helpers

    /// Substitute generic placeholders in a TypeName with concrete types
    /// Example: substitute "Element" with "Swift.String" in Array<Element>.map's closure signature
    private func substituteGenericPlaceholders(
        _ typeName: TypeName,
        substitutions: [String: TypeName]
    ) -> TypeName {
        let substituted: TypeName

        if typeName.path.count == 1,
           let replacement = substitutions[typeName.path[0]] {
            substituted = replacement
        } else {
            let substitutedArgs = typeName.genericArguments.map {
                substituteGenericPlaceholders($0, substitutions: substitutions)
            }

            var substitutedFuncSig: TypeName.FunctionSignature? = nil
            if let funcSig = typeName.functionSignature {
                let substParams = funcSig.parameters.map {
                    substituteGenericPlaceholders($0, substitutions: substitutions)
                }
                let substReturn = substituteGenericPlaceholders(funcSig.returnType, substitutions: substitutions)
                substitutedFuncSig = TypeName.FunctionSignature(
                    parameters: substParams,
                    returnType: substReturn,
                    isAsync: funcSig.isAsync,
                    throwsKind: funcSig.throwsKind
                )
            }

            substituted = TypeName(
                path: typeName.path,
                genericArguments: substitutedArgs,
                genericOwnerIndex: typeName.genericOwnerIndex,
                functionSignature: substitutedFuncSig
            )
        }

        return substituted.resolvingPlaceholders(
            using: methodSignatureRegistry.genericParameterInventorySnapshot,
            substitutions: substitutions
        )
    }

    /// Parse a canonical type string (e.g., "Swift.Int", "Swift.Array<Swift.String>") into TypeName
    private func parseCanonicalTypeString(_ typeString: String) -> TypeName {
        if let parsed = try? TypeNameParser.parse(typeString) {
            return parsed
        }
        let path = typeString.split(separator: ".").map(String.init)
        return TypeName(path: path)
    }

    /// Infer return type from a single-expression closure body
    /// This implements a simplified version of Phase 2 type inference (B4)
    private func inferClosureReturnType(
        _ clo: ClosureExprSyntax,
        parameterType: String?,
        knownTypes: Set<String>?
    ) -> String? {
        let stmtArray = Array(clo.statements)
        guard stmtArray.count == 1 else { return nil }  // Only single-expression closures for now

        let onlyNode = Syntax(stmtArray[0].item)
        let returnExpr: ExprSyntax?

        if let exprStmt = onlyNode.as(ExpressionStmtSyntax.self) {
            returnExpr = exprStmt.expression
        } else if let returnStmt = onlyNode.as(ReturnStmtSyntax.self) {
            returnExpr = returnStmt.expression
        } else if let expr = onlyNode.as(ExprSyntax.self) {
            returnExpr = expr
        } else {
            return nil
        }

        guard let expr = returnExpr else { return nil }

        // Identity closure: { value in value } — return type = parameter type
        if let declRef = expr.as(DeclReferenceExprSyntax.self) {
            let params = extractClosureParams(clo)
            if params.contains(declRef.baseName.text), let paramType = parameterType {
                return paramType  // Identity: return same type as parameter
            }
        }

        // Property access or complex expressions: use registry-based type inference
        // Set up a temporary type environment with the closure parameter(s)
        if let paramType = parameterType {
            // Parse parameter type string into TypeName
            let paramTypeName = parseCanonicalTypeString(paramType)

            // Extract closure parameter names
            let params = extractClosureParams(clo)

            // Push temporary type environment and bind parameter(s)
            pushTypeEnvironment()
            for paramName in params {
                registerVariableType(name: paramName, type: paramTypeName)
            }

            // Use inferExpressionType to dynamically look up types via registry
            let inferredType = inferExpressionType(expr, knownTypes: knownTypes)

            // Clean up temporary environment
            popTypeEnvironment()

            if let type = inferredType {
                return type.canonicalDescription()
            }
        }

        return nil
    }

    private func extractClosureParams(_ clo: ClosureExprSyntax) -> Set<String> {
        var params = Set<String>()
        if let pc = clo.signature?.parameterClause {
            switch pc {
            case .parameterClause(let clause):
                for p in clause.parameters {
//                    if let firstName =  {
                        params.insert(p.firstName.text)
//                    } else if let secondName = p.secondName {
//                        params.insert(secondName.text)
//                    }
                }
            case .simpleInput(let list):
                for tok in list.tokens(viewMode: .sourceAccurate) {
                    let t = tok.text.trimmingCharacters(in: .whitespacesAndNewlines)
                    if !t.isEmpty && t != "," && t != "_" {
                        params.insert(t)
                    }
                }
            @unknown default:
                break
            }
        }
        return params
    }

    /// Compile a closure expression by emitting an inline body with a prologue jump,
    /// then constructing a closure value that captures referenced outer names.
    private func compileClosureExpr(
        _ clo: ClosureExprSyntax,
        knownTypes: Set<String>,
        knownFunctions: Set<String>,
        builder b: BytecodeBuilder,
        inferredParamTypes: [String]? = nil,  // From context (e.g., method signature)
        inferredReturnType: String? = nil     // From context or body analysis
    ) throws {
        // Extract param names (no support for shorthand $0 params)
        var params: [String] = []
        if let pc = clo.signature?.parameterClause {
            switch pc {
            case .parameterClause(let clause):
                for (i, p) in clause.parameters.enumerated() {
                    let name = closureParamLocalName(i, p)
                    params.append(name)
                }
            case .simpleInput(let list):
                // Extract identifiers from the shorthand list tokens (e.g., "a, b")
                var idx = 0
                for tok in list.tokens(viewMode: .sourceAccurate) {
                    let t = tok.text.trimmingCharacters(in: .whitespacesAndNewlines)
                    if t.isEmpty || t == "," { continue }
                    params.append(t == "_" ? "arg\(idx)" : t)
                    idx += 1
                }
            @unknown default:
                throw CompileError.unsupported("Unknown closure parameter clause form")
            }
        }

        // Collect captures: referenced names minus params/locals/known types/funcs
        let captureNames = collectClosureCaptures(from: clo, params: Set(params), knownTypes: knownTypes, knownFunctions: knownFunctions)

        // Emit body with a jump-over prologue
        let prologueJump = b.emit(.jump(0), at: clo)
        let entry = b.count
        // Temporarily disable slot-resolution inside closure bodies (closure frames are name-mode)
        let prevSlot = slotResolutionEnabled
        if options.mode == .slot { slotResolutionEnabled = false }
        defer { slotResolutionEnabled = prevSlot }
        // Body statements with implicit return for single-expression body
        let stmtArray = Array(clo.statements)
        if stmtArray.count == 1 {
            let onlyNode = Syntax(stmtArray[0].item)
            if let onlyExprStmt = onlyNode.as(ExpressionStmtSyntax.self) {
                try compileExpr(onlyExprStmt.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                b.emit(.ret, at: onlyExprStmt)
            } else if let onlyExpr = onlyNode.as(ExprSyntax.self) {
                try compileExpr(onlyExpr, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                b.emit(.ret, at: onlyExpr)
            } else {
                // Fallback: generic statements + default return
                for item in stmtArray {
                    try compileStatementNode(Syntax(item.item), knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                }
                b.emit(.loadConst(.bool(true)), at: clo)
                b.emit(.ret, at: clo)
            }
        } else {
            for item in stmtArray {
                try compileStatementNode(Syntax(item.item), knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            }
            // Ensure a default return value if none provided
            b.emit(.loadConst(.bool(true)), at: clo)
            b.emit(.ret, at: clo)
        }
        // Patch prologue to jump past body
        let afterBody = b.count
        b.patch(prologueJump, to: .jump(afterBody - (prologueJump + 1)))

        // Create closure value
        let capsSorted = Array(captureNames).sorted()
        // Use inferred types if available, otherwise default to empty (backward compatible)
        let paramTypes: [String] = inferredParamTypes ?? []
        let returnType: String? = inferredReturnType

        if options.mode == .slot, let slotRes = slot {
            // Push capture values in order, resolving to loadLocal when possible
            for name in capsSorted {
                if let hit = slotRes.resolve(name) {
                    b.emit(.loadLocal(depth: hit.depth, slot: hit.slot), at: clo)
                } else {
                    b.emit(.loadVar(name), at: clo)
                }
            }
            b.emit(.makeClosureVals(params: params, paramTypes: paramTypes, returnType: returnType, entry: entry, captures: capsSorted), at: clo)
        } else {
            b.emit(.makeClosure(params: params, paramTypes: paramTypes, returnType: returnType, entry: entry, captures: capsSorted), at: clo)
        }
    }

    private func closureParamLocalName(_ index: Int, _ p: ClosureParameterSyntax) -> String {
        if let second = p.secondName, second.text != "_" { return second.text }
        let first = p.firstName.text
        return first == "_" ? "arg\(index)" : first
    }

    /// Walk closure body to find free variable names to capture.
    private func collectClosureCaptures(
        from clo: ClosureExprSyntax,
        params: Set<String>,
        knownTypes: Set<String>,
        knownFunctions: Set<String>
    ) -> Set<String> {
        collectFreeNames(
            in: clo.statements,
            initialDeclared: params,
            knownTypes: knownTypes,
            knownFunctions: knownFunctions
        )
    }

    private func collectFunctionCaptures(
        from fn: FunctionDeclSyntax,
        params: Set<String>,
        knownTypes: Set<String>,
        knownFunctions: Set<String>
    ) -> Set<String> {
        guard let body = fn.body else { return [] }
        var declared = params
        declared.insert(fn.name.text)
        return collectFreeNames(
            in: body.statements,
            initialDeclared: declared,
            knownTypes: knownTypes,
            knownFunctions: knownFunctions
        )
    }

    private func collectFreeNames(
        in statements: CodeBlockItemListSyntax,
        initialDeclared: Set<String>,
        knownTypes: Set<String>,
        knownFunctions: Set<String>
    ) -> Set<String> {
        var used: Set<String> = []
        var declared: Set<String> = initialDeclared
        var typeLikeNames: Set<String> = []

        func visitExpr(_ e: ExprSyntax) {
            if let id = e.as(IdentifierExprSyntax.self) {
                let name = id.identifier.text
                used.insert(name)
                if canonicalTypeCandidate(for: name, knownTypes: knownTypes) != nil {
                    typeLikeNames.insert(name)
                }
                return
            }
            if let ref = e.as(DeclReferenceExprSyntax.self) {
                let name = ref.baseName.text
                used.insert(name)
                if canonicalTypeCandidate(for: name, knownTypes: knownTypes) != nil {
                    typeLikeNames.insert(name)
                }
                return
            }
            if let mem = e.as(MemberAccessExprSyntax.self) {
                if let base = mem.base { visitExpr(base) }
                return
            }
            if let call = e.as(FunctionCallExprSyntax.self) {
                let callee = call.calledExpression
                if let typeExpr = callee.as(TypeExprSyntax.self) {
                    if let identType = typeExpr.type.as(IdentifierTypeSyntax.self) {
                        typeLikeNames.insert(identType.name.text)
                    } else {
                        let raw = typeExpr.type.description.trimmingCharacters(in: .whitespacesAndNewlines)
                        if let last = raw.split(separator: ".").last {
                            let name = String(last)
                            if startsWithUppercase(name) { typeLikeNames.insert(name) }
                        }
                    }
                } else if let memCallee = callee.as(MemberAccessExprSyntax.self) {
                    if memCallee.declName.baseName.text == "init" {
                        if let baseRef = memCallee.base?.as(DeclReferenceExprSyntax.self) {
                            typeLikeNames.insert(baseRef.baseName.text)
                        } else if let baseIdent = memCallee.base?.as(IdentifierExprSyntax.self) {
                            typeLikeNames.insert(baseIdent.identifier.text)
                        } else if let baseType = memCallee.base?.as(TypeExprSyntax.self),
                                  let identType = baseType.type.as(IdentifierTypeSyntax.self) {
                            typeLikeNames.insert(identType.name.text)
                        }
                    }
                } else if let idCallee = callee.as(IdentifierExprSyntax.self) {
                    let name = idCallee.identifier.text
                    if startsWithUppercase(name) { typeLikeNames.insert(name) }
                }
                visitExpr(callee)
                let args = collectCallArguments(call)
                for arg in args { visitExpr(arg.expression) }
                return
            }
            if let infix = e.as(InfixOperatorExprSyntax.self) {
                visitExpr(infix.leftOperand)
                visitExpr(infix.rightOperand)
                return
            }
            if let seq = e.as(SequenceExprSyntax.self) {
                for element in seq.elements {
                    if let ex = element.as(ExprSyntax.self) { visitExpr(ex) }
                }
                return
            }
            if let tuple = e.as(TupleExprSyntax.self) {
                for el in tuple.elements { visitExpr(el.expression) }
                return
            }
            if let _ = e.as(ClosureExprSyntax.self) {
                return
            }
            if let ifExpr = e.as(IfExprSyntax.self) {
                if let cond = ifExpr.conditions.first?.condition.as(ExprSyntax.self) { visitExpr(cond) }
                return
            }
            if let typeExpr = e.as(TypeExprSyntax.self) {
                if let identType = typeExpr.type.as(IdentifierTypeSyntax.self) {
                    typeLikeNames.insert(identType.name.text)
                } else {
                    let raw = typeExpr.type.description.trimmingCharacters(in: .whitespacesAndNewlines)
                    if let last = raw.split(separator: ".").last {
                        let name = String(last)
                        if startsWithUppercase(name) { typeLikeNames.insert(name) }
                    }
                }
                return
            }
        }

        func visitStmtItem(_ item: Syntax) {
            if let fnDecl = item.as(FunctionDeclSyntax.self) {
                declared.insert(fnDecl.name.text)
                return
            }
            if let v = item.as(VariableDeclSyntax.self) {
                for binding in v.bindings {
                    if let id = binding.pattern.as(IdentifierPatternSyntax.self) {
                        declared.insert(id.identifier.text)
                    }
                    if let initExpr = binding.initializer?.value { visitExpr(initExpr) }
                }
                return
            }
            if let forIn = item.as(ForInStmtSyntax.self) {
                if let pat = forIn.pattern.as(IdentifierPatternSyntax.self) {
                    declared.insert(pat.identifier.text)
                }
                if let seqExpr = forIn.sequence.as(ExprSyntax.self) { visitExpr(seqExpr) }
                for stmt in forIn.body.statements { visitStmtItem(Syntax(stmt.item)) }
                return
            }
            if let guardStmt = item.as(GuardStmtSyntax.self) {
                for cond in guardStmt.conditions {
                    let condition = cond.condition
                    if let expr = condition.as(ExprSyntax.self) {
                        visitExpr(expr)
                    } else if let bind = condition.as(OptionalBindingConditionSyntax.self) {
                        if let id = bind.pattern.as(IdentifierPatternSyntax.self) {
                            declared.insert(id.identifier.text)
                        }
                        if let initExpr = bind.initializer?.value { visitExpr(initExpr) }
                    }
                }
                for stmt in guardStmt.body.statements { visitStmtItem(Syntax(stmt.item)) }
                return
            }
            if let repeatStmt = item.as(RepeatStmtSyntax.self) {
                for stmt in repeatStmt.body.statements { visitStmtItem(Syntax(stmt.item)) }
                if let cond = repeatStmt.condition.as(ExprSyntax.self) { visitExpr(cond) }
                return
            }
            if let whileStmt = item.as(WhileStmtSyntax.self) {
                if let cond = whileStmt.conditions.first?.condition.as(ExprSyntax.self) { visitExpr(cond) }
                for stmt in whileStmt.body.statements { visitStmtItem(Syntax(stmt.item)) }
                return
            }
            if let ifExpr = item.as(IfExprSyntax.self) {
                for condElement in ifExpr.conditions {
                    let condition = condElement.condition
                    if let expr = condition.as(ExprSyntax.self) {
                        visitExpr(expr)
                    } else if let bind = condition.as(OptionalBindingConditionSyntax.self) {
                        if let id = bind.pattern.as(IdentifierPatternSyntax.self) {
                            declared.insert(id.identifier.text)
                        }
                        if let initExpr = bind.initializer?.value { visitExpr(initExpr) }
                    }
                }
                for stmt in ifExpr.body.statements { visitStmtItem(Syntax(stmt.item)) }
                if let elseBody = ifExpr.elseBody {
                    if let nestedIf = elseBody.as(IfExprSyntax.self) {
                        visitStmtItem(Syntax(nestedIf))
                    } else if let elseBlock = elseBody.as(CodeBlockSyntax.self) {
                        for stmt in elseBlock.statements { visitStmtItem(Syntax(stmt.item)) }
                    }
                }
                return
            }
            if let returnStmt = item.as(ReturnStmtSyntax.self) {
                if let expr = returnStmt.expression { visitExpr(expr) }
                return
            }
            if let exprStmt = item.as(ExpressionStmtSyntax.self) {
                visitExpr(exprStmt.expression)
                return
            }
            if let expr = item.as(ExprSyntax.self) {
                visitExpr(expr)
                return
            }
            if let cbItem = item.as(CodeBlockItemSyntax.self) {
                visitStmtItem(Syntax(cbItem.item))
                return
            }
        }

        for item in statements { visitStmtItem(Syntax(item.item)) }

        var result = used.subtracting(declared)
        result.subtract(knownTypes)
        result.subtract(knownFunctions)
        result.remove("print")
        result.subtract(typeLikeNames)
        return result
    }
    
    /// Try to pull a base name out of a callee expression (print, Person, Person.init, Module.Person, etc.)
    private func calleeBaseName(from expr: ExprSyntax) -> String? {
        if let id = expr.as(IdentifierExprSyntax.self) {
            return id.identifier.text
        }
        if let ref = expr.as(DeclReferenceExprSyntax.self) {
            return ref.baseName.text
        }
        if let typeExpr = expr.as(TypeExprSyntax.self) {
            if let identType = typeExpr.type.as(IdentifierTypeSyntax.self) {
                return identType.name.text
            } else {
                // Fallback: textual name (handles simple generic sugar too)
                return typeExpr.type.description.trimmingCharacters(in: .whitespacesAndNewlines)
            }
        }
        if let member = expr.as(MemberAccessExprSyntax.self) {
            // Handle Person.init(...) or Module.Person(...)
            if let baseRef = member.base?.as(DeclReferenceExprSyntax.self) {
                return baseRef.baseName.text
            }
            if let baseIdent = member.base?.as(IdentifierExprSyntax.self) {
                return baseIdent.identifier.text
            }
            if let baseType = member.base?.as(TypeExprSyntax.self),
               let identType = baseType.type.as(IdentifierTypeSyntax.self) {
                return identType.name.text
            }
            // As a last resort, returning the member name can still match builtins like `print`
            return member.declName.baseName.text
        }
        return nil
    }

    /// Precedence table for a tiny subset.
    /// "=" lowest (right-assoc), "+/-" mid (left), "*/" high (left).
    private func precedence(for op: String) -> (prec: Int, leftAssoc: Bool)? {
        switch op {
        case "=":                return (0, false)
        case "||":               return (2, true)   // handled specially (short-circuit)
        case "&&":               return (3, true)   // handled specially (short-circuit)
        case "==":               return (7, true)
        case "<", "<=", ">", ">=": return (8, true)
        case "+", "-":           return (10, true)
        case "*", "/", "%":      return (20, true)
        default:                 return nil
        }
    }

    /// Map compound assignments like "+=" to their base operator (e.g. "+").
    private func compoundAssignmentBaseOperator(for sym: String) -> String? {
        let trimmed = sym.trimmingCharacters(in: .whitespacesAndNewlines)
        guard trimmed.hasSuffix("="), trimmed.count >= 2 else { return nil }
        let base = String(trimmed.dropLast())
        switch base {
        case "+", "-", "*", "/", "%":
            return base
        default:
            return nil
        }
    }

    private func compileCompoundAssignment(
        lhs: ExprSyntax,
        baseOperator: String,
        opNode: SyntaxProtocol,
        rhsElems: [ExprSyntax],
        knownTypes: Set<String>?,
        knownFunctions: Set<String>?,
        builder b: BytecodeBuilder
    ) throws {
        guard !rhsElems.isEmpty else {
            throw CompileError.unsupported("Compound assignment missing RHS expression")
        }

        let emitRHS: () throws -> Void = {
            if rhsElems.count == 1 {
                try self.compileExpr(rhsElems[0], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            } else {
                try self.compileBinaryChain(from: rhsElems, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            }
        }

        if let ident = lhs.as(IdentifierExprSyntax.self) {
            let name = ident.identifier.text

            if let wd = resolveWrapperLocal(name) {
                let noteStart = b.count
                if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let hit = slotRes.resolve(wd.backingName) {
                    b.emit(.loadLocal(depth: hit.depth, slot: hit.slot), at: ident)
                } else {
                    b.emit(.loadVar(wd.backingName), at: ident)
                }
                b.emit(.dup, at: ident)
                let pidWV = b.internProperty("wrappedValue")
                b.emit(.getProp(pidWV), at: ident)
                try emitRHS()
                try emitBinaryOp(baseOperator, at: opNode, builder: b)
                b.emit(.setProp(pidWV), at: ident)
                let noteEnd = b.count - 1
                b.addScopeNote(start: noteStart, end: noteEnd, kind: "wrapperAssignLocal", name: name)
                return
            }

            if let ctx = implicitSelfContext, ctx.fieldNames.contains(name) && name != "self" {
                if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let selfHit = slotRes.resolve("self") {
                    b.emit(.loadLocal(depth: selfHit.depth, slot: selfHit.slot), at: ident)
                } else {
                    b.emit(.loadVar("self"), at: ident)
                }
                b.emit(.dup, at: ident)
                let pid = b.internProperty(name)
                b.emit(.getProp(pid), at: ident)
                try emitRHS()
                try emitBinaryOp(baseOperator, at: opNode, builder: b)
                b.emit(.setProp(pid), at: ident)
                return
            }

            let slotHit: (depth: Int, slot: Int, isConst: Bool)?
            if options.mode == .slot && slotResolutionEnabled, let slotRes = slot {
                slotHit = slotRes.resolve(name)
            } else {
                slotHit = nil
            }
            if let hit = slotHit {
                b.emit(.loadLocal(depth: hit.depth, slot: hit.slot), at: ident)
            } else {
                b.emit(.loadVar(name), at: ident)
            }
            try emitRHS()
            try emitBinaryOp(baseOperator, at: opNode, builder: b)
            if let hit = slotHit {
                b.emit(.setLocal(depth: hit.depth, slot: hit.slot), at: ident)
            } else {
                b.emit(.setVar(name), at: ident)
            }
            return
        }

        throw CompileError.unsupported("Unsupported compound assignment LHS kind: \(lhs.kind)")
    }
    
    /// Emit the VM instruction for a binary operator we've compiled RHS/LHS for.
    private func emitBinaryOp(_ op: String, at node: SyntaxProtocol, builder b: BytecodeBuilder) throws {
        switch op {
        case "+":  b.emit(.add, at: node)
        case "-":  b.emit(.sub, at: node)
        case "*":  b.emit(.mul, at: node)
        case "/":  b.emit(.div, at: node)
        case "%":  b.emit(.mod, at: node)
        case "==": b.emit(.eq,  at: node)
        case "<":  b.emit(.lt,  at: node)
        case "<=": b.emit(.le,  at: node)
        case ">":  b.emit(.gt,  at: node)
        case ">=": b.emit(.ge,  at: node)
        default:
            throw CompileError.unsupported("Operator '\(op)' not supported")
        }
}
    
    /// Compile a SequenceExpr either as assignment (if it contains '=') or as a binary chain.
    private func compileSequenceExpr(
        _ seq: SequenceExprSyntax,
        knownTypes: Set<String>?,
        knownFunctions: Set<String>?,
        builder b: BytecodeBuilder
    ) throws {
        let elems = Array(seq.elements)

        // Handle unresolved ternary represented as a sequence element: [cond, UnresolvedTernaryExpr]
        if let idx = elems.firstIndex(where: { Syntax($0).kind == .unresolvedTernaryExpr }) {
            guard idx > 0 else { throw CompileError.unsupported("Malformed ternary: missing condition") }
            let cond = elems[idx - 1]
            let uNode = Syntax(elems[idx])
            let branches = exprChildren(of: uNode)
            var thenE: ExprSyntax
            var elseE: ExprSyntax
            if branches.count >= 2 {
                thenE = branches[0]
                elseE = branches[1]
            } else if branches.count == 1 {
                thenE = branches[0]
                guard idx + 1 < elems.count else { throw CompileError.unsupported("Malformed ternary: missing else branch") }
                elseE = elems[idx + 1]
            } else {
                throw CompileError.unsupported("Malformed ternary branches")
            }
            try compileExpr(cond, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            let jif = b.emit(.jumpIfFalse(0), at: cond)
            try compileExpr(thenE, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            let jmpEnd = b.emit(.jump(0), at: uNode)
            let toElse = b.count - (jif + 1)
            b.patch(jif, to: .jumpIfFalse(toElse))
            try compileExpr(elseE, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            let toEnd = b.count - (jmpEnd + 1)
            b.patch(jmpEnd, to: .jump(toEnd))
            return
        }

        // Trivial: single element
        if elems.count == 1 {
            try compileExpr(elems[0], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            return
        }

        // Nil-coalescing chains (right-associative), lowered via dup/jumpIfNil/pop
        if elems.contains(where: { e in
            if let bin = e.as(BinaryOperatorExprSyntax.self) {
                return bin.operatorToken.text.trimmingCharacters(in: .whitespacesAndNewlines) == "??"
            }
            return false
        }) {
            try compileNilCoalescingSequence(elems, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b, seqNodeForLoc: seq)
            return
        }

        // Compound assignments like "+=" or "-=" -> rewrite to LHS op RHS + store.
        if let compound = elems.enumerated().first(where: { idx, expr in
            guard let bin = expr.as(BinaryOperatorExprSyntax.self) else { return false }
            return compoundAssignmentBaseOperator(for: bin.operatorToken.text) != nil
        }) {
            let (opIndex, opExpr) = compound
            guard opIndex == 1 else { throw CompileError.unsupported("Complex LHS in compound assignment sequence") }
            guard let bin = opExpr.as(BinaryOperatorExprSyntax.self) else {
                throw CompileError.unsupported("Invalid compound assignment operator")
            }
            guard let baseOp = compoundAssignmentBaseOperator(for: bin.operatorToken.text) else {
                throw CompileError.unsupported("Operator '\(bin.operatorToken.text)' not supported")
            }
            let lhs = elems[0]
            let rhsElems = Array(elems[(opIndex + 1)..<elems.count])
            try compileCompoundAssignment(
                lhs: lhs,
                baseOperator: baseOp,
                opNode: bin,
                rhsElems: rhsElems,
                knownTypes: knownTypes,
                knownFunctions: knownFunctions,
                builder: b
            )
            return
        }

        // If it contains "=", treat as simple assignment: LHS (=) RHS*
        if let eqIndex = elems.firstIndex(where: { $0.is(AssignmentExprSyntax.self) }) {
            guard eqIndex == 1 else { throw CompileError.unsupported("Complex LHS in assignment sequence") }
            let lhs = elems[0]
            let rhsElems = Array(elems[(eqIndex + 1)..<elems.count])

            // LHS: variable or member
            // Discard assignment: `_ = <RHS>` → evaluate RHS and drop the result
            if lhs.is(DiscardAssignmentExprSyntax.self) || Syntax(lhs).kind == .discardAssignmentExpr {
                if rhsElems.count == 1 {
                    try compileExpr(rhsElems[0], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                } else {
                    try compileBinaryChain(from: rhsElems, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                }
                b.emit(.pop, at: lhs)
                return
            }

            if let ident = lhs.as(IdentifierExprSyntax.self) {
                let name = ident.identifier.text
                // Local/global wrapper variable assignment: x = RHS -> _x.wrappedValue = RHS
                if let wd = resolveWrapperLocal(name) {
                    let noteStart = b.count
                    // push backing
                    if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let hit = slotRes.resolve(wd.backingName) {
                        b.emit(.loadLocal(depth: hit.depth, slot: hit.slot), at: ident)
                    } else {
                        b.emit(.loadVar(wd.backingName), at: ident)
                    }
                    // push value
                    if rhsElems.count == 1 {
                        try compileExpr(rhsElems[0], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    } else {
                        try compileBinaryChain(from: rhsElems, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    }
                    let pidWV = b.internProperty("wrappedValue")
                    b.emit(.setProp(pidWV), at: ident)
                    let noteEnd = b.count - 1
                    b.addScopeNote(start: noteStart, end: noteEnd, kind: "wrapperAssignLocal", name: name)
                    return
                }
                // Implicit self property set when in method and name matches field
                if let ctx = implicitSelfContext, ctx.fieldNames.contains(name) && name != "self" {
                    // push self receiver
                    if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let selfHit = slotRes.resolve("self") {
                        b.emit(.loadLocal(depth: selfHit.depth, slot: selfHit.slot), at: ident)
                    } else {
                        b.emit(.loadVar("self"), at: ident)
                    }
                    // push value (RHS chain)
                    if rhsElems.count == 1 {
                        try compileExpr(rhsElems[0], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    } else {
                        try compileBinaryChain(from: rhsElems, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    }
                    let pid = b.internProperty(name)
                    b.emit(.setProp(pid), at: ident)
                    return
                }
                // x = <RHS> as variable set
                if rhsElems.count == 1 {
                    try compileExpr(rhsElems[0], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                } else {
                    try compileBinaryChain(from: rhsElems, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                }
                if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, let hit = slotRes.resolve(name) {
                    b.emit(.setLocal(depth: hit.depth, slot: hit.slot), at: ident)
                } else {
                    b.emit(.setVar(name), at: ident)
                }
                return
            }
            if let member = lhs.as(MemberAccessExprSyntax.self) {
                // obj.prop = <RHS>
                guard let base = member.base else { throw CompileError.unsupported("Member assignment without base") }
                let rawProp = member.declName.baseName.text
                let isProjection = rawProp.hasPrefix("$")
                let prop = isProjection ? String(rawProp.dropFirst()) : rawProp
                var wrapper: WrapperDescriptor? = nil
                if let knownTypes = knownTypes {
                    if let baseType = inferExprTypeName(base, knownTypes: knownTypes) {
                        wrapper = wrappersByType[baseType]?[prop]
                    }
                    if wrapper == nil, let ctx = implicitSelfContext {
                        if let baseIdent = base.as(IdentifierExprSyntax.self), baseIdent.identifier.text == "self" {
                            wrapper = wrappersByType[ctx.typeName]?[prop]
                        } else if let baseDecl = base.as(DeclReferenceExprSyntax.self), baseDecl.baseName.text == "self" {
                            wrapper = wrappersByType[ctx.typeName]?[prop]
                        }
                    }
                }
                // Push object (or wrapper receiver when wrapped)
                try compileExpr(base, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                if let wd = wrapper {
                    // base._prop.wrappedValue = <RHS>
                    let pidBacking = b.internProperty(wd.backingName)
                    b.emit(.getProp(pidBacking), at: member)
                    if rhsElems.count == 1 {
                        try compileExpr(rhsElems[0], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    } else {
                        try compileBinaryChain(from: rhsElems, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    }
                    let pidWV = b.internProperty("wrappedValue")
                    b.emit(.setProp(pidWV), at: member)
                } else {
                    // base.prop = <RHS>
                    if rhsElems.count == 1 {
                        try compileExpr(rhsElems[0], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    } else {
                        try compileBinaryChain(from: rhsElems, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                    }
                    let pid = b.internProperty(prop)
                    b.emit(.setProp(pid), at: member)
                }
                return
            }
            if let sub = lhs.as(SubscriptCallExprSyntax.self) {
                // base[index] = <RHS>
                let base = sub.calledExpression
                let args = Array(sub.arguments)
                guard let first = args.first else { throw CompileError.unsupported("Subscript requires an index") }
                // Push base and index
                try compileExpr(base, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                try compileExpr(first.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                // Push value
                if rhsElems.count == 1 {
                    try compileExpr(rhsElems[0], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                } else {
                    try compileBinaryChain(from: rhsElems, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                }
                b.emit(.setIndex, at: sub)
                return
            }
            throw CompileError.unsupported("Unsupported assignment LHS kind: \(lhs.kind)")
        }

        // Detect logical chain at the top level (handled specially for short-circuit)
        let hasLogical = elems.contains {
            if let bin = $0.as(BinaryOperatorExprSyntax.self) {
                let s = bin.operatorToken.text.trimmingCharacters(in: .whitespacesAndNewlines)
                return s == "&&" || s == "||"
            }
            return false
        }

        if hasLogical {
            try compileLogicalSequence(
                elems,
                knownTypes: knownTypes,
                knownFunctions: knownFunctions,
                builder: b,
                seqNodeForLoc: seq
            )
            return
        }

        // Otherwise: generic binary chain (handles + - * / % == < <= > >=, etc.)
        try compileBinaryChain(from: elems, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
    }

    /// Right-associative lowering of nil-coalescing expressions using dup/jumpIfNil/pop.
    private func compileNilCoalescingSequence(
        _ elems: [ExprSyntax],
        knownTypes: Set<String>?,
        knownFunctions: Set<String>?,
        builder b: BytecodeBuilder,
        seqNodeForLoc seq: SequenceExprSyntax
    ) throws {
        // Find rightmost '??'
        var splitIndex: Int? = nil
        var i = elems.count - 1
        while i >= 0 {
            if let bin = elems[i].as(BinaryOperatorExprSyntax.self) {
                let s = bin.operatorToken.text.trimmingCharacters(in: .whitespacesAndNewlines)
                if s == "??" { splitIndex = i; break }
            }
            i -= 1
        }
        guard let idx = splitIndex else {
            // No '??' at this level: delegate back to generic handling
            let hasLogical = elems.contains {
                if let bin = $0.as(BinaryOperatorExprSyntax.self) {
                    let s = bin.operatorToken.text.trimmingCharacters(in: .whitespacesAndNewlines)
                    return s == "&&" || s == "||"
                }
                return false
            }
            if hasLogical {
                try compileLogicalSequence(elems, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b, seqNodeForLoc: seq)
            } else {
                try compileBinaryChain(from: elems, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            }
            return
        }

        let leftSlice = Array(elems[0..<idx])
        let rightSlice = Array(elems[(idx + 1)..<elems.count])

        // Compile LHS (may contain nested ?? or logical/arithmetic chains)
        try compileSlice(leftSlice, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b, seqNodeForLoc: seq)
        // dup; jumpIfNil -> nil path
        b.emit(.dup, at: seq)
        let jf = b.emit(.jumpIfNil(0), at: seq)
        // Non-nil path: result is lhs; jump to end
        let jEnd = b.emit(.jump(0), at: seq)
        // Nil path: patch, pop lhs, compile RHS
        let toNil = b.count - (jf + 1)
        b.patch(jf, to: .jumpIfNil(toNil))
        b.emit(.pop, at: seq)
        try compileSlice(rightSlice, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b, seqNodeForLoc: seq)
        // End
        let toEnd = b.count - (jEnd + 1)
        b.patch(jEnd, to: .jump(toEnd))
    }

    private func compileSlice(
        _ elems: [ExprSyntax],
        knownTypes: Set<String>?,
        knownFunctions: Set<String>?,
        builder b: BytecodeBuilder,
        seqNodeForLoc seq: SequenceExprSyntax
    ) throws {
        if elems.count == 1 {
            try compileExpr(elems[0], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            return
        }
        // Recurse if it still contains '??' to enforce right-assoc
        if elems.contains(where: { e in
            if let bin = e.as(BinaryOperatorExprSyntax.self) {
                return bin.operatorToken.text.trimmingCharacters(in: .whitespacesAndNewlines) == "??"
            }
            return false
        }) {
            try compileNilCoalescingSequence(elems, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b, seqNodeForLoc: seq)
            return
        }
        // Otherwise choose logical or generic binary chain
        let hasLogical = elems.contains {
            if let bin = $0.as(BinaryOperatorExprSyntax.self) {
                let s = bin.operatorToken.text.trimmingCharacters(in: .whitespacesAndNewlines)
                return s == "&&" || s == "||"
            }
            return false
        }
        if hasLogical {
            try compileLogicalSequence(elems, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b, seqNodeForLoc: seq)
        } else {
            try compileBinaryChain(from: elems, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
        }
    }

    // Extract immediate expression children from a Syntax node (version-agnostic helper)
    private func exprChildren(of node: Syntax) -> [ExprSyntax] {
        var out: [ExprSyntax] = []
        for child in node.children(viewMode: .sourceAccurate) {
            if let e = child.as(ExprSyntax.self) { out.append(e) }
        }
        return out
    }

    // Find the first expression strictly under a Syntax node (deep search, prefer descendants over self).
    // This avoids returning the same node and recursing indefinitely when callers already have an ExprSyntax.
    private func firstExprDescendant(of node: Syntax) -> ExprSyntax? {
        // Search children first
        for child in node.children(viewMode: .sourceAccurate) {
            if let found = firstExprDescendant(of: child) { return found }
        }
        // Fallback to self only if no deeper expression exists
        if let e = node.as(ExprSyntax.self) { return e }
        return nil
    }

    // Choose the widest (by textual length) descendant expression under a Syntax node.
    // Useful for string interpolation segments where multiple nested expression nodes
    // exist (e.g., SequenceExpr containing integer literals and operators).
    private func largestExprDescendant(of node: Syntax) -> ExprSyntax? {
        var best: ExprSyntax? = nil
        func walk(_ n: Syntax) {
            if let e = n.as(ExprSyntax.self) {
                if let cur = best {
                    if e.description.count >= cur.description.count { best = e }
                } else {
                    best = e
                }
            }
            for child in n.children(viewMode: .sourceAccurate) { walk(child) }
        }
        walk(node)
        return best
    }

    /// Precedence-aware compilation for logical sequences within a SequenceExpr.
    /// Implements: OR of AND-terms, where AND has higher precedence than OR.
    private func compileLogicalSequence(
        _ elems: [ExprSyntax],
        knownTypes: Set<String>?,
        knownFunctions: Set<String>?,
        builder b: BytecodeBuilder,
        seqNodeForLoc seq: SequenceExprSyntax
    ) throws {
        var i = 0

        func nextIsLogical(_ sym: String) -> Bool {
            guard i < elems.count, let bin = elems[i].as(BinaryOperatorExprSyntax.self) else { return false }
            return bin.operatorToken.text.trimmingCharacters(in: .whitespacesAndNewlines) == sym
        }

        func isLogical(_ e: ExprSyntax) -> String? {
            if let bin = e.as(BinaryOperatorExprSyntax.self) {
                let s = bin.operatorToken.text.trimmingCharacters(in: .whitespacesAndNewlines)
                return (s == "&&" || s == "||") ? s : nil
            }
            return nil
        }

        func compilePlainTerm() throws {
            var slice: [ExprSyntax] = []
            // Collect until next logical operator or end
            while i < elems.count {
                if isLogical(elems[i]) != nil { break }
                slice.append(elems[i])
                i += 1
            }
            if slice.isEmpty { throw CompileError.unsupported("Logical operator with empty operand") }
            if slice.count == 1 {
                try compileExpr(slice[0], knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            } else {
                try compileBinaryChain(from: slice, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            }
        }

        // AND-chain: left already compiled on stack; fold successive && terms with short-circuit
        func compileAndChain() throws {
            try compilePlainTerm()
            while nextIsLogical("&&") {
                // Emit jump-if-false based on current accumulated result
                let lhsLocNode = (i > 0 ? elems[i-1] : ExprSyntax(seq))
                _ = i // current points at '&&'
                let jfLeft = b.emit(.jumpIfFalse(0), at: lhsLocNode)
                // consume '&&'
                i += 1
                // compile RHS term
                try compilePlainTerm()
                let rhsLocNode = (i > 0 ? elems[i-1] : ExprSyntax(seq))
                let jfRight = b.emit(.jumpIfFalse(0), at: rhsLocNode)
                // both true → true; jump to end
                b.emit(.loadConst(.bool(true)), at: rhsLocNode)
                let jEnd = b.emit(.jump(0), at: rhsLocNode)
                // patch falses → false
                let toFalseFromLeft  = b.count - (jfLeft + 1)
                b.patch(jfLeft,  to: .jumpIfFalse(toFalseFromLeft))
                let toFalseFromRight = b.count - (jfRight + 1)
                b.patch(jfRight, to: .jumpIfFalse(toFalseFromRight))
                b.emit(.loadConst(.bool(false)), at: rhsLocNode)
                // end
                let toEnd = b.count - (jEnd + 1)
                b.patch(jEnd, to: .jump(toEnd))
            }
        }

        // OR-chain: fold conjunction terms separated by ||
        try compileAndChain()
        while nextIsLogical("||") {
            // If accumulated left is false → evaluate RHS; else → true
            let lhsLocNode = (i > 0 ? elems[i-1] : ExprSyntax(seq))
            let jfToRight = b.emit(.jumpIfFalse(0), at: lhsLocNode)
            b.emit(.loadConst(.bool(true)), at: lhsLocNode)
            let jEnd1 = b.emit(.jump(0), at: lhsLocNode)
            let toRight = b.count - (jfToRight + 1)
            b.patch(jfToRight, to: .jumpIfFalse(toRight))
            // consume '||' and compile RHS as an AND-chain term
            i += 1
            try compileAndChain()
            let rhsLocNode = (i > 0 ? elems[i-1] : ExprSyntax(seq))
            let jfRight = b.emit(.jumpIfFalse(0), at: rhsLocNode)
            b.emit(.loadConst(.bool(true)), at: rhsLocNode)
            let jEnd2 = b.emit(.jump(0), at: rhsLocNode)
            let toFalse = b.count - (jfRight + 1)
            b.patch(jfRight, to: .jumpIfFalse(toFalse))
            b.emit(.loadConst(.bool(false)), at: rhsLocNode)
            // patch both ends to here
            let toEnd1 = b.count - (jEnd1 + 1)
            b.patch(jEnd1, to: .jump(toEnd1))
            let toEnd2 = b.count - (jEnd2 + 1)
            b.patch(jEnd2, to: .jump(toEnd2))
        }
    }
    
    /// Compile RHS part of a sequence (no assignment), honoring operator precedence.
    private func compileSequenceRHSChain(
        _ elems: [ExprSyntax],
        knownTypes: Set<String>?,
        knownFunctions: Set<String>?,
        builder b: BytecodeBuilder
    ) throws {
        try compileBinaryChain(from: elems, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
    }
    
    /// Shunting-yard style compilation: compile operands immediately (push) and
    /// enqueue operators; emit operators according to precedence/associativity.
    private func compileBinaryChain(
    from elems: [ExprSyntax],
    knownTypes: Set<String>?,
    knownFunctions: Set<String>?,
    builder b: BytecodeBuilder
) throws {
    var opStack: [PendingOp] = []

    func flushWhile(_ shouldPop: (PendingOp) -> Bool) throws {
        while let top = opStack.last, shouldPop(top) {
            _ = opStack.popLast()
            try emitBinaryOp(top.symbol, at: top.node, builder: b)
        }
    }

    var expectOperand = true

    for e in elems {
        if let bin = e.as(BinaryOperatorExprSyntax.self) {
            let sym = bin.operatorToken.text.trimmingCharacters(in: .whitespacesAndNewlines)
            // assignment handled in compileSequenceExpr; logical handled by the logical splitter
            if sym == "=" { throw CompileError.unsupported("Unexpected '=' in binary chain") }
            if sym == "&&" || sym == "||" { throw CompileError.unsupported("Logical operator '\(sym)' must be handled in the logical chain") }
            guard let (pCur, leftAssoc) = precedence(for: sym) else {
                throw CompileError.unsupported("Operator '\(sym)' not supported")
            }
            try flushWhile { top in
                guard let (pTop, _) = precedence(for: top.symbol) else { return false }
                return (pTop > pCur) || (pTop == pCur && leftAssoc)
            }
            opStack.append(.init(symbol: sym, node: bin))
            expectOperand = true
        } else {
            try compileExpr(e, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            expectOperand = false
        }
    }

    // If the chain ends with an operator, it's malformed.
    if expectOperand, !opStack.isEmpty {
        throw CompileError.unsupported("Trailing operator in binary chain")
    }

    while let top = opStack.popLast() {
        try emitBinaryOp(top.symbol, at: top.node, builder: b)
    }
}
    
    private func compileCodeBlock(
        _ block: CodeBlockSyntax,
        knownTypes: Set<String>,
        knownFunctions: Set<String>,
        builder b: BytecodeBuilder,
        treatAsTopScope: Bool = false
    ) throws {
        let hasBindings = blockIntroducesBindings(block)
        if options.mode == .slot {
            // In slot-mode, emit enterScope only if block introduces bindings
            if !treatAsTopScope && hasBindings {
                let n = countBindings(block)
                if let slotRes = slot { slotRes.push(total: n) }
                let enterIdx = b.emit(.enterScope(nLocals: n), at: block)
                for item in block.statements {
                    try compileStatementNode(item.item, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                }
                let leaveIdx = b.emit(.leaveScope, at: block)
                if let scope = slot?.scopes.last {
                    var bySlot: [Int:String] = [:]
                    for (name, info) in scope.map {
                        bySlot[info.slot] = name
                    }
                    b.addLocalsMap(start: enterIdx, end: leaveIdx, depth: 0, namesBySlot: bySlot)
                    b.addScopeNote(start: enterIdx, end: leaveIdx, kind: "block", name: nil)
                }
                slot?.pop()
                return
            }
        } else {
            let needsScope = !treatAsTopScope && hasBindings
            if needsScope {
                b.emit(.pushScope, at: block)
                nameBindingsStack.append([])
                typeBindingsStack.append([:])
                wrapperLocalStack.append([:])
            }
            for item in block.statements {
                try compileStatementNode(item.item, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            }
            if needsScope {
                b.emit(.popScope, at: block)
                _ = nameBindingsStack.popLast()
                _ = typeBindingsStack.popLast()
                _ = wrapperLocalStack.popLast()
            }
            return
        }
        // No scope needed (either mode): just compile statements
        for item in block.statements {
            try compileStatementNode(item.item, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
        }
    }

    // Analyze whether a block introduces bindings at this depth
    private func blockIntroducesBindings(_ block: CodeBlockSyntax) -> Bool {
        for item in block.statements {
            if let vd = item.item.as(VariableDeclSyntax.self) {
                let spec = vd.bindingSpecifier.text
                if spec == "var" || spec == "let" { return true }
            }
            if let fn = item.item.as(FunctionDeclSyntax.self), fn.body != nil { return true }
        }
        return false
    }

    private func countBindings(_ block: CodeBlockSyntax) -> Int {
        var n = 0
        for item in block.statements {
            if let vd = item.item.as(VariableDeclSyntax.self) {
                let spec = vd.bindingSpecifier.text
                if spec == "var" || spec == "let" {
                    for binding in vd.bindings {
                        if binding.pattern.is(IdentifierPatternSyntax.self) { n += 1 }
                    }
                }
            }
            if let fn = item.item.as(FunctionDeclSyntax.self), fn.body != nil {
                n += 1
            }
        }
        return n
    }

    private func countBindings(in stmts: CodeBlockItemListSyntax) -> Int {
        var n = 0
        for item in stmts {
            if let vd = item.item.as(VariableDeclSyntax.self) {
                let spec = vd.bindingSpecifier.text
                if spec == "var" || spec == "let" {
                    for binding in vd.bindings {
                        if binding.pattern.is(IdentifierPatternSyntax.self) { n += 1 }
                    }
                }
            }
            if let fn = item.item.as(FunctionDeclSyntax.self), fn.body != nil {
                n += 1
            }
        }
        return n
    }

    private func compileLocalFunctionDecl(
        _ fn: FunctionDeclSyntax,
        knownTypes: Set<String>,
        knownFunctions: Set<String>,
        builder b: BytecodeBuilder
    ) throws {
        guard let body = fn.body else {
            throw CompileError.unsupported("Local function declaration requires a body")
        }
        let name = fn.name.text
        let params: [String] = fn.signature.parameterClause.parameters.enumerated().map { paramLocalName($0, $1) }

        let usingSlots = options.mode == .slot && slotResolutionEnabled
        var declaredSlot: Int? = nil
        if usingSlots, let slotRes = slot, slotRes.hasActiveScope {
            declaredSlot = try slotRes.declare(name, isConst: true)
        } else if options.mode == .name {
            if var top = nameBindingsStack.popLast() {
                top.insert(name)
                nameBindingsStack.append(top)
            }
        }

        let prologueJump = b.emit(.jump(0), at: fn)
        let compiled = try compileCallableBody(
            params: params,
            body: body,
            kind: "function",
            name: name,
            knownTypes: knownTypes,
            knownFunctions: knownFunctions,
            builder: b,
            noteName: name
        )
        let afterBody = b.count
        b.patch(prologueJump, to: .jump(afterBody - (prologueJump + 1)))

        let captureSet = collectFunctionCaptures(
            from: fn,
            params: Set(params),
            knownTypes: knownTypes,
            knownFunctions: knownFunctions
        )
        let captures = Array(captureSet).sorted()

        if options.mode == .slot && slotResolutionEnabled, let slotRes = slot, slotRes.hasActiveScope {
            for cap in captures {
                if let hit = slotRes.resolve(cap) {
                    b.emit(.loadLocal(depth: hit.depth, slot: hit.slot), at: fn)
                } else {
                    b.emit(.loadVar(cap), at: fn)
                }
            }
            // TODO: Extract type information from function signature for local functions
            let paramTypes: [String] = []
            let returnType: String? = nil
            b.emit(.makeClosureVals(params: params, paramTypes: paramTypes, returnType: returnType, entry: compiled.entry, captures: captures), at: fn)
            let slotIndex: Int
            if let s = declaredSlot {
                slotIndex = s
            } else {
                slotIndex = try slotRes.declare(name, isConst: true)
            }
            b.emit(.defLocal(slot: slotIndex, isConst: true), at: fn)
        } else {
            for cap in captures {
                b.emit(.loadVar(cap), at: fn)
            }
            // TODO: Extract type information from function signature for local functions
            let paramTypes: [String] = []
            let returnType: String? = nil
            b.emit(.makeClosure(params: params, paramTypes: paramTypes, returnType: returnType, entry: compiled.entry, captures: captures), at: fn)
            b.emit(.defLet(name), at: fn)
            if options.mode != .slot, var top = nameBindingsStack.popLast() {
                top.insert(name)
                nameBindingsStack.append(top)
            }
        }
    }

    private func compileStatementNode<T: SyntaxProtocol>(
        _ nodeAny: T,
        knownTypes: Set<String>,
        knownFunctions: Set<String>,
        builder b: BytecodeBuilder
    ) throws {
        let node = Syntax(nodeAny)
        if node.is(StructDeclSyntax.self) { return }
        if let fn = node.as(FunctionDeclSyntax.self) {
            try compileLocalFunctionDecl(fn, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            return
        }

        if let rep = node.as(RepeatStmtSyntax.self) {
            try compileRepeatStmt(rep, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            return
        }

        if let fi = node.as(ForInStmtSyntax.self) {
            try compileForInStmt(fi, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            return
        }

        if let w = node.as(WhileStmtSyntax.self) {
            try compileWhileStmt(w, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            return
        }
        if let g = node.as(GuardStmtSyntax.self) {
            try compileGuardStmt(g, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            return
        }
        if let br = node.as(BreakStmtSyntax.self) {
            // break: jump to end of current loop (patch later)
            guard !b.loopStack.isEmpty else { throw CompileError.unsupported("'break' outside of loop") }
            let j = b.emit(.jump(0), at: br)
            b.recordBreakSite(j)
            return
        }
        if let cont = node.as(ContinueStmtSyntax.self) {
            // continue: jump to loop head (condition) (patch later)
            guard !b.loopStack.isEmpty else { throw CompileError.unsupported("'continue' outside of loop") }
            let j = b.emit(.jump(0), at: cont)
            b.recordContinueSite(j)
            return
        }
        if let v = node.as(VariableDeclSyntax.self) {
            // Disallow shadowing 'self' inside method bodies at statement boundary as well
            if implicitSelfContext != nil {
                for binding in v.bindings {
                    if let id = binding.pattern.as(IdentifierPatternSyntax.self), id.identifier.text == "self" {
                        throw CompileError.message("cannot redeclare 'self' in method scope")
                    }
                    // Fallback: token-level scan for 'self' within the pattern
                    for tok in binding.pattern.tokens(viewMode: .sourceAccurate) {
                        if tok.text == "self" {
                            throw CompileError.message("cannot redeclare 'self' in method scope")
                        }
                    }
                }
            }
            try compileVariableDecl(v, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            return
        }
        if let e = node.as(ExpressionStmtSyntax.self) {
            try compileExpr(e.expression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            return
        }
        if let i = node.as(IfExprSyntax.self) {
            try compileIfExpr(i, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            return
        }
        if let r = node.as(ReturnStmtSyntax.self) {
            if let value = r.expression {
                try compileExpr(value, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            } else {
                b.emit(.loadConst(.bool(true)), at: r)
            }
            b.emit(.ret, at: r)
            return
        }
        if let expr = ExprSyntax(node) {
            try compileExpr(expr, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            return
        }
    }
    
    // MARK: - repeat { ... } while cond

    private func compileGuardStmt(
        _ gs: GuardStmtSyntax,
        knownTypes: Set<String>,
        knownFunctions: Set<String>,
        builder b: BytecodeBuilder
    ) throws {
        struct PendingBinding { let name: String; let isConst: Bool; let node: Syntax }
        var failJumps: [(index: Int, isNil: Bool, bindingDepth: Int)] = []
        var bindings: [PendingBinding] = []

        for cond in gs.conditions {
            let c = cond.condition
            if let e = c.as(ExprSyntax.self) {
                try compileExpr(e, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                let j = b.emit(.jumpIfFalse(0), at: e)
                failJumps.append((j, false, 0))
                continue
            }
            if let ob = c.as(OptionalBindingConditionSyntax.self) {
                guard let initExpr = ob.initializer?.value else {
                    throw CompileError.unsupported("guard binding without initializer")
                }
                guard let idPat = ob.pattern.as(IdentifierPatternSyntax.self) else {
                    throw CompileError.unsupported("guard binding only supports identifier pattern")
                }
                let isConst = ob.bindingSpecifier.text == "let"
                let name = idPat.identifier.text
                try compileExpr(initExpr, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                b.emit(.dup, at: ob)
                let jn = b.emit(.jumpIfNil(0), at: ob)
                failJumps.append((jn, true, 0))
                bindings.append(.init(name: name, isConst: isConst, node: Syntax(ob)))
                continue
            }
            throw CompileError.unsupported("Unsupported guard condition kind: \(c.kind)")
        }

        // Success path: define bindings in reverse order to consume stack
        if !bindings.isEmpty {
            for bind in bindings.reversed() {
                if bind.isConst { b.emit(.defLet(bind.name), at: bind.node) }
                else            { b.emit(.defVar(bind.name), at: bind.node) }
                if options.mode == .name, !nameBindingsStack.isEmpty {
                    var top = nameBindingsStack.removeLast(); top.insert(bind.name); nameBindingsStack.append(top)
                }
            }
        }

        // Jump over the else body when all conditions pass
        let jmpEnd = b.emit(.jump(0), at: gs)
        // Patch all fail jumps to land at else start
        let elseStart = b.count
        for failure in failJumps {
            let offset = elseStart - (failure.index + 1)
            if failure.isNil { b.patch(failure.index, to: .jumpIfNil(offset)) }
            else { b.patch(failure.index, to: .jumpIfFalse(offset)) }
        }
        // On failure, pop any stacked binding values
        if !bindings.isEmpty { for _ in bindings { b.emit(.pop, at: gs) } }
        // Compile else body
        try compileCodeBlock(gs.body, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
        // Land after else
        let toEnd = b.count - (jmpEnd + 1)
        b.patch(jmpEnd, to: .jump(toEnd))
    }

    private func compileRepeatStmt(
        _ rs: RepeatStmtSyntax,
        knownTypes: Set<String>,
        knownFunctions: Set<String>,
        builder b: BytecodeBuilder
    ) throws {
        // Layout:
        //   jmp Lbody            ; skip initial cond
        // Lcond:
        //   eval cond; jumpIfFalse Lend
        // Lbody:
        //   ... body ...
        //   jump Lcond
        // Lend:

        // Jump over initial condition on first entry
        let jmpToBody = b.emit(.jump(0), at: rs)

        // Condition label
        let condStart = b.count
        guard let condExpr = rs.condition.as(ExprSyntax.self) else {
            throw CompileError.unsupported("repeat-while without condition expression")
        }
        try compileExpr(condExpr, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
        let jifIndex = b.emit(.jumpIfFalse(0), at: condExpr)

        // Body
        let bodyStart = b.count
        // Patch jump-over-cond to land at body start
        b.patch(jmpToBody, to: .jump(bodyStart - (jmpToBody + 1)))

        // Continue should target condition start
        b.pushLoop(startIndex: condStart)
        try compileCodeBlock(rs.body, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)

        // Back edge to condition
        let back = condStart - (b.count + 1)
        b.emit(.jump(back), at: rs)

        // Patch exits and loop sites
        let endIndex = b.count
        let toEnd = endIndex - (jifIndex + 1)
        b.patch(jifIndex, to: .jumpIfFalse(toEnd))
        if let ctx = b.popLoop() {
            for site in ctx.breakSites {
                b.patch(site, to: .jump(endIndex - (site + 1)))
            }
            for site in ctx.continueSites {
                b.patch(site, to: .jump(condStart - (site + 1)))
            }
        }
    }

    // MARK: - for-in over numeric ranges: for i in a..<b { ... } / for i in a...b { ... }
    private func compileForInStmt(
        _ fs: ForInStmtSyntax,
        knownTypes: Set<String>,
        knownFunctions: Set<String>,
        builder b: BytecodeBuilder
    ) throws {
        guard let pat = fs.pattern.as(IdentifierPatternSyntax.self) else {
            throw CompileError.unsupported("for-in supports only simple identifier pattern")
        }
        if fs.whereClause != nil {
            throw CompileError.unsupported("for-in 'where' clause not supported")
        }

        if let (lowerExpr, isClosed, upperExpr) = parseRangeExpr(fs.sequence) {
            try compileForInRange(
                loopVar: pat,
                lowerExpr: lowerExpr,
                isClosed: isClosed,
                upperExpr: upperExpr,
                stmt: fs,
                knownTypes: knownTypes,
                knownFunctions: knownFunctions,
                builder: b
            )
        } else {
            try compileForInSequence(
                loopVar: pat,
                sequenceExpr: fs.sequence,
                stmt: fs,
                knownTypes: knownTypes,
                knownFunctions: knownFunctions,
                builder: b
            )
        }
    }

    private func compileForInRange(
        loopVar: IdentifierPatternSyntax,
        lowerExpr: ExprSyntax,
        isClosed: Bool,
        upperExpr: ExprSyntax,
        stmt fs: ForInStmtSyntax,
        knownTypes: Set<String>,
        knownFunctions: Set<String>,
        builder b: BytecodeBuilder
    ) throws {
        let loopVarName = loopVar.identifier.text

        var loopVarSlot: Int? = nil
        var enterIdx: Int? = nil
        if options.mode == .slot {
            slot?.push(total: 1)
            enterIdx = b.emit(.enterScope(nLocals: 1), at: fs)
            loopVarSlot = try slot?.declare(loopVarName, isConst: false)
        } else {
            b.emit(.pushScope, at: fs)
        }

        try compileExpr(lowerExpr, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
        if let slotIndex = loopVarSlot {
            b.emit(.defLocal(slot: slotIndex, isConst: false), at: loopVar)
        } else {
            b.emit(.defVar(loopVarName), at: loopVar)
        }

        let condStart = b.count
        if let slotIndex = loopVarSlot {
            b.emit(.loadLocal(depth: 0, slot: slotIndex), at: fs)
        } else {
            b.emit(.loadVar(loopVarName), at: fs)
        }
        try compileExpr(upperExpr, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
        b.emit(isClosed ? .le : .lt, at: fs)
        let jifIndex = b.emit(.jumpIfFalse(0), at: fs)

        let jmpToBody = b.emit(.jump(0), at: fs)

        let continueTarget = b.count
        if let slotIndex = loopVarSlot {
            b.emit(.loadLocal(depth: 0, slot: slotIndex), at: fs)
        } else {
            b.emit(.loadVar(loopVarName), at: fs)
        }
        b.emit(.loadConst(.int(1)), at: fs)
        b.emit(.add, at: fs)
        if let slotIndex = loopVarSlot {
            b.emit(.setLocal(depth: 0, slot: slotIndex), at: fs)
        } else {
            b.emit(.setVar(loopVarName), at: fs)
        }
        let back = condStart - (b.count + 1)
        b.emit(.jump(back), at: fs)

        let bodyStart = b.count
        b.patch(jmpToBody, to: .jump(bodyStart - (jmpToBody + 1)))

        b.pushLoop(startIndex: continueTarget)
        try compileCodeBlock(fs.body, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
        b.emit(.jump(continueTarget - (b.count + 1)), at: fs)

        let leaveIdx: Int
        if options.mode == .slot {
            leaveIdx = b.emit(.leaveScope, at: fs)
            if let slotRes = slot, let scope = slotRes.scopes.last, let enter = enterIdx {
                var bySlot: [Int: String] = [:]
                for (name, info) in scope.map {
                    bySlot[info.slot] = name
                }
                b.addLocalsMap(start: enter, end: leaveIdx, depth: 0, namesBySlot: bySlot)
                b.addScopeNote(start: enter, end: leaveIdx, kind: "forScope", name: loopVarName)
            }
            slot?.pop()
        } else {
            leaveIdx = b.emit(.popScope, at: fs)
        }

        b.patch(jifIndex, to: .jumpIfFalse(leaveIdx - (jifIndex + 1)))

        if let ctx = b.popLoop() {
            for site in ctx.breakSites {
                b.patch(site, to: .jump(leaveIdx - (site + 1)))
            }
            for site in ctx.continueSites {
                b.patch(site, to: .jump(continueTarget - (site + 1)))
            }
        }
    }

    private func compileForInSequence(
        loopVar: IdentifierPatternSyntax,
        sequenceExpr: ExprSyntax,
        stmt fs: ForInStmtSyntax,
        knownTypes: Set<String>,
        knownFunctions: Set<String>,
        builder b: BytecodeBuilder
    ) throws {
        let loopVarName = loopVar.identifier.text
        let seqName = "$__for_seq_\(loopVarName)_\(b.count)"
        let countName = "$__for_count_\(loopVarName)_\(b.count)"
        let indexName = "$__for_index_\(loopVarName)_\(b.count)"

        var loopVarSlot: Int? = nil
        var seqSlot: Int? = nil
        var countSlot: Int? = nil
        var indexSlot: Int? = nil
        var enterIdx: Int? = nil

        if options.mode == .slot {
            slot?.push(total: 4)
            enterIdx = b.emit(.enterScope(nLocals: 4), at: fs)
            loopVarSlot = try slot?.declare(loopVarName, isConst: false)
            seqSlot = try slot?.declare(seqName, isConst: true)
            countSlot = try slot?.declare(countName, isConst: true)
            indexSlot = try slot?.declare(indexName, isConst: false)
        } else {
            b.emit(.pushScope, at: fs)
        }

        b.emit(.loadConst(.nilValue), at: loopVar)
        if let slotIndex = loopVarSlot {
            b.emit(.defLocal(slot: slotIndex, isConst: false), at: loopVar)
        } else {
            b.emit(.defVar(loopVarName), at: loopVar)
        }

        try compileExpr(sequenceExpr, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
        if let slotIndex = seqSlot {
            b.emit(.defLocal(slot: slotIndex, isConst: true), at: sequenceExpr)
        } else {
            b.emit(.defLet(seqName), at: sequenceExpr)
        }

        if let slotIndex = seqSlot {
            b.emit(.loadLocal(depth: 0, slot: slotIndex), at: sequenceExpr)
        } else {
            b.emit(.loadVar(seqName), at: sequenceExpr)
        }
        let pidCount = b.internProperty("count")
        b.emit(.getProp(pidCount), at: sequenceExpr)
        if let slotIndex = countSlot {
            b.emit(.defLocal(slot: slotIndex, isConst: true), at: sequenceExpr)
        } else {
            b.emit(.defLet(countName), at: sequenceExpr)
        }

        b.emit(.loadConst(.int(0)), at: sequenceExpr)
        if let slotIndex = indexSlot {
            b.emit(.defLocal(slot: slotIndex, isConst: false), at: sequenceExpr)
        } else {
            b.emit(.defVar(indexName), at: sequenceExpr)
        }

        let condStart = b.count
        if let slotIndex = indexSlot {
            b.emit(.loadLocal(depth: 0, slot: slotIndex), at: fs)
        } else {
            b.emit(.loadVar(indexName), at: fs)
        }
        if let slotIndex = countSlot {
            b.emit(.loadLocal(depth: 0, slot: slotIndex), at: fs)
        } else {
            b.emit(.loadVar(countName), at: fs)
        }
        b.emit(.lt, at: fs)
        let jifIndex = b.emit(.jumpIfFalse(0), at: fs)

        let jmpToBody = b.emit(.jump(0), at: fs)

        let continueTarget = b.count
        if let slotIndex = indexSlot {
            b.emit(.loadLocal(depth: 0, slot: slotIndex), at: fs)
        } else {
            b.emit(.loadVar(indexName), at: fs)
        }
        b.emit(.loadConst(.int(1)), at: fs)
        b.emit(.add, at: fs)
        if let slotIndex = indexSlot {
            b.emit(.setLocal(depth: 0, slot: slotIndex), at: fs)
        } else {
            b.emit(.setVar(indexName), at: fs)
        }
        let back = condStart - (b.count + 1)
        b.emit(.jump(back), at: fs)

        let bodyStart = b.count
        b.patch(jmpToBody, to: .jump(bodyStart - (jmpToBody + 1)))

        if let slotIndex = seqSlot {
            b.emit(.loadLocal(depth: 0, slot: slotIndex), at: fs)
        } else {
            b.emit(.loadVar(seqName), at: fs)
        }
        if let slotIndex = indexSlot {
            b.emit(.loadLocal(depth: 0, slot: slotIndex), at: fs)
        } else {
            b.emit(.loadVar(indexName), at: fs)
        }
        b.emit(.getIndex, at: fs)
        if let slotIndex = loopVarSlot {
            b.emit(.setLocal(depth: 0, slot: slotIndex), at: loopVar)
        } else {
            b.emit(.setVar(loopVarName), at: loopVar)
        }

        b.pushLoop(startIndex: continueTarget)
        try compileCodeBlock(fs.body, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
        b.emit(.jump(continueTarget - (b.count + 1)), at: fs)

        let leaveIdx: Int
        if options.mode == .slot {
            leaveIdx = b.emit(.leaveScope, at: fs)
            if let slotRes = slot, let scope = slotRes.scopes.last, let enter = enterIdx {
                var bySlot: [Int: String] = [:]
                for (name, info) in scope.map {
                    bySlot[info.slot] = name
                }
                b.addLocalsMap(start: enter, end: leaveIdx, depth: 0, namesBySlot: bySlot)
                b.addScopeNote(start: enter, end: leaveIdx, kind: "forScope", name: loopVarName)
            }
            slot?.pop()
        } else {
            leaveIdx = b.emit(.popScope, at: fs)
        }

        b.patch(jifIndex, to: .jumpIfFalse(leaveIdx - (jifIndex + 1)))

        if let ctx = b.popLoop() {
            for site in ctx.breakSites {
                b.patch(site, to: .jump(leaveIdx - (site + 1)))
            }
            for site in ctx.continueSites {
                b.patch(site, to: .jump(continueTarget - (site + 1)))
            }
        }
    }

    // Parse 'a..<b' or 'a...b' out of an expression; returns (lower, isClosed, upper)
    private func parseRangeExpr(_ expr: ExprSyntax) -> (ExprSyntax, Bool, ExprSyntax)? {
        if let seq = expr.as(SequenceExprSyntax.self) {
            let elems = Array(seq.elements)
            if elems.count == 3, let bin = elems[1].as(BinaryOperatorExprSyntax.self) {
                let op = bin.operatorToken.text.trimmingCharacters(in: .whitespacesAndNewlines)
                if op == "..<" || op == "..." {
                    return (elems[0], op == "...", elems[2])
                }
            }
        }
        if let bin = expr.as(InfixOperatorExprSyntax.self), let bop = bin.operator.as(BinaryOperatorExprSyntax.self) {
            let op = bop.operatorToken.text.trimmingCharacters(in: .whitespacesAndNewlines)
            if op == "..<" || op == "..." {
                return (bin.leftOperand, op == "...", bin.rightOperand)
            }
        }
        return nil
    }

    private func compileRangeLiteral(
        lowerExpr: ExprSyntax,
        isClosed: Bool,
        upperExpr: ExprSyntax,
        node: ExprSyntax,
        knownTypes: Set<String>?,
        knownFunctions: Set<String>?,
        builder b: BytecodeBuilder
    ) throws {
        try compileExpr(lowerExpr, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
        try compileExpr(upperExpr, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
        b.emit(.makeRange(isClosed: isClosed), at: node)
    }

    private func compileWhileStmt(
        _ ws: WhileStmtSyntax,
        knownTypes: Set<String>,
        knownFunctions: Set<String>,
        builder b: BytecodeBuilder
    ) throws {
        let loopStart = b.count
        b.pushLoop(startIndex: loopStart)
        // condition
        guard let condExpr = ws.conditions.first?.condition.as(ExprSyntax.self) else {
            throw CompileError.unsupported("while without condition")
        }
        try compileExpr(condExpr, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
        let jifIndex = b.emit(.jumpIfFalse(0), at: condExpr)
        
        // body
        try compileCodeBlock(ws.body, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
        
        // back edge
        let back = loopStart - (b.count + 1)
        b.emit(.jump(back), at: ws)
        // patch exit
        let toEnd = b.count - (jifIndex + 1)
        b.patch(jifIndex, to: .jumpIfFalse(toEnd))

        // Patch any pending breaks to end, and continues to loopStart
        if let ctx = b.popLoop() {
            for site in ctx.breakSites {
                let off = b.count - (site + 1)
                b.patch(site, to: .jump(off))
            }
            for site in ctx.continueSites {
                let off = loopStart - (site + 1)
                b.patch(site, to: .jump(off))
            }
        }
    }
    
    private func compileLogicalInfix(
        lhs: ExprSyntax,
        opSymbol: String,
        rhs: ExprSyntax,
        knownTypes: Set<String>?,
        knownFunctions: Set<String>?,
        builder b: BytecodeBuilder
    ) throws {
        switch opSymbol {
        case "&&":
            // LHS
            try compileExpr(lhs, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            let jfLeft = b.emit(.jumpIfFalse(0), at: lhs)
            
            // RHS
            try compileExpr(rhs, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            let jfRight = b.emit(.jumpIfFalse(0), at: rhs)
            
            
            // both true -> push true; then jump to end
            b.emit(.loadConst(.bool(true)), at: rhs)
            let jEnd = b.emit(.jump(0), at: rhs)
            
            // patch false paths
            let toFalseFromLeft  = b.count - (jfLeft + 1)
            b.patch(jfLeft,  to: .jumpIfFalse(toFalseFromLeft))
            let toFalseFromRight = b.count - (jfRight + 1)
            b.patch(jfRight, to: .jumpIfFalse(toFalseFromRight))
            
            // push false
            b.emit(.loadConst(.bool(false)), at: rhs)
            
            // patch end
            let toEnd = b.count - (jEnd + 1)
            b.patch(jEnd, to: .jump(toEnd))
        case "||":
            // LHS
            try compileExpr(lhs, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            let jfToRight = b.emit(.jumpIfFalse(0), at: lhs)
            
            // left true -> result true
            b.emit(.loadConst(.bool(true)), at: lhs)
            let jEnd1 = b.emit(.jump(0), at: lhs)
            
            // patch to evaluate RHS
            let toRight = b.count - (jfToRight + 1)
            b.patch(jfToRight, to: .jumpIfFalse(toRight))
            
            
            // RHS
            try compileExpr(rhs, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            let jfRight = b.emit(.jumpIfFalse(0), at: rhs)
        
            
            // right true -> true
            b.emit(.loadConst(.bool(true)), at: rhs)
            let jEnd2 = b.emit(.jump(0), at: rhs)
            
            // right false -> false
            let toFalse = b.count - (jfRight + 1)
            b.patch(jfRight, to: .jumpIfFalse(toFalse))
            b.emit(.loadConst(.bool(false)), at: rhs)
                     
            // patch ends
            let toEnd1 = b.count - (jEnd1 + 1)
            b.patch(jEnd1, to: .jump(toEnd1))
            let toEnd2 = b.count - (jEnd2 + 1)
            b.patch(jEnd2, to: .jump(toEnd2))
        default:
            throw CompileError.unsupported("Logical operator '\(opSymbol)' not supported")
        }
    }
    
    
    private func compileIfExpr(
        _ ifs: IfExprSyntax,
        knownTypes: Set<String>,
        knownFunctions: Set<String>,
        builder b: BytecodeBuilder
    ) throws {
        guard !ifs.conditions.isEmpty else {
            throw CompileError.unsupported("if without condition")
        }

        struct PendingBinding { let name: String; let isConst: Bool; let node: Syntax }
        var failJumps: [(index: Int, isNil: Bool, bindingDepth: Int)] = []
        var bindingStackDepth = 0
        var bindings: [PendingBinding] = []

        for conditionElement in ifs.conditions {
            let condition = conditionElement.condition
            if let expr = condition.as(ExprSyntax.self) {
                try compileExpr(expr, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                let idx = b.emit(.jumpIfFalse(0), at: expr)
                failJumps.append((idx, false, bindingStackDepth))
                continue
            }
            if let ob = condition.as(OptionalBindingConditionSyntax.self) {
                guard let initExpr = ob.initializer?.value else {
                    throw CompileError.unsupported("if binding without initializer")
                }
                guard let idPat = ob.pattern.as(IdentifierPatternSyntax.self) else {
                    throw CompileError.unsupported("if binding only supports identifier pattern")
                }
                let isConst = ob.bindingSpecifier.text == "let"
                let name = idPat.identifier.text
                try compileExpr(initExpr, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
                b.emit(.dup, at: ob)
                let jn = b.emit(.jumpIfNil(0), at: ob)
                failJumps.append((jn, true, bindingStackDepth + 1))
                bindings.append(.init(name: name, isConst: isConst, node: Syntax(ob)))
                bindingStackDepth += 1
                continue
            }
            throw CompileError.unsupported("Unsupported if condition kind: \(condition.kind)")
        }

        let usingSlots = options.mode == .slot && slotResolutionEnabled
        let needsBindingScope = !bindings.isEmpty
        var scopeStartIdx: Int? = nil
        var slotEnterIdx: Int? = nil
        var pushedBindingStacks = false

        if needsBindingScope {
            if usingSlots, let slotRes = slot {
                let totalSlots = bindings.count + countBindings(ifs.body)
                slotRes.push(total: totalSlots)
                slotEnterIdx = b.emit(.enterScope(nLocals: totalSlots), at: ifs)
            } else {
                scopeStartIdx = b.emit(.pushScope, at: ifs)
            }
            nameBindingsStack.append([])
            typeBindingsStack.append([:])
            wrapperLocalStack.append([:])
            pushedBindingStacks = true
        }

        var slotNamesBySlot: [Int: String] = [:]
        if !bindings.isEmpty {
            for bind in bindings.reversed() {
                if usingSlots, let slotRes = slot, slotRes.hasActiveScope {
                    let slotIndex = try slotRes.declare(bind.name, isConst: bind.isConst)
                    slotNamesBySlot[slotIndex] = bind.name
                    b.emit(.defLocal(slot: slotIndex, isConst: bind.isConst), at: bind.node)
                } else {
                    if bind.isConst { b.emit(.defLet(bind.name), at: bind.node) }
                    else { b.emit(.defVar(bind.name), at: bind.node) }
                    if options.mode == .name, !nameBindingsStack.isEmpty {
                        var top = nameBindingsStack.removeLast()
                        top.insert(bind.name)
                        nameBindingsStack.append(top)
                    }
                }
            }
        }

        let treatAsTopScope = needsBindingScope
        try compileCodeBlock(
            ifs.body,
            knownTypes: knownTypes,
            knownFunctions: knownFunctions,
            builder: b,
            treatAsTopScope: treatAsTopScope
        )

        if needsBindingScope {
            if usingSlots, let slotRes = slot {
                if let scope = slotRes.scopes.last {
                    for (name, info) in scope.map {
                        slotNamesBySlot[info.slot] = name
                    }
                }
                let leaveIdx = b.emit(.leaveScope, at: ifs)
                if let enter = slotEnterIdx {
                    b.addLocalsMap(start: enter, end: leaveIdx, depth: 0, namesBySlot: slotNamesBySlot)
                    b.addScopeNote(start: enter, end: leaveIdx, kind: "if", name: nil)
                }
                slotRes.pop()
            } else if let start = scopeStartIdx {
                let popIdx = b.emit(.popScope, at: ifs)
                b.addScopeNote(start: start, end: popIdx, kind: "if", name: nil)
            }
            if pushedBindingStacks {
                _ = nameBindingsStack.popLast()
                _ = typeBindingsStack.popLast()
                _ = wrapperLocalStack.popLast()
            }
        }

        let needsJumpOverElse = ifs.elseBody != nil || !failJumps.isEmpty
        let jmpOverElseIdx = needsJumpOverElse ? b.emit(.jump(0), at: ifs) : nil
        var failureBlocks: [(start: Int, jumpToElse: Int)] = []
        for failure in failJumps {
            let start = b.count
            if failure.bindingDepth > 0 {
                for _ in 0..<failure.bindingDepth {
                    b.emit(.pop, at: ifs)
                }
            }
            let jumpIdx = b.emit(.jump(0), at: ifs)
            failureBlocks.append((start: start, jumpToElse: jumpIdx))
        }

        let elseStart = b.count
        for (failure, block) in zip(failJumps, failureBlocks) {
            let offset = block.start - (failure.index + 1)
            if failure.isNil { b.patch(failure.index, to: .jumpIfNil(offset)) }
            else { b.patch(failure.index, to: .jumpIfFalse(offset)) }
        }
        for block in failureBlocks {
            let offset = elseStart - (block.jumpToElse + 1)
            b.patch(block.jumpToElse, to: .jump(offset))
        }

        if let elseBody = ifs.elseBody {
            if let nestedIf = elseBody.as(IfExprSyntax.self) {
                try compileIfExpr(nestedIf, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            } else if let elseBlock = elseBody.as(CodeBlockSyntax.self) {
                try compileCodeBlock(elseBlock, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
            } else {
                throw CompileError.unsupported("Unsupported else body: \(elseBody.kind)")
            }
        }

        if let jumpIdx = jmpOverElseIdx {
            let toEnd = b.count - (jumpIdx + 1)
            b.patch(jumpIdx, to: .jump(toEnd))
        }
    }

    private func compileTernaryExpr(
        _ te: TernaryExprSyntax,
        knownTypes: Set<String>,
        knownFunctions: Set<String>,
        builder b: BytecodeBuilder
    ) throws {
        // cond ? then : else
        try compileExpr(te.condition, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
        let jif = b.emit(.jumpIfFalse(0), at: te.condition)
        try compileExpr(te.thenExpression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
        let jmpEnd = b.emit(.jump(0), at: te)
        // else branch
        let toElse = b.count - (jif + 1)
        b.patch(jif, to: .jumpIfFalse(toElse))
        try compileExpr(te.elseExpression, knownTypes: knownTypes, knownFunctions: knownFunctions, builder: b)
        let toEnd = b.count - (jmpEnd + 1)
        b.patch(jmpEnd, to: .jump(toEnd))
    }
}
