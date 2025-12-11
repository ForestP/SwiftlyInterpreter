//
//  VM.swift
//  InterpreterVM
//
//  Created by Forest Plasencia on 9/18/25.
//

import InterpreterModels
import Foundation

public struct HostClosureHandle {
    fileprivate let closure: Closure
    fileprivate let arity: Int
    fileprivate let programID: UInt64

    fileprivate init(closure: Closure, arity: Int, programID: UInt64) {
        self.closure = closure
        self.arity = arity
        self.programID = programID
    }

    public var parameterCount: Int { arity }

    /// Closure parameter types inferred at compile-time (e.g., ["Swift.String", "Swift.Int"])
    /// Empty array if types were not inferred
    public var parameterTypes: [String] { closure.paramTypes }

    /// Closure return type inferred at compile-time (e.g., "Swift.String")
    /// nil if type could not be inferred
    public var returnType: String? { closure.returnType }
}

public struct VM {
    public private(set) var output: String = ""
    private var stack: [Value] = []

    // Global install hooks populated by generated host bridge installers.

    // TODO
    nonisolated(unsafe)
    private static var hostBridgeInstallers: [(inout VM) -> Void] = []
    public static func registerHostBridgeInstaller(_ installer: @escaping (inout VM) -> Void) {
        hostBridgeInstallers.append(installer)
    }
    static func _resetHostBridgeInstallersForTesting() {
        hostBridgeInstallers.removeAll()
    }
    
    // Lexical environments
    struct Binding { var value: Value; let isConst: Bool; let declLoc: SourceLoc? }
    struct Frame {
        var table: [String: Binding]            // name-mode dictionary
        var locals: [Binding?]                  // slot-mode array (nil = uninitialized)
    }
    // envStack[0] is global
    private var envStack: [Frame] = [Frame(table: [:], locals: [])]
    
    // NEW: functions and calls
    private var functionTable: [String: (params: [String], entry: Int, localCount: Int)] = [:]
    private struct CallSite { let retIP: Int; let prevDepth: Int }
    private var callStack: [CallSite] = []
    
    // Types unchanged
    private var typeFields: [String: [String]] = [:]
    
    // Host bridging registry
    public typealias HostThunk = (inout VM, [Value]) throws -> Value
    struct HostTypeDescriptor {
        let name: String
        var methods: [SelectorID: HostThunk]
        var propGet: [PropertyID: HostThunk]   // getter: args = [receiver]
        var propSet: [PropertyID: HostThunk]   // setter: args = [receiver, newValue]; returns receiver
    }
    private var hostTypes: [TypeID: HostTypeDescriptor] = [:]
    // Pending, public-facing registrations keyed by type name and selector string
    private var pendingHostRegistrations: [String: [String: HostThunk]] = [:]
    private var pendingHostPropGets: [String: [String: HostThunk]] = [:]
    private var pendingHostPropSets: [String: [String: HostThunk]] = [:]
    private var hostTypeAliases: [String: String] = [:]
    private var hostTypeExportedNames: [String: String] = [:]
    private var hostAllowlist: HostAllowlist? = nil
    private var resolvedHostTypeIDs: [String: TypeID] = [:]
    private var nextNegativeTypeID: Int = -100
    private var hostDispatchByProgram: [UInt64: [Program.MethodKey: HostThunk]] = [:]
    private var hostPropGetDispatchByProgram: [UInt64: [Program.PropertyKey: HostThunk]] = [:]
    private var hostPropSetDispatchByProgram: [UInt64: [Program.PropertyKey: HostThunk]] = [:]
    // The Program the VM is currently prepared for and executing. Needed for nested calls from host thunks.
    private var currentProgram: Program? = nil
    private var curTypeNames: [String] = []
    private var curSelectorNames: [String] = []
    private var selectorIndexByName: [String: SelectorID] = [:]
    private var preparedProgramID: UInt64? = nil
    // Reserve negative TypeIDs for VM built-ins
    private let TID_Array      = TypeID(raw: -1)
    private let TID_Dictionary = TypeID(raw: -2)
    
    public init() {}

    public final class HostClosureAdapter {
        private let vmPointer: UnsafeMutablePointer<VM>
        private let handle: HostClosureHandle

        fileprivate init(vmPointer: UnsafeMutablePointer<VM>, handle: HostClosureHandle) {
            self.vmPointer = vmPointer
            self.handle = handle
        }

        public var parameterCount: Int { handle.parameterCount }

        public func invoke(_ args: [Value]) throws -> Value {
            try vmPointer.pointee.invoke(handle: handle, args: args)
        }

        public func hostTypeID(named name: String) -> TypeID {
            vmPointer.pointee.hostTypeID(named: name)
        }

        public func convertToAny(_ value: Value) throws -> Any {
            try vmPointer.pointee.convertToAny(value)
        }

        public func convertToAnyDictionary(_ value: Value) throws -> [AnyHashable: Any] {
            try vmPointer.pointee.convertToAnyDictionary(value)
        }

        public func withHost<T, R>(_ value: Value,
                                   typeName: String,
                                   as _: T.Type,
                                   _ body: (inout T) throws -> R) throws -> R {
            try vmPointer.pointee.withHost(value, typeName: typeName, as: T.self, body)
        }
    }

    // Public API: Host registration and allowlist
    public mutating func registerHostMethod(type typeName: String, selector: String, _ thunk: @escaping HostThunk) {
        var m = pendingHostRegistrations[typeName] ?? [:]
        m[selector] = thunk
        pendingHostRegistrations[typeName] = m
    }
    public mutating func registerHostMethods(type typeName: String, methods: [String: HostThunk]) {
        var m = pendingHostRegistrations[typeName] ?? [:]
        for (sel, thunk) in methods { m[sel] = thunk }
        pendingHostRegistrations[typeName] = m
    }
    // Host properties registration
    // Unified property registration API (J13): getters/setters use HostThunk
    public mutating func registerHostProperty(type typeName: String, name: String, get: HostThunk? = nil, set: HostThunk? = nil) {
        if let g = get {
            var m = pendingHostPropGets[typeName] ?? [:]
            m[name] = g
            pendingHostPropGets[typeName] = m
        }
        if let s = set {
            var m = pendingHostPropSets[typeName] ?? [:]
            m[name] = s
            pendingHostPropSets[typeName] = m
        }
    }
    public mutating func setHostAllowlist(_ allowlist: HostAllowlist?) { self.hostAllowlist = allowlist }

    // Public helper: obtain a TypeID for a host type name.
    // Uses program-interned ID if available; otherwise assigns a stable negative ID for this VM instance.
    public mutating func registerHostTypeAlias(canonical: String, exported: String) {
        guard !canonical.isEmpty, !exported.isEmpty else { return }
        if canonical != exported {
            hostTypeAliases[exported] = canonical
        }
        hostTypeExportedNames[canonical] = exported
    }

    public mutating func makeHostClosureHandle(from value: Value, expectedArity: Int) throws -> HostClosureHandle {
        guard case let .closure(closure) = value else {
            throw VMError.typeError("value is not a closure")
        }
        if closure.params.count != expectedArity {
            throw VMError.badInitializer("Closure expects \(closure.params.count) args, got \(expectedArity)")
        }
        guard let preparedID = preparedProgramID, let program = currentProgram else {
            throw VMError.programMismatch(expected: preparedProgramID ?? 0, got: currentProgram?.id ?? 0)
        }
        guard preparedID == program.id else {
            throw VMError.programMismatch(expected: preparedID, got: program.id)
        }
        return HostClosureHandle(closure: closure, arity: expectedArity, programID: program.id)
    }

    public mutating func invoke(handle: HostClosureHandle, args: [Value]) throws -> Value {
        if handle.parameterCount != args.count {
            throw VMError.badInitializer("Closure expects \(handle.parameterCount) args, got \(args.count)")
        }
        guard let preparedID = preparedProgramID else {
            throw VMError.programMismatch(expected: handle.programID, got: 0)
        }
        guard preparedID == handle.programID else {
            throw VMError.programMismatch(expected: handle.programID, got: preparedID)
        }
        guard let program = currentProgram, program.id == handle.programID else {
            throw VMError.programMismatch(expected: handle.programID, got: currentProgram?.id ?? 0)
        }
        return try callClosure(.closure(handle.closure), args: args)
    }

    public mutating func withHostClosureAdapter<R>(_ handle: HostClosureHandle,
                                                   _ body: (HostClosureAdapter) throws -> R) rethrows -> R {
        try withUnsafeMutablePointer(to: &self) { pointer in
            let adapter = HostClosureAdapter(vmPointer: pointer, handle: handle)
            return try body(adapter)
        }
    }

    public mutating func hostTypeID(named rawName: String) -> TypeID {
        let canonicalName = hostTypeAliases[rawName] ?? rawName
        if let tid = resolvedHostTypeIDs[canonicalName] { return tid }
        if canonicalName == "Array" { resolvedHostTypeIDs[canonicalName] = TID_Array; resolvedHostTypeIDs[rawName] = TID_Array; return TID_Array }
        if canonicalName == "Dictionary" { resolvedHostTypeIDs[canonicalName] = TID_Dictionary; resolvedHostTypeIDs[rawName] = TID_Dictionary; return TID_Dictionary }
        if let idx = curTypeNames.firstIndex(of: canonicalName) {
            let tid = TypeID(raw: idx)
            resolvedHostTypeIDs[canonicalName] = tid
            if canonicalName != rawName { resolvedHostTypeIDs[rawName] = tid }
            return tid
        }
        if let exported = hostTypeExportedNames[canonicalName],
           let idx = curTypeNames.firstIndex(of: exported) {
            let tid = TypeID(raw: idx)
            resolvedHostTypeIDs[canonicalName] = tid
            if exported != canonicalName { resolvedHostTypeIDs[exported] = tid }
            if canonicalName != rawName { resolvedHostTypeIDs[rawName] = tid }
            return tid
        }
        // Assign a new negative ID for this host type
        let tid = TypeID(raw: nextNegativeTypeID)
        nextNegativeTypeID -= 1
        resolvedHostTypeIDs[canonicalName] = tid
        if canonicalName != rawName { resolvedHostTypeIDs[rawName] = tid }
        if let exported = hostTypeExportedNames[canonicalName], exported != canonicalName {
            resolvedHostTypeIDs[exported] = tid
        }
        return tid
    }

    @inline(__always)
    public mutating func withHost<T, R>(_ value: Value, typeName expectedTypeName: String, as _: T.Type, _ body: (inout T) throws -> R) throws -> R {
        let (ref, actualTid) = try value.expectHost()
        let expectedTid = hostTypeID(named: expectedTypeName)
        guard actualTid == expectedTid else {
            let actualName = typeName(for: actualTid)
            throw VMError.typeError("value must be host \(expectedTypeName), got \(actualName)")
        }
        return try ref.withProjectedValue(as: T.self, body)
    }

    // MARK: - Any bridging helpers

    public func convertToAny(_ value: Value) throws -> Any {
        switch value {
        case .nilValue:
            throw VMError.typeError("expected Any, got nil")
        case .int(let n): return n
        case .uint(let u): return u
        case .double(let d): return d
        case .bool(let b): return b
        case .string(let s): return s
        case .host(let ref, _): return ref.asAny()
        default:
            throw VMError.typeError("cannot convert value to Any")
        }
    }

    public func convertToAnyDictionary(_ value: Value) throws -> [AnyHashable: Any] {
        switch value {
        case .nilValue:
            throw VMError.typeError("expected [AnyHashable: Any], got nil")
        case .host(let ref, _):
            let anyObj = ref.asAny()
            if let dict = anyObj as? [AnyHashable: Any] {
                return dict
            }
            if let ns = anyObj as? NSDictionary {
                // Bridge to Swift dictionary
                return (ns as? [AnyHashable: Any]) ?? [:]
            }
            throw VMError.typeError("host is not NSDictionary or [AnyHashable: Any]")
        case .dict(let box):
            var result: [AnyHashable: Any] = [:]
            for (k, v) in box.storage {
                result[AnyHashable(k)] = try convertToAny(v)
            }
            return result
        default:
            throw VMError.typeError("cannot convert value to [AnyHashable: Any]")
        }
    }
    
    // Prepare the VM for a specific program: load intern tables, built-ins, apply host registrations,
    // enforce allowlist filters and collision rules, and cache function metadata.
    public mutating func prepare(_ program: Program) throws {
        // Initialize interned tables
        curTypeNames = program.typeTable
        curSelectorNames = program.selectorTable
        selectorIndexByName = Dictionary(uniqueKeysWithValues: curSelectorNames.enumerated().map { ($0.element, SelectorID(raw: $0.offset)) })

        hostTypes.removeAll(keepingCapacity: false)
        resolvedHostTypeIDs.removeAll(keepingCapacity: false)
        nextNegativeTypeID = -100

        var dispatchEntries: [Program.MethodKey: HostThunk] = [:]
        var propGetEntries: [Program.PropertyKey: HostThunk] = [:]
        var propSetEntries: [Program.PropertyKey: HostThunk] = [:]

        // Allow generated installers to register host dispatchers before applying pending registrations.
        for installer in VM.hostBridgeInstallers {
            installer(&self)
        }

        // Early collision rule based on pending registrations regardless of selector usage in this program
        if !pendingHostRegistrations.isEmpty || !pendingHostPropGets.isEmpty || !pendingHostPropSets.isEmpty {
            let allTypeNames = Set(pendingHostRegistrations.keys)
                .union(pendingHostPropGets.keys)
                .union(pendingHostPropSets.keys)
            for typeName in allTypeNames {
                if let al = hostAllowlist, !al.isTypeAllowed(typeName) { continue }
                let getKeys: Set<String> = pendingHostPropGets[typeName].map { Set($0.keys) } ?? Set<String>()
                let setKeys: Set<String> = pendingHostPropSets[typeName].map { Set($0.keys) } ?? Set<String>()
                let props = getKeys.union(setKeys)
                if props.isEmpty { continue }
                let zeroArgMethods: Set<String> = pendingHostRegistrations[typeName].map { regs in
                    Set(regs.keys.compactMap { s in s.hasSuffix("()") ? String(s.dropLast(2)) : nil })
                } ?? Set<String>()
                let collisions = props.intersection(zeroArgMethods)
                if let name = collisions.first {
                    throw VMExecutionError(underlying: .unsupported("Property/method collision on type '\(typeName)': '\(name)' and '\(name)()'"), loc: nil)
                }
            }
        }

        // Register built-in collection "types" with fixed negative IDs
        var arrMethods: [SelectorID: HostThunk] = [:]
        var arrPropGet: [PropertyID: HostThunk] = [:]
        
        // Not used (we currently dont have any builtins with setters)
        let arrPropSet: [PropertyID: HostThunk] = [:]
        
        if let selAppend = selectorIndexByName["append(_:)"] {
            arrMethods[selAppend] = { _, args in
                guard args.count == 2 else { throw VMError.typeError("append expects 1 arg") }
                guard case let .array(box) = args[0] else { throw VMError.typeError("append expects Array + 1 arg") }
                box.elements.append(args[1])
                return args[0]
            }
        }
        if let selSwapAt = selectorIndexByName["swapAt(_:_:)"] {
            arrMethods[selSwapAt] = { _, args in
                guard args.count == 3 else { throw VMError.typeError("swapAt expects 2 args") }
                guard case let .array(box) = args[0] else { throw VMError.typeError("swapAt expects Array receiver") }
                guard case let .int(i) = args[1], case let .int(j) = args[2] else {
                    throw VMError.typeError("swapAt indices must be Int")
                }
                if i < 0 || i >= box.elements.count { throw VMError.indexOutOfRange(i, box.elements.count) }
                if j < 0 || j >= box.elements.count { throw VMError.indexOutOfRange(j, box.elements.count) }
                if i != j { box.elements.swapAt(i, j) }
                return args[0]
            }
        }
        if let selReversed = selectorIndexByName["reversed()"] {
            arrMethods[selReversed] = { _, args in
                guard args.count == 1 else { throw VMError.typeError("reversed expects no args") }
                guard case let .array(box) = args[0] else { throw VMError.typeError("reversed expects Array receiver") }
                return .array(ArrBox(Array(box.elements.reversed())))
            }
        }
        // Expose count as a property for Array/Dictionary if property is present in this program
        let propertyIndexByName: [String: PropertyID] = Dictionary(uniqueKeysWithValues: program.propertyTable.enumerated().map { ($0.element, PropertyID(raw: $0.offset)) })
        if let pid = propertyIndexByName["count"] {
            let thunk: HostThunk = { _, args in
                guard args.count == 1 else { throw VMError.typeError("count getter arity") }
                switch args[0] {
                case .array(let b): return .int(b.elements.count)
                case .dict(let d):  return .int(d.storage.count)
                default: throw VMError.typeError("count property only on Array/Dictionary")
                }
            }
            arrPropGet[pid] = thunk
            propGetEntries[Program.PropertyKey(type: TID_Array, property: pid)] = thunk
            propGetEntries[Program.PropertyKey(type: TID_Dictionary, property: pid)] = thunk
        }
        hostTypes[TID_Array]      = HostTypeDescriptor(name: "Array",      methods: arrMethods, propGet: arrPropGet, propSet: arrPropSet)
        hostTypes[TID_Dictionary] = HostTypeDescriptor(name: "Dictionary", methods: arrMethods, propGet: arrPropGet, propSet: arrPropSet)
        if let arrayMethods = hostTypes[TID_Array]?.methods {
            for (selectorID, thunk) in arrayMethods {
                dispatchEntries[Program.MethodKey(type: TID_Array, selector: selectorID)] = thunk
                dispatchEntries[Program.MethodKey(type: TID_Dictionary, selector: selectorID)] = thunk
            }
        }

        // Apply public registrations against this program's interning tables (type/selector)
        if !pendingHostRegistrations.isEmpty {
            for (typeName, methods) in pendingHostRegistrations {
                if let al = hostAllowlist, !al.isTypeAllowed(typeName) { continue }
                let typeID = hostTypeID(named: typeName)
                if hostTypes[typeID] == nil { hostTypes[typeID] = HostTypeDescriptor(name: typeName, methods: [:], propGet: [:], propSet: [:]) }
                for (selName, thunk) in methods {
                    if let al = hostAllowlist, !al.isSelectorAllowed(type: typeName, selector: selName) { continue }
                    guard let selID = selectorIndexByName[selName] else { continue }
                    hostTypes[typeID]!.methods[selID] = thunk
                    dispatchEntries[Program.MethodKey(type: typeID, selector: selID)] = thunk
                }
            }
        }
        // Apply pending property registrations
        if !pendingHostPropGets.isEmpty || !pendingHostPropSets.isEmpty {
            let allTypeNames = Set(pendingHostPropGets.keys).union(pendingHostPropSets.keys)
            for typeName in allTypeNames {
                if let al = hostAllowlist, !al.isTypeAllowed(typeName) { continue }
                let typeID = hostTypeID(named: typeName)
                if hostTypes[typeID] == nil { hostTypes[typeID] = HostTypeDescriptor(name: typeName, methods: [:], propGet: [:], propSet: [:]) }
                if let gets = pendingHostPropGets[typeName] {
                    for (propName, g) in gets {
                        if let al = hostAllowlist {
                            if !al.isTypeAllowed(typeName) { continue }
                            if !al.isPropertyAllowed(type: typeName, property: propName) { continue }
                        }
                        guard let pid = propertyIndexByName[propName] else { continue }
                        hostTypes[typeID]!.propGet[pid] = g
                        propGetEntries[Program.PropertyKey(type: typeID, property: pid)] = g
                    }
                }
                if let sets = pendingHostPropSets[typeName] {
                    for (propName, s) in sets {
                        if let al = hostAllowlist {
                            if !al.isTypeAllowed(typeName) { continue }
                            if !al.isPropertyAllowed(type: typeName, property: propName) { continue }
                        }
                        guard let pid = propertyIndexByName[propName] else { continue }
                        hostTypes[typeID]!.propSet[pid] = s
                        propSetEntries[Program.PropertyKey(type: typeID, property: pid)] = s
                }
            }
        }
        }
        // Enforce collision rule after resolution
        for (_, desc) in hostTypes {
            let propNamesGet: [String] = desc.propGet.keys.map { program.propertyTable[safe: $0.raw] ?? "#\($0.raw)" }
            let propNamesSet: [String] = desc.propSet.keys.map { program.propertyTable[safe: $0.raw] ?? "#\($0.raw)" }
            let propNames = Set(propNamesGet).union(propNamesSet)
            if propNames.isEmpty { continue }
            let zeroArgMethodNames: Set<String> = Set(desc.methods.keys.compactMap { selID in
                let s = curSelectorNames[safe: selID.raw] ?? ""
                return s.hasSuffix("()") ? String(s.dropLast(2)) : nil
            })
            let collisions = propNames.intersection(zeroArgMethodNames)
            if !collisions.isEmpty {
                let typeName = desc.name
                let name = collisions.first!
                throw VMExecutionError(underlying: .unsupported("Property/method collision on type '\(typeName)': '\(name)' and '\(name)()'"), loc: nil)
            }
        }

        // Cache function table from program ops
        functionTable.removeAll(keepingCapacity: false)
        for op in program.ops {
            if case let .defineFunction(name, params, entry, localCount) = op {
                functionTable[name] = (params, entry, localCount)
            }
        }
        hostDispatchByProgram[program.id] = dispatchEntries
        hostPropGetDispatchByProgram[program.id] = propGetEntries
        hostPropSetDispatchByProgram[program.id] = propSetEntries
        preparedProgramID = program.id
        currentProgram = program
    }
    
    public mutating func run(_ program: Program) throws {
        if preparedProgramID != program.id { try prepare(program) }
        // Track current program even if already prepared for identical ID
        currentProgram = program
        // fresh runtime state
        stack.removeAll(keepingCapacity: false)
        envStack = [Frame(table: [:], locals: [])]
        callStack.removeAll(keepingCapacity: false)
        var ip = 0
        var lastIP = 0
        do {
            while ip < program.ops.count {
                lastIP = ip
                let op = program.ops[ip]
                switch op {
                    // ------- values / vars / arithmetic / print (unchanged except env) -------
                case .loadConst(let v): stack.append(v)
                case .loadVar(let name):
                    guard let v = loadVarValue(name) else { throw VMError.varNotFoundWithSuggestions(name: name, suggestions: suggestions(for: name)) }
                    stack.append(v)

                case .pushScope:
                    envStack.append(Frame(table: [:], locals: []))
                case .popScope:
                    if envStack.count <= 1 { throw VMError.unsupported("popScope at global scope") }
                    _ = envStack.popLast()
                case .defVar(let name):
                    guard let top = stack.popLast() else { throw VMError.stackUnderflow }
                    try def(name, value: top, isConst: false, declLoc: program.locs[safe: ip] ?? nil)
                case .defLet(let name):
                    guard let top = stack.popLast() else { throw VMError.stackUnderflow }
                    try def(name, value: top, isConst: true, declLoc: program.locs[safe: ip] ?? nil)
                case .setVar(let name):
                    guard let top = stack.popLast() else { throw VMError.stackUnderflow }
                    let ok = try setVar(name, to: top)
                    if !ok { throw VMError.varNotFoundWithSuggestions(name: name, suggestions: suggestions(for: name)) }

                // ------- Slot-mode (scaffold) -------
                case .enterScope(let n):
                    envStack.append(Frame(table: [:], locals: Array(repeating: nil, count: n)))
                case .leaveScope:
                    if envStack.count <= 1 { throw VMError.unsupported("leaveScope at global scope") }
                    _ = envStack.popLast()
                case .defLocal(let slot, let isConst):
                    guard var cur = envStack.popLast() else { throw VMError.stackUnderflow }
                    guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                    if slot < 0 || slot >= cur.locals.count { throw VMError.unsupported("defLocal slot out of range") }
                    if let existing = cur.locals[slot], existing.declLoc != nil {
                        throw VMError.redeclaration(name: "slot_\(slot)", originalAt: existing.declLoc)
                    }
                    // Parameters are seeded with declLoc == nil so they can be shadowed by the first body binding.
                    cur.locals[slot] = Binding(value: v, isConst: isConst, declLoc: program.locs[safe: ip] ?? nil)
                    envStack.append(cur)
                case .loadLocal(let depth, let slot):
                    let idx = envStack.count - 1 - depth
                    if idx < 0 || idx >= envStack.count { throw VMError.unsupported("loadLocal depth out of range") }
                    let frame = envStack[idx]
                    if slot < 0 || slot >= frame.locals.count { throw VMError.unsupported("loadLocal slot out of range") }
                    guard let b = frame.locals[slot] else { throw VMError.unsupported("loadLocal from uninitialized local") }
                    stack.append(b.value)
                case .setLocal(let depth, let slot):
                    guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                    let idx = envStack.count - 1 - depth
                    if idx < 0 || idx >= envStack.count { throw VMError.unsupported("setLocal depth out of range") }
                    var frame = envStack[idx]
                    if slot < 0 || slot >= frame.locals.count { throw VMError.unsupported("setLocal slot out of range") }
                    guard let b = frame.locals[slot] else { throw VMError.unsupported("setLocal to uninitialized local") }
                    if b.isConst { throw VMError.constAssignment(name: "slot_\(slot)", declaredAt: b.declLoc) }
                    frame.locals[slot] = Binding(value: v, isConst: b.isConst, declLoc: b.declLoc)
                    envStack[idx] = frame
                    
                    // MARK: Logical
                case .add: try numericAdd()
                case .sub: try numericSub()
                case .mul: try numericMul()
                case .div: try numericDiv()
                case .mod: try numericMod()
                    
                case .lt:  try numericCmp(<,  <,  <)
                case .le:  try numericCmp(<=, <=, <=)
                case .gt:  try numericCmp(>,  >,  >)
                case .ge:  try numericCmp(>=, >=, >=)
                    
                case .eq:
                    guard let b = stack.popLast(), let a = stack.popLast() else { throw VMError.stackUnderflow }
                    let result: Bool
                    switch (a, b) {
                    case (.nilValue, .nilValue):
                        result = true
                    case (.nilValue, _), (_, .nilValue):
                        result = false
                    case let (.int(x),    .int(y)):    result = (x == y)
                    case let (.uint(x),   .uint(y)):   result = (x == y)
                    case let (.double(x), .double(y)): result = (x == y)
                    case let (.int(x),    .double(y)): result = Double(x) == y
                    case let (.double(x), .int(y)):    result = x == Double(y)
                    case let (.int(x),    .uint(y)):
                        result = x >= 0 && UInt64(x) == y
                    case let (.uint(x),   .int(y)):
                        result = y >= 0 && x == UInt64(y)
                    case let (.double(x), .uint(y)):   result = x == Double(y)
                    case let (.uint(x),   .double(y)): result = Double(x) == y
                    case let (.bool(x),   .bool(y)):   result = (x == y)
                    case let (.string(x), .string(y)): result = (x == y)
                    case let (.object(x), .object(y)): result = (x == y)
                    default: throw VMError.typeError("== requires matching or numeric-compatible types")
                    }
                    stack.append(.bool(result))
                    
                case .logicalNot:
                    guard case let .bool(v)? = stack.popLast() else {
                        throw VMError.typeError("! expects Bool")
                    }
                    stack.append(.bool(!v))
                case .neg:
                    guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                    switch v {
                    case .int(let n):    stack.append(.int(-n))
                    case .double(let d): stack.append(.double(-d))
                    default:
                        throw VMError.typeError("unary - expects numeric")
                    }
                    
                case .printTop:
                    guard let top = stack.popLast() else { throw VMError.stackUnderflow }
                    writeLine(top.description)
                case .dup:
                    guard let v = stack.last else { throw VMError.stackUnderflow }
                    stack.append(v)
                case .pop:
                    guard let _ = stack.popLast() else { throw VMError.stackUnderflow }
                    
                    // ------- types / objects (unchanged) -------
                case .defineType(let name, let fields):
                    typeFields[name] = fields
                    
                case .newObject(let typeName, let argLabels):
                    guard let fields = typeFields[typeName] else { throw VMError.unknownType(typeName) }
                    // Require all fields for now (simple synthesized init)
                    guard Set(argLabels) == Set(fields), argLabels.count == fields.count else {
                        throw VMError.badInitializer("Expected labeled args \(fields) for \(typeName), got \(argLabels)")
                    }
                    // Pop in reverse of arg order
                    var storage: [String: Value] = [:]
                    for label in argLabels.reversed() {
                        guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                        storage[label] = v
                    }
                    stack.append(.object(.init(typeName: typeName, storage: storage)))
                    
                case .getProp(let prop):
                    let propName = program.propertyTable[safe: prop.raw] ?? "#\(prop.raw)"
                    guard let recv = stack.popLast() else { throw VMError.stackUnderflow }
                    switch recv {
                    case .object(let obj):
                        guard let v = obj.storage[propName] else { throw VMError.missingProperty(obj.typeName, propName) }
                        stack.append(v)
                    case .range(let range):
                        switch propName {
                        case "lowerBound":
                            stack.append(range.lower)
                        case "upperBound":
                            stack.append(range.upper)
                        case "isEmpty":
                            stack.append(.bool(try rangeIsEmpty(range)))
                        default:
                            throw VMError.missingProperty(rangeTypeName(range), propName)
                        }
                    case .host, .array, .dict, .metatype:
                        let value = try loadHostProperty(prop, named: propName, from: recv, in: program)
                        stack.append(value)
                    default:
                        throw VMError.typeError("getProp requires object/host/collection/metatype on stack")
                    }
                    
                case .setProp(let prop):
                    let propName = program.propertyTable[safe: prop.raw] ?? "#\(prop.raw)"
                    // Expect: ... receiver value
                    guard let val = stack.popLast() else { throw VMError.stackUnderflow }
                    guard let recv = stack.popLast() else { throw VMError.stackUnderflow }
                    switch recv {
                    case .object(let obj):
                        obj.storage[propName] = val
                    case .range:
                        throw VMError.unsupported("Range properties are read-only")
                    case .host, .array, .dict, .metatype:
                        try storeHostProperty(prop, named: propName, on: recv, value: val, in: program)
                    default:
                        throw VMError.typeError("setProp requires object/host/collection/metatype receiver")
                    }
                    // mutating, no push
                    
                    // ------- control flow (unchanged) -------
                case .jump(let offset):
                    // jump relative to this instruction: go to ip + 1 + offset
                    ip += 1 + offset
                    continue
                    
                case .jumpIfFalse(let offset):
                    guard case let .bool(flag)? = stack.popLast() else {
                        throw VMError.typeError("jumpIfFalse expects Bool on stack")
                    }
                    if flag == false {
                        ip += 1 + offset
                        continue
                    }
                case .jumpIfNil(let offset):
                    guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                    if case .nilValue = v {
                        ip += 1 + offset
                        continue
                    }
                    
                    // ------- NEW: functions -------
                case .defineFunction(let name, let params, let entry, let localCount):
                    functionTable[name] = (params, entry, localCount)
                    
                case .callFunc(let name, let argc):
                    guard let meta = functionTable[name] else {
                        throw VMError.unsupported("Unknown function: \(name)")
                    }
                    if argc != meta.params.count {
                        throw VMError.badInitializer("Function \(name) expects \(meta.params.count) args, got \(argc)")
                    }
                    let prevDepth = envStack.count
                    if meta.localCount > 0 {
                        // Slot-mode function frame: pre-allocate locals and place params in slots [0..<paramCount]
                        var frame = Frame(table: [:], locals: Array(repeating: nil, count: meta.localCount))
                        for slot in stride(from: meta.params.count - 1, through: 0, by: -1) {
                            guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                            frame.locals[slot] = Binding(value: v, isConst: false, declLoc: nil)
                        }
                        envStack.append(frame)
                    } else {
                        // Name-mode function frame: bind params by name
                        var frame = Frame(table: [:], locals: [])
                        for param in meta.params.reversed() {
                            guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                            frame.table[param] = Binding(value: v, isConst: false, declLoc: nil)
                        }
                        envStack.append(frame)
                        // Create a fresh body scope so locals can shadow params like Swift allows.
                        envStack.append(Frame(table: [:], locals: []))
                    }
                    callStack.append(.init(retIP: ip + 1, prevDepth: prevDepth))
                    ip = meta.entry
                    continue
                    
                case .ret:
                    // Convention: function should have left its return value on stack.
                    // If not, return true by default.
                    let retVal: Value = stack.popLast() ?? .bool(true)
                    assert(envStack.last?.locals.count ?? 0 <= 4096, "suspiciously large local frame")
                    guard let callSite = callStack.popLast() else { throw VMError.unsupported("ret outside of function") }
                    // Unwind scopes back to the depth before the call
                    while envStack.count > callSite.prevDepth { _ = envStack.popLast() }
                    let retIP = callSite.retIP
                    stack.append(retVal)
                    ip = retIP
                    continue

                case .makeClosure(let params, let paramTypes, let returnType, let entry, let captureNames):
                    var caps: [String: Value] = [:]
                    for name in captureNames {
                        guard let v = loadVarValue(name) else { throw VMError.varNotFoundWithSuggestions(name: name, suggestions: suggestions(for: name)) }
                        caps[name] = v
                    }
                    stack.append(.closure(.init(params: params, paramTypes: paramTypes, returnType: returnType, entry: entry, captures: caps)))

                case .makeClosureVals(let params, let paramTypes, let returnType, let entry, let captureNames):
                    var caps: [String: Value] = [:]
                    // Values are expected on stack in the same order as captureNames
                    for i in stride(from: captureNames.count - 1, through: 0, by: -1) {
                        guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                        caps[captureNames[i]] = v
                    }
                    stack.append(.closure(.init(params: params, paramTypes: paramTypes, returnType: returnType, entry: entry, captures: caps)))

                case .callValue(let argc):
                    // Stack: ... callee args...
                    // Pop args first into array (reverse), then callee
                    var args: [Value] = []
                    for _ in 0..<argc {
                        guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                        args.append(v)
                    }
                    guard let callee = stack.popLast() else { throw VMError.stackUnderflow }
                    switch callee {
                    case .closure(let clo):
                        if argc != clo.params.count {
                            throw VMError.badInitializer("Closure expects \(clo.params.count) args, got \(argc)")
                        }
                        // Build call frame with captures, then assign params (params win)
                        var frame = Frame(table: [:], locals: [])
                        for (k, v) in clo.captures { frame.table[k] = Binding(value: v, isConst: false, declLoc: nil) }
                        for (name, value) in zip(clo.params, args.reversed()) {
                            frame.table[name] = Binding(value: value, isConst: false, declLoc: nil)
                        }
                        let prevDepth = envStack.count
                        envStack.append(frame)
                        envStack.append(Frame(table: [:], locals: []))
                        callStack.append(.init(retIP: ip + 1, prevDepth: prevDepth))
                        ip = clo.entry
                        continue
                    default:
                        throw VMError.typeError("call of non-callable value: \(String(describing: callee))")
                    }
                
                // ------- collections -------
                case .makeArray(let count):
                    var arr: [Value] = []
                    arr.reserveCapacity(count)
                    for _ in 0..<count {
                        guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                        arr.append(v)
                    }
                    stack.append(.array(ArrBox(arr.reversed())))
                case .makeDict(let count):
                    var dict: [String: Value] = [:]
                    for _ in 0..<count {
                        guard let val = stack.popLast() else { throw VMError.stackUnderflow }
                        guard let keyVal = stack.popLast() else { throw VMError.stackUnderflow }
                        guard case let .string(key) = keyVal else {
                            throw VMError.typeError("dictionary keys must be String")
                        }
                        dict[key] = val
                    }
                    stack.append(.dict(DictBox(dict)))
                case .makeRange(let isClosed):
                    guard let upper = stack.popLast() else { throw VMError.stackUnderflow }
                    guard let lower = stack.popLast() else { throw VMError.stackUnderflow }
                    let kind: RangeBox.Kind = isClosed ? .closed : .halfOpen
                    try validateRangeBounds(lower: lower, upper: upper, kind: kind)
                    stack.append(.range(RangeBox(lower: lower, upper: upper, kind: kind)))
                case .getIndex:
                    guard let idx = stack.popLast() else { throw VMError.stackUnderflow }
                    guard let container = stack.popLast() else { throw VMError.stackUnderflow }
                    switch (container, idx) {
                    case let (.array(box), .int(i)):
                        if i < 0 || i >= box.elements.count { throw VMError.indexOutOfRange(i, box.elements.count) }
                        stack.append(box.elements[i])
                    case let (.dict(box), .string(k)):
                        guard let v = box.storage[k] else { throw VMError.keyNotFound(k) }
                        stack.append(v)
                    default:
                        throw VMError.typeError("invalid indexing operation")
                    }
                case .setIndex:
                    guard let value = stack.popLast() else { throw VMError.stackUnderflow }
                    guard let idx = stack.popLast() else { throw VMError.stackUnderflow }
                    guard let container = stack.popLast() else { throw VMError.stackUnderflow }
                    switch (container, idx) {
                    case let (.array(box), .int(i)):
                        if i < 0 || i >= box.elements.count { throw VMError.indexOutOfRange(i, box.elements.count) }
                        box.elements[i] = value
                    case let (.dict(box), .string(k)):
                        box.storage[k] = value
                    default:
                        throw VMError.typeError("invalid indexing assignment")
                    }
                case .callMethod(let sel, let argc):
                    // Pop argc values into temp, then reverse to get receiver-first order
                    var tmp: [Value] = []
                    for _ in 0..<argc { guard let v = stack.popLast() else { throw VMError.stackUnderflow }; tmp.append(v) }
                    let args = Array(tmp.reversed())
                    guard let recv = args.first else { throw VMError.stackUnderflow }
                    let hostDispatchTable = hostDispatchByProgram[program.id] ?? [:]
                    let selectorName = curSelectorNames[safe: sel.raw] ?? "#\(sel.raw)"
                    func dispatch(to tid: TypeID) throws {
                        let tname = typeName(for: tid)
                        if !isMethodAllowed(typeName: tname, selectorID: sel) {
                            throw VMError.missingMethod(tname, selectorName)
                        }
                        guard let thunk = hostDispatchTable[Program.MethodKey(type: tid, selector: sel)] else {
                            throw VMError.missingMethod(tname, selectorName)
                        }
                        let rv = try thunk(&self, args)
                        stack.append(rv)
                    }
                    switch recv {
                    case .metatype(let tid):
                        try dispatch(to: tid)
                    case .host(_, let tid):
                        try dispatch(to: tid)
                    case .array:
                        // Try host specializations first (Array<Int>, Array<String>, etc.)
                        if let hostResult = try dispatchArrayHostSpecialization(selector: sel, args: args, hostDispatchTable: hostDispatchTable) {
                            stack.append(hostResult)
                        } else {
                            // Fall back to VM-native array dispatch
                            try dispatch(to: TID_Array)
                        }
                    case .dict:
                        // Try host specializations first (Dictionary<String, Int>, etc.)
                        if let hostResult = try dispatchDictionaryHostSpecialization(selector: sel, args: args, hostDispatchTable: hostDispatchTable) {
                            stack.append(hostResult)
                        } else {
                            // Fall back to VM-native dictionary dispatch
                            try dispatch(to: TID_Dictionary)
                        }
                    case .range(let range):
                        switch selectorName {
                        case "contains(_:)":
                            if args.count != 2 { throw VMError.typeError("Range.contains expects 1 argument") }
                            let needle = args[1]
                            let contains = try rangeContains(range, value: needle)
                            stack.append(.bool(contains))
                        default:
                            throw VMError.missingMethod(rangeTypeName(range), selectorName)
                        }
                    case .object(let obj):
                        // User-defined methods via Program.userMethods
                        guard let typeIndex = curTypeNames.firstIndex(of: obj.typeName) else {
                            throw VMError.missingMethod(obj.typeName, selectorName)
                        }
                        let tid = TypeID(raw: typeIndex)
                        let key = Program.MethodKey(type: tid, selector: sel)
                        if let meta = program.userMethods[key] {
                            if args.count != meta.params.count {
                                throw VMError.badInitializer("Method expects \(meta.params.count - 1) args, got \(args.count - 1)")
                            }
                            let prevDepth = envStack.count
                            if meta.localCount > 0 {
                                var frame = Frame(table: [:], locals: Array(repeating: nil, count: meta.localCount))
                                for slot in stride(from: meta.params.count - 1, through: 0, by: -1) {
                                    frame.locals[slot] = Binding(value: args[slot], isConst: false, declLoc: nil)
                                }
                                envStack.append(frame)
                            } else {
                                var frame = Frame(table: [:], locals: [])
                                for (name, value) in zip(meta.params, args) { frame.table[name] = Binding(value: value, isConst: false, declLoc: nil) }
                                envStack.append(frame)
                                envStack.append(Frame(table: [:], locals: []))
                            }
                            callStack.append(.init(retIP: ip + 1, prevDepth: prevDepth))
                            ip = meta.entry
                            continue
                        } else {
                            throw VMError.missingMethod(obj.typeName, selectorName)
                        }
                    default:
                        // Try optional host specializations for other value types
                        if let hostResult = try dispatchOptionalHostSpecialization(selector: sel, args: args, hostDispatchTable: hostDispatchTable) {
                            stack.append(hostResult)
                        } else {
                            throw VMError.typeError("Receiver has no methods")
                        }
                    }
                case .trap(let message):
                    throw VMError.unsupported(message)
                }
                ip += 1
            }
        } catch let e as VMError {
            let loc: SourceLoc? = program.locs[safe: lastIP] ?? nil
            throw VMExecutionError(underlying: e, loc: loc)
        }
    }
    
    // MARK: - Numerics

    // MARK: - Allowlist helpers
    private func typeName(for tid: TypeID) -> String {
        if let desc = hostTypes[tid] { return desc.name }
        if tid.raw >= 0, let n = curTypeNames[safe: tid.raw] { return n }
        return "host"
    }

    public func debugTypeName(_ tid: TypeID) -> String {
        typeName(for: tid)
    }
    private func isMethodAllowed(typeName: String, selectorID: SelectorID) -> Bool {
        guard let al = hostAllowlist else { return true }
        if !al.isTypeAllowed(typeName) { return false }
        let selName = curSelectorNames[safe: selectorID.raw] ?? "#\(selectorID.raw)"
        return al.isSelectorAllowed(type: typeName, selector: selName)
    }
    private func isPropertyAllowed(typeName: String, property: String) -> Bool {
        guard let al = hostAllowlist else { return true }
        if !al.isTypeAllowed(typeName) { return false }
        return al.isPropertyAllowed(type: typeName, property: property)
    }

    private mutating func loadHostProperty(_ property: PropertyID,
                                           named propName: String,
                                           from receiver: Value,
                                           in program: Program) throws -> Value {
        let table = hostPropGetDispatchByProgram[program.id] ?? [:]
        func dispatch(to tid: TypeID) throws -> Value {
            let typeString = typeName(for: tid)
            if !isPropertyAllowed(typeName: typeString, property: propName) {
                throw VMError.missingProperty(typeString, propName)
            }
            if let builtin = builtinMetatypeProperty(typeName: typeString, property: propName) {
                return builtin
            }
            guard let thunk = table[Program.PropertyKey(type: tid, property: property)] else {
                throw VMError.missingProperty(typeString, propName)
            }
            return try thunk(&self, [receiver])
        }
        switch receiver {
        case .host(_, let tid):
            return try dispatch(to: tid)
        case .array:
            return try dispatch(to: TID_Array)
        case .dict:
            return try dispatch(to: TID_Dictionary)
        case .metatype(let tid):
            return try dispatch(to: tid)
        default:
            throw VMError.typeError("getProp requires object/host/collection/metatype on stack")
        }
    }

    private func builtinMetatypeProperty(typeName: String, property: String) -> Value? {
        switch typeName {
        case "Int":
            switch property {
            case "max": return .int(Int.max)
            case "min": return .int(Int.min)
            case "bitWidth": return .int(Int.bitWidth)
            case "zero": return .int(0)
            case "isSigned": return .bool(Int.isSigned)
            default: return nil
            }
        default:
            return nil
        }
    }

    private mutating func storeHostProperty(_ property: PropertyID,
                                            named propName: String,
                                            on receiver: Value,
                                            value: Value,
                                            in program: Program) throws {
        let table = hostPropSetDispatchByProgram[program.id] ?? [:]
        func dispatch(to tid: TypeID) throws {
            let typeString = typeName(for: tid)
            if !isPropertyAllowed(typeName: typeString, property: propName) {
                throw VMError.missingProperty(typeString, propName)
            }
            guard let thunk = table[Program.PropertyKey(type: tid, property: property)] else {
                throw VMError.missingProperty(typeString, propName)
            }
            _ = try thunk(&self, [receiver, value])
        }
        switch receiver {
        case .host(_, let tid):
            try dispatch(to: tid)
        case .array:
            try dispatch(to: TID_Array)
        case .dict:
            try dispatch(to: TID_Dictionary)
        case .metatype(let tid):
            try dispatch(to: tid)
        default:
            throw VMError.typeError("setProp requires object/host/collection/metatype receiver")
        }
    }

    // MARK: - Host specialization dispatchers

    /// Iterate through host array specializations (e.g., Array<Int>, Array<String>) and try each
    /// generated thunk with raw Value arguments. Return the result from the first successful thunk,
    /// or nil if all conversions fail. Does not throw VMExecutionError on conversion failures.
    private mutating func dispatchArrayHostSpecialization(
        selector: SelectorID,
        args: [Value],
        hostDispatchTable: [Program.MethodKey: HostThunk]
    ) throws -> Value? {
        // Collect all host type descriptors whose names start with "Swift.Array<"
        let candidates = hostTypes
            .filter { _, desc in desc.name.hasPrefix("Swift.Array<") && !desc.name.contains("<Element>") }
            .sorted { $0.key.raw < $1.key.raw }

        for (typeID, _) in candidates {
            let key = Program.MethodKey(type: typeID, selector: selector)
            guard let thunk = hostDispatchTable[key] else { continue }

            do {
                // Invoke the thunk with raw Value arguments. Generated code will attempt conversions
                // and throw VMError.typeError if this specialization cannot handle the given values.
                let result = try thunk(&self, args)
                return result
            } catch is VMError {
                // Conversion failed for this specialization; try the next one.
                continue
            }
        }

        // All specializations failed; caller should fall back to VM-native array dispatch.
        return nil
    }

    /// Iterate through host optional specializations (e.g., Optional<Int>, Optional<String>) and try each
    /// generated thunk with raw Value arguments. Return the result from the first successful thunk,
    /// or nil if all conversions fail. Does not throw VMExecutionError on conversion failures.
    private mutating func dispatchOptionalHostSpecialization(
        selector: SelectorID,
        args: [Value],
        hostDispatchTable: [Program.MethodKey: HostThunk]
    ) throws -> Value? {
        // Collect all host type descriptors whose names start with "Swift.Optional<"
        let candidates = hostTypes
            .filter { _, desc in desc.name.hasPrefix("Swift.Optional<") && !desc.name.contains("<Wrapped>") }
            .sorted { $0.key.raw < $1.key.raw }

        for (typeID, _) in candidates {
            let key = Program.MethodKey(type: typeID, selector: selector)
            guard let thunk = hostDispatchTable[key] else { continue }

            do {
                let result = try thunk(&self, args)
                return result
            } catch is VMError {
                continue
            }
        }

        return nil
    }

    /// Iterate through host dictionary specializations (e.g., Dictionary<String, Int>) and try each
    /// generated thunk with raw Value arguments. Return the result from the first successful thunk,
    /// or nil if all conversions fail. Does not throw VMExecutionError on conversion failures.
    private mutating func dispatchDictionaryHostSpecialization(
        selector: SelectorID,
        args: [Value],
        hostDispatchTable: [Program.MethodKey: HostThunk]
    ) throws -> Value? {
        // Collect all host type descriptors whose names start with "Swift.Dictionary<"
        let candidates = hostTypes
            .filter { _, desc in desc.name.hasPrefix("Swift.Dictionary<") && !desc.name.contains("<Key") && !desc.name.contains("<Value") }
            .sorted { $0.key.raw < $1.key.raw }

        for (typeID, _) in candidates {
            let key = Program.MethodKey(type: typeID, selector: selector)
            guard let thunk = hostDispatchTable[key] else { continue }

            do {
                let result = try thunk(&self, args)
                return result
            } catch is VMError {
                continue
            }
        }

        return nil
    }

    // Public API: invoke a function by name with arguments. Requires prior prepare() for the same Program.
    public mutating func invoke(name: String, args: [Value], in program: Program) throws -> Value {
        guard preparedProgramID == program.id else { throw VMError.programMismatch(expected: preparedProgramID ?? 0, got: program.id) }
        guard let meta = functionTable[name] else { throw VMError.unsupported("Unknown function: \(name)") }
        if args.count != meta.params.count { throw VMError.badInitializer("Function \(name) expects \(meta.params.count) args, got \(args.count)") }
        // Reset runtime state
        stack.removeAll(keepingCapacity: false)
        envStack = [Frame(table: [:], locals: [])]
        callStack.removeAll(keepingCapacity: false)
        // Build the call frame like callFunc
        if meta.localCount > 0 {
            var frame = Frame(table: [:], locals: Array(repeating: nil, count: meta.localCount))
            for slot in stride(from: meta.params.count - 1, through: 0, by: -1) {
                frame.locals[slot] = Binding(value: args[slot], isConst: false, declLoc: nil)
            }
            envStack.append(frame)
        } else {
            var frame = Frame(table: [:], locals: [])
            for (param, value) in zip(meta.params, args) {
                frame.table[param] = Binding(value: value, isConst: false, declLoc: nil)
            }
            envStack.append(frame)
            envStack.append(Frame(table: [:], locals: []))
        }
        // Sentinel callsite: after ret, ip jumps to program.ops.count (break condition)
        callStack.append(.init(retIP: program.ops.count, prevDepth: 1))
        try runFrom(program: program, startIP: meta.entry, stopIP: program.ops.count)
        return stack.popLast() ?? .bool(true)
    }

    // Public API: call a closure value with arguments from host thunks.
    // Requires that the VM was prepared for and is currently running a Program.
    public mutating func callClosure(_ value: Value, args: [Value]) throws -> Value {
        guard case let .closure(clo) = value else {
            throw VMError.typeError("call of non-closure value")
        }
        guard let program = currentProgram, preparedProgramID == program.id else {
            throw VMError.programMismatch(expected: preparedProgramID ?? 0, got: 0)
        }
        if args.count != clo.params.count {
            throw VMError.badInitializer("Closure expects \(clo.params.count) args, got \(args.count)")
        }
        // Build a frame with captures and params (name -> value)
        var frame = Frame(table: [:], locals: [])
        for (k, v) in clo.captures { frame.table[k] = Binding(value: v, isConst: false, declLoc: nil) }
        for (name, value) in zip(clo.params, args) { frame.table[name] = Binding(value: value, isConst: false, declLoc: nil) }
        let prevDepth = envStack.count
        envStack.append(frame)
        envStack.append(Frame(table: [:], locals: []))
        // Sentinel to bring us back after ret
        callStack.append(.init(retIP: program.ops.count, prevDepth: prevDepth))
        try runFrom(program: program, startIP: clo.entry, stopIP: program.ops.count)
        return stack.popLast() ?? .bool(true)
    }

    // Internal runner that executes from a given ip and stops when ip reaches stopIP (if provided)
    private mutating func runFrom(program: Program, startIP: Int, stopIP: Int?) throws {
        var ip = startIP
        var lastIP = startIP
        do {
            while ip < program.ops.count {
                if let stop = stopIP, ip == stop { break }
                lastIP = ip
                let op = program.ops[ip]
                switch op {
                    // ------- values / vars / arithmetic / print (unchanged except env) -------
                case .loadConst(let v): stack.append(v)
                case .loadVar(let name):
                    guard let v = loadVarValue(name) else { throw VMError.varNotFoundWithSuggestions(name: name, suggestions: suggestions(for: name)) }
                    stack.append(v)

                case .pushScope:
                    envStack.append(Frame(table: [:], locals: []))
                case .popScope:
                    if envStack.count <= 1 { throw VMError.unsupported("popScope at global scope") }
                    _ = envStack.popLast()
                case .defVar(let name):
                    guard let top = stack.popLast() else { throw VMError.stackUnderflow }
                    try def(name, value: top, isConst: false, declLoc: program.locs[safe: ip] ?? nil)
                case .defLet(let name):
                    guard let top = stack.popLast() else { throw VMError.stackUnderflow }
                    try def(name, value: top, isConst: true, declLoc: program.locs[safe: ip] ?? nil)
                case .setVar(let name):
                    guard let top = stack.popLast() else { throw VMError.stackUnderflow }
                    let ok = try setVar(name, to: top)
                    if !ok { throw VMError.varNotFoundWithSuggestions(name: name, suggestions: suggestions(for: name)) }

                // ------- Slot-mode (scaffold) -------
                case .enterScope(let n):
                    envStack.append(Frame(table: [:], locals: Array(repeating: nil, count: n)))
                case .leaveScope:
                    if envStack.count <= 1 { throw VMError.unsupported("leaveScope at global scope") }
                    _ = envStack.popLast()
                case .defLocal(let slot, let isConst):
                    guard var cur = envStack.popLast() else { throw VMError.stackUnderflow }
                    guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                    if slot < 0 || slot >= cur.locals.count { throw VMError.unsupported("defLocal slot out of range") }
                    if let existing = cur.locals[slot], existing.declLoc != nil {
                        throw VMError.redeclaration(name: "slot_\(slot)", originalAt: existing.declLoc)
                    }
                    // Parameters are seeded with declLoc == nil so they can be shadowed by the first body binding.
                    cur.locals[slot] = Binding(value: v, isConst: isConst, declLoc: program.locs[safe: ip] ?? nil)
                    envStack.append(cur)
                case .loadLocal(let depth, let slot):
                    let idx = envStack.count - 1 - depth
                    if idx < 0 || idx >= envStack.count { throw VMError.unsupported("loadLocal depth out of range") }
                    let frame = envStack[idx]
                    if slot < 0 || slot >= frame.locals.count { throw VMError.unsupported("loadLocal slot out of range") }
                    guard let b = frame.locals[slot] else { throw VMError.unsupported("loadLocal from uninitialized local") }
                    stack.append(b.value)
                case .setLocal(let depth, let slot):
                    guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                    let idx = envStack.count - 1 - depth
                    if idx < 0 || idx >= envStack.count { throw VMError.unsupported("setLocal depth out of range") }
                    var frame = envStack[idx]
                    if slot < 0 || slot >= frame.locals.count { throw VMError.unsupported("setLocal slot out of range") }
                    guard let b = frame.locals[slot] else { throw VMError.unsupported("setLocal to uninitialized local") }
                    if b.isConst { throw VMError.constAssignment(name: "slot_\(slot)", declaredAt: b.declLoc) }
                    frame.locals[slot] = Binding(value: v, isConst: b.isConst, declLoc: b.declLoc)
                    envStack[idx] = frame
                    
                    // MARK: Logical
                case .add: try numericAdd()
                case .sub: try numericSub()
                case .mul: try numericMul()
                case .div: try numericDiv()
                case .mod: try numericMod()
                    
                case .lt:  try numericCmp(<,  <,  <)
                case .le:  try numericCmp(<=, <=, <=)
                case .gt:  try numericCmp(>,  >,  >)
                case .ge:  try numericCmp(>=, >=, >=)
                    
                case .eq:
                    guard let b = stack.popLast(), let a = stack.popLast() else { throw VMError.stackUnderflow }
                    let result: Bool
                    switch (a, b) {
                    case (.nilValue, .nilValue):
                        result = true
                    case (.nilValue, _), (_, .nilValue):
                        result = false
                    case let (.int(x),    .int(y)):    result = (x == y)
                    case let (.uint(x),   .uint(y)):   result = (x == y)
                    case let (.double(x), .double(y)): result = (x == y)
                    case let (.int(x),    .double(y)): result = Double(x) == y
                    case let (.double(x), .int(y)):    result = x == Double(y)
                    case let (.int(x),    .uint(y)):
                        result = x >= 0 && UInt64(x) == y
                    case let (.uint(x),   .int(y)):
                        result = y >= 0 && x == UInt64(y)
                    case let (.double(x), .uint(y)):   result = x == Double(y)
                    case let (.uint(x),   .double(y)): result = Double(x) == y
                    case let (.bool(x),   .bool(y)):   result = (x == y)
                    case let (.string(x), .string(y)): result = (x == y)
                    case let (.object(x), .object(y)): result = (x == y)
                    default: throw VMError.typeError("== requires matching or numeric-compatible types")
                    }
                    stack.append(.bool(result))
                    
                case .logicalNot:
                    guard case let .bool(v)? = stack.popLast() else {
                        throw VMError.typeError("! expects Bool")
                    }
                    stack.append(.bool(!v))
                case .neg:
                    guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                    switch v {
                    case .int(let n):    stack.append(.int(-n))
                    case .double(let d): stack.append(.double(-d))
                    default:
                        throw VMError.typeError("unary - expects numeric")
                    }
                    
                case .printTop:
                    guard let top = stack.popLast() else { throw VMError.stackUnderflow }
                    writeLine(top.description)
                case .dup:
                    guard let v = stack.last else { throw VMError.stackUnderflow }
                    stack.append(v)
                case .pop:
                    guard let _ = stack.popLast() else { throw VMError.stackUnderflow }
                    
                    // ------- types / objects (unchanged) -------
                case .defineType(let name, let fields):
                    typeFields[name] = fields
                    
                case .newObject(let typeName, let argLabels):
                    guard let fields = typeFields[typeName] else { throw VMError.unknownType(typeName) }
                    // Require all fields for now (simple synthesized init)
                    guard Set(argLabels) == Set(fields), argLabels.count == fields.count else {
                        throw VMError.badInitializer("Expected labeled args \(fields) for \(typeName), got \(argLabels)")
                    }
                    // Pop in reverse of arg order
                    var storage: [String: Value] = [:]
                    for label in argLabels.reversed() {
                        guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                        storage[label] = v
                    }
                    stack.append(.object(.init(typeName: typeName, storage: storage)))
                    
                case .getProp(let prop):
                    let propName = program.propertyTable[safe: prop.raw] ?? "#\(prop.raw)"
                    guard let recv = stack.popLast() else { throw VMError.stackUnderflow }
                    switch recv {
                    case .object(let obj):
                        guard let v = obj.storage[propName] else { throw VMError.missingProperty(obj.typeName, propName) }
                        stack.append(v)
                    case .range(let range):
                        switch propName {
                        case "lowerBound":
                            stack.append(range.lower)
                        case "upperBound":
                            stack.append(range.upper)
                        case "isEmpty":
                            stack.append(.bool(try rangeIsEmpty(range)))
                        default:
                            throw VMError.missingProperty(rangeTypeName(range), propName)
                        }
                    case .host, .array, .dict, .metatype:
                        let value = try loadHostProperty(prop, named: propName, from: recv, in: program)
                        stack.append(value)
                    default:
                        throw VMError.typeError("getProp requires object/host/collection/metatype on stack")
                    }
                    
                case .setProp(let prop):
                    let propName = program.propertyTable[safe: prop.raw] ?? "#\(prop.raw)"
                    // Expect: ... receiver value
                    guard let val = stack.popLast() else { throw VMError.stackUnderflow }
                    guard let recv = stack.popLast() else { throw VMError.stackUnderflow }
                    switch recv {
                    case .object(let obj):
                        obj.storage[propName] = val
                    case .range:
                        throw VMError.unsupported("Range properties are read-only")
                    case .host, .array, .dict, .metatype:
                        try storeHostProperty(prop, named: propName, on: recv, value: val, in: program)
                    default:
                        throw VMError.typeError("setProp requires object/host/collection/metatype receiver")
                    }
                    // mutating, no push
                    
                    // ------- control flow (unchanged) -------
                case .jump(let offset):
                    ip += 1 + offset
                    continue
                case .jumpIfFalse(let offset):
                    guard case let .bool(flag)? = stack.popLast() else { throw VMError.typeError("jumpIfFalse expects Bool on stack") }
                    if flag == false { ip += 1 + offset; continue }
                case .jumpIfNil(let offset):
                    guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                    if case .nilValue = v { ip += 1 + offset; continue }
                    
                // ------- NEW: functions -------
                case .defineFunction(let name, let params, let entry, let localCount):
                    functionTable[name] = (params, entry, localCount)
                case .callFunc(let name, let argc):
                    guard let meta = functionTable[name] else { throw VMError.unsupported("Unknown function: \(name)") }
                    if argc != meta.params.count { throw VMError.badInitializer("Function \(name) expects \(meta.params.count) args, got \(argc)") }
                    let prevDepth = envStack.count
                    if meta.localCount > 0 {
                        var frame = Frame(table: [:], locals: Array(repeating: nil, count: meta.localCount))
                        for slot in stride(from: meta.params.count - 1, through: 0, by: -1) {
                            guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                            frame.locals[slot] = Binding(value: v, isConst: false, declLoc: nil)
                        }
                        envStack.append(frame)
                    } else {
                        var frame = Frame(table: [:], locals: [])
                        for param in meta.params.reversed() {
                            guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                            frame.table[param] = Binding(value: v, isConst: false, declLoc: nil)
                        }
                        envStack.append(frame)
                        envStack.append(Frame(table: [:], locals: []))
                    }
                    callStack.append(.init(retIP: ip + 1, prevDepth: prevDepth))
                    ip = meta.entry
                    continue
                case .ret:
                    let retVal: Value = stack.popLast() ?? .bool(true)
                    assert(envStack.last?.locals.count ?? 0 <= 4096, "suspiciously large local frame")
                    guard let callSite = callStack.popLast() else { throw VMError.unsupported("ret outside of function") }
                    while envStack.count > callSite.prevDepth { _ = envStack.popLast() }
                    let retIP = callSite.retIP
                    stack.append(retVal)
                    ip = retIP
                    continue

                case .makeClosure(let params, let paramTypes, let returnType, let entry, let captureNames):
                    var caps: [String: Value] = [:]
                    for name in captureNames {
                        guard let v = loadVarValue(name) else { throw VMError.varNotFoundWithSuggestions(name: name, suggestions: suggestions(for: name)) }
                        caps[name] = v
                    }
                    stack.append(.closure(.init(params: params, paramTypes: paramTypes, returnType: returnType, entry: entry, captures: caps)))

                case .makeClosureVals(let params, let paramTypes, let returnType, let entry, let captureNames):
                    var caps: [String: Value] = [:]
                    for i in stride(from: captureNames.count - 1, through: 0, by: -1) {
                        guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                        caps[captureNames[i]] = v
                    }
                    stack.append(.closure(.init(params: params, paramTypes: paramTypes, returnType: returnType, entry: entry, captures: caps)))

                case .callValue(let argc):
                    var args: [Value] = []
                    for _ in 0..<argc {
                        guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                        args.append(v)
                    }
                    guard let callee = stack.popLast() else { throw VMError.stackUnderflow }
                    switch callee {
                    case .closure(let clo):
                        if argc != clo.params.count { throw VMError.badInitializer("Closure expects \(clo.params.count) args, got \(argc)") }
                        var frame = Frame(table: [:], locals: [])
                        for (k, v) in clo.captures { frame.table[k] = Binding(value: v, isConst: false, declLoc: nil) }
                        for (name, value) in zip(clo.params, args.reversed()) {
                            frame.table[name] = Binding(value: value, isConst: false, declLoc: nil)
                        }
                        let prevDepth = envStack.count
                        envStack.append(frame)
                        envStack.append(Frame(table: [:], locals: []))
                        callStack.append(.init(retIP: ip + 1, prevDepth: prevDepth))
                        ip = clo.entry
                        continue
                    default:
                        throw VMError.typeError("call of non-callable value: \(String(describing: callee))")
                    }

                // ------- collections -------
                case .makeArray(let count):
                    var arr: [Value] = []
                    arr.reserveCapacity(count)
                    for _ in 0..<count {
                        guard let v = stack.popLast() else { throw VMError.stackUnderflow }
                        arr.append(v)
                    }
                    stack.append(.array(ArrBox(arr.reversed())))
                case .makeDict(let count):
                    var dict: [String: Value] = [:]
                    for _ in 0..<count {
                        guard let val = stack.popLast() else { throw VMError.stackUnderflow }
                        guard let keyVal = stack.popLast() else { throw VMError.stackUnderflow }
                        guard case let .string(key) = keyVal else { throw VMError.typeError("dictionary keys must be String") }
                        dict[key] = val
                    }
                    stack.append(.dict(DictBox(dict)))
                case .makeRange(let isClosed):
                    guard let upper = stack.popLast() else { throw VMError.stackUnderflow }
                    guard let lower = stack.popLast() else { throw VMError.stackUnderflow }
                    let kind: RangeBox.Kind = isClosed ? .closed : .halfOpen
                    try validateRangeBounds(lower: lower, upper: upper, kind: kind)
                    stack.append(.range(RangeBox(lower: lower, upper: upper, kind: kind)))
                case .getIndex:
                    guard let idx = stack.popLast() else { throw VMError.stackUnderflow }
                    guard let container = stack.popLast() else { throw VMError.stackUnderflow }
                    switch (container, idx) {
                    case let (.array(box), .int(i)):
                        if i < 0 || i >= box.elements.count { throw VMError.indexOutOfRange(i, box.elements.count) }
                        stack.append(box.elements[i])
                    case let (.dict(box), .string(k)):
                        guard let v = box.storage[k] else { throw VMError.keyNotFound(k) }
                        stack.append(v)
                    default:
                        throw VMError.typeError("invalid indexing operation")
                    }
                case .setIndex:
                    guard let value = stack.popLast() else { throw VMError.stackUnderflow }
                    guard let idx = stack.popLast() else { throw VMError.stackUnderflow }
                    guard let container = stack.popLast() else { throw VMError.stackUnderflow }
                    switch (container, idx) {
                    case let (.array(box), .int(i)):
                        if i < 0 || i >= box.elements.count { throw VMError.indexOutOfRange(i, box.elements.count) }
                        box.elements[i] = value
                    case let (.dict(box), .string(k)):
                        box.storage[k] = value
                    default:
                        throw VMError.typeError("invalid indexing assignment")
                    }
                case .callMethod(let sel, let argc):
                    var tmp: [Value] = []
                    for _ in 0..<argc { guard let v = stack.popLast() else { throw VMError.stackUnderflow }; tmp.append(v) }
                    let args = Array(tmp.reversed())
                    guard let recv = args.first else { throw VMError.stackUnderflow }
                    let hostDispatchTable = hostDispatchByProgram[program.id] ?? [:]
                    let selectorName = curSelectorNames[safe: sel.raw] ?? "#\(sel.raw)"
                    func dispatch(to tid: TypeID) throws {
                        let tname = typeName(for: tid)
                        if !isMethodAllowed(typeName: tname, selectorID: sel) {
                            throw VMError.missingMethod(tname, selectorName)
                        }
                        guard let thunk = hostDispatchTable[Program.MethodKey(type: tid, selector: sel)] else {
                            throw VMError.missingMethod(tname, selectorName)
                        }
                        let rv = try thunk(&self, args)
                        stack.append(rv)
                    }
                    switch recv {
                    case .metatype(let tid):
                        try dispatch(to: tid)
                    case .host(_, let tid):
                        try dispatch(to: tid)
                    case .array:
                        // Try host specializations first (Array<Int>, Array<String>, etc.)
                        if let hostResult = try dispatchArrayHostSpecialization(selector: sel, args: args, hostDispatchTable: hostDispatchTable) {
                            stack.append(hostResult)
                        } else {
                            // Fall back to VM-native array dispatch
                            try dispatch(to: TID_Array)
                        }
                    case .dict:
                        // Try host specializations first (Dictionary<String, Int>, etc.)
                        if let hostResult = try dispatchDictionaryHostSpecialization(selector: sel, args: args, hostDispatchTable: hostDispatchTable) {
                            stack.append(hostResult)
                        } else {
                            // Fall back to VM-native dictionary dispatch
                            try dispatch(to: TID_Dictionary)
                        }
                    case .range(let range):
                        switch selectorName {
                        case "contains(_:)":
                            if args.count != 2 { throw VMError.typeError("Range.contains expects 1 argument") }
                            let needle = args[1]
                            let contains = try rangeContains(range, value: needle)
                            stack.append(.bool(contains))
                        default:
                            throw VMError.missingMethod(rangeTypeName(range), selectorName)
                        }
                    case .object(let obj):
                        // User-defined methods: lookup in Program.userMethods by (TypeID, SelectorID)
                        // Resolve TypeID by name from current program table
                        guard let typeIndex = curTypeNames.firstIndex(of: obj.typeName) else {
                            throw VMError.missingMethod(obj.typeName, selectorName)
                        }
                        let tid = TypeID(raw: typeIndex)
                        let key = Program.MethodKey(type: tid, selector: sel)
                        if let meta = program.userMethods[key] {
                            // Arity check (params include implicit self)
                            if args.count != meta.params.count {
                                throw VMError.badInitializer("Method expects \(meta.params.count - 1) args, got \(args.count - 1)")
                            }
                            let prevDepth = envStack.count
                            if meta.localCount > 0 {
                                var frame = Frame(table: [:], locals: Array(repeating: nil, count: meta.localCount))
                                // Place params in slots [0..<paramCount]
                                for slot in stride(from: meta.params.count - 1, through: 0, by: -1) {
                                    frame.locals[slot] = Binding(value: args[slot], isConst: false, declLoc: nil)
                                }
                                envStack.append(frame)
                            } else {
                                var frame = Frame(table: [:], locals: [])
                                for (name, value) in zip(meta.params, args) { frame.table[name] = Binding(value: value, isConst: false, declLoc: nil) }
                                envStack.append(frame)
                                envStack.append(Frame(table: [:], locals: []))
                            }
                            callStack.append(.init(retIP: ip + 1, prevDepth: prevDepth))
                            ip = meta.entry
                            continue
                        } else {
                            throw VMError.missingMethod(obj.typeName, selectorName)
                        }
                    default:
                        // Try optional host specializations for other value types
                        if let hostResult = try dispatchOptionalHostSpecialization(selector: sel, args: args, hostDispatchTable: hostDispatchTable) {
                            stack.append(hostResult)
                        } else {
                            throw VMError.typeError("Receiver has no methods")
                        }
                    }
                case .trap(let message):
                    throw VMError.unsupported(message)
                }
                ip += 1
            }
        } catch let e as VMError {
            let loc: SourceLoc? = program.locs[safe: lastIP] ?? nil
            throw VMExecutionError(underlying: e, loc: loc)
        }
    }
    
    private enum NumericValue {
        case int(Int)
        case uint(UInt64)
        case double(Double)

        var asDouble: Double {
            switch self {
            case .double(let d): return d
            case .int(let n): return Double(n)
            case .uint(let u): return Double(u)
            }
        }
    }

    private func numericValue(from value: Value) -> NumericValue? {
        switch value {
        case .int(let n): return .int(n)
        case .uint(let u): return .uint(u)
        case .double(let d): return .double(d)
        default: return nil
        }
    }

    private func requireNumeric(_ value: Value) throws -> NumericValue {
        guard let numeric = numericValue(from: value) else {
            throw VMError.typeError("numeric operation on non-numeric types")
        }
        return numeric
    }

    private mutating func popNumericPair() throws -> (NumericValue, NumericValue) {
        guard let rhsRaw = stack.popLast() else { throw VMError.typeError("expected numeric (rhs)") }
        guard let lhsRaw = stack.popLast() else { throw VMError.typeError("expected numeric (lhs)") }
        let lhs = try requireNumeric(lhsRaw)
        let rhs = try requireNumeric(rhsRaw)
        return (lhs, rhs)
    }
    
    private mutating func numericAdd() throws {
        guard let rhs = stack.popLast() else { throw VMError.stackUnderflow }
        guard let lhs = stack.popLast() else { throw VMError.stackUnderflow }

        // If either operand is a string, coerce both to string and concatenate.
        if case let .string(ls) = lhs {
            stack.append(.string(ls + stringifyForAddition(rhs)))
            return
        }
        if case let .string(rs) = rhs {
            stack.append(.string(stringifyForAddition(lhs) + rs))
            return
        }

        let lhsNum = try requireNumeric(lhs)
        let rhsNum = try requireNumeric(rhs)
        switch (lhsNum, rhsNum) {
        case (.double(let a), .double(let b)):
            stack.append(.double(a + b))
        case (.double(let a), let b):
            stack.append(.double(a + b.asDouble))
        case (let a, .double(let b)):
            stack.append(.double(a.asDouble + b))
        case (.int(let a), .int(let b)):
            stack.append(.int(a + b))
        case (.uint(let a), .uint(let b)):
            stack.append(.uint(a + b))
        case (.uint(let a), .int(let b)):
            guard b >= 0 else { throw VMError.typeError("numeric operation on incompatible numeric types") }
            stack.append(.uint(a + UInt64(b)))
        case (.int(let a), .uint(let b)):
            guard a >= 0 else { throw VMError.typeError("numeric operation on incompatible numeric types") }
            stack.append(.uint(UInt64(a) + b))
        default:
            throw VMError.typeError("numeric operation on incompatible numeric types")
        }
    }
    private mutating func numericSub() throws {
        let (lhs, rhs) = try popNumericPair()
        switch (lhs, rhs) {
        case (.double(let a), .double(let b)):
            stack.append(.double(a - b))
        case (.double(let a), _):
            stack.append(.double(a - rhs.asDouble))
        case (_, .double(let b)):
            stack.append(.double(lhs.asDouble - b))
        case (.int(let a), .int(let b)):
            stack.append(.int(a - b))
        case (.uint(let a), .uint(let b)):
            stack.append(.uint(a - b))
        case (.uint(let a), .int(let b)):
            guard b >= 0 else { throw VMError.typeError("numeric operation on incompatible numeric types") }
            stack.append(.uint(a - UInt64(b)))
        case (.int(let a), .uint(let b)):
            guard a >= 0 else { throw VMError.typeError("numeric operation on incompatible numeric types") }
            stack.append(.uint(UInt64(a) - b))
        default:
            throw VMError.typeError("numeric operation on incompatible numeric types")
        }
    }
    private mutating func numericMul() throws {
        let (lhs, rhs) = try popNumericPair()
        switch (lhs, rhs) {
        case (.double(let a), .double(let b)):
            stack.append(.double(a * b))
        case (.double(let a), _):
            stack.append(.double(a * rhs.asDouble))
        case (_, .double(let b)):
            stack.append(.double(lhs.asDouble * b))
        case (.int(let a), .int(let b)):
            stack.append(.int(a * b))
        case (.uint(let a), .uint(let b)):
            stack.append(.uint(a * b))
        case (.uint(let a), .int(let b)):
            guard b >= 0 else { throw VMError.typeError("numeric operation on incompatible numeric types") }
            stack.append(.uint(a * UInt64(b)))
        case (.int(let a), .uint(let b)):
            guard a >= 0 else { throw VMError.typeError("numeric operation on incompatible numeric types") }
            stack.append(.uint(UInt64(a) * b))
        default:
            throw VMError.typeError("numeric operation on incompatible numeric types")
        }
    }
    
    private mutating func numericDiv() throws {
        let (lhs, rhs) = try popNumericPair()
        switch (lhs, rhs) {
        case (.double(let a), .double(let b)):
            if b == 0 { throw VMError.divideByZero }
            stack.append(.double(a / b))
        case (.double(let a), _):
            let denom = rhs.asDouble
            if denom == 0 { throw VMError.divideByZero }
            stack.append(.double(a / denom))
        case (_, .double(let b)):
            if b == 0 { throw VMError.divideByZero }
            stack.append(.double(lhs.asDouble / b))
        case (.int(let a), .int(let b)):
            if b == 0 { throw VMError.divideByZero }
            stack.append(.int(a / b))                 // Int / Int stays Int (Swift-like)
        case (.uint(let a), .uint(let b)):
            if b == 0 { throw VMError.divideByZero }
            stack.append(.uint(a / b))
        case (.uint(let a), .int(let b)):
            if b == 0 { throw VMError.divideByZero }
            guard b > 0 else { throw VMError.typeError("numeric operation on incompatible numeric types") }
            stack.append(.uint(a / UInt64(b)))
        case (.int(let a), .uint(let b)):
            guard a >= 0 else { throw VMError.typeError("numeric operation on incompatible numeric types") }
            if b == 0 { throw VMError.divideByZero }
            stack.append(.uint(UInt64(a) / b))
        default:
            throw VMError.typeError("numeric operation on incompatible numeric types")
        }
    }
    
    private mutating func numericMod() throws {
        let (lhs, rhs) = try popNumericPair()
        switch (lhs, rhs) {
        case (.double(let a), .double(let b)):
            if b == 0 { throw VMError.divideByZero }
            stack.append(.double(a.truncatingRemainder(dividingBy: b)))
        case (.double(let a), _):
            let denom = rhs.asDouble
            if denom == 0 { throw VMError.divideByZero }
            stack.append(.double(a.truncatingRemainder(dividingBy: denom)))
        case (_, .double(let b)):
            if b == 0 { throw VMError.divideByZero }
            stack.append(.double(lhs.asDouble.truncatingRemainder(dividingBy: b)))
        case (.int(let a), .int(let b)):
            if b == 0 { throw VMError.divideByZero }
            stack.append(.int(a % b))
        case (.uint(let a), .uint(let b)):
            if b == 0 { throw VMError.divideByZero }
            stack.append(.uint(a % b))
        case (.uint(let a), .int(let b)):
            if b == 0 { throw VMError.divideByZero }
            guard b > 0 else { throw VMError.typeError("numeric operation on incompatible numeric types") }
            stack.append(.uint(a % UInt64(b)))
        case (.int(let a), .uint(let b)):
            guard a >= 0 else { throw VMError.typeError("numeric operation on incompatible numeric types") }
            if b == 0 { throw VMError.divideByZero }
            stack.append(.uint(UInt64(a) % b))
        default:
            throw VMError.typeError("numeric operation on incompatible numeric types")
        }
    }

    private mutating func numericCmp(_ f: (Double, Double) -> Bool,
                                     _ i: (Int, Int) -> Bool,
                                     _ u: (UInt64, UInt64) -> Bool) throws {
        let (lhs, rhs) = try popNumericPair()
        switch (lhs, rhs) {
        case (.int(let a), .int(let b)):
            stack.append(.bool(i(a, b)))
        case (.uint(let a), .uint(let b)):
            stack.append(.bool(u(a, b)))
        case (.int(let a), .uint(let b)):
            if a >= 0 {
                stack.append(.bool(u(UInt64(a), b)))
            } else {
                stack.append(.bool(f(Double(a), Double(b))))
            }
        case (.uint(let a), .int(let b)):
            if b >= 0 {
                stack.append(.bool(u(a, UInt64(b))))
            } else {
                stack.append(.bool(f(Double(a), Double(b))))
            }
        default:
            stack.append(.bool(f(lhs.asDouble, rhs.asDouble)))
        }
    }

    private func numericAsDouble(_ value: Value, label: String) throws -> Double {
        switch value {
        case .double(let d): return d
        case .int(let n): return Double(n)
        case .uint(let u): return Double(u)
        default:
            throw VMError.typeError("\(label) expects a number")
        }
    }

    private func validateRangeBounds(lower: Value, upper: Value, kind: RangeBox.Kind) throws {
        let lo = try numericAsDouble(lower, label: "range lower bound")
        let hi = try numericAsDouble(upper, label: "range upper bound")
        if lo.isNaN || hi.isNaN {
            throw VMError.typeError("range bounds must be finite numbers")
        }
        if lo > hi {
            let label = (kind == .closed) ? "ClosedRange" : "Range"
            throw VMError.typeError("\(label) lowerBound must be <= upperBound")
        }
    }

    private func rangeContains(_ range: RangeBox, value: Value) throws -> Bool {
        try validateRangeBounds(lower: range.lower, upper: range.upper, kind: range.kind)
        let lo = try numericAsDouble(range.lower, label: "range lower bound")
        let hi = try numericAsDouble(range.upper, label: "range upper bound")
        let probe = try numericAsDouble(value, label: "contains argument")
        if probe < lo { return false }
        if range.kind == .closed { return probe <= hi }
        return probe < hi
    }

    private func rangeIsEmpty(_ range: RangeBox) throws -> Bool {
        try validateRangeBounds(lower: range.lower, upper: range.upper, kind: range.kind)
        let lo = try numericAsDouble(range.lower, label: "range lower bound")
        let hi = try numericAsDouble(range.upper, label: "range upper bound")
        switch range.kind {
        case .closed:
            return false
        case .halfOpen:
            return lo == hi
        }
    }

    private func rangeTypeName(_ range: RangeBox) -> String {
        switch range.kind {
        case .closed: return "ClosedRange"
        case .halfOpen: return "Range"
        }
    }

    private func stringifyForAddition(_ value: Value) -> String {
        switch value {
        case .string(let s): return s
        default: return value.description
        }
    }
    
    // MARK: - Helpers
    private mutating func writeLine(_ s: String) {
        output.append(s)
        output.append("\n")
    }
    
    private func findBindingIndex(_ name: String) -> Int? {
        for i in envStack.indices.reversed() {
            if envStack[i].table[name] != nil { return i }
        }
        return nil
    }

    private func loadVarValue(_ name: String) -> Value? {
        if let idx = findBindingIndex(name), let b = envStack[idx].table[name] { return b.value }
        return nil
    }

    private mutating func def(_ name: String, value: Value, isConst: Bool, declLoc: SourceLoc?) throws {
        var cur = envStack.removeLast()
        if let existing = cur.table[name] {
            throw VMError.redeclaration(name: name, originalAt: existing.declLoc)
        }
        cur.table[name] = Binding(value: value, isConst: isConst, declLoc: declLoc)
        envStack.append(cur)
    }

    // Returns true if an existing binding was updated; false if not found
    private mutating func setVar(_ name: String, to value: Value) throws -> Bool {
        if let idx = findBindingIndex(name) {
            if let b = envStack[idx].table[name] {
                if b.isConst { throw VMError.constAssignment(name: name, declaredAt: b.declLoc) }
                var frame = envStack[idx]
                frame.table[name] = Binding(value: value, isConst: b.isConst, declLoc: b.declLoc)
                envStack[idx] = frame
                return true
            }
        }
        return false
    }

    // Suggestions: gather visible names and rank via edit distance + prefix
    private func suggestions(for name: String) -> [String] {
        var seen = Set<String>()
        var names: [String] = []
        for i in envStack.indices.reversed() {
            for k in envStack[i].table.keys where !seen.contains(k) {
                seen.insert(k); names.append(k)
            }
        }
        func score(_ cand: String) -> Int {
            let lcName = name.lowercased(), lcCand = cand.lowercased()
            let dist = levenshtein(lcName, lcCand)
            let prefixBonus = lcCand.hasPrefix(lcName) ? -1 : 0
            return dist * 2 + prefixBonus
        }
        return names.sorted { score($0) < score($1) }.prefix(3).map { $0 }
    }

    private func levenshtein(_ aStr: String, _ bStr: String) -> Int {
        let a = Array(aStr), b = Array(bStr)
        let n = a.count, m = b.count
        if n == 0 { return m }; if m == 0 { return n }
        var dp = Array(repeating: Array(repeating: 0, count: m + 1), count: n + 1)
        for i in 0...n { dp[i][0] = i }
        for j in 0...m { dp[0][j] = j }
        for i in 1...n {
            for j in 1...m {
                let cost = (a[i - 1] == b[j - 1]) ? 0 : 1
                dp[i][j] = min(
                    dp[i - 1][j] + 1,
                    dp[i][j - 1] + 1,
                    dp[i - 1][j - 1] + cost
                )
            }
        }
        return dp[n][m]
    }
}

extension Array {
    fileprivate subscript(safe i: Int) -> Element? { (i >= 0 && i < count) ? self[i] : nil }
}
