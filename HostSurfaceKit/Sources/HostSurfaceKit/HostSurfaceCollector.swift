//
//  HostSurfaceCollector.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/19/25.
//

import Foundation
import SwiftSyntax

#if DEBUG
enum HostSurfaceCollectorDebugMetrics {
    private static let lock = NSLock()
    nonisolated(unsafe) private static var _fallbackPlaceholderHitCount: Int = 0

    static var fallbackPlaceholderHitCount: Int {
        lock.lock()
        defer { lock.unlock() }
        return _fallbackPlaceholderHitCount
    }

    static func recordFallbackPlaceholderHit() {
        lock.lock()
        _fallbackPlaceholderHitCount += 1
        lock.unlock()
    }

    static func reset() {
        lock.lock()
        _fallbackPlaceholderHitCount = 0
        lock.unlock()
    }
}
#endif
#if canImport(SwiftParser)
import SwiftParser
#elseif canImport(SwiftSyntaxParser)
import SwiftSyntaxParser
#endif

struct HostSurfaceParameter: Equatable {
    let label: String?
    let name: String
    let type: TypeName
    let isInout: Bool
    let isVariadic: Bool
}

enum HostSurfaceMethodKind: Equatable {
    case initializer(isFailable: Bool)
    case instance(isMutating: Bool)
    case `static`
}

struct HostSurfaceMethod: Equatable {
    let type: TypeName
    let kind: HostSurfaceMethodKind
    let baseName: String
    let selector: String
    let returnType: TypeName?
    let parameters: [HostSurfaceParameter]
    let genericParameters: [GenericParameter]
    let whereClause: String?
    let availability: [String]
    let attributes: [String]
    let throwsKind: TypeName.FunctionSignature.ThrowsKind
    let isAsync: Bool
    let sourceLine: Int

    var isThrowing: Bool { throwsKind != .none }
}

struct HostSurfaceProperty: Equatable {
    let type: TypeName
    let name: String
    let propertyType: TypeName
    let isStatic: Bool
    let isSettable: Bool
    let availability: [String]
    let attributes: [String]
    let sourceLine: Int
}

struct HostSurfaceCollection {
    var methods: [HostSurfaceMethod]
    var properties: [HostSurfaceProperty]
    var diagnostics: [HostSurfaceDiagnostic]
    var fallbackPlaceholderHitCount: Int
    var conformances: [String: [String]]  // typeName -> [protocolNames]
    var protocolAssociatedTypes: [String: [String]]  // protocolName -> [associatedTypeNames]
    var typeAliases: [String: [String: String]]  // typeName -> [aliasName -> targetType]
    var genericParameterConstraints: [String: [String: Set<String>]]

    init(methods: [HostSurfaceMethod],
         properties: [HostSurfaceProperty],
         diagnostics: [HostSurfaceDiagnostic],
         fallbackPlaceholderHitCount: Int = 0,
         conformances: [String: [String]] = [:],
         protocolAssociatedTypes: [String: [String]] = [:],
         typeAliases: [String: [String: String]] = [:],
         genericParameterConstraints: [String: [String: Set<String>]] = [:]) {
        self.methods = methods
        self.properties = properties
        self.diagnostics = diagnostics
        self.fallbackPlaceholderHitCount = fallbackPlaceholderHitCount
        self.conformances = conformances
        self.protocolAssociatedTypes = protocolAssociatedTypes
        self.typeAliases = typeAliases
        self.genericParameterConstraints = genericParameterConstraints
    }
}

public struct HostSurfaceDiagnostic: Equatable, CustomStringConvertible {
    public enum Kind: Equatable {
        case parseError(String)
        case unsupported(String)
        case missingPlaceholders(String)
    }

    public let kind: Kind
    let line: Int?
    
    public init(kind: Kind, line: Int?) {
        self.kind = kind
        self.line = line
    }

    public var description: String {
        switch kind {
        case .parseError(let message):
            return "Parse error: \(message)"
        case .unsupported(let message):
            return "Unsupported: \(message)"
        case .missingPlaceholders(let type):
            return "Missing generic placeholders for \(type)"
        }
    }
}

struct HostSurfaceCollector {
    let genericParameterInventory: [String: [String]]

    init(genericParameterInventory: [String: [String]] = [:]) {
        self.genericParameterInventory = genericParameterInventory
    }

    func collect(moduleName: String, contents: String) -> HostSurfaceCollection {
#if DEBUG
        HostSurfaceCollectorDebugMetrics.reset()
#endif
        let tree: SourceFileSyntax
        #if canImport(SwiftParser)
        tree = Parser.parse(source: contents)
        #elseif canImport(SwiftSyntaxParser)
        if let parsed = try? SyntaxParser.parse(source: contents) {
            tree = parsed
        } else {
            return HostSurfaceCollection(methods: [],
                                         properties: [],
                                         diagnostics: [.init(kind: .parseError("Failed to parse source"), line: nil)],
                                         fallbackPlaceholderHitCount: 0)
        }
        #else
        return HostSurfaceCollection(methods: [],
                                     properties: [],
                                     diagnostics: [.init(kind: .parseError("No Swift parser available"), line: nil)],
                                     fallbackPlaceholderHitCount: 0)
        #endif
        let converter = SourceLocationConverter(file: moduleName, tree: tree)
        let visitor = SurfaceVisitor(moduleName: moduleName,
                                     converter: converter,
                                     genericParameterInventory: genericParameterInventory)
        visitor.walk(tree)
#if DEBUG
        let fallbackHits = HostSurfaceCollectorDebugMetrics.fallbackPlaceholderHitCount
#else
        let fallbackHits = 0
        #endif
        return HostSurfaceCollection(methods: visitor.methods,
                                     properties: visitor.properties,
                                     diagnostics: visitor.diagnostics,
                                     fallbackPlaceholderHitCount: fallbackHits,
                                     conformances: visitor.conformances,
                                     protocolAssociatedTypes: visitor.protocolAssociatedTypes,
                                     typeAliases: visitor.typeAliases,
                                     genericParameterConstraints: visitor.genericParameterConstraints)
    }
}

private final class SurfaceVisitor: SyntaxVisitor {
    private struct TypeContext {
        let typeName: TypeName
        let inheritedAvailability: [String]
        let isProtocol: Bool  // Track if this context is a protocol
        let whereClause: String?  // Track extension-level generic constraints
    }

    private let moduleName: String
    private let converter: SourceLocationConverter
    private var contextStack: [TypeContext] = []
    private var typeGenericParameters: [String: [String]] = [:]
    private let genericParameterInventory: [String: [String]]
    private(set) var methods: [HostSurfaceMethod] = []
    private(set) var properties: [HostSurfaceProperty] = []
    private(set) var diagnostics: [HostSurfaceDiagnostic] = []
    private(set) var conformances: [String: [String]] = [:]  // typeName -> [protocolNames]
    private(set) var protocolAssociatedTypes: [String: [String]] = [:]  // protocolName -> [associatedTypeNames]
    private(set) var typeAliases: [String: [String: String]] = [:]  // typeName -> [aliasName -> targetType]
    private(set) var genericParameterConstraints: [String: [String: Set<String>]] = [:]

    init(moduleName: String,
         converter: SourceLocationConverter,
         genericParameterInventory: [String: [String]]) {
        self.moduleName = moduleName
        self.converter = converter
        self.genericParameterInventory = genericParameterInventory
        self.typeGenericParameters = genericParameterInventory
        super.init(viewMode: .sourceAccurate)
    }

    override func visit(_ node: StructDeclSyntax) -> SyntaxVisitorContinueKind {
        let generics = node.genericParameterClause?.parameters.map { $0.name.text } ?? []
        let access = accessInfo(for: node.modifiers)
        recordNominal(named: node.identifier.text,
                      generics: generics,
                      access: access)
        guard access.isPublicOrOpen else { return .skipChildren }

        let (availability, _) = partitionAttributes(node.attributes)

        // Capture conformances from inheritance clause BEFORE pushing type to stack
        if let inheritanceClause = node.inheritanceClause {
            recordConformances(for: node.identifier.text, from: inheritanceClause)
        }

        pushType(named: node.identifier.text,
                 genericParameters: generics,
                 availability: availability)

        if let current = contextStack.last {
            recordGenericParameterConstraints(from: node.genericParameterClause,
                                              for: current.typeName)
        }

        return .visitChildren
    }

    override func visitPost(_ node: StructDeclSyntax) {
        if accessInfo(for: node.modifiers).isPublicOrOpen {
            popType()
        }
    }

    override func visit(_ node: ClassDeclSyntax) -> SyntaxVisitorContinueKind {
        let generics = node.genericParameterClause?.parameters.map { $0.name.text } ?? []
        let access = accessInfo(for: node.modifiers)
        recordNominal(named: node.identifier.text,
                      generics: generics,
                      access: access)
        guard access.isPublicOrOpen else { return .skipChildren }

        let (availability, _) = partitionAttributes(node.attributes)

        // Capture conformances from inheritance clause BEFORE pushing type to stack
        if let inheritanceClause = node.inheritanceClause {
            recordConformances(for: node.identifier.text, from: inheritanceClause)
        }

        pushType(named: node.identifier.text,
                 genericParameters: generics,
                 availability: availability)

        if let current = contextStack.last {
            recordGenericParameterConstraints(from: node.genericParameterClause,
                                              for: current.typeName)
        }

        return .visitChildren
    }

    override func visitPost(_ node: ClassDeclSyntax) {
        if accessInfo(for: node.modifiers).isPublicOrOpen {
            popType()
        }
    }

    override func visit(_ node: ActorDeclSyntax) -> SyntaxVisitorContinueKind {
        let generics = node.genericParameterClause?.parameters.map { $0.name.text } ?? []
        let access = accessInfo(for: node.modifiers)
        recordNominal(named: node.identifier.text,
                      generics: generics,
                      access: access)
        guard access.isPublicOrOpen else { return .skipChildren }

        let (availability, _) = partitionAttributes(node.attributes)
        pushType(named: node.identifier.text,
                 genericParameters: generics,
                 availability: availability)

        if let current = contextStack.last {
            recordGenericParameterConstraints(from: node.genericParameterClause,
                                              for: current.typeName)
        }

        return .visitChildren
    }

    override func visitPost(_ node: ActorDeclSyntax) {
        if accessInfo(for: node.modifiers).isPublicOrOpen {
            popType()
        }
    }

    override func visit(_ node: EnumDeclSyntax) -> SyntaxVisitorContinueKind {
        let generics = node.genericParameterClause?.parameters.map { $0.name.text } ?? []
        let access = accessInfo(for: node.modifiers)
        recordNominal(named: node.identifier.text,
                      generics: generics,
                      access: access)
        guard access.isPublicOrOpen else { return .skipChildren }

        let (availability, _) = partitionAttributes(node.attributes)

        // Capture conformances from inheritance clause BEFORE pushing type to stack
        if let inheritanceClause = node.inheritanceClause {
            recordConformances(for: node.identifier.text, from: inheritanceClause)
        }

        pushType(named: node.identifier.text,
                 genericParameters: generics,
                 availability: availability)

        if let current = contextStack.last {
            recordGenericParameterConstraints(from: node.genericParameterClause,
                                              for: current.typeName)
        }

        return .visitChildren
    }

    override func visitPost(_ node: EnumDeclSyntax) {
        if accessInfo(for: node.modifiers).isPublicOrOpen {
            popType()
        }
    }

    override func visit(_ node: ProtocolDeclSyntax) -> SyntaxVisitorContinueKind {
        // Protocols don't have genericParameterClause - they use associatedtype instead
        let generics: [String] = []
        let access = accessInfo(for: node.modifiers)
        recordNominal(named: node.identifier.text,
                      generics: generics,
                      access: access)
        guard access.isPublicOrOpen else { return .skipChildren }

        let (availability, _) = partitionAttributes(node.attributes)

        // Capture conformances from inheritance clause BEFORE pushing type to stack
        if let inheritanceClause = node.inheritanceClause {
            recordConformances(for: node.identifier.text, from: inheritanceClause)
        }

        pushType(named: node.identifier.text,
                 genericParameters: generics,
                 availability: availability,
                 isProtocol: true)

        return .visitChildren
    }

    override func visitPost(_ node: ProtocolDeclSyntax) {
        if accessInfo(for: node.modifiers).isPublicOrOpen {
            popType()
        }
    }

    override func visit(_ node: ExtensionDeclSyntax) -> SyntaxVisitorContinueKind {
        guard var typeName = parseTypeName(node.extendedType, line: node.startLine(converter: converter)) else {
            return .skipChildren
        }

        // Capture the extension's where clause for tracking constraints
        var extensionWhereClause: String? = nil

        // If the extension has a where-clause constraining Self to a concrete type,
        // resolve the receiver to that concrete type so members attach to the
        // specialized receiver rather than the protocol/base.
        if let clause = node.genericWhereClause?.description.trimmingCharacters(in: .whitespacesAndNewlines) {
            if let selfTypeText = parseSelfEquality(from: clause),
               let selfType = try? TypeNameParser.parse(selfTypeText) {
                typeName = selfType
            } else {
                // This extension has generic constraints (not just Self equality)
                // Store them so we can mark methods as constrained
                extensionWhereClause = clause
            }
        }

        let extensionAvailability = partitionAttributes(node.attributes).availability
        if typeName.path.count == 1 {
            typeName = TypeName(path: [moduleName] + typeName.path,
                                genericArguments: typeName.genericArguments)
        }
        let expectedPlaceholders = placeholderInventory(for: typeName.path)
        if typeName.genericArguments.isEmpty,
           let placeholders = expectedPlaceholders {
            typeName.genericArguments = placeholders.map { TypeName(path: [$0]) }
            if !typeName.path.isEmpty {
                typeName.genericOwnerIndex = max(typeName.path.count - 1, 0)
            }
        }
        if typeName.genericArguments.isEmpty {
            if let placeholders = expectedPlaceholders, !placeholders.isEmpty {
#if DEBUG
                HostSurfaceCollectorDebugMetrics.recordFallbackPlaceholderHit()
#endif
                diagnostics.append(.init(kind: .missingPlaceholders(typeName.path.joined(separator: ".")),
                                         line: node.startLine(converter: converter)))
            }
        } else {
            let finalKey = typeName.path.joined(separator: ".")
            if typeGenericParameters[finalKey] == nil {
                let placeholderNames = typeName.genericArguments.compactMap { argument -> String? in
                    guard argument.genericArguments.isEmpty,
                          argument.functionSignature == nil,
                          argument.path.count == 1 else { return nil }
                    return argument.path[0]
                }
                if placeholderNames.count == typeName.genericArguments.count,
                   !placeholderNames.isEmpty {
                    typeGenericParameters[finalKey] = placeholderNames
                }
            }
        }
        contextStack.append(TypeContext(typeName: typeName, inheritedAvailability: extensionAvailability, isProtocol: false, whereClause: extensionWhereClause))

        if let clause = extensionWhereClause {
            recordConstraints(fromWhereClause: clause, for: typeName)
        }

        // Capture conformances from extension's inheritance clause
        if let inheritanceClause = node.inheritanceClause {
            let extendedTypeName = typeName.path.joined(separator: ".")
            // Get base type name (strip generic arguments for lookup key)
            let baseTypeName: String
            if let genericStart = extendedTypeName.firstIndex(of: "<") {
                baseTypeName = String(extendedTypeName[..<genericStart])
            } else {
                baseTypeName = extendedTypeName
            }

            var protocols: [String] = []
            for inheritedType in inheritanceClause.inheritedTypes {
                let typeText = inheritedType.type.description.trimmingCharacters(in: .whitespaces)
                if let parsed = try? TypeNameParser.parse(typeText) {
                    var protocolPath = parsed.path
                    // If the protocol name is not module-qualified, add the current module
                    if protocolPath.count == 1 {
                        protocolPath = [moduleName] + protocolPath
                    }
                    let protocolName = protocolPath.joined(separator: ".")
                    protocols.append(protocolName)
                }
            }

            if !protocols.isEmpty {
                // Merge with existing conformances
                var existing = conformances[baseTypeName] ?? []
                for proto in protocols {
                    if !existing.contains(proto) {
                        existing.append(proto)
                    }
                }
                conformances[baseTypeName] = existing
            }
        }

        return .visitChildren
    }

    override func visitPost(_ node: ExtensionDeclSyntax) {
        popType()
    }

    override func visit(_ node: VariableDeclSyntax) -> SyntaxVisitorContinueKind {
        guard let context = contextStack.last else { return .skipChildren }
        let access = accessInfo(for: node.modifiers)
        // Protocol properties inherit access from the protocol, so don't check access modifiers
        guard context.isProtocol || access.isPublicOrOpen else { return .skipChildren }
        guard node.bindingKeyword.tokenKind == .keyword(.var) else { return .skipChildren }
        guard node.bindings.count == 1, let binding = node.bindings.first else { return .skipChildren }
        guard let identifierPattern = binding.pattern.as(IdentifierPatternSyntax.self) else {
            diagnostics.append(.init(kind: .unsupported("Unsupported property pattern"),
                                     line: node.startLine(converter: converter)))
            return .skipChildren
        }
        guard let typeAnnotation = binding.typeAnnotation else {
            diagnostics.append(.init(kind: .parseError("Property '\(identifierPattern.identifier.text)' must have an explicit type"),
                                     line: node.startLine(converter: converter)))
            return .skipChildren
        }
        guard var propertyType = parseTypeName(typeAnnotation.type,
                                               line: node.startLine(converter: converter)) else {
            return .skipChildren
        }

        if propertyType.path.count == 1,
           let receiverName = context.typeName.path.last,
           propertyType.path[0] == receiverName,
           context.typeName.path.count > 1 {
            propertyType = TypeName(path: context.typeName.path,
                                    genericArguments: propertyType.genericArguments,
                                    genericOwnerIndex: context.typeName.genericOwnerIndex)
        }

        let (availability, otherAttributes) = partitionAttributes(node.attributes)
        let allAvailability = context.inheritedAvailability + availability

        let accessorInfo = accessors(from: binding.accessorBlock)
        guard accessorInfo.hasGetter else {
            diagnostics.append(.init(kind: .unsupported("Property '\(identifierPattern.identifier.text)' missing getter"),
                                     line: node.startLine(converter: converter)))
            return .skipChildren
        }

        let isStatic = (node.modifiers.contains(where: { modifier in
            let token = modifier.name.tokenKind
            switch token {
            case .keyword(.static), .keyword(.class): return true
            default: return false
            }
        }))

        properties.append(HostSurfaceProperty(type: context.typeName,
                                              name: identifierPattern.identifier.text,
                                              propertyType: propertyType,
                                              isStatic: isStatic,
                                              isSettable: accessorInfo.hasSetter,
                                              availability: allAvailability,
                                              attributes: otherAttributes,
                                              sourceLine: node.startLine(converter: converter)))
        return .skipChildren
    }

    override func visit(_ node: InitializerDeclSyntax) -> SyntaxVisitorContinueKind {
        guard let context = contextStack.last else { return .skipChildren }
        let access = accessInfo(for: node.modifiers)
        guard access.isPublicOrOpen else { return .skipChildren }

        let (availability, otherAttributes) = partitionAttributes(node.attributes)
        let parametersResult = convertParameters(node.signature.parameterClause,
                                                 line: node.startLine(converter: converter))

        guard let params = parametersResult.parameters else {
            if let message = parametersResult.diagnostic {
                diagnostics.append(.init(kind: .parseError(message), line: node.startLine(converter: converter)))
            }
            return .skipChildren
        }

        let baseName = initializerBaseName(parameters: params)
        let selector = makeSelector(baseName: "init", params: params)
        let genericParams = node.genericParameterClause?.parameters.map {
            GenericParameter(name: $0.name.text, origin: .method)
        } ?? []
        let whereClause = node.genericWhereClause?.description.trimmingCharacters(in: .whitespacesAndNewlines)
        let isFailable = node.optionalMark != nil
        let isAsync = node.signature.effectSpecifiers?.asyncSpecifier != nil
        let throwsKind = throwsKind(from: node.signature.effectSpecifiers?.throwsSpecifier)
        let line = node.startLine(converter: converter)

        let combinedAvailability = context.inheritedAvailability + availability

        // Combine extension-level and method-level where clauses
        let combinedWhereClause: String?
        if let extensionClause = context.whereClause, let methodClause = whereClause {
            combinedWhereClause = extensionClause + ", " + methodClause
        } else {
            combinedWhereClause = context.whereClause ?? whereClause
        }

        methods.append(HostSurfaceMethod(
            type: context.typeName,
            kind: .initializer(isFailable: isFailable),
            baseName: baseName,
            selector: selector,
            returnType: context.typeName,
            parameters: params,
            genericParameters: genericParams,
            whereClause: combinedWhereClause?.isEmpty == true ? nil : combinedWhereClause,
            availability: combinedAvailability,
            attributes: otherAttributes,
            throwsKind: throwsKind,
            isAsync: isAsync,
            sourceLine: line
        ))

        return .skipChildren
    }

    override func visit(_ node: FunctionDeclSyntax) -> SyntaxVisitorContinueKind {
        guard let context = contextStack.last else { return .skipChildren }
        let access = accessInfo(for: node.modifiers)
        // Protocol methods inherit access from the protocol, so don't check access modifiers
        guard context.isProtocol || access.isPublicOrOpen else { return .skipChildren }

        let (availability, otherAttributes) = partitionAttributes(node.attributes)
        let parametersResult = convertParameters(node.signature.parameterClause,
                                                 line: node.startLine(converter: converter))

        guard let params = parametersResult.parameters else {
            if let message = parametersResult.diagnostic {
                diagnostics.append(.init(kind: .parseError(message), line: node.startLine(converter: converter)))
            }
            return .skipChildren
        }

        let baseName = node.name.text.trimmingCharacters(in: .whitespacesAndNewlines)
        if baseName.isEmpty { return .skipChildren }

        let selector = makeSelector(baseName: baseName, params: params)
        let returnType: TypeName?
        if let typeSyntax = node.signature.returnClause?.type {
            returnType = parseTypeName(typeSyntax, line: node.startLine(converter: converter))
            if returnType == nil {
                diagnostics.append(.init(kind: .parseError("Failed to parse return type for \(baseName)"),
                                         line: node.startLine(converter: converter)))
                return .skipChildren
            }
        } else {
            returnType = nil
        }

        let genericParams = node.genericParameterClause?.parameters.map {
            GenericParameter(name: $0.name.text, origin: .method)
        } ?? []
        let whereClause = node.genericWhereClause?.description.trimmingCharacters(in: .whitespacesAndNewlines)
        let isStatic = access.isStatic
        let isMutating = access.isMutating
        let isAsync = node.signature.effectSpecifiers?.asyncSpecifier != nil
        let throwsKind = throwsKind(from: node.signature.effectSpecifiers?.throwsSpecifier)
        let line = node.startLine(converter: converter)

        let kind: HostSurfaceMethodKind = isStatic ? .static : .instance(isMutating: isMutating)

        let combinedAvailability = context.inheritedAvailability + availability

        // Combine extension-level and method-level where clauses
        let combinedWhereClause: String?
        if let extensionClause = context.whereClause, let methodClause = whereClause {
            combinedWhereClause = extensionClause + ", " + methodClause
        } else {
            combinedWhereClause = context.whereClause ?? whereClause
        }

        methods.append(HostSurfaceMethod(
            type: context.typeName,
            kind: kind,
            baseName: baseName,
            selector: selector,
            returnType: returnType,
            parameters: params,
            genericParameters: genericParams,
            whereClause: combinedWhereClause?.isEmpty == true ? nil : combinedWhereClause,
            availability: combinedAvailability,
            attributes: otherAttributes,
            throwsKind: throwsKind,
            isAsync: isAsync,
            sourceLine: line
        ))

        return .skipChildren
    }

    override func visitPost(_ node: FunctionDeclSyntax) {
        // no-op; override required to pair visit/visitPost in some SwiftSyntax versions
    }

    // MARK: - Associated Type and TypeAlias Collection

    override func visit(_ node: AssociatedTypeDeclSyntax) -> SyntaxVisitorContinueKind {
        guard let context = contextStack.last, context.isProtocol else {
            return .skipChildren
        }

        let access = accessInfo(for: node.modifiers)
        guard access.isPublicOrOpen else { return .skipChildren }

        let associatedTypeName = node.name.text
        let protocolName = context.typeName.path.joined(separator: ".")

        // Add to protocol's associated types list
        var assocTypes = protocolAssociatedTypes[protocolName] ?? []
        if !assocTypes.contains(associatedTypeName) {
            assocTypes.append(associatedTypeName)
        }
        protocolAssociatedTypes[protocolName] = assocTypes

        return .skipChildren
    }

    override func visit(_ node: TypeAliasDeclSyntax) -> SyntaxVisitorContinueKind {
        guard let context = contextStack.last else {
            return .skipChildren
        }

        let access = accessInfo(for: node.modifiers)
        guard access.isPublicOrOpen else { return .skipChildren }

        let aliasName = node.name.text
        let targetType = node.initializer.value.trimmedDescription
        let typeName = context.typeName.path.joined(separator: ".")

        // Add to type's typealiases
        var aliases = typeAliases[typeName] ?? [:]
        aliases[aliasName] = targetType
        typeAliases[typeName] = aliases

        return .skipChildren
    }

    // MARK: - Type context helpers

    private func recordNominal(named name: String, generics: [String], access: AccessInfo) {
        guard access.isPublicOrOpen else { return }
        let parentPath = contextStack.last?.typeName.path ?? [moduleName]
        let key = (parentPath + [name]).joined(separator: ".")
        if !generics.isEmpty && typeGenericParameters[key] == nil {
            typeGenericParameters[key] = generics
        }
    }

    private func pushType(named name: String,
                          genericParameters: [String],
                          availability: [String],
                          isProtocol: Bool = false,
                          whereClause: String? = nil) {
        let parentContext = contextStack.last
        var path: [String]
        if let parent = parentContext {
            path = parent.typeName.path + [name]
        } else {
            path = [moduleName, name]
        }
        let generics = genericParameters.map { TypeName(path: [$0]) }
        if !genericParameters.isEmpty {
            let key = path.joined(separator: ".")
            if typeGenericParameters[key] == nil {
                typeGenericParameters[key] = genericParameters
            }
        }
        let inheritedAvailability = (parentContext?.inheritedAvailability ?? []) + availability
        let typeName = TypeContext(typeName: TypeName(path: path, genericArguments: generics),
                                   inheritedAvailability: inheritedAvailability,
                                   isProtocol: isProtocol,
                                   whereClause: whereClause)
        contextStack.append(typeName)
    }

    private func popType() {
        if !contextStack.isEmpty {
            contextStack.removeLast()
        }
    }

    private func recordConformances(for typeName: String, from clause: InheritanceClauseSyntax) {
        let parentPath = contextStack.last?.typeName.path ?? [moduleName]
        let fullTypeName = (parentPath + [typeName]).joined(separator: ".")

        var protocols: [String] = []
        for inheritedType in clause.inheritedTypes {
            let typeText = inheritedType.type.description.trimmingCharacters(in: .whitespaces)
            // Parse the protocol name - handle module-qualified names
            if let parsed = try? TypeNameParser.parse(typeText) {
                var protocolPath = parsed.path
                // If the protocol name is not module-qualified, add the current module
                if protocolPath.count == 1 {
                    protocolPath = [moduleName] + protocolPath
                }
                let protocolName = protocolPath.joined(separator: ".")
                protocols.append(protocolName)
            }
        }

        if !protocols.isEmpty {
            conformances[fullTypeName] = protocols
        }
    }

    private func placeholderInventory(for path: [String]) -> [String]? {
        let canonical = path.joined(separator: ".")
        if let stored = typeGenericParameters[canonical], !stored.isEmpty {
            return stored
        }
        if let direct = genericParameterInventory[canonical], !direct.isEmpty {
            return direct
        }
        if path.count > 1 {
            let suffixKey = path.suffix(from: 1).joined(separator: ".")
            if let stored = typeGenericParameters[suffixKey], !stored.isEmpty {
                return stored
            }
            if let suffix = genericParameterInventory[suffixKey], !suffix.isEmpty {
                return suffix
            }
        }
        return nil
    }

    private func recordGenericParameterConstraints(from clause: GenericParameterClauseSyntax?,
                                                   for typeName: TypeName) {
        guard let clause else { return }
        for parameter in clause.parameters {
            guard let inheritedType = parameter.inheritedType else { continue }
            let protocols = canonicalProtocolNames(from: inheritedType)
            guard !protocols.isEmpty else { continue }
            for proto in protocols {
                addConstraint(for: typeName, parameter: parameter.name.text, protocolName: proto)
            }
        }
    }

    private func recordConstraints(fromWhereClause clause: String, for typeName: TypeName) {
        let constraints = WhereClauseParser.parse(clause)
        for constraint in constraints {
            switch constraint {
            case .conformance(let typePath, let protoName):
                guard case .genericParam(let name) = typePath else { continue }
                let canonicalProto = canonicalProtocolName(protoName)
                addConstraint(for: typeName, parameter: name, protocolName: canonicalProto)
            }
        }
    }

    private func addConstraint(for typeName: TypeName, parameter: String, protocolName: String) {
        for key in canonicalTypeKeys(typeName) {
            var parameterMap = genericParameterConstraints[key] ?? [:]
            var protocols = parameterMap[parameter] ?? Set<String>()
            protocols.insert(protocolName)
            parameterMap[parameter] = protocols
            genericParameterConstraints[key] = parameterMap
        }
    }

    private func canonicalTypeKeys(_ typeName: TypeName) -> [String] {
        let canonical = typeName.canonicalDescription()
        let base = typeName.path.joined(separator: ".")
        if canonical == base {
            return [canonical]
        } else {
            return [canonical, base]
        }
    }

    private func canonicalProtocolNames(from typeSyntax: TypeSyntax) -> [String] {
        let raw = typeSyntax.description.split(separator: "&")
        var result: [String] = []
        for fragment in raw {
            let trimmed = fragment.trimmingCharacters(in: .whitespacesAndNewlines)
            guard !trimmed.isEmpty,
                  let parsed = try? TypeNameParser.parse(trimmed) else { continue }
            result.append(parsed.canonicalDescription())
        }
        return result
    }

    private func canonicalProtocolName(_ name: String) -> String {
        if name.contains(".") {
            return name
        } else {
            return "Swift." + name
        }
    }

    // MARK: - Attribute handling

    private func partitionAttributes(_ attrs: AttributeListSyntax?) -> (availability: [String], others: [String]) {
        guard let attrs else { return ([], []) }
        var availability: [String] = []
        var others: [String] = []
        for element in attrs {
            guard let attribute = element.as(AttributeSyntax.self) else { continue }
            let text = attribute.description.trimmingCharacters(in: .whitespacesAndNewlines)
            let name = attribute.attributeName.trimmedDescription
            if name == "available" {
                availability.append(text)
            } else {
                others.append(text)
            }
        }
        return (availability, others)
    }

    // MARK: - Parameter conversion

    private func convertParameters(_ clause: FunctionParameterClauseSyntax,
                                   line: Int) -> (parameters: [HostSurfaceParameter]?, diagnostic: String?) {
        var result: [HostSurfaceParameter] = []
        for (idx, param) in clause.parameters.enumerated() {
            let firstName = param.firstName.text ?? "_"
            let secondName = param.secondName?.text
            let label = firstName == "_" ? nil : firstName
            let name: String
            if let second = secondName {
                name = second
            } else if let label {
                name = label
            } else {
                name = "value\(idx)"
            }

            let rawTypeSyntax = param.type
            var typeText = trimmedDescription(of: rawTypeSyntax)
            var isInout = false
            if typeText.hasPrefix("inout ") {
                isInout = true
                typeText.removeFirst("inout".count)
                typeText = typeText.trimmingCharacters(in: .whitespacesAndNewlines)
            }

            guard let parsedType = try? TypeNameParser.parse(typeText) else {
                return (nil, "Failed to parse type for parameter \(name)")
            }

            let isVariadic = param.ellipsis != nil
            let parameter = HostSurfaceParameter(label: label,
                                                 name: name,
                                                 type: parsedType,
                                                 isInout: isInout,
                                                 isVariadic: isVariadic)
            result.append(parameter)
        }
        return (result, nil)
    }

    // MARK: - Type parsing helpers

    private func parseTypeName(_ typeSyntax: TypeSyntax, line: Int) -> TypeName? {
        let raw = trimmedDescription(of: typeSyntax)
        do {
            return try TypeNameParser.parse(raw)
        } catch {
            diagnostics.append(.init(kind: .parseError("\(error)"), line: line))
            return nil
        }
    }

    private func initializerBaseName(parameters: [HostSurfaceParameter]) -> String {
        guard let first = parameters.first else { return "init" }
        return first.label ?? first.name
    }

    private func makeSelector(baseName: String, params: [HostSurfaceParameter]) -> String {
        guard !params.isEmpty else { return "\(baseName)()" }
        let pieces = params.map { param -> String in
            let label = param.label ?? "_"
            return "\(label):"
        }
        return "\(baseName)(\(pieces.joined()))"
    }

    private func throwsKind<T: SyntaxProtocol>(from clause: T?) -> TypeName.FunctionSignature.ThrowsKind {
        guard let clause else { return .none }
        let text = clause.description.trimmingCharacters(in: .whitespacesAndNewlines)
        if text == "rethrows" { return .rethrows }
        return .throws
    }
}

// MARK: - Support types & helpers

private struct AccessInfo {
    var isPublicOrOpen: Bool = false
    var isStatic: Bool = false
    var isMutating: Bool = false
}

private func accessInfo(for modifiers: DeclModifierListSyntax?) -> AccessInfo {
    guard let modifiers else { return AccessInfo() }
    var info = AccessInfo()
    for modifier in modifiers {
        let name = modifier.name.text
        switch name {
        case "public", "open":
            info.isPublicOrOpen = true
        case "static", "class":
            info.isStatic = true
        case "mutating":
            info.isMutating = true
        default:
            break
        }
    }
    return info
}

private func accessors(from block: AccessorBlockSyntax?) -> (hasGetter: Bool, hasSetter: Bool) {
    guard let block else { return (true, true) }
    switch block.accessors {
    case .getter:
        return (true, false)
    case .accessors(let list):
        var hasGetter = false
        var hasSetter = false
        for accessor in list {
            switch accessor.accessorSpecifier.tokenKind {
            case .keyword(.get): hasGetter = true
            case .keyword(.set): hasSetter = true
            default: break
            }
        }
        return (hasGetter, hasSetter)
    }
}

private extension SyntaxProtocol {
    func startLine(converter: SourceLocationConverter) -> Int {
        let pos = positionAfterSkippingLeadingTrivia
        let location = converter.location(for: pos)
        return location.line ?? 0
    }
}

private extension TypeSyntax {
    var trimmedDescription: String {
        description.trimmingCharacters(in: .whitespacesAndNewlines)
    }
}

private func trimmedDescription(of typeSyntax: TypeSyntax) -> String {
    typeSyntax.trimmedDescription
}

// Naive parser for a simple Self equality requirement in an extension where-clause.
// Supports forms like: "where Self == Some.Type<Name> , ..." and returns the RHS type text.
private func parseSelfEquality(from whereClause: String) -> String? {
    // Normalize whitespace for simpler scanning
    var text = whereClause
        .replacingOccurrences(of: "\n", with: " ")
        .replacingOccurrences(of: "\t", with: " ")
    if text.hasPrefix("where") {
        text.removeFirst("where".count)
    }
    text = text.trimmingCharacters(in: .whitespacesAndNewlines)
    // Find "Self" followed by "==" with no member access between them.
    // We only want to match constraints of the form: `Self == Some.Type<...>`
    // and explicitly ignore cases like `Self.FormatInput == T`.
    guard let selfRange = text.range(of: "Self") else { return nil }
    // Ensure that the next non-whitespace token after "Self" is '=' (i.e. part of '==').
    let tail = text[selfRange.upperBound...]
    // Skip whitespace
    let nonWsIndex = tail.firstIndex(where: { !$0.isWhitespace })
    guard let idx = nonWsIndex else { return nil }
    let nextChar = tail[idx]
    // If there's a dot/member access or any identifier before '==', bail.
    if nextChar == "." { return nil }
    // Confirm we have '==' following (possibly with whitespace) immediately after Self
    guard let eqRange = text.range(of: "==", range: selfRange.upperBound..<text.endIndex) else { return nil }
    // Also ensure there were no non-whitespace characters between Self and '=='
    let between = text[selfRange.upperBound..<eqRange.lowerBound].trimmingCharacters(in: .whitespacesAndNewlines)
    guard between.isEmpty else { return nil }
    let rhsStart = eqRange.upperBound
    let rhsTail = text[rhsStart...]
    // Split at first comma to isolate this requirement
    let rhsFirst = rhsTail.split(separator: ",", maxSplits: 1, omittingEmptySubsequences: false).first ?? Substring(rhsTail)
    let candidate = rhsFirst.trimmingCharacters(in: .whitespacesAndNewlines)
    return candidate.isEmpty ? nil : candidate
}
