//
//  HostServiceResolver.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/19/25.
//

import Foundation

#if DEBUG
enum HostSurfaceResolverDebugMetrics {
    private static let lock = NSLock()
    nonisolated(unsafe) private static var _placeholderDropHitCount: Int = 0
    nonisolated(unsafe) private static var _placeholderDropContexts: [String] = []

    static var placeholderDropHitCount: Int {
        lock.lock()
        defer { lock.unlock() }
        return _placeholderDropHitCount
    }

    static var placeholderDropContexts: [String] {
        lock.lock()
        defer { lock.unlock() }
        return _placeholderDropContexts
    }

    static func recordPlaceholderDrop(context: String, before: Int, after: Int) {
        lock.lock()
        _placeholderDropHitCount += 1
        _placeholderDropContexts.append("\(context) (\(before) -> \(after))")
        lock.unlock()
    }

    static func reset() {
        lock.lock()
        _placeholderDropHitCount = 0
        _placeholderDropContexts = []
        lock.unlock()
    }
}
#endif

public struct ApiSurface: Equatable {
    public var types: [HostTypeSurface]
    
    public init(types: [HostTypeSurface]) {
        self.types = types
    }
}

public struct HostTypeSurface: Equatable {
    public let canonicalName: String
    public let availability: [String]
    public let members: [HostMember]
    
    public init(canonicalName: String, availability: [String], members: [HostMember]) {
        self.canonicalName = canonicalName
        self.availability = availability
        self.members = members
    }
}

public enum HostMember: Equatable {
    case initializer(ResolvedInitializer)
    case instanceMethod(ResolvedInstanceMethod)
    case staticMethod(ResolvedStaticMethod)
    case instanceProperty(ResolvedProperty)
    case staticProperty(ResolvedProperty)

    public var selector: String {
        switch self {
        case .initializer(let info):
            return info.signature.selector
        case .instanceMethod(let info):
            return info.signature.selector
        case .staticMethod(let info):
            return info.signature.selector
        case .instanceProperty(let info), .staticProperty(let info):
            return info.name
        }
    }

    var hasWhereClause: Bool {
        switch self {
        case .initializer(let info):
            return info.signature.whereClause != nil
        case .instanceMethod(let info):
            return info.signature.whereClause != nil
        case .staticMethod(let info):
            return info.signature.whereClause != nil
        case .instanceProperty, .staticProperty:
            return false
        }
    }

    var genericParameters: [GenericParameter] {
        switch self {
        case .initializer(let info):
            return info.signature.genericParameters
        case .instanceMethod(let info):
            return info.signature.genericParameters
        case .staticMethod(let info):
            return info.signature.genericParameters
        case .instanceProperty, .staticProperty:
            return []
        }
    }

    var whereClause: String? {
        switch self {
        case .initializer(let info):
            return info.signature.whereClause
        case .instanceMethod(let info):
            return info.signature.whereClause
        case .staticMethod(let info):
            return info.signature.whereClause
        case .instanceProperty, .staticProperty:
            return nil
        }
    }

    fileprivate var kindRank: Int {
        switch self {
        case .initializer: return 0
        case .instanceMethod: return 1
        case .staticMethod: return 2
        case .instanceProperty: return 3
        case .staticProperty: return 4
        }
    }

    fileprivate var sourceLine: Int {
        switch self {
        case .initializer(let info): return info.signature.sourceLine
        case .instanceMethod(let info): return info.signature.sourceLine
        case .staticMethod(let info): return info.signature.sourceLine
        case .instanceProperty(let info), .staticProperty(let info): return info.sourceLine
        }
    }
}

public struct ResolvedInitializer: Equatable {
    public let signature: ResolvedCallable
    public let isFailable: Bool
}

public struct ResolvedInstanceMethod: Equatable {
    public let signature: ResolvedCallable
    public let isMutating: Bool
    
    public init(signature: ResolvedCallable, isMutating: Bool) {
        self.signature = signature
        self.isMutating = isMutating
    }
}

public struct ResolvedStaticMethod: Equatable {
    public let signature: ResolvedCallable
}

public struct ResolvedProperty: Equatable {
    public let name: String
    public let type: TypeName
    public let isSettable: Bool
    public let availability: [String]
    public let attributes: [String]
    public let sourceLine: Int
    
    public init(name: String, type: TypeName, isSettable: Bool, availability: [String], attributes: [String], sourceLine: Int) {
        self.name = name
        self.type = type
        self.isSettable = isSettable
        self.availability = availability
        self.attributes = attributes
        self.sourceLine = sourceLine
    }
}

public struct ResolvedCallable: Equatable {
    public let throwsKind: TypeName.FunctionSignature.ThrowsKind
    public let selector: String
    public let baseName: String
    public let returnType: TypeName?
    public let parameters: [ResolvedParameter]
    public let genericParameters: [GenericParameter]
    public let whereClause: String?
    public let availability: [String]
    public let attributes: [String]
    public let isAsync: Bool
    public let sourceLine: Int

    public init(selector: String,
                baseName: String,
                returnType: TypeName?,
                parameters: [ResolvedParameter],
                genericParameters: [GenericParameter],
                whereClause: String?,
                availability: [String],
                attributes: [String],
                throwsKind: TypeName.FunctionSignature.ThrowsKind,
                isAsync: Bool,
                sourceLine: Int) {
        self.throwsKind = throwsKind
        self.selector = selector
        self.baseName = baseName
        self.returnType = returnType
        self.parameters = parameters
        self.genericParameters = genericParameters
        self.whereClause = whereClause
        self.availability = availability
        self.attributes = attributes
        self.isAsync = isAsync
        self.sourceLine = sourceLine
    }

    public var isThrowing: Bool { throwsKind != .none }
}

public struct ResolvedParameter: Equatable {
    public let label: String?
    public let name: String
    public let type: TypeName
    public let isInout: Bool
    public let isVariadic: Bool
    
    public init(label: String?, name: String, type: TypeName, isInout: Bool, isVariadic: Bool) {
        self.label = label
        self.name = name
        self.type = type
        self.isInout = isInout
        self.isVariadic = isVariadic
    }
}

public struct HostSurfaceResolveResult {
    public let apiSurface: ApiSurface
    public let diagnostics: [HostSurfaceDiagnostic]
    public let typeAliases: [ResolvedTypeAlias]
    public let collectorFallbackPlaceholderHitCount: Int
    public let resolverPlaceholderDropHitCount: Int
    public let genericParameterInventory: [String: [String]]
    public let typeGenericParameterConstraints: [String: [String: Set<String>]]

    public init(apiSurface: ApiSurface,
                diagnostics: [HostSurfaceDiagnostic],
                typeAliases: [ResolvedTypeAlias],
                collectorFallbackPlaceholderHitCount: Int,
                resolverPlaceholderDropHitCount: Int,
                genericParameterInventory: [String: [String]],
                typeGenericParameterConstraints: [String: [String: Set<String>]]) {
        self.apiSurface = apiSurface
        self.diagnostics = diagnostics
        self.typeAliases = typeAliases
        self.collectorFallbackPlaceholderHitCount = collectorFallbackPlaceholderHitCount
        self.resolverPlaceholderDropHitCount = resolverPlaceholderDropHitCount
        self.genericParameterInventory = genericParameterInventory
        self.typeGenericParameterConstraints = typeGenericParameterConstraints
    }
}

public struct ResolvedTypeAlias: Equatable {
    public let name: String
    public let qualifiedName: String
    public let canonicalTarget: String
    public let sourceLine: Int
}

struct HostSurfaceResolver {
    private var aliasResolver: TypeAliasResolver

    init(aliasResolver: TypeAliasResolver) {
        self.aliasResolver = aliasResolver
    }

    mutating func resolve(surface: HostSurfaceCollection) -> HostSurfaceResolveResult {
#if DEBUG
        HostSurfaceResolverDebugMetrics.reset()
#endif
        var diagnostics = surface.diagnostics
        var buckets: [String: TypeBucket] = [:]

        for method in surface.methods {
            guard let result = canonicalize(method: method, diagnostics: &diagnostics) else {
                continue
            }
            var bucket = buckets[result.typeKey] ?? TypeBucket()
            bucket.appendAvailability(from: result.typeAvailability)
            bucket.members.append(.init(member: result.member,
                                         sortKey: memberSortKey(for: result.member)))
            buckets[result.typeKey] = bucket
        }

        for property in surface.properties {
            guard let result = canonicalize(property: property, diagnostics: &diagnostics) else {
                continue
            }
            var bucket = buckets[result.typeKey] ?? TypeBucket()
            bucket.appendAvailability(from: result.typeAvailability)
            bucket.members.append(.init(member: result.member,
                                         sortKey: memberSortKey(for: result.member)))
            buckets[result.typeKey] = bucket
        }

        let sortedTypeNames = buckets.keys.sorted()
        var types: [HostTypeSurface] = []
        types.reserveCapacity(sortedTypeNames.count)
        for typeName in sortedTypeNames {
            guard let bucket = buckets[typeName] else { continue }
            let sortedMembers = bucket.members.sorted { $0.sortKey < $1.sortKey }.map { $0.member }
            types.append(HostTypeSurface(canonicalName: typeName,
                                         availability: bucket.availability,
                                         members: sortedMembers))
        }

        // Flatten protocol methods into concrete types using parsed conformances
        types = flattenProtocolMethods(types: types,
                                       conformances: surface.conformances,
                                       typeAliases: surface.typeAliases,
                                       typeGenericParameters: aliasResolver.genericParameterInventorySnapshot,
                                       typeParameterConstraints: surface.genericParameterConstraints)

        let aliasRecords: [ResolvedTypeAlias]
        do {
            let records = try aliasResolver.resolvedAliasRecords()
            aliasRecords = records.map { record in
                ResolvedTypeAlias(name: record.name,
                                  qualifiedName: record.qualifiedName,
                                  canonicalTarget: record.canonicalTarget,
                                  sourceLine: record.line)
            }
        } catch {
            diagnostics.append(.init(kind: .unsupported("Failed to resolve typealiases: \(error)"), line: nil))
            aliasRecords = []
        }

#if DEBUG
        let placeholderDropHits = HostSurfaceResolverDebugMetrics.placeholderDropHitCount
#else
        let placeholderDropHits = 0
#endif
        return HostSurfaceResolveResult(apiSurface: ApiSurface(types: types),
                                        diagnostics: diagnostics,
                                        typeAliases: aliasRecords,
                                        collectorFallbackPlaceholderHitCount: surface.fallbackPlaceholderHitCount,
                                        resolverPlaceholderDropHitCount: placeholderDropHits,
                                        genericParameterInventory: aliasResolver.genericParameterInventorySnapshot,
                                        typeGenericParameterConstraints: surface.genericParameterConstraints)
    }

    private struct CanonicalizationOutcome {
        let typeKey: String
        let typeAvailability: [String]
        let member: HostMember
    }

    private struct TypeBucket {
        var availability: [String] = []
        var members: [BucketMember] = []

        mutating func appendAvailability(from items: [String]) {
            for item in items {
                appendUnique(item, to: &availability)
            }
        }
    }

    private struct BucketMember {
        let member: HostMember
        let sortKey: String
    }

    private mutating func canonicalize(method: HostSurfaceMethod,
                                       diagnostics: inout [HostSurfaceDiagnostic]) -> CanonicalizationOutcome? {
        guard let canonicalReceiver = canonicalTypeName(method.type,
                                                        line: method.sourceLine,
                                                        context: "receiver",
                                                        diagnostics: &diagnostics) else {
            return nil
        }

        let typeKey = canonicalReceiver.canonicalDescription()

        let receiverModulePath = Array(canonicalReceiver.path.dropLast())
        var protectedNames = placeholderNames(for: canonicalReceiver)
        for parameter in method.genericParameters {
            protectedNames.insert(parameter.name)
        }

        var resolvedParameters: [ResolvedParameter] = []
        resolvedParameters.reserveCapacity(method.parameters.count)
        for parameter in method.parameters {
            guard var canonicalParam = canonicalTypeName(parameter.type,
                                                         line: method.sourceLine,
                                                         context: "parameter '\(parameter.name)'",
                                                         diagnostics: &diagnostics,
                                                         protectedNames: protectedNames) else {
                return nil
            }
            // Replace occurrences of 'Self' with the canonical receiver type
            canonicalParam = canonicalParam.applyingSubstitutions(["Self": canonicalReceiver])
            if canonicalParam.path.count == 1,
               let receiverName = canonicalReceiver.path.last,
               canonicalParam.path[0] == receiverName,
               !receiverModulePath.isEmpty {
                canonicalParam = TypeName(path: receiverModulePath + [receiverName],
                                         genericArguments: canonicalReceiver.genericArguments,
                                         genericOwnerIndex: canonicalReceiver.genericOwnerIndex)
            }
            resolvedParameters.append(ResolvedParameter(label: parameter.label,
                                                         name: parameter.name,
                                                         type: canonicalParam,
                                                         isInout: parameter.isInout,
                                                         isVariadic: parameter.isVariadic))
        }

        var canonicalReturn: TypeName? = nil
        if let returnType = method.returnType {
            if case .initializer(let isFailable) = method.kind {
                canonicalReturn = isFailable ? TypeName.optional(canonicalReceiver) : canonicalReceiver
            } else {
                canonicalReturn = canonicalTypeName(returnType,
                                                    line: method.sourceLine,
                                                    context: "return type",
                                                    diagnostics: &diagnostics,
                                                    protectedNames: protectedNames)
                if canonicalReturn == nil { return nil }
                if var returnValue = canonicalReturn,
                   returnValue.path.count == 1,
                   let receiverName = canonicalReceiver.path.last,
                   returnValue.path[0] == receiverName,
                   !receiverModulePath.isEmpty {
                    returnValue = TypeName(path: receiverModulePath + [receiverName],
                                           genericArguments: canonicalReceiver.genericArguments,
                                           genericOwnerIndex: canonicalReceiver.genericOwnerIndex)
                    canonicalReturn = returnValue
                }
                // Replace 'Self' in return position as well
                if let value = canonicalReturn {
                    canonicalReturn = value.applyingSubstitutions(["Self": canonicalReceiver])
                }
            }
        }

        let availability = deduplicated(method.availability)
        let attributes = deduplicated(method.attributes)
        
        let callable = ResolvedCallable(
            selector: method.selector,
            baseName: method.baseName,
            returnType: canonicalReturn,
            parameters: resolvedParameters,
            genericParameters: method.genericParameters,
            whereClause: method.whereClause,
            availability: availability,
            attributes: attributes,
            throwsKind: method.throwsKind,
            isAsync: method.isAsync,
            sourceLine: method.sourceLine
        )

        let member: HostMember
        switch method.kind {
        case .initializer(let isFailable):
            member = .initializer(ResolvedInitializer(signature: callable, isFailable: isFailable))
        case .instance(let isMutating):
            member = .instanceMethod(ResolvedInstanceMethod(signature: callable, isMutating: isMutating))
        case .static:
            member = .staticMethod(ResolvedStaticMethod(signature: callable))
        }

        return CanonicalizationOutcome(typeKey: typeKey,
                                       typeAvailability: availability,
                                       member: member)
    }

    private mutating func canonicalize(property: HostSurfaceProperty,
                                       diagnostics: inout [HostSurfaceDiagnostic]) -> CanonicalizationOutcome? {
        guard let canonicalReceiver = canonicalTypeName(property.type,
                                                        line: property.sourceLine,
                                                        context: "receiver",
                                                        diagnostics: &diagnostics) else {
            return nil
        }

        let typeKey = canonicalReceiver.canonicalDescription()
        let receiverModulePath = Array(canonicalReceiver.path.dropLast())
        let protectedNames = placeholderNames(for: canonicalReceiver)

        guard var canonicalPropertyType = canonicalTypeName(property.propertyType,
                                                            line: property.sourceLine,
                                                            context: "property '\(property.name)' type",
                                                            diagnostics: &diagnostics,
                                                            protectedNames: protectedNames) else {
            return nil
        }
        // Replace 'Self' with the canonical receiver in property types
        canonicalPropertyType = canonicalPropertyType.applyingSubstitutions(["Self": canonicalReceiver])
        if canonicalPropertyType.path.count == 1,
           let receiverName = canonicalReceiver.path.last,
           canonicalPropertyType.path[0] == receiverName,
           !receiverModulePath.isEmpty {
            canonicalPropertyType = TypeName(path: receiverModulePath + [receiverName],
                                             genericArguments: canonicalReceiver.genericArguments,
                                             genericOwnerIndex: canonicalReceiver.genericOwnerIndex)
        }

        let availability = deduplicated(property.availability)
        let attributes = deduplicated(property.attributes)

        let resolved = ResolvedProperty(
            name: property.name,
            type: canonicalPropertyType,
            isSettable: property.isSettable,
            availability: availability,
            attributes: attributes,
            sourceLine: property.sourceLine
        )
        
        let member: HostMember = property.isStatic ? .staticProperty(resolved) : .instanceProperty(resolved)

        return CanonicalizationOutcome(typeKey: typeKey,
                                       typeAvailability: availability,
                                       member: member)
    }

    private mutating func canonicalTypeName(_ type: TypeName,
                                            line: Int,
                                            context: String,
                                            diagnostics: inout [HostSurfaceDiagnostic],
                                            protectedNames: Set<String> = []) -> TypeName? {
        do {
            let canonical = try aliasResolver.canonicalName(for: type,
                                                            protectedSimpleNames: protectedNames)
            let parsed = try TypeNameParser.parse(canonical)
#if DEBUG
            recordPlaceholderDiff(original: type, canonical: parsed, context: context)
#endif
            return parsed
        } catch {
            diagnostics.append(.init(kind: .parseError("Failed to canonicalize \(context): \(error)"),
                                     line: line))
            return nil
        }
    }

#if DEBUG
    private func recordPlaceholderDiff(original: TypeName,
                                       canonical: TypeName,
                                       context: String) {
        let originalCount = original.genericArguments.count
        let canonicalCount = canonical.genericArguments.count
        if canonicalCount < originalCount {
            HostSurfaceResolverDebugMetrics.recordPlaceholderDrop(context: context,
                                                                  before: originalCount,
                                                                  after: canonicalCount)
        }
        for (originalArg, canonicalArg) in zip(original.genericArguments, canonical.genericArguments) {
            recordPlaceholderDiff(original: originalArg, canonical: canonicalArg, context: context)
        }
    }
#endif

    private func placeholderNames(for type: TypeName) -> Set<String> {
        guard let key = placeholderLookupKey(for: type) else { return [] }
        if let placeholders = aliasResolver.genericParameters(for: key) {
            return Set(placeholders)
        }
        return []
    }

    private func placeholderLookupKey(for type: TypeName) -> String? {
        if let ownerIndex = type.genericOwnerIndex ?? defaultOwnerIndex(for: type) {
            var ownerPath = Array(type.path.prefix(ownerIndex + 1))
            if ownerPath.count == 1, let canonical = PrimitiveTypeMetadata.canonicalPaths[ownerPath[0]] {
                ownerPath = canonical
            }
            guard !ownerPath.isEmpty else { return nil }
            return ownerPath.joined(separator: ".")
        }
        if type.path.count == 1, let canonical = PrimitiveTypeMetadata.canonicalPaths[type.path[0]] {
            return canonical.joined(separator: ".")
        }
        if !type.path.isEmpty {
            return type.path.joined(separator: ".")
        }
        return nil
    }

    private func defaultOwnerIndex(for type: TypeName) -> Int? {
        guard !type.genericArguments.isEmpty else { return nil }
        return max(type.path.count - 1, 0)
    }

    // Flatten protocol methods into concrete types
    // This ensures that methods defined on protocols (like Sequence.map) are available
    // when looking up methods on concrete types (like Array)
    private func flattenProtocolMethods(types: [HostTypeSurface],
                                        conformances: [String: [String]],
                                        typeAliases: [String: [String: String]],
                                        typeGenericParameters: [String: [String]],
                                        typeParameterConstraints: [String: [String: Set<String>]]) -> [HostTypeSurface] {
        // Build a map of protocol names to their members
        var protocolMembers: [String: [HostMember]] = [:]
        for type in types {
            // Check if this type is referenced as a protocol in conformances
            let isProtocol = conformances.values.contains { $0.contains(type.canonicalName) }
            if isProtocol {
                protocolMembers[type.canonicalName] = type.members
            }
        }

        // Compute transitive conformances
        // If A conforms to B, and B conforms to C, then A effectively conforms to C
        func computeTransitiveConformances(_ typeName: String, visited: inout Set<String>) -> [String] {
            guard !visited.contains(typeName) else { return [] }
            visited.insert(typeName)

            var allConformances: [String] = []
            if let direct = conformances[typeName] {
                allConformances.append(contentsOf: direct)
                // Recursively get conformances of protocols we conform to
                for proto in direct {
                    allConformances.append(contentsOf: computeTransitiveConformances(proto, visited: &visited))
                }
            }
            return allConformances
        }

        // Copy protocol methods into concrete types
        var result: [HostTypeSurface] = []
        for type in types {
            // Extract base type name (strip generic arguments)
            let baseTypeName: String
            if let genericStart = type.canonicalName.firstIndex(of: "<") {
                baseTypeName = String(type.canonicalName[..<genericStart])
            } else {
                baseTypeName = type.canonicalName
            }

            // Check if this type has conformances - try both the full name and base name
            let directProtocols = conformances[type.canonicalName] ?? conformances[baseTypeName]

            guard let directProtocols = directProtocols else {
                // No conformances - keep as-is
                result.append(type)
                continue
            }

            // Compute all protocols this type conforms to (including transitive)
            var visited = Set<String>()
            let protocols = computeTransitiveConformances(baseTypeName, visited: &visited)

            if baseTypeName == "Swift.Array" {
                print("[FLATTEN] Swift.Array conforms to: \(protocols.sorted())")
            }

            guard !protocols.isEmpty else {
                // No protocols after transitive resolution - keep as-is
                result.append(type)
                continue
            }

            // Parse the concrete type for substitution
            let concreteType = try? TypeNameParser.parse(type.canonicalName)

            // Collect protocol methods to add
            var additionalMembers: [HostMember] = []
            var existingSelectors = Set(type.members.map { $0.selector })

            for protocolName in protocols {
                guard let protocolMems = protocolMembers[protocolName] else { continue }

                if baseTypeName == "Swift.Array" && protocolName == "Swift.Sequence" {
                    print("[FLATTEN] Processing Swift.Sequence members for Swift.Array")
                    print("[FLATTEN] Total Sequence members: \(protocolMems.count)")
                }
                if baseTypeName == "Swift.Array" && protocolName.contains("RangeReplaceableCollection") {
                    print("[FLATTEN] Processing \(protocolName) members for Swift.Array")
                }

                for member in protocolMems {
                    if baseTypeName == "Swift.Array" && protocolName == "Swift.Sequence" && member.selector == "map(_:)" {
                        print("[FLATTEN] Examining Sequence.map(_:) for Array")
                        print("[FLATTEN] Has where clause: \(member.hasWhereClause)")
                        if let whereClause = member.whereClause {
                            print("[FLATTEN] Where clause: \(whereClause)")
                        }
                    }
                    if baseTypeName == "Swift.Array" && protocolName.contains("RangeReplaceableCollection") && (member.selector == "removeLast()" || member.selector == "popLast()") {
                        print("[FLATTEN] Examining \(protocolName).\(member.selector) for Swift.Array")
                        if let whereClause = member.whereClause {
                            print("[FLATTEN] Where clause: \(whereClause)")
                        }
                    }

                    // Check if member has where-clause constraints
                    if member.hasWhereClause {
                        // Evaluate if the constraints are satisfied for this concrete type
                        guard let concreteType = concreteType,
                              let whereClause = member.whereClause,
                              evaluateWhereClause(whereClause,
                                                  for: concreteType,
                                                  methodGenericParameters: member.genericParameters,
                                                  conformances: conformances,
                                                  typeAliases: typeAliases,
                                                  typeGenericParameters: typeGenericParameters,
                                                  typeParameterConstraints: typeParameterConstraints) else {
                            // Constraints not satisfied or cannot be evaluated - skip this member
                            if baseTypeName == "Swift.Array" && protocolName == "Swift.Sequence" && member.selector == "map(_:)" {
                                print("[FLATTEN] Sequence.map(_:) SKIPPED due to where-clause evaluation failure")
                            }
                            if baseTypeName == "Swift.Array" && protocolName.contains("RangeReplaceableCollection") && (member.selector == "removeLast()" || member.selector == "popLast()") {
                                print("[FLATTEN] \(protocolName).\(member.selector) skipped due to where-clause failure")
                            }
                            continue
                        }

                        if baseTypeName == "Swift.Array" && protocolName == "Swift.Sequence" && member.selector == "map(_:)" {
                            print("[FLATTEN] Sequence.map(_:) passed where-clause evaluation")
                        }
                        if baseTypeName == "Swift.Array" && protocolName.contains("RangeReplaceableCollection") && (member.selector == "removeLast()" || member.selector == "popLast()") {
                            print("[FLATTEN] \(protocolName).\(member.selector) passed where-clause evaluation")
                        }
                    }

                    // Only add if not already present
                    if !existingSelectors.contains(member.selector) {
                        if baseTypeName == "Swift.Array" && protocolName == "Swift.Sequence" && member.selector == "map(_:)" {
                            print("[FLATTEN] Adding Sequence.map(_:) to Swift.Array")
                        }
                        if baseTypeName == "Swift.Array" && protocolName.contains("RangeReplaceableCollection") && (member.selector == "removeLast()" || member.selector == "popLast()") {
                            print("[FLATTEN] Adding \(protocolName).\(member.selector) to Swift.Array")
                        }

                        // Substitute protocol types with the concrete type when copying protocol members
                        // We need to substitute ALL protocols the type conforms to, not just the immediate one
                        let substitutedMember: HostMember
                        if let concreteType = concreteType {
                            var result = substituteProtocolTypeInMember(member, protocolName: protocolName, with: concreteType)
                            // Also substitute other protocols in the conformance hierarchy
                            for otherProtocol in protocols where otherProtocol != protocolName {
                                result = substituteProtocolTypeInMember(result, protocolName: otherProtocol, with: concreteType)
                            }
                            substitutedMember = result
                        } else {
                            substitutedMember = member
                        }
                        additionalMembers.append(substitutedMember)
                        existingSelectors.insert(member.selector)
                    }
                }
            }

            // Create new type with combined members
            let combinedMembers = type.members + additionalMembers

            // Deduplicate members by selector (keep first occurrence)
            // This handles cases where protocol extensions provide default implementations
            // that duplicate protocol requirements
            var seenSelectors = Set<String>()
            let deduplicatedMembers = combinedMembers.filter { member in
                if seenSelectors.contains(member.selector) {
                    return false
                }
                seenSelectors.insert(member.selector)
                return true
            }

            result.append(HostTypeSurface(canonicalName: type.canonicalName,
                                         availability: type.availability,
                                         members: deduplicatedMembers))
        }

        return result
    }

    // Evaluate if a where clause's constraints are satisfied for a concrete type
    private func evaluateWhereClause(_ whereClause: String,
                                     for concreteType: TypeName,
                                     methodGenericParameters: [GenericParameter],
                                     conformances: [String: [String]],
                                     typeAliases: [String: [String: String]],
                                     typeGenericParameters: [String: [String]],
                                     typeParameterConstraints: [String: [String: Set<String>]]) -> Bool {
        let evaluator = WhereClauseEvaluator(conformances: conformances,
                                            typeAliases: typeAliases,
                                            typeGenericParameters: typeGenericParameters,
                                            typeParameterConstraints: typeParameterConstraints)
        return evaluator.evaluate(whereClause: whereClause,
                                 for: concreteType,
                                 methodGenericParameters: methodGenericParameters)
    }

    // Substitute protocol type with concrete type in a member
    // In protocol methods, Self is resolved to the protocol name during parsing,
    // so we need to substitute the protocol name with the concrete type
    private func substituteProtocolTypeInMember(_ member: HostMember, protocolName: String, with concreteType: TypeName) -> HostMember {
        // Parse protocol type for substitution
        guard let protocolType = try? TypeNameParser.parse(protocolName) else {
            return member
        }

        switch member {
        case .instanceMethod(let method):
            let newSignature = substituteProtocolTypeInCallable(method.signature, protocolType: protocolType, with: concreteType)
            return .instanceMethod(ResolvedInstanceMethod(signature: newSignature, isMutating: method.isMutating))

        case .staticMethod(let method):
            let newSignature = substituteProtocolTypeInCallable(method.signature, protocolType: protocolType, with: concreteType)
            return .staticMethod(ResolvedStaticMethod(signature: newSignature))

        case .initializer(let initializer):
            let newSignature = substituteProtocolTypeInCallable(initializer.signature, protocolType: protocolType, with: concreteType)
            return .initializer(ResolvedInitializer(signature: newSignature, isFailable: initializer.isFailable))

        case .instanceProperty(let property):
            let newType = substituteTypeRecursively(property.type, protocolType: protocolType, with: concreteType)
            return .instanceProperty(ResolvedProperty(
                name: property.name,
                type: newType,
                isSettable: property.isSettable,
                availability: property.availability,
                attributes: property.attributes,
                sourceLine: property.sourceLine
            ))

        case .staticProperty(let property):
            let newType = substituteTypeRecursively(property.type, protocolType: protocolType, with: concreteType)
            return .staticProperty(ResolvedProperty(
                name: property.name,
                type: newType,
                isSettable: property.isSettable,
                availability: property.availability,
                attributes: property.attributes,
                sourceLine: property.sourceLine
            ))
        }
    }

    private func substituteProtocolTypeInCallable(_ callable: ResolvedCallable, protocolType: TypeName, with concreteType: TypeName) -> ResolvedCallable {
        let newParameters = callable.parameters.map { param in
            ResolvedParameter(
                label: param.label,
                name: param.name,
                type: substituteTypeRecursively(param.type, protocolType: protocolType, with: concreteType),
                isInout: param.isInout,
                isVariadic: param.isVariadic
            )
        }

        let newReturnType = callable.returnType.map { substituteTypeRecursively($0, protocolType: protocolType, with: concreteType) }

        return ResolvedCallable(
            selector: callable.selector,
            baseName: callable.baseName,
            returnType: newReturnType,
            parameters: newParameters,
            genericParameters: callable.genericParameters,
            whereClause: callable.whereClause,
            availability: callable.availability,
            attributes: callable.attributes,
            throwsKind: callable.throwsKind,
            isAsync: callable.isAsync,
            sourceLine: callable.sourceLine
        )
    }

    // Recursively substitute protocol type with concrete type
    private func substituteTypeRecursively(_ type: TypeName, protocolType: TypeName, with concreteType: TypeName) -> TypeName {
        // Check if this type matches the protocol type exactly
        if type.path == protocolType.path && type.genericArguments.isEmpty {
            return concreteType
        }

        // Check if this type's path starts with the protocol type (e.g., Collection.Element, Demo.Collection.Element, or Swift.Collection.Element)
        // Handle both fully qualified and unqualified protocol names, and cross-module references
        var matchesProtocolPrefix = false
        var protocolPrefixLength = 0

        // First try exact match (fully qualified, same module)
        if type.path.count > protocolType.path.count &&
           Array(type.path.prefix(protocolType.path.count)) == protocolType.path {
            matchesProtocolPrefix = true
            protocolPrefixLength = protocolType.path.count
        }
        // Try matching protocol name regardless of module
        // E.g., if type is ["Swift", "Collection", "Element"] and protocol is ["Demo", "Collection"]
        // or if type is ["Collection", "Element"] and protocol is ["Demo", "Collection"]
        else if type.path.count >= 2 && protocolType.path.count >= 1 {
            let protocolName = protocolType.path.last!

            // Check if protocol name appears in the type path followed by more segments (like .Element)
            if let protocolIndex = type.path.firstIndex(of: protocolName),
               protocolIndex < type.path.count - 1 {
                // Found the protocol name with associated type after it
                matchesProtocolPrefix = true
                protocolPrefixLength = protocolIndex + 1
            }
        }

        if matchesProtocolPrefix {
            // Replace the protocol prefix with the concrete type, preserving the concrete type's generic arguments
            let remainingPath = Array(type.path.suffix(from: protocolPrefixLength))
            let newPath = concreteType.path + remainingPath

            // Preserve the concrete type's generic arguments
            let newGenericOwnerIndex = concreteType.path.count - 1

            // Recursively substitute in generic arguments if present
            if !type.genericArguments.isEmpty {
                let newGenericArgs = type.genericArguments.map { substituteTypeRecursively($0, protocolType: protocolType, with: concreteType) }
                return TypeName(path: newPath, genericArguments: newGenericArgs, genericOwnerIndex: newGenericOwnerIndex)
            }

            // Use the concrete type's generic arguments for the associated type
            return TypeName(path: newPath, genericArguments: concreteType.genericArguments, genericOwnerIndex: newGenericOwnerIndex)
        }

        // Check if this type's path starts with "Self" (e.g., Self.Index)
        // This handles associated types in protocol methods where Self is literal "Self"
        if type.path.count > 1 && type.path[0] == "Self" {
            // Replace "Self" with the concrete type, preserving the concrete type's generic arguments
            let remainingPath = Array(type.path.suffix(from: 1))
            let newPath = concreteType.path + remainingPath

            // Preserve the concrete type's generic arguments
            // The genericOwnerIndex should point to where the concrete type ends (before the associated type part)
            let newGenericOwnerIndex = concreteType.path.count - 1

            // Recursively substitute in generic arguments if present
            if !type.genericArguments.isEmpty {
                let newGenericArgs = type.genericArguments.map { substituteTypeRecursively($0, protocolType: protocolType, with: concreteType) }
                return TypeName(path: newPath, genericArguments: newGenericArgs, genericOwnerIndex: newGenericOwnerIndex)
            }

            // Use the concrete type's generic arguments for the associated type
            return TypeName(path: newPath, genericArguments: concreteType.genericArguments, genericOwnerIndex: newGenericOwnerIndex)
        }

        // Recursively substitute in generic arguments
        if !type.genericArguments.isEmpty {
            let newGenericArgs = type.genericArguments.map { substituteTypeRecursively($0, protocolType: protocolType, with: concreteType) }
            return TypeName(path: type.path, genericArguments: newGenericArgs, genericOwnerIndex: type.genericOwnerIndex)
        }

        // Recursively substitute in function signatures
        if let funcSig = type.functionSignature {
            let newParams = funcSig.parameters.map { substituteTypeRecursively($0, protocolType: protocolType, with: concreteType) }
            let newReturn = substituteTypeRecursively(funcSig.returnType, protocolType: protocolType, with: concreteType)
            let newFuncSig = TypeName.FunctionSignature(
                parameters: newParams,
                returnType: newReturn,
                isAsync: funcSig.isAsync,
                throwsKind: funcSig.throwsKind
            )
            return TypeName(path: type.path, functionSignature: newFuncSig)
        }

        return type
    }
}

private func deduplicated(_ items: [String]) -> [String] {
    var result: [String] = []
    for item in items {
        appendUnique(item, to: &result)
    }
    return result
}

private func appendUnique(_ item: String, to array: inout [String]) {
    if !array.contains(item) {
        array.append(item)
    }
}

private func memberSortKey(for member: HostMember) -> String {
    let selector = member.selector
    let rank = member.kindRank
    let line = member.sourceLine
    return String(format: "%02d|%@|%08d", rank, selector, line)
}
