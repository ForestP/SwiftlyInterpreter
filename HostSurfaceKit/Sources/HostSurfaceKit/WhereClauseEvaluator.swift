//
//  WhereClauseEvaluator.swift
//  HostSurfaceKit
//
//  Evaluates where-clause constraints to determine if protocol methods
//  should be flattened into concrete types.
//
//  Current implementation: Option 2 (direct params + associated types)
//  Future extensibility: Designed to support Option 3 (full where-clause parsing)
//

import Foundation

// MARK: - Constraint Model

/// Represents a type path that can appear in a where clause
enum TypePath: Equatable {
    case genericParam(String)           // Element, K, V
    case associatedType([String])       // Self.Element, Self.Index
    // Future: case concrete(TypeName) for type equality constraints

    var description: String {
        switch self {
        case .genericParam(let name):
            return name
        case .associatedType(let path):
            return path.joined(separator: ".")
        }
    }
}

/// Represents a constraint in a where clause
enum WhereConstraint: Equatable {
    case conformance(type: TypePath, protocol: String)
    // Future: case equality(lhs: TypePath, rhs: TypePath)
    // Future: case combined([WhereConstraint])

    var description: String {
        switch self {
        case .conformance(let type, let proto):
            return "\(type.description): \(proto)"
        }
    }
}

// MARK: - Where Clause Parser

struct WhereClauseParser {
    /// Parse a where clause into structured constraints
    /// Currently handles:
    /// - "Element: Comparable"
    /// - "K: Hashable"
    /// - "Self.Element: Equatable"
    /// - "Self.Index: Comparable"
    static func parse(_ whereClause: String) -> [WhereConstraint] {
        var constraints: [WhereConstraint] = []

        // Normalize whitespace
        let normalized = whereClause
            .replacingOccurrences(of: "\n", with: " ")
            .replacingOccurrences(of: "\t", with: " ")
            .trimmingCharacters(in: .whitespaces)

        // Remove leading "where" if present
        var text = normalized
        if text.hasPrefix("where ") {
            text = String(text.dropFirst(6))
        }

        // Split by comma to get individual constraints
        let parts = text.components(separatedBy: ",")

        for part in parts {
            let trimmed = part.trimmingCharacters(in: .whitespaces)
            if let constraint = parseConformanceConstraint(trimmed) {
                constraints.append(constraint)
            }
        }

        return constraints
    }

    private static func parseConformanceConstraint(_ text: String) -> WhereConstraint? {
        // Look for pattern: <type>: <protocol>
        guard let colonIndex = text.firstIndex(of: ":") else {
            return nil
        }

        let typePart = text[..<colonIndex].trimmingCharacters(in: .whitespaces)
        let protocolPart = text[text.index(after: colonIndex)...].trimmingCharacters(in: .whitespaces)

        // Parse the type path
        let typePath: TypePath
        if typePart.hasPrefix("Self.") {
            // Associated type: Self.Element, Self.Index, etc.
            let path = typePart.components(separatedBy: ".")
            typePath = .associatedType(path)
        } else {
            // Direct generic parameter: Element, K, V, etc.
            typePath = .genericParam(typePart)
        }

        // For now, take the first protocol if there's a & combination
        // Future: Handle combined protocols properly
        let firstProtocol = protocolPart.components(separatedBy: "&")
            .first?
            .trimmingCharacters(in: .whitespaces) ?? protocolPart

        return .conformance(type: typePath, protocol: firstProtocol)
    }
}

// MARK: - Associated Type Database

/// Maps concrete types to their associated type definitions
/// This is built from parsed Swift interface files rather than being hardcoded
struct AssociatedTypeDatabase {
    private var associations: [String: [String: String]] = [:]
    private let typeGenericParameters: [String: [String]]

    init(typeAliases: [String: [String: String]], typeGenericParameters: [String: [String]]) {
        self.typeGenericParameters = typeGenericParameters

        // Build associations from parsed typealiases
        for (typeName, aliases) in typeAliases {
            associations[typeName] = aliases
        }

        // Infer implicit associations from generic parameter names
        inferImplicitAssociations()
    }

    /// Infer associations when generic parameter names match associated type names
    /// For example, Array<Element> implicitly defines Element associated type
    private mutating func inferImplicitAssociations() {
        for (typeName, genericParams) in typeGenericParameters {
            var existing = associations[typeName] ?? [:]

            for (index, paramName) in genericParams.enumerated() {
                // If there's no explicit typealias for this param name, add an implicit one
                if existing[paramName] == nil {
                    existing[paramName] = "genericParam:\(index)"
                }
            }

            if !existing.isEmpty {
                associations[typeName] = existing
            }
        }
    }

    /// Resolve an associated type for a concrete type
    /// Returns the resolved type name, or nil if it cannot be resolved
    func resolve(associatedType: String, for concreteType: TypeName) -> TypeName? {
        let baseTypeName = concreteType.path.joined(separator: ".")

        guard let typeAssociations = associations[baseTypeName] else {
            return nil
        }

        guard let resolution = typeAssociations[associatedType] else {
            return nil
        }

        // Handle different resolution types
        if resolution.hasPrefix("genericParam:") {
            // Extract the generic parameter at the specified index
            let indexStr = resolution.dropFirst("genericParam:".count)
            guard let index = Int(indexStr),
                  index < concreteType.genericArguments.count else {
                return nil
            }
            return concreteType.genericArguments[index]
        } else if resolution.hasPrefix("(") && resolution.hasSuffix(")") {
            // Tuple type - these don't conform to protocols in Swift
            // Return a special marker that will fail conformance checks
            return nil
        } else {
            // Concrete type or complex type - parse it
            return try? TypeNameParser.parse(resolution)
        }
    }
}

// MARK: - Constraint Evaluator

struct WhereClauseEvaluator {
    private let conformances: [String: [String]]
    private var associatedTypeDB: AssociatedTypeDatabase
    private let typeGenericParameters: [String: [String]]
    private let typeParameterConstraints: [String: [String: Set<String>]]

    init(conformances: [String: [String]],
         typeAliases: [String: [String: String]],
         typeGenericParameters: [String: [String]],
         typeParameterConstraints: [String: [String: Set<String>]]) {
        self.conformances = conformances
        self.typeGenericParameters = typeGenericParameters
        self.typeParameterConstraints = typeParameterConstraints
        self.associatedTypeDB = AssociatedTypeDatabase(typeAliases: typeAliases,
                                                       typeGenericParameters: typeGenericParameters)
    }

    /// Evaluate if all constraints in a where clause are satisfied for a concrete type
    /// - Parameters:
    ///   - whereClause: The where clause to evaluate
    ///   - concreteType: The concrete type to check constraints against
    ///   - methodGenericParameters: Generic parameters defined on the method itself (not on the type)
    ///                             Constraints on these are always considered satisfied
    /// - Returns: true if all type-level constraints are satisfied
    func evaluate(whereClause: String, for concreteType: TypeName, methodGenericParameters: [GenericParameter] = []) -> Bool {
        let constraints = WhereClauseParser.parse(whereClause)
        let methodGenericNames = Set(methodGenericParameters.filter { $0.origin == .method }.map { $0.name })

        // All constraints must be satisfied (AND logic)
        for constraint in constraints {
            // Skip constraints on method-level generic parameters
            // These are always considered satisfied because they're resolved at call site
            if constraintReferencesMethodGeneric(constraint, methodGenerics: methodGenericNames) {
                continue
            }

            if !evaluateConstraint(constraint, for: concreteType) {
                return false
            }
        }

        return true
    }

    private func constraintReferencesMethodGeneric(_ constraint: WhereConstraint, methodGenerics: Set<String>) -> Bool {
        switch constraint {
        case .conformance(let typePath, _):
            switch typePath {
            case .genericParam(let name):
                return methodGenerics.contains(name)
            case .associatedType:
                return false
            }
        }
    }

    private func evaluateConstraint(_ constraint: WhereConstraint, for concreteType: TypeName) -> Bool {
        switch constraint {
        case .conformance(let typePath, let protocolName):
            return evaluateConformance(typePath: typePath, protocol: protocolName, for: concreteType)
        }
    }

    private func evaluateConformance(typePath: TypePath, protocol protocolName: String, for concreteType: TypeName) -> Bool {
        // Step 1: Resolve the type path to a concrete TypeName
        guard let resolvedType = resolveTypePath(typePath, for: concreteType) else {
            return false
        }

        // Step 2: Normalize protocol name (add Swift. prefix if needed)
        let normalizedProtocol: String
        if protocolName.contains(".") {
            normalizedProtocol = protocolName
        } else {
            normalizedProtocol = "Swift." + protocolName
        }

        // Step 2.5: If the resolved type is a placeholder, consult recorded constraints
        if let placeholder = placeholderName(from: resolvedType),
           placeholderSatisfiesConstraint(placeholder,
                                          protocolName: normalizedProtocol,
                                          for: concreteType) {
            return true
        }

        // Step 3: Check if the resolved type conforms to the protocol
        return checkConformance(type: resolvedType, protocol: normalizedProtocol)
    }

    private func placeholderName(from type: TypeName) -> String? {
        guard type.genericArguments.isEmpty,
              type.functionSignature == nil,
              type.path.count == 1 else { return nil }
        return type.path[0]
    }

    private func placeholderSatisfiesConstraint(_ placeholder: String,
                                               protocolName: String,
                                               for concreteType: TypeName) -> Bool {
        for key in canonicalLookupKeys(for: concreteType) {
            if typeParameterConstraints[key]?[placeholder]?.contains(protocolName) == true {
                return true
            }
        }
        return false
    }

    private func resolveTypePath(_ typePath: TypePath, for concreteType: TypeName) -> TypeName? {
        switch typePath {
        case .genericParam(let name):
            if name == "Self" {
                return concreteType
            }
            // First check if there's a typealias for this parameter
            if let resolved = associatedTypeDB.resolve(associatedType: name, for: concreteType) {
                return resolved
            }

            // Fall back to generic parameter index
            let index = genericParamIndex(name, for: concreteType)
            guard index < concreteType.genericArguments.count else {
                return nil
            }
            return concreteType.genericArguments[index]

        case .associatedType(let path):
            // Handle Self.Element, Self.Index, etc.
            guard path.count >= 2, path[0] == "Self" else {
                return nil
            }

            let associatedTypeName = path[1]
            // Future: Handle nested paths like Self.SubSequence.Element

            return associatedTypeDB.resolve(associatedType: associatedTypeName, for: concreteType)
        }
    }

    private func genericParamIndex(_ name: String, for concreteType: TypeName) -> Int {
        // Look up the actual generic parameter names for this type
        let baseTypeName = concreteType.path.joined(separator: ".")

        if let params = typeGenericParameters[baseTypeName],
           let index = params.firstIndex(of: name) {
            return index
        }

        // Fallback: return 0 if we can't find the parameter
        // This shouldn't happen in normal cases
        return 0
    }

    private func checkConformance(type: TypeName, protocol protocolName: String) -> Bool {
        let typeName = type.canonicalDescription()

        // Helper to check if a type conforms to the protocol (with flexible matching)
        func hasConformance(_ typeConformances: [String]) -> Bool {
            // Exact match
            if typeConformances.contains(protocolName) {
                return true
            }

            // If protocol name doesn't have a module qualifier, check if any conformance ends with it
            // This handles cases where we're looking for "Comparable" and could match "Swift.Comparable" or "Demo.Comparable"
            if !protocolName.contains(".") {
                return typeConformances.contains { $0.hasSuffix(".\(protocolName)") }
            }

            // If the protocol we're looking for is qualified (like "Swift.Comparable"),
            // also check for matches without the module prefix
            if let dotIndex = protocolName.firstIndex(of: ".") {
                let unqualifiedProtocol = String(protocolName[protocolName.index(after: dotIndex)...])
                return typeConformances.contains { $0.hasSuffix(".\(unqualifiedProtocol)") }
            }

            return false
        }

        // Look up conformances for this type
        // Try both the full type name and the base name (without generics)
        if let typeConformances = conformances[typeName] {
            if hasConformance(typeConformances) {
                return true
            }
        }

        // Try base name without generics
        let baseTypeName: String
        if let genericStart = typeName.firstIndex(of: "<") {
            baseTypeName = String(typeName[..<genericStart])
        } else {
            baseTypeName = typeName
        }

        if baseTypeName != typeName, let typeConformances = conformances[baseTypeName] {
            if hasConformance(typeConformances) {
                return true
            }
        }

        return false
    }

    private func canonicalLookupKeys(for type: TypeName) -> [String] {
        let canonical = type.canonicalDescription()
        let base = type.path.joined(separator: ".")
        if canonical == base {
            return [canonical]
        } else {
            return [canonical, base]
        }
    }
}
