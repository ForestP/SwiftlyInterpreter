//
//  GenericSpecializationPlanner.swift
//  HostCodegenCore
//
//  Created by Forest Plasencia on 9/19/25.
//

import HostSurfaceKit
import Foundation

/// Describes a generic requirement we can evaluate in the specialization planner.
enum GenericRequirement: Equatable {
    /// `Param == Type`
    case sameType(param: String, TypeName)
    /// `ParamLHS == ParamRHS`
    case sameTypeParameters(lhs: String, rhs: String)
    /// `Param: Protocol`
    case conformsTo(param: String, protocolName: String)
    /// Arbitrary same-type requirement between resolvable type expressions (e.g., `Self.Element == Swift.String`).
    case sameTypeResolved(lhs: TypeName, rhs: TypeName)
    /// Arbitrary conformance requirement for resolvable type expressions (e.g., `Self.Element: Swift.Sequence`).
    case conformsResolved(type: TypeName, protocolName: String)
}

/// Errors emitted by ``GenericSpecializationPlanner``.
enum GenericSpecializationPlannerError: Error, CustomStringConvertible, Equatable {
    case missingSpecializations(parameter: String)
    case requirementReferencesUnknownParameter(parameter: String)
    case noValidSpecializations

    var description: String {
        switch self {
        case .missingSpecializations(let parameter):
            return "No specializations configured for generic parameter '\(parameter)'"
        case .requirementReferencesUnknownParameter(let parameter):
            return "Requirement references unknown generic parameter '\(parameter)'"
        case .noValidSpecializations:
            return "No specialization candidates satisfy the generic constraints"
        }
    }
}

/// Represents one concrete binding for a generic declaration.
struct GenericSpecialization: Equatable {
    /// Ordered `(parameterName, Type)` pairs. Order matches the declaration's parameter list.
    let arguments: [(name: String, type: TypeName)]

    /// Convenience lookup by parameter name.
    var argumentsByName: [String: TypeName] {
        Dictionary(uniqueKeysWithValues: arguments.map { ($0.name, $0.type) })
    }

    /// Deterministic key used for sorting and comparisons in tests.
    var canonicalKey: String {
        arguments
            .sorted { $0.name < $1.name }
            .map { "\($0.name)=\($0.type.canonicalDescription())" }
            .joined(separator: "|")
    }

    static func == (lhs: GenericSpecialization, rhs: GenericSpecialization) -> Bool {
        guard lhs.arguments.count == rhs.arguments.count else { return false }
        for (l, r) in zip(lhs.arguments, rhs.arguments) {
            if l.name != r.name { return false }
            if l.type != r.type { return false }
        }
        return true
    }
}

/// Computes cartesian products of generic parameter specializations and filters
/// them using the supplied requirements and conformance table.
struct GenericSpecializationPlanner {
    let parameters: [GenericParameter]
    let requirements: [GenericRequirement]
    let conformanceTable: [String: Set<String>]
    let placeholderMap: [String: [String]]
    let receiverTemplate: TypeName

    init(parameters: [GenericParameter],
         requirements: [GenericRequirement] = [],
         conformanceTable: [String: Set<String>] = [:],
         placeholderMap: [String: [String]] = [:],
         receiverTemplate: TypeName = TypeName(path: ["Swift", "Any"])) {
        self.parameters = parameters
        self.requirements = requirements
        self.conformanceTable = conformanceTable
        self.placeholderMap = placeholderMap
        self.receiverTemplate = receiverTemplate
    }

    func planSpecializations(from allowed: [String: [TypeName]]) throws -> [GenericSpecialization] {
        let names = parameters.map(\.name)
        try validateInputs(parameterNames: names, allowed: allowed)
        let candidates = cartesianProduct(parameters: parameters, allowed: allowed)
        let filtered = candidates.filter { satisfiesRequirements($0) }
        guard !filtered.isEmpty else { throw GenericSpecializationPlannerError.noValidSpecializations }
        return filtered.sorted { $0.canonicalKey < $1.canonicalKey }
    }

    private func validateInputs(parameterNames: [String],
                                allowed: [String: [TypeName]]) throws {
        for name in parameterNames {
            guard let options = allowed[name], !options.isEmpty else {
                throw GenericSpecializationPlannerError.missingSpecializations(parameter: name)
            }
        }
        let paramSet = Set(parameterNames)
        for requirement in requirements {
            switch requirement {
            case .sameType(let param, _), .conformsTo(let param, _):
                if !paramSet.contains(param) {
                    throw GenericSpecializationPlannerError.requirementReferencesUnknownParameter(parameter: param)
                }
            case .sameTypeParameters(let lhs, let rhs):
                if !paramSet.contains(lhs) {
                    throw GenericSpecializationPlannerError.requirementReferencesUnknownParameter(parameter: lhs)
                }
                if !paramSet.contains(rhs) {
                    throw GenericSpecializationPlannerError.requirementReferencesUnknownParameter(parameter: rhs)
                }
            case .sameTypeResolved(let lhsType, let rhsType):
                if let missing = missingGenericParameter(in: lhsType, known: paramSet) {
                    throw GenericSpecializationPlannerError.requirementReferencesUnknownParameter(parameter: missing)
                }
                if let missing = missingGenericParameter(in: rhsType, known: paramSet) {
                    throw GenericSpecializationPlannerError.requirementReferencesUnknownParameter(parameter: missing)
                }
            case .conformsResolved(let type, _):
                if let missing = missingGenericParameter(in: type, known: paramSet) {
                    throw GenericSpecializationPlannerError.requirementReferencesUnknownParameter(parameter: missing)
                }
            }
        }
    }

    private func missingGenericParameter(in type: TypeName,
                                         known: Set<String>) -> String? {
        guard let root = type.path.first else { return nil }
        if root == "Self" { return nil }
        if type.path.count == 1 && !known.contains(root) {
            return root
        }
        for argument in type.genericArguments {
            if let missing = missingGenericParameter(in: argument, known: known) {
                return missing
            }
        }
        return nil
    }

    private func cartesianProduct(parameters: [GenericParameter],
                                  allowed: [String: [TypeName]]) -> [GenericSpecialization] {
        guard let first = parameters.first else { return [GenericSpecialization(arguments: [])] }
        let tail = Array(parameters.dropFirst())
        let headOptions = allowed[first.name] ?? []
        var results: [GenericSpecialization] = []
        for option in headOptions {
            let tailProducts = cartesianProduct(parameters: tail, allowed: allowed)
            for specialization in tailProducts {
                var args = specialization.arguments
                args.insert((name: first.name, type: option), at: 0)
                results.append(GenericSpecialization(arguments: args))
            }
        }
        return results
    }

    private func satisfiesRequirements(_ specialization: GenericSpecialization) -> Bool {
        let mapping = specialization.argumentsByName
        let receiver = receiverSubstitution(using: mapping)
        var substitutions = mapping
        substitutions["Self"] = receiver
        for requirement in requirements {
            switch requirement {
            case .sameType(let param, let expectedType):
                guard let actual = mapping[param] else { return false }
                if actual.canonicalDescription() != expectedType.canonicalDescription() { return false }
            case .sameTypeParameters(let lhs, let rhs):
                guard let left = mapping[lhs], let right = mapping[rhs] else { return false }
                if left.canonicalDescription() != right.canonicalDescription() { return false }
            case .conformsTo(let param, let protocolName):
                // Ignore negated layout constraints (e.g. `~Copyable`, `~Escapable`).
                if protocolName.hasPrefix("~") { continue }
                guard let actual = mapping[param] else { return false }
                let canonicalType = actual.canonicalDescription()
                guard let protocols = conformanceTable[canonicalType], protocols.contains(protocolName) else {
                    return false
                }
            case .sameTypeResolved(let lhs, let rhs):
                guard let left = resolveOperand(lhs,
                                                substitutions: substitutions),
                      let right = resolveOperand(rhs,
                                                 substitutions: substitutions) else {
                    return false
                }
                if left.canonicalDescription() != right.canonicalDescription() { return false }
            case .conformsResolved(let operand, let protocolName):
                guard let resolved = resolveOperand(operand,
                                                    substitutions: substitutions) else {
                    return false
                }
                if !checkConformance(of: resolved, to: protocolName) {
                    return false
                }
            }
        }
        return true
    }

    private func receiverSubstitution(using mapping: [String: TypeName]) -> TypeName {
        let substituted = receiverTemplate.applyingSubstitutions(mapping)
        return substituted.resolvingPlaceholders(using: placeholderMap,
                                                 substitutions: mapping)
    }

    private func resolveOperand(_ operand: TypeName,
                                substitutions: [String: TypeName]) -> TypeName? {
        if operand.path.isEmpty {
            if operand.genericArguments.isEmpty { return operand }
            let resolvedGenerics = operand.genericArguments.compactMap {
                resolveOperand($0, substitutions: substitutions)
            }
            return TypeName(path: operand.path,
                            genericArguments: resolvedGenerics,
                            genericOwnerIndex: operand.genericOwnerIndex,
                            functionSignature: operand.functionSignature)
        }

        let root = operand.path[0]
        if let substitution = substitutions[root] {
            if operand.path.count == 1 {
                return substitution.resolvingPlaceholders(using: placeholderMap,
                                                          substitutions: substitutions)
            }
            let resolved = resolveAssociatedPath(on: substitution,
                                                 path: Array(operand.path.dropFirst()),
                                                 substitutions: substitutions)
            return resolved?.resolvingPlaceholders(using: placeholderMap,
                                                   substitutions: substitutions)
        }

        if operand.genericArguments.isEmpty {
            return operand
        }

        let resolvedArguments = operand.genericArguments.compactMap {
            resolveOperand($0, substitutions: substitutions)
        }
        return TypeName(path: operand.path,
                        genericArguments: resolvedArguments,
                        genericOwnerIndex: operand.genericOwnerIndex,
                        functionSignature: operand.functionSignature)
    }

    private func resolveAssociatedPath(on base: TypeName,
                                       path: [String],
                                       substitutions: [String: TypeName]) -> TypeName? {
        guard !path.isEmpty else {
            return base.resolvingPlaceholders(using: placeholderMap,
                                              substitutions: substitutions)
        }
        var current = base.resolvingPlaceholders(using: placeholderMap,
                                                 substitutions: substitutions)
        for component in path {
            guard let next = resolveAssociatedComponent(component, of: current,
                                                        substitutions: substitutions) else {
                return nil
            }
            current = next.resolvingPlaceholders(using: placeholderMap,
                                                 substitutions: substitutions)
        }
        return current
    }

    private func resolveAssociatedComponent(_ name: String,
                                            of type: TypeName,
                                            substitutions: [String: TypeName]) -> TypeName? {
        let key = basePlaceholderKey(for: type)
        guard let placeholders = placeholderMap[key],
              let index = placeholders.firstIndex(of: name),
              index < type.genericArguments.count else {
            return nil
        }
        return type.genericArguments[index]
    }

    private func basePlaceholderKey(for type: TypeName) -> String {
        let canonical = type.canonicalDescription()
        if let idx = canonical.firstIndex(of: "<") {
            return String(canonical[..<idx])
        }
        return canonical
    }

    private func checkConformance(of type: TypeName, to protocolName: String) -> Bool {
        let canonicalType = type.canonicalDescription()
        if let conformances = conformanceTable[canonicalType],
           hasConformance(conformances, matching: protocolName) {
            return true
        }
        let baseName: String
        if let idx = canonicalType.firstIndex(of: "<") {
            baseName = String(canonicalType[..<idx])
        } else {
            baseName = canonicalType
        }
        if baseName != canonicalType,
           let conformances = conformanceTable[baseName],
           hasConformance(conformances, matching: protocolName) {
            return true
        }
        return false
    }

    private func hasConformance(_ conformances: Set<String>, matching protocolName: String) -> Bool {
        if conformances.contains(protocolName) { return true }
        if !protocolName.contains(".") {
            return conformances.contains(where: { $0.hasSuffix(".\(protocolName)") })
        }
        if let dotIndex = protocolName.firstIndex(of: ".") {
            let unqualified = String(protocolName[protocolName.index(after: dotIndex)...])
            if conformances.contains(unqualified) { return true }
            return conformances.contains(where: { $0.hasSuffix(".\(unqualified)") })
        }
        return false
    }
}
