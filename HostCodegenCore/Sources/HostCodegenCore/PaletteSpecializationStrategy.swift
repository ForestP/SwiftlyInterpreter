//
//  PaletteSpecializationStrategy.swift
//  HostCodegenCore
//
//  Created by Codex on 5/1/24.
//

import HostSurfaceKit

/// Auto-specialization strategy that supplies a curated list of foundational types
/// for every unconstrained generic parameter.
public struct PaletteSpecializationStrategy: AutoSpecializationStrategy {
    public enum Kind: String {
        case standard

        var identifierSuffix: String { rawValue }
    }

    public let kind: Kind
    public let identifier: String

    private let palette: [TypeName]
    private let errorPalette: [TypeName]

    public init(kind: Kind = .standard) {
        self.kind = kind
        self.identifier = "palette.\(kind.identifierSuffix)"
        self.palette = PaletteSpecializationStrategy.makePalette(kind: kind)
        self.errorPalette = [
            TypeName(path: ["Swift", "Never"])
        ]
    }

    public func methodDomains(for descriptor: HostMethodDescriptor,
                              surface: ApiSurface) -> [String: [TypeName]] {
        // Heuristic: avoid proposing palette domains for PredicateExpressions surfaces.
        // Their generic constraints are rich and frequently missing from the interface,
        // leading to invalid specializations (e.g., Wrapped: PredicateExpression).
        let receiver = descriptor.receiver.canonicalDescription()
        if receiver.hasPrefix("Foundation.PredicateExpressions.") {
            return [:]
        }
        return mapping(for: descriptor.genericParameters,
                       requirements: descriptor.requirements)
    }

    public func propertyDomains(for descriptor: HostPropertyDescriptor,
                                surface: ApiSurface) -> [String: [TypeName]] {
        let receiver = descriptor.receiver.canonicalDescription()
        if receiver.hasPrefix("Foundation.PredicateExpressions.") {
            return [:]
        }
        return mapping(for: descriptor.genericParameters,
                       requirements: descriptor.requirements)
    }

    private func mapping(for parameters: [GenericParameter],
                         requirements: [GenericRequirement]) -> [String: [TypeName]] {
        guard !parameters.isEmpty else { return [:] }
        var result: [String: [TypeName]] = [:]
        for parameter in parameters {
            result[parameter.name] = domain(for: parameter.name,
                                            requirements: requirements)
        }
        return result
    }

    private func domain(for parameter: String,
                        requirements: [GenericRequirement]) -> [TypeName] {
        if requiresErrorConformance(parameter, requirements: requirements) {
            return errorPalette
        }
        return palette
    }

    private func requiresErrorConformance(_ parameter: String,
                                          requirements: [GenericRequirement]) -> Bool {
        for requirement in requirements {
            if case let .conformsTo(param, proto) = requirement,
               param == parameter,
               proto == "Swift.Error" {
                return true
            }
        }
        return false
    }

    private static func makePalette(kind: Kind) -> [TypeName] {
        switch kind {
        case .standard:
            return [
                TypeName(path: ["Swift", "Int"]),
                TypeName(path: ["Swift", "String"]),
                TypeName(path: ["Swift", "Bool"]),
                TypeName(path: ["Swift", "Double"]),
                TypeName(path: ["Swift", "UInt8"]),
                TypeName(path: ["Foundation", "Data"])
            ]
        }
    }
}
