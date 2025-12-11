//
//  HostMethodDescriptorBuilder.swift
//  HostCodegenCore
//
//  Created by Forest Plasencia on 9/19/25.
//

import HostSurfaceKit
import Foundation

struct HostMethodDescriptorBuilder {
    func build(
        from surface: ApiSurface,
        conformanceTable: [String: Set<String>] = [:]
    ) -> (methods: [HostMethodDescriptor], properties: [HostPropertyDescriptor], diagnostics: [HostSurfaceDiagnostic]) {
        var diagnostics: [HostSurfaceDiagnostic] = []
        var methods: [HostMethodDescriptor] = []
        var properties: [HostPropertyDescriptor] = []
        for typeSurface in surface.types {
            guard let receiverType = try? TypeNameParser.parse(typeSurface.canonicalName) else {
                diagnostics.append(.init(kind: .parseError("Failed to parse canonical type name \(typeSurface.canonicalName)"),
                                         line: nil))
                continue
            }
            for member in typeSurface.members {
                switch member {
                case .initializer(let info):
                    let parsed = parseRequirements(for: info.signature)
                    diagnostics.append(contentsOf: parsed.diagnostics)
                    methods.append(
                        makeInitializerDescriptor(
                            receiver: receiverType,
                            info: info,
                            requirements: parsed.requirements,
                            conformanceTable: conformanceTable
                        )
                    )
                case .instanceMethod(let info):
                    let parsed = parseRequirements(for: info.signature)
                    diagnostics.append(contentsOf: parsed.diagnostics)
                    methods.append(
                        makeInstanceDescriptor(
                            receiver: receiverType,
                            info: info,
                            requirements: parsed.requirements,
                            conformanceTable: conformanceTable
                        )
                    )
                case .staticMethod(let info):
                    let parsed = parseRequirements(for: info.signature)
                    diagnostics.append(contentsOf: parsed.diagnostics)
                    methods.append(
                        makeStaticDescriptor(
                            receiver: receiverType,
                            info: info,
                            requirements: parsed.requirements,
                            conformanceTable: conformanceTable
                        )
                    )
                case .instanceProperty(let info):
                    properties.append(
                        makePropertyDescriptor(
                            receiver: receiverType,
                            info: info,
                            isStatic: false,
                            conformanceTable: conformanceTable
                        )
                    )
                case .staticProperty(let info):
                    properties.append(
                        makePropertyDescriptor(
                            receiver: receiverType,
                            info: info,
                            isStatic: true,
                            conformanceTable: conformanceTable
                        )
                    )
                }
            }
        }
        return (methods, properties, diagnostics)
    }

    private func makeInitializerDescriptor(
        receiver: TypeName,
        info: ResolvedInitializer,
        requirements: [GenericRequirement],
        conformanceTable: [String: Set<String>]
    ) -> HostMethodDescriptor {
        let params = hostParameters(from: info.signature.parameters)
        let typeParams = typeGenericParameters(of: receiver)
        let generics = combinedGenericParameters(method: info.signature.genericParameters,
                                                 type: typeParams)
        return HostMethodDescriptor(
            receiver: receiver,
            kind: .initializer(isFailable: info.isFailable),
            baseName: info.signature.baseName,
            selector: info.signature.selector,
            displayName: displayName(for: receiver,
                                     selector: info.signature.selector),
            returnType: info.signature.returnType,
            parameters: params,
            genericParameters: generics,
            requirements: requirements,
            conformanceTable: conformanceTable,
            throwsKind: info.signature.throwsKind,
            availability: info.signature.availability
        )
    }

    private func makeInstanceDescriptor(
        receiver: TypeName,
        info: ResolvedInstanceMethod,
        requirements: [GenericRequirement],
        conformanceTable: [String: Set<String>]
    ) -> HostMethodDescriptor {
        let params = hostParameters(from: info.signature.parameters)
        let typeParams = typeGenericParameters(of: receiver)
        let generics = combinedGenericParameters(method: info.signature.genericParameters,
                                                 type: typeParams)
        return HostMethodDescriptor(
            receiver: receiver,
            kind: .instance(isMutating: info.isMutating),
            baseName: info.signature.baseName,
            selector: info.signature.selector,
            displayName: displayName(for: receiver,
                                     selector: info.signature.selector),
            returnType: info.signature.returnType,
            parameters: params,
            genericParameters: generics,
            requirements: requirements,
            conformanceTable: conformanceTable,
            throwsKind: info.signature.throwsKind,
            availability: info.signature.availability
        )
    }

    private func makeStaticDescriptor(
        receiver: TypeName,
        info: ResolvedStaticMethod,
        requirements: [GenericRequirement],
        conformanceTable: [String: Set<String>]
    ) -> HostMethodDescriptor {
        let params = hostParameters(from: info.signature.parameters)
        let typeParams = typeGenericParameters(of: receiver)
        let generics = combinedGenericParameters(method: info.signature.genericParameters,
                                                 type: typeParams)
        return HostMethodDescriptor(
            receiver: receiver,
            kind: .static,
            baseName: info.signature.baseName,
            selector: info.signature.selector,
            displayName: displayName(for: receiver,
                                     selector: info.signature.selector),
            returnType: info.signature.returnType,
            parameters: params,
            genericParameters: generics,
            requirements: requirements,
            conformanceTable: conformanceTable,
            throwsKind: info.signature.throwsKind,
            availability: info.signature.availability
        )
    }

    private func makePropertyDescriptor(
        receiver: TypeName,
        info: ResolvedProperty,
        isStatic: Bool,
        conformanceTable: [String: Set<String>]
    ) -> HostPropertyDescriptor {
        HostPropertyDescriptor(
            receiver: receiver,
            kind: isStatic ? .static : .instance,
            name: info.name,
            type: info.type,
            isSettable: info.isSettable,
            genericParameters: typeGenericParameters(of: receiver),
            requirements: [],
            conformanceTable: conformanceTable,
            availability: info.availability
        )
    }

    private func hostParameters(from parameters: [ResolvedParameter]) -> [HostMethodParameter] {
        parameters.map { param in
            HostMethodParameter(
                label: param.label,
                name: param.name,
                type: param.type,
                isInout: param.isInout,
                isVariadic: param.isVariadic
            )
        }
    }

    private func displayName(for receiver: TypeName, selector: String) -> String {
        let shortName = receiver.path.last ?? receiver.canonicalDescription()
        return "\(shortName).\(selector)"
    }

    private func typeGenericParameters(of receiver: TypeName) -> [GenericParameter] {
        var params: [GenericParameter] = []
        var seen: Set<String> = []
        for argument in receiver.genericArguments {
            guard argument.genericArguments.isEmpty, argument.path.count == 1 else { continue }
            let name = argument.path[0]
            if seen.insert(name).inserted {
                params.append(GenericParameter(name: name, origin: .receiver))
            }
        }
        return params
    }

    private func combinedGenericParameters(method: [GenericParameter],
                                           type: [GenericParameter]) -> [GenericParameter] {
        var result: [GenericParameter] = []
        var seen: Set<String> = []
        for parameter in method + type {
            if seen.insert(parameter.name).inserted {
                result.append(parameter)
            }
        }
        return result
    }

    private func parseRequirements(
        for signature: ResolvedCallable
    ) -> (requirements: [GenericRequirement], diagnostics: [HostSurfaceDiagnostic]) {
        guard let rawClause = signature.whereClause?.trimmingCharacters(in: .whitespacesAndNewlines),
              !rawClause.isEmpty else {
            return ([], [])
        }

        var clause = rawClause
        if clause.hasPrefix("where") {
            clause.removeFirst("where".count)
        }
        clause = clause.trimmingCharacters(in: .whitespacesAndNewlines)
        if clause.isEmpty { return ([], []) }

        var requirements: [GenericRequirement] = []
        var diagnostics: [HostSurfaceDiagnostic] = []
        let pieces = clause.split(separator: ",")
        let genericParams = Set(signature.genericParameters.map(\.name))
        for piece in pieces {
            let fragment = piece.trimmingCharacters(in: .whitespacesAndNewlines)
            if fragment.isEmpty { continue }
            if fragment.contains("==") {
                let parts = fragment.components(separatedBy: "==")
                guard parts.count == 2 else {
                    diagnostics.append(.init(kind: .unsupported("Unsupported generic equality constraint '\(fragment)'"),
                                              line: signature.sourceLine))
                    continue
                }
                let lhs = parts[0].trimmed()
                let rhs = parts[1].trimmed()
                if let lhsType = try? TypeNameParser.parse(lhs),
                   let rhsType = try? TypeNameParser.parse(rhs) {
                    requirements.append(.sameTypeResolved(lhs: lhsType, rhs: rhsType))
                    continue
                }
                guard genericParams.contains(lhs) else {
                    diagnostics.append(.init(kind: .unsupported("Unsupported constraint '\(fragment)' (unknown generic parameter)"),
                                              line: signature.sourceLine))
                    continue
                }
                if genericParams.contains(rhs) {
                    requirements.append(.sameTypeParameters(lhs: lhs, rhs: rhs))
                } else {
                    do {
                        let type = try TypeNameParser.parse(rhs)
                        requirements.append(.sameType(param: lhs, type))
                    } catch {
                        diagnostics.append(.init(kind: .unsupported("Failed to parse type in constraint '\(fragment)'"),
                                                  line: signature.sourceLine))
                    }
                }
                continue
            }
            if let colonIndex = fragment.firstIndex(of: ":") {
                let lhs = String(fragment[..<colonIndex]).trimmed()
                let rhs = String(fragment[fragment.index(after: colonIndex)...]).trimmed()
                let protocolPieces = rhs.split(separator: "&").map { $0.trimmingCharacters(in: .whitespacesAndNewlines) }
                if protocolPieces.isEmpty {
                    diagnostics.append(.init(kind: .unsupported("Unsupported protocol constraint '\(fragment)'"),
                                              line: signature.sourceLine))
                    continue
                }
                if genericParams.contains(lhs) {
                    for proto in protocolPieces where !proto.isEmpty {
                        do {
                            let protoType = try TypeNameParser.parse(proto)
                            requirements.append(.conformsTo(param: lhs, protocolName: protoType.canonicalDescription()))
                        } catch {
                            diagnostics.append(.init(kind: .unsupported("Failed to parse protocol in constraint '\(fragment)'"),
                                                      line: signature.sourceLine))
                        }
                    }
                } else if let lhsType = try? TypeNameParser.parse(lhs) {
                    for proto in protocolPieces where !proto.isEmpty {
                        do {
                            let protoType = try TypeNameParser.parse(proto)
                            requirements.append(.conformsResolved(type: lhsType, protocolName: protoType.canonicalDescription()))
                        } catch {
                            diagnostics.append(.init(kind: .unsupported("Failed to parse protocol in constraint '\(fragment)'"),
                                                      line: signature.sourceLine))
                        }
                    }
                } else {
                    diagnostics.append(.init(kind: .unsupported("Unsupported constraint '\(fragment)' (unknown generic parameter)"),
                                              line: signature.sourceLine))
                }
                continue
            }

            diagnostics.append(.init(kind: .unsupported("Unsupported generic constraint '\(fragment)'"),
                                      line: signature.sourceLine))
        }
        return (requirements, diagnostics)
    }
}

private extension String {
    func trimmed() -> String {
        trimmingCharacters(in: .whitespacesAndNewlines)
    }
}
