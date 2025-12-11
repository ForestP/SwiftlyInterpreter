//
//  HostInteropGenerator.swift
//  HostCodegenCore
//
//  Created by Forest Plasencia on 9/19/25.
//

import HostSurfaceKit

import Foundation

struct HostMethodParameter: Equatable {
    let label: String?
    let name: String
    let type: TypeName
    let isInout: Bool
    let isVariadic: Bool

    var callArgument: String {
        let varName = escapeIdentifierIfNeeded(name)
        if let label, label != "_" {
            return "\(label): \(varName)"
        }
        return varName
    }
}

enum HostMethodKind: Equatable {
    case initializer(isFailable: Bool)
    case instance(isMutating: Bool)
    case `static`
}

public struct HostMethodDescriptor: Equatable {
    let receiver: TypeName
    let kind: HostMethodKind
    let baseName: String
    let selector: String
    let displayName: String
    let returnType: TypeName?
    let parameters: [HostMethodParameter]
    let genericParameters: [GenericParameter]
    let requirements: [GenericRequirement]
    let conformanceTable: [String: Set<String>]
    let throwsKind: TypeName.FunctionSignature.ThrowsKind
    let availability: [String]

    var isThrowing: Bool { throwsKind != .none }

    // Use the full selector (including argument labels) to disambiguate
    // overloads that share the same base name (e.g., index(after:) vs index(before:)).
    // Sanitize into an identifier-friendly stem; generic specializations are
    // appended elsewhere.
    var functionBaseName: String {
        var stem = sanitizeIdentifier(selector)
        // For operator members, the selector sanitization can erase the operator token (e.g., "+(lhs:rhs:)" → "___lhs_rhs__").
        // Append a stable encoding of the operator itself to avoid collisions between different operators
        // that share the same labels and parameter types (e.g., + vs -).
        if isOperatorName(baseName) {
            stem += "__op_" + encodeOperatorToken(baseName)
        }
        return stem
    }
}

enum HostPropertyKind: Equatable {
    case instance
    case `static`
}

public struct HostPropertyDescriptor: Equatable {
    let receiver: TypeName
    let kind: HostPropertyKind
    let name: String
    let type: TypeName
    let isSettable: Bool
    let genericParameters: [GenericParameter]
    let requirements: [GenericRequirement]
    let conformanceTable: [String: Set<String>]
    let availability: [String]

    var displayName: String {
        "\(receiver.canonicalDescription()).\(name)"
    }
}

// Memberwise initializers synthesized by Swift are used for these structs.

struct HostMethodRender {
    struct Thunk {
        let name: String
        let receiverTypeName: String
        let runtimeTypeName: String
        let specialization: GenericSpecialization
        let parameterTypeDescriptions: [String]
        let source: String
    }
    struct Fallback {
        let name: String
        let source: String
    }
    let dispatcherName: String
    let dispatcherSource: String
    let thunks: [Thunk]
    let fallback: Fallback?
}

struct HostMethodGenerator {
    let method: HostMethodDescriptor
    let rewriteMap: [String: String]
    let exportedTypeNameProvider: (String) -> String
    let primitiveTypeAliases: [String: String]
    let genericPlaceholderMap: [String: [String]]

    func render(allowedSpecializations: [String: [TypeName]]) throws -> HostMethodRender {
        let specializations = try planSpecializations(allowed: allowedSpecializations)
        return try render(specializations: specializations, overflow: [])
    }

    func render(specializations: [GenericSpecialization], overflow: [GenericSpecialization]) throws -> HostMethodRender {
        var thunks: [HostMethodRender.Thunk] = []
        for specialization in specializations {
            let thunk = try renderThunk(for: specialization)
            thunks.append(thunk)
        }
        let fallback = overflow.isEmpty ? nil : try renderFallback(for: overflow)
        let dispatcher = renderDispatcher(for: thunks, fallbackName: fallback?.name)
        return HostMethodRender(dispatcherName: dispatcher.name,
                                 dispatcherSource: dispatcher.source,
                                 thunks: thunks,
                                 fallback: fallback)
    }

    func planSpecializations(allowed: [String: [TypeName]]) throws -> [GenericSpecialization] {
        let planner = GenericSpecializationPlanner(
            parameters: method.genericParameters,
            requirements: method.requirements,
            conformanceTable: method.conformanceTable,
            placeholderMap: genericPlaceholderMap,
            receiverTemplate: method.receiver
        )
        let domain = method.genericParameters.isEmpty ? [:] : allowed
        return try planner.planSpecializations(from: domain)
    }

    private func normalize(_ type: TypeName, substitutions: [String: TypeName]) -> TypeName {
        type.resolvingPlaceholders(using: genericPlaceholderMap, substitutions: substitutions)
    }

    private func normalizeOptional(_ type: TypeName?, substitutions: [String: TypeName]) -> TypeName? {
        type.map { normalize($0, substitutions: substitutions) }
    }

    private func renderThunk(for specialization: GenericSpecialization) throws -> HostMethodRender.Thunk {
        let substitutions = specialization.argumentsByName
        let specializedReceiver = method.receiver.applyingSubstitutions(substitutions)
        let canonicalReceiverName = specializedReceiver.canonicalDescription()
        // Normalize placeholder generic Any types in the receiver using configured rewrite map
        let receiverTypeName = rewriteConfiguredAny(in: canonicalReceiverName, using: rewriteMap)
        let runtimeTypeName = exportedTypeNameProvider(canonicalReceiverName)
        let functionName = makeFunctionName(for: specialization)
        let parameterTypes = parameterTypeDescriptions(for: specialization)

        var builder: [String] = []
        // Emit availability attributes if present
        for attr in method.availability { builder.append(attr) }
        builder.append("public func \(functionName)(vm: inout VM, _ args: [Value]) throws -> Value {")
        builder.append(contentsOf: indentLines(renderThunkBody(for: specialization,
                                                              receiverTypeName: receiverTypeName,
                                                              runtimeTypeName: runtimeTypeName), indent: 1))
        builder.append("}")

        return HostMethodRender.Thunk(name: functionName,
                                      receiverTypeName: receiverTypeName,
                                      runtimeTypeName: runtimeTypeName,
                                      specialization: specialization,
                                      parameterTypeDescriptions: parameterTypes,
                                      source: builder.joined(separator: "\n"))
    }

    private func renderFallback(for overflow: [GenericSpecialization]) throws -> HostMethodRender.Fallback {
        var fallbackBase = sanitizeIdentifier("HostFallback_\(method.receiver.canonicalDescription())_\(method.functionBaseName)")
        // Disambiguate overloads by parameter types to avoid duplicate fallback names
        let paramSuffix = method.parameters.isEmpty ? "" : "__" + method.parameters.map { sanitizeIdentifier($0.type.canonicalDescription()) }.joined(separator: "__")
        fallbackBase += paramSuffix
        let name = fallbackBase
        var lines: [String] = []
        for attr in method.availability { lines.append(attr) }
        lines.append("public func \(name)(vm: inout VM, _ args: [Value]) throws -> Value {")
        var body: [String] = []
        body.append("guard !args.isEmpty else { throw VMError.typeError(\"missing receiver\") }")
        switch method.kind {
        case .initializer(_), .static:
            body.append("guard case let .metatype(tid) = args[0] else { throw VMError.typeError(\"receiver must be metatype\") }")
        case .instance(_):
            body.append("guard case let .host(_, tid) = args[0] else { throw VMError.typeError(\"receiver must be host\") }")
        }
        for specialization in overflow {
            let substitutions = specialization.argumentsByName
            let specializedReceiver = method.receiver.applyingSubstitutions(substitutions)
            let canonicalReceiverName = specializedReceiver.canonicalDescription()
            let receiverTypeName = rewriteConfiguredAny(in: canonicalReceiverName, using: rewriteMap)
            let runtimeTypeName = exportedTypeNameProvider(canonicalReceiverName)
            body.append("if tid == vm.hostTypeID(named: \"\(runtimeTypeName)\") {")
            let branchBody = renderThunkBody(for: specialization,
                                             receiverTypeName: receiverTypeName,
                                             runtimeTypeName: runtimeTypeName,
                                             includeReceiverGuard: false)
            body.append(contentsOf: indentLines(branchBody, indent: 1))
            body.append("}")
        }
        let summaries = overflow.map { specializationSummary(for: $0) }
        body.append("let receiverName = vm.debugTypeName(tid)")
        body.append("let supported = [")
        for summary in summaries {
            body.append("    \"\(escapeForSwiftLiteral(summary))\",")
        }
        body.append("] .joined(separator: \"\\n\")")
        body.append("throw VMError.typeError(\"\(escapeForSwiftLiteral(method.displayName)) has no specialization for receiver \\(receiverName). Supported specializations:\\(supported)\")")
        lines.append(contentsOf: indentLines(body, indent: 1))
        lines.append("}")
        return HostMethodRender.Fallback(name: name,
                                         source: lines.joined(separator: "\n"))
    }

    private func renderThunkBody(for specialization: GenericSpecialization,
                                  receiverTypeName: String,
                                  runtimeTypeName: String,
                                  includeReceiverGuard: Bool = true) -> [String] {
        var lines: [String] = []
        let substitutions = specialization.argumentsByName
        let specializedReceiver = method.receiver.applyingSubstitutions(substitutions)
        let specializedReturn: TypeName?
        switch method.kind {
        case .initializer(let isFailable):
            if let baseReturn = method.returnType?.applyingSubstitutions(substitutions) {
                specializedReturn = baseReturn
            } else {
                specializedReturn = isFailable ? TypeName.optional(specializedReceiver) : specializedReceiver
            }
        default:
            specializedReturn = method.returnType?.applyingSubstitutions(substitutions)
        }
        let expectedArgCount = 1 + method.parameters.count
        let arityMessage = "\(method.displayName) expects \(method.parameters.count) args"
        lines.append("guard args.count == \(expectedArgCount) else { throw VMError.typeError(\"\(arityMessage)\") }")

        if includeReceiverGuard {
            switch method.kind {
            case .initializer(_):
                lines.append("guard case let .metatype(tid) = args[0] else { throw VMError.typeError(\"receiver must be \(runtimeTypeName) metatype\") }")
            case .instance(_):
                lines.append("guard case let .host(_, tid) = args[0] else { throw VMError.typeError(\"receiver must be \(runtimeTypeName) host\") }")
            case .static:
                lines.append("guard case let .metatype(tid) = args[0] else { throw VMError.typeError(\"receiver must be \(runtimeTypeName) metatype\") }")
            }
        }

        let normalizedReturn = normalizeOptional(specializedReturn, substitutions: substitutions)

        var parameterBindings: [String] = []
        for (index, parameter) in method.parameters.enumerated() {
            let argIndex = index + 1
            let specializedType = parameter.type.applyingSubstitutions(substitutions)
            parameterBindings.append(contentsOf: inboundBinding(for: parameter,
                                                               specializedType: specializedType,
                                                               argIndex: argIndex,
                                                               rewriteMap: rewriteMap,
                                                               substitutions: substitutions))
        }
        lines.append(contentsOf: parameterBindings)

        lines.append(contentsOf: callLines(receiverTypeName: receiverTypeName,
                                           runtimeTypeName: runtimeTypeName,
                                           returnType: normalizedReturn,
                                           specialization: specialization,
                                           specializedReceiver: specializedReceiver))
        return lines
    }

    private func callLines(receiverTypeName: String,
                           runtimeTypeName: String,
                           returnType: TypeName?,
                           specialization: GenericSpecialization,
                           specializedReceiver: TypeName,
                           skipClosureSpecialization: Bool = false) -> [String] {
        let substitutions = specialization.argumentsByName
        if !skipClosureSpecialization,
           let custom = closureAwareCallLines(receiverTypeName: receiverTypeName,
                                              runtimeTypeName: runtimeTypeName,
                                              returnType: returnType,
                                              specialization: specialization,
                                              specializedReceiver: specializedReceiver,
                                              substitutions: substitutions) {
            return custom
        }
        let argumentList = method.parameters.map { $0.callArgument }.joined(separator: ", ")
        let tryPrefix = method.isThrowing ? "try " : ""
        switch method.kind {
        case .initializer(_):
            let callExpression = "\(tryPrefix)\(receiverTypeName)(\(argumentList))"
            return renderReturn(callExpression: callExpression,
                                 returnType: returnType,
                                 hostTypeIDExpression: hostTypeIDOverride(),
                                 runtimeTypeName: runtimeTypeName)
        case .instance(let isMutating):
            if isMutating {
                return renderMutatingCall(receiverTypeName: receiverTypeName,
                                          runtimeTypeName: runtimeTypeName,
                                          returnType: returnType,
                                          argumentList: argumentList,
                                          substitutions: substitutions)
            }
            // Non-mutating instance: handle inout parameters
            let inoutIndices = method.parameters.enumerated().compactMap { $1.isInout ? $0 : nil }
            if !inoutIndices.isEmpty {
                var lines: [String] = []
                let runtimeReceiver = runtimeTypeName
                lines.append("let receiver = try vm.withHost(args[0], typeName: \"\(runtimeReceiver)\", as: \(receiverTypeName).self) { host in")
                lines.append("    host")
                lines.append("}")
                // Determine host inout params to unwrap via vm.withHost
                let hostInout = method.parameters.enumerated().compactMap { (i, p) -> (Int, String, String)? in
                    guard p.isInout else { return nil }
                    let specializedType = p.type.applyingSubstitutions(substitutions)
                    let resolvedType = normalize(specializedType, substitutions: substitutions)
                    switch typeCategory(for: resolvedType,
                                         primitiveAliases: primitiveTypeAliases) {
                    case .host(var canonical):
                        canonical = rewriteConfiguredAny(in: canonical, using: rewriteMap)
                        return (i, canonical, escapeIdentifierIfNeeded(p.name))
                    default:
                        return nil
                    }
                }
                // Helper to build argument list with & for inouts
                func makeArgList() -> String {
                    method.parameters.enumerated().map { idx, p in
                        let nm = escapeIdentifierIfNeeded(p.name)
                        let expr = p.isInout ? "&\(nm)" : nm
                        if let label = p.label, label != "_" { return "\(label): \(expr)" }
                        return expr
                    }.joined(separator: ", ")
                }
                if hostInout.isEmpty {
                    // Only non-host inouts; just call with & vars
                    let args = makeArgList()
                    let callExpression: String
                    let memberName = memberAccessIdentifier(for: method.baseName)
                    if method.parameters.isEmpty {
                        callExpression = "\(tryPrefix)receiver.\(memberName)()"
                    } else {
                        callExpression = "\(tryPrefix)receiver.\(memberName)(\(args))"
                    }
                    lines.append(contentsOf: renderReturn(callExpression: callExpression,
                                                          returnType: returnType,
                                                          hostTypeIDExpression: hostTypeIDOverride(),
                                                          runtimeTypeName: runtimeTypeName))
                    return lines
                }
                // Wrap each host inout param with vm.withHost closures (nested)
                for (i, canonical, name) in hostInout {
                    let argIndex = i + 1 // args[0] is receiver
                    lines.append("return try vm.withHost(args[\(argIndex)], typeName: \"\(canonical)\", as: \(canonical).self) { \(name) in")
                }
                let args = makeArgList()
                let callExpression: String
                let memberName = memberAccessIdentifier(for: method.baseName)
                if method.parameters.isEmpty {
                    callExpression = "\(tryPrefix)receiver.\(memberName)()"
                } else {
                    callExpression = "\(tryPrefix)receiver.\(memberName)(\(args))"
                }
                let inner = renderReturn(callExpression: callExpression,
                                         returnType: returnType,
                                         hostTypeIDExpression: hostTypeIDOverride(),
                                         runtimeTypeName: runtimeTypeName)
                lines.append(contentsOf: indentLines(inner, indent: hostInout.count))
                for _ in hostInout { lines.append("}") }
                return lines
            }
            return renderNonMutatingInstanceCall(receiverTypeName: receiverTypeName,
                                                 runtimeTypeName: runtimeTypeName,
                                                 returnType: returnType,
                                                 argumentList: argumentList)
        case .static:
            // Handle inout host params by mutating in-place via withHost on the argument.
            if let inoutIdx = method.parameters.firstIndex(where: { $0.isInout }) {
                let param = method.parameters[inoutIdx]
                let specializedType = param.type.applyingSubstitutions(substitutions)
                let resolvedType = normalize(specializedType, substitutions: substitutions)
                if case .host(var canonical) = typeCategory(for: resolvedType,
                                                            primitiveAliases: primitiveTypeAliases) {
                    canonical = rewriteConfiguredAny(in: canonical, using: rewriteMap)
                    let argIndex = inoutIdx + 1 // args[0] is the metatype
                    var lines: [String] = []
                    let inoutName = escapeIdentifierIfNeeded(param.name)
                    lines.append("return try vm.withHost(args[\(argIndex)], typeName: \"\(canonical)\", as: \(canonical).self) { \(inoutName) in")
                    var inner: [String] = []
                    if isOperatorName(method.baseName) {
                        if method.parameters.count == 2 {
                            let left = method.parameters[0].name
                            let right = method.parameters[1].name
                            inner.append("\(tryPrefix)\(left) \(method.baseName) \(right)")
                        } else if method.parameters.count == 1 {
                            let operand = method.parameters[0].name
                            inner.append("\(tryPrefix)\(method.baseName)\(operand)")
                        } else {
                            // Fallback for unexpected arity: call as static method
                            let args = method.parameters.enumerated().map { idx, p in
                                let nm = escapeIdentifierIfNeeded(p.name)
                                let expr = (idx == inoutIdx) ? "&\(nm)" : nm
                                if let label = p.label, label != "_" { return "\(label): \(expr)" }
                                return expr
                            }.joined(separator: ", ")
                            let memberName = memberAccessIdentifier(for: method.baseName)
                            inner.append("\(tryPrefix)\(receiverTypeName).\(memberName)(\(args))")
                        }
                    } else {
                        // Non-operator static with inout: pass &name for the inout parameter
                        let args = method.parameters.enumerated().map { idx, p in
                            let nm = escapeIdentifierIfNeeded(p.name)
                            let expr = (idx == inoutIdx) ? "&\(nm)" : nm
                            if let label = p.label, label != "_" { return "\(label): \(expr)" }
                            return expr
                        }.joined(separator: ", ")
                        let memberName = memberAccessIdentifier(for: method.baseName)
                        inner.append("\(tryPrefix)\(receiverTypeName).\(memberName)(\(args))")
                    }
                    // Most operator inout signatures return Void; if not, return a placeholder true.
                    if let returnType, !isVoid(returnType) {
                        inner.append("return .bool(true)")
                    } else {
                        inner.append("return .bool(true)")
                    }
                    lines.append(contentsOf: indentLines(inner, indent: 1))
                    lines.append("}")
                    return lines
                }
            }
            // Emit operator invocations in infix/prefix form to avoid parser ambiguities.
            if isOperatorName(method.baseName) {
                if method.parameters.count == 2 {
                    let left = method.parameters[0].name
                    let right = method.parameters[1].name
                    let callExpression = "\(tryPrefix)\(left) \(method.baseName) \(right)"
                    return renderReturn(callExpression: callExpression,
                                         returnType: returnType,
                                         runtimeTypeName: runtimeTypeName)
                } else if method.parameters.count == 1 {
                    let operand = method.parameters[0].name
                    let callExpression = "\(tryPrefix)\(method.baseName)\(operand)"
                    return renderReturn(callExpression: callExpression,
                                         returnType: returnType,
                                         runtimeTypeName: runtimeTypeName)
                }
            }
            let memberName = memberAccessIdentifier(for: method.baseName)
            let callExpression = "\(tryPrefix)\(receiverTypeName).\(memberName)(\(argumentList))"
            return renderReturn(callExpression: callExpression,
                                 returnType: returnType,
                                 runtimeTypeName: runtimeTypeName)
        }
    }

    private func closureAwareCallLines(receiverTypeName: String,
                                       runtimeTypeName: String,
                                       returnType: TypeName?,
                                       specialization: GenericSpecialization,
                                       specializedReceiver: TypeName,
                                       substitutions: [String: TypeName]) -> [String]? {
        let closureParameters = method.parameters.compactMap { parameter -> ClosureParameterPlan? in
            let specializedType = parameter.type.applyingSubstitutions(substitutions)
            let resolvedType = normalize(specializedType, substitutions: substitutions)
            if let signature = resolvedType.functionSignature {
                return ClosureParameterPlan(parameter: parameter,
                                             specializedType: resolvedType,
                                             signature: signature,
                                             isOptional: false,
                                             substitutions: substitutions)
            }
            if let wrapped = optionalWrappedType(resolvedType),
               let signature = wrapped.functionSignature {
                return ClosureParameterPlan(parameter: parameter,
                                             specializedType: resolvedType,
                                             signature: signature,
                                             isOptional: true,
                                             substitutions: substitutions)
            }
            return nil
        }
        guard !closureParameters.isEmpty else { return nil }
        // Bail if any closure parameter requires async, variadic arguments, or unsupported conversions.
        for plan in closureParameters {
            if plan.signature.isAsync { return nil }
            if plan.parameter.isVariadic { return nil }
            if plan.parameter.isInout { return nil }
        }

        let baseCall = callLines(receiverTypeName: receiverTypeName,
                                 runtimeTypeName: runtimeTypeName,
                                 returnType: returnType,
                                 specialization: specialization,
                                 specializedReceiver: specializedReceiver,
                                 skipClosureSpecialization: true)
        guard let wrapped = wrap(baseCall, with: closureParameters) else { return nil }
        return wrapped
    }

    private struct ClosureParameterPlan {
        let parameter: HostMethodParameter
        let specializedType: TypeName
        let signature: TypeName.FunctionSignature
        let isOptional: Bool
        let substitutions: [String: TypeName]
    }

    private func wrap(_ body: [String],
                      with plans: [ClosureParameterPlan]) -> [String]? {
        var result = body
        for plan in plans.reversed() {
            guard let wrapped = wrap(body: result, with: plan) else { return nil }
            result = wrapped
        }
        return result
    }

    private func wrap(body: [String], with plan: ClosureParameterPlan) -> [String]? {
        if plan.isOptional {
            return wrapOptional(body: body, with: plan)
        }
        let adapterVar = closureAdapterVariableName(for: plan.parameter)
        guard let closureType = closureTypeDescription(for: plan),
              let closureLines = closureReturnLines(for: plan, adapterVar: adapterVar) else { return nil }
        let handleVar = closureHandleVariableName(for: plan.parameter)
        var lines: [String] = []
        let closureName = escapeIdentifierIfNeeded(plan.parameter.name)
        lines.append("let \(closureName): \(closureType) = try vm.withHostClosureAdapter(\(handleVar)) { \(adapterVar) in")
        lines.append(contentsOf: indentLines(closureLines, indent: 1))
        lines.append("}")
        lines.append(contentsOf: body)
        return lines
    }

    private func wrapOptional(body: [String], with plan: ClosureParameterPlan) -> [String]? {
        let adapterVar = closureAdapterVariableName(for: plan.parameter)
        guard let closureLines = closureReturnLines(for: plan, adapterVar: adapterVar) else { return nil }
        guard let closureType = closureTypeDescription(for: plan) else { return nil }
        let handleVar = closureHandleVariableName(for: plan.parameter)
        let handleBinding = sanitizeIdentifier("\(plan.parameter.name)_handle_unwrapped")
        let closureName = escapeIdentifierIfNeeded(plan.parameter.name)
        var lines: [String] = []
        lines.append("var \(closureName): \(closureType) = nil")
        lines.append("if let \(handleBinding) = \(handleVar) {")
        lines.append("    \(closureName) = try vm.withHostClosureAdapter(\(handleBinding)) { \(adapterVar) in")
        lines.append(contentsOf: indentLines(closureLines, indent: 2))
        lines.append("    }")
        lines.append("} else {")
        lines.append("    \(closureName) = nil")
        lines.append("}")
        lines.append(contentsOf: body)
        return lines
    }

    private func closureHandleVariableName(for parameter: HostMethodParameter) -> String {
        sanitizeIdentifier("\(parameter.name)_handle")
    }

    private func closureAdapterVariableName(for parameter: HostMethodParameter) -> String {
        sanitizeIdentifier("\(parameter.name)_adapter")
    }

    private func resolvedSignature(for plan: ClosureParameterPlan) -> TypeName.FunctionSignature {
        let resolvedParams = plan.signature.parameters.map { $0.applyingSubstitutions(plan.substitutions) }
        let resolvedReturn = plan.signature.returnType.applyingSubstitutions(plan.substitutions)
        return TypeName.FunctionSignature(parameters: resolvedParams,
                                          returnType: resolvedReturn,
                                          isAsync: plan.signature.isAsync,
                                          throwsKind: plan.signature.throwsKind)
    }

    private func resolvedFunctionType(for plan: ClosureParameterPlan) -> TypeName {
        let signature = resolvedSignature(for: plan)
        let functionType = TypeName(path: [],
                                    genericArguments: [],
                                    genericOwnerIndex: nil,
                                    functionSignature: signature)
        if plan.isOptional {
            return .optional(functionType)
        }
        return functionType
    }

    private func closureTypeDescription(for plan: ClosureParameterPlan) -> String? {
        var canonical = resolvedFunctionType(for: plan).canonicalDescription()
        canonical = rewriteConfiguredAny(in: canonical, using: rewriteMap)
        return canonical.isEmpty ? nil : canonical
    }

    private func closureReturnLines(for plan: ClosureParameterPlan,
                                    adapterVar: String) -> [String]? {
        guard let literal = closureLiteralComponents(for: plan, adapterVar: adapterVar) else { return nil }
        var lines: [String] = []
        lines.append("return { \(literal.header) in")
        lines.append(contentsOf: indentLines(literal.body, indent: 1))
        lines.append("}")
        return lines
    }

    private func closureLiteralComponents(for plan: ClosureParameterPlan,
                                          adapterVar: String) -> (header: String, body: [String])? {
        let signature = resolvedSignature(for: plan)
        let parameterNames: [String] = signature.parameters.enumerated().map { index, _ in
            "arg\(index)"
        }
        let headerParams: String
        if parameterNames.isEmpty {
            headerParams = "()"
        } else if parameterNames.count == 1 {
            headerParams = parameterNames[0]
        } else {
            headerParams = parameterNames.joined(separator: ", ")
        }
        var body: [String] = []
        var argLiterals: [String] = []
        for (index, parameterType) in signature.parameters.enumerated() {
            let variable = parameterNames[index]
            guard let literal = valueLiteral(for: parameterType,
                                             variable: variable,
                                             vmIdentifier: adapterVar) else { return nil }
            argLiterals.append(literal)
        }
        if isVoid(signature.returnType) {
            if argLiterals.isEmpty {
                body.append("_ = try \(adapterVar).invoke([])")
            } else {
                body.append("_ = try \(adapterVar).invoke([\(argLiterals.joined(separator: ", "))])")
            }
            body.append("return ()")
        } else {
            let invocationArgs = argLiterals.joined(separator: ", ")
            let adapterCall: String
            if argLiterals.isEmpty {
                adapterCall = "let adapterResult = try \(adapterVar).invoke([])"
            } else {
                adapterCall = "let adapterResult = try \(adapterVar).invoke([\(invocationArgs)])"
            }
            body.append(adapterCall)
            let tempNameStem = sanitizeIdentifier("\(plan.parameter.name)_result")
            guard let conversion = emitValueConversion(source: "adapterResult",
                                                       type: signature.returnType,
                                                       label: "\(method.baseName) result",
                                                       tempNameStem: tempNameStem,
                                                       rewriteMap: rewriteMap,
                                                       primitiveAliases: primitiveTypeAliases,
                                                       vmIdentifier: adapterVar) else { return nil }
            body.append(contentsOf: conversion.lines)
            body.append("return \(conversion.expression)")
        }
        return (headerParams, body)
    }

    private func valueLiteral(for type: TypeName,
                              variable: String,
                              vmIdentifier: String = "vm") -> String? {
        if let wrapped = optionalWrappedType(type) { return nil }
        switch typeCategory(for: type, primitiveAliases: primitiveTypeAliases) {
        case .primitive(let primitive):
            switch primitive {
            case .int:
                return ".int(\(variable))"
            case .double:
                return ".double(\(variable))"
            case .bool:
                return ".bool(\(variable))"
            case .string:
                return ".string(\(variable))"
            case .void:
                return nil
            }
        case .host(var canonical):
            canonical = rewriteConfiguredAny(in: canonical, using: rewriteMap)
            let runtimeName = exportedTypeNameProvider(canonical)
            return ".host(HostRef(box: makeOpaqueBox(\(variable))), \(vmIdentifier).hostTypeID(named: \"\(runtimeName)\"))"
        case .array, .dictionary, .closure:
            return nil
        }
    }

    private func renderMutatingCall(receiverTypeName: String,
                                    runtimeTypeName: String,
                                    returnType: TypeName?,
                                    argumentList: String,
                                    substitutions: [String: TypeName]) -> [String] {
        var lines: [String] = []
        let canonicalReceiver = rewriteConfiguredAny(in: receiverTypeName, using: rewriteMap)
        var hostTypeIDExpression = hostTypeIDOverride()
        if hostTypeIDExpression == nil,
           let returnType,
           !isVoid(returnType),
           let runtimeName = returnHostRuntimeName(for: returnType) {
            let sanitized = sanitizeIdentifier("returnHostTypeID_\(runtimeName)")
            lines.append("let \(sanitized) = vm.hostTypeID(named: \"\(runtimeName)\")")
            hostTypeIDExpression = sanitized
        }
        lines.append("return try vm.withHost(args[0], typeName: \"\(runtimeTypeName)\", as: \(canonicalReceiver).self) { receiver in")
        var inner: [String] = []
        let invocation: String
        let memberName = memberAccessIdentifier(for: method.baseName)
        if method.parameters.isEmpty {
            invocation = "\(method.isThrowing ? "try " : "")receiver.\(memberName)()"
        } else {
            invocation = "\(method.isThrowing ? "try " : "")receiver.\(memberName)(\(argumentList))"
        }
        if let returnType, !isVoid(returnType) {
            inner.append("let result = \(invocation)")
            inner.append(contentsOf: returnConversionLines(resultExpression: "result",
                                                          returnType: returnType,
                                                          fallbackExpression: "args[0]",
                                                          hostTypeIDExpression: hostTypeIDExpression,
                                                          runtimeTypeName: runtimeTypeName))
        } else {
            inner.append("\(invocation)")
            inner.append("return args[0]")
        }
        lines.append(contentsOf: indentLines(inner, indent: 1))
        lines.append("}")
        return lines
    }

    private func renderNonMutatingInstanceCall(receiverTypeName: String,
                                               runtimeTypeName: String,
                                               returnType: TypeName?,
                                               argumentList: String) -> [String] {
        var lines: [String] = []
        let canonicalReceiver = rewriteConfiguredAny(in: receiverTypeName, using: rewriteMap)
        lines.append("let receiver = try vm.withHost(args[0], typeName: \"\(runtimeTypeName)\", as: \(canonicalReceiver).self) { host in")
        lines.append("    host")
        lines.append("}")
        let callExpression: String
        let memberName = memberAccessIdentifier(for: method.baseName)
        if method.parameters.isEmpty {
            callExpression = "\(method.isThrowing ? "try " : "")receiver.\(memberName)()"
        } else {
            callExpression = "\(method.isThrowing ? "try " : "")receiver.\(memberName)(\(argumentList))"
        }
        lines.append(contentsOf: renderReturn(callExpression: callExpression,
                                              returnType: returnType,
                                              hostTypeIDExpression: hostTypeIDOverride(),
                                              runtimeTypeName: runtimeTypeName))
        return lines
    }

    private func renderReturn(callExpression: String,
                              returnType: TypeName?,
                              hostTypeIDExpression: String? = nil,
                              runtimeTypeName: String? = nil,
                              vmIdentifier: String = "vm") -> [String] {
        guard let returnType else {
            return [callExpression, "return .bool(true)"]
        }
        if isVoid(returnType) {
            return [callExpression, "return .bool(true)"]
        }
        var lines: [String] = ["let result = \(callExpression)"]
        lines.append(contentsOf: emitReturnValue(for: returnType,
                                                expression: "result",
                                                hostTypeIDExpression: hostTypeIDExpression,
                                                runtimeTypeName: runtimeTypeName,
                                                primitiveAliases: primitiveTypeAliases,
                                                vmIdentifier: vmIdentifier))
        return lines
    }

    private func inboundBinding(for parameter: HostMethodParameter,
                                 specializedType: TypeName,
                                 argIndex: Int,
                                 rewriteMap: [String: String],
                                 substitutions: [String: TypeName]) -> [String] {
        let resolvedType = normalize(specializedType, substitutions: substitutions)
        let varName = escapeIdentifierIfNeeded(parameter.name)
        if let signature = resolvedType.functionSignature {
            let arity = signature.parameters.count
            let handleName = closureHandleVariableName(for: parameter)
            return ["let \(handleName) = try vm.makeHostClosureHandle(from: args[\(argIndex)], expectedArity: \(arity))"]
        }
        if let wrapped = optionalWrappedType(resolvedType),
           let signature = wrapped.functionSignature {
            let arity = signature.parameters.count
            let handleName = closureHandleVariableName(for: parameter)
            return [
                "var \(handleName): HostClosureHandle? = nil",
                "if case .nilValue = args[\(argIndex)] {",
                "    \(handleName) = nil",
                "} else {",
                "    \(handleName) = try vm.makeHostClosureHandle(from: args[\(argIndex)], expectedArity: \(arity))",
                "}"
            ]
        }
        // Inout host parameters are mutated via withHost at call-site; do not bind here.
        if parameter.isInout {
            switch typeCategory(for: resolvedType,
                                 primitiveAliases: primitiveTypeAliases) {
            case .host:
                return []
            case .primitive(let primitive):
                switch primitive {
                case .int, .double, .bool, .string:
                    var decl = bindingLinesDeclaring(name: varName,
                                                     type: resolvedType,
                                                     source: "args[\(argIndex)]",
                                                     label: parameter.name,
                                                     rewriteMap: rewriteMap,
                                                     primitiveAliases: primitiveTypeAliases)
                    if !decl.isEmpty {
                        decl[0] = decl[0].replacingOccurrences(of: "let \(varName)", with: "var \(varName)")
                    }
                    return decl
                case .void:
                    return []
                }
            case .array, .dictionary:
                return []
            case .closure(_):
                fatalError("closure inout parameters should be handled before inboundBinding")
            }
        }
        if let wrapped = optionalWrappedType(resolvedType) {
            // Rewrite Any and configured Any-placeholders in optional declarations
            var declarationType = optionalTypeDescription(for: resolvedType)
            declarationType = rewriteConfiguredAny(in: declarationType, using: rewriteMap)
            var lines: [String] = []
            lines.append("var \(varName): \(declarationType) = nil")
            lines.append("if case .nilValue = args[\(argIndex)] {")
            lines.append("    \(varName) = nil")
            lines.append("} else {")
            lines.append(contentsOf: indentLines(bindingLinesAssigning(name: varName,
                                                                       type: wrapped,
                                                                       source: "args[\(argIndex)]",
                                                                       label: parameter.name,
                                                                       rewriteMap: rewriteMap,
                                                                       primitiveAliases: primitiveTypeAliases),
                                                indent: 1))
            lines.append("}")
            return lines
        }
        return bindingLinesDeclaring(name: varName,
                                     type: resolvedType,
                                     source: "args[\(argIndex)]",
                                     label: parameter.name,
                                     rewriteMap: rewriteMap,
                                     primitiveAliases: primitiveTypeAliases)
    }

    private func renderDispatcher(for thunks: [HostMethodRender.Thunk],
                                  fallbackName: String?) -> (name: String, source: String) {
        var base = sanitizeIdentifier("HostDispatcher_\(method.receiver.canonicalDescription())_\(method.functionBaseName)")
        // Disambiguate overloads by parameter types to avoid duplicate dispatcher names
        let paramSuffix = method.parameters.isEmpty ? "" : "__" + method.parameters.map { sanitizeIdentifier($0.type.canonicalDescription()) }.joined(separator: "__")
        base += paramSuffix
        var lines: [String] = []
        for attr in method.availability { lines.append(attr) }
        lines.append("public func \(base)(vm: inout VM, _ args: [Value]) throws -> Value {")
        var body: [String] = []
        body.append("guard !args.isEmpty else { throw VMError.typeError(\"missing receiver\") }")

        // Check if receiver is a collection type that needs dual-path handling
        let isArrayReceiver = method.receiver.path == ["Swift", "Array"]
        let isDictionaryReceiver = method.receiver.path == ["Swift", "Dictionary"]
        let isOptionalReceiver = method.receiver.path == ["Swift", "Optional"]
        let needsVMNativeSupport = isArrayReceiver || isDictionaryReceiver || isOptionalReceiver

        switch method.kind {
        case .initializer(_), .static:
            body.append("guard case let .metatype(tid) = args[0] else { throw VMError.typeError(\"receiver must be metatype\") }")
            // Generate thunk dispatch for static/init methods
            let methodAvailCond = strictAvailabilityConditionFromAttributes(method.availability)
            for thunk in thunks {
                body.append("if tid == vm.hostTypeID(named: \"\(thunk.receiverTypeName)\") {")
                if methodAvailCond != nil {
                    let cond = methodAvailCond!
                    body.append(contentsOf: indentLines(["if \(cond) {", "    return try \(thunk.name)(vm: &vm, args)", "} else {", "    throw VMError.unsupported(\"\(escapeForSwiftLiteral(method.displayName)) is not available on this platform\")", "}"], indent: 1))
                } else {
                    body.append(contentsOf: indentLines(["return try \(thunk.name)(vm: &vm, args)"], indent: 1))
                }
                body.append("}")
            }
            if let fallbackName {
                body.append("return try \(fallbackName)(vm: &vm, args)")
            } else {
                body.append("let receiverName = vm.debugTypeName(tid)")
                body.append("let supported = [")
                for thunk in thunks {
                    let summary = specializationSummary(for: thunk)
                    body.append("    \"\(escapeForSwiftLiteral(summary))\",")
                }
                body.append("] .joined(separator: \"\\n\")")
                body.append("throw VMError.typeError(\"\(escapeForSwiftLiteral(method.displayName)) has no specialization for receiver \\(receiverName). Supported specializations:\\(supported)\")")
            }
        case .instance(_):
            if needsVMNativeSupport {
                // Dual-path dispatcher: handle both .host and VM-native receivers
                body.append("// Path 1: Host receiver")
                body.append("if case let .host(_, tid) = args[0] {")
                var hostPath: [String] = []
                // If this method has availability requirements, guard calls to the thunks
                let methodAvailCond = strictAvailabilityConditionFromAttributes(method.availability)
                for thunk in thunks {
                    hostPath.append("if tid == vm.hostTypeID(named: \"\(thunk.receiverTypeName)\") {")
                    if methodAvailCond != nil {
                        let cond = methodAvailCond!
                        hostPath.append(contentsOf: indentLines(["if \(cond) {", "    return try \(thunk.name)(vm: &vm, args)", "} else {", "    throw VMError.unsupported(\"\(escapeForSwiftLiteral(method.displayName)) is not available on this platform\")", "}"], indent: 1))
                    } else {
                        hostPath.append(contentsOf: indentLines(["return try \(thunk.name)(vm: &vm, args)"], indent: 1))
                    }
                    hostPath.append("}")
                }
                if fallbackName != nil {
                    hostPath.append("return try \(fallbackName!)(vm: &vm, args)")
                } else {
                    hostPath.append("let receiverName = vm.debugTypeName(tid)")
                    hostPath.append("let supported = [")
                    for thunk in thunks {
                        let summary = specializationSummary(for: thunk)
                        hostPath.append("    \"\(escapeForSwiftLiteral(summary))\",")
                    }
                    hostPath.append("] .joined(separator: \"\\n\")")
                    hostPath.append("throw VMError.typeError(\"\(escapeForSwiftLiteral(method.displayName)) has no specialization for receiver \\(receiverName). Supported specializations:\\(supported)\")")
                }
                body.append(contentsOf: indentLines(hostPath, indent: 1))
                body.append("}")

                // Path 2: VM-native receiver
                if isArrayReceiver {
                    body.append("// Path 2: VM-native array receiver")
                    body.append("else if case let .array(box) = args[0] {")
                    body.append(contentsOf: indentLines(renderVMArrayDispatch(thunks: thunks, methodAvailCond: methodAvailCond), indent: 1))
                    body.append("}")
                } else if isDictionaryReceiver {
                    body.append("// Path 2: VM-native dictionary receiver")
                    body.append("else if case let .dict(box) = args[0] {")
                    body.append(contentsOf: indentLines(renderVMDictDispatch(thunks: thunks, methodAvailCond: methodAvailCond), indent: 1))
                    body.append("}")
                } else if isOptionalReceiver {
                    body.append("// Path 2: VM-native optional receiver")
                    body.append("else if case .nilValue = args[0] {")
                    body.append(contentsOf: indentLines(renderVMOptionalDispatch(thunks: thunks, methodAvailCond: methodAvailCond, isNil: true), indent: 1))
                    body.append("}")
                    body.append("else {")
                    body.append(contentsOf: indentLines(renderVMOptionalDispatch(thunks: thunks, methodAvailCond: methodAvailCond, isNil: false), indent: 1))
                    body.append("}")
                }

                // If we reach here, no path matched
                if !isOptionalReceiver {
                    body.append("throw VMError.typeError(\"receiver must be host or VM-native \(method.receiver.canonicalDescription())\")")
                }
            } else {
                // Simple host-only dispatcher (not a collection type)
                body.append("guard case let .host(_, tid) = args[0] else { throw VMError.typeError(\"receiver must be host\") }")
                // If this method has availability requirements, guard calls to the thunks
                let methodAvailCond = strictAvailabilityConditionFromAttributes(method.availability)
                for thunk in thunks {
                    body.append("if tid == vm.hostTypeID(named: \"\(thunk.receiverTypeName)\") {")
                    if methodAvailCond != nil {
                        let cond = methodAvailCond!
                        body.append(contentsOf: indentLines(["if \(cond) {", "    return try \(thunk.name)(vm: &vm, args)", "} else {", "    throw VMError.unsupported(\"\(escapeForSwiftLiteral(method.displayName)) is not available on this platform\")", "}"], indent: 1))
                    } else {
                        body.append(contentsOf: indentLines(["return try \(thunk.name)(vm: &vm, args)"], indent: 1))
                    }
                    body.append("}")
                }
                if let fallbackName {
                    body.append("return try \(fallbackName)(vm: &vm, args)")
                } else {
                    body.append("let receiverName = vm.debugTypeName(tid)")
                    body.append("let supported = [")
                    for thunk in thunks {
                        let summary = specializationSummary(for: thunk)
                        body.append("    \"\(escapeForSwiftLiteral(summary))\",")
                    }
                    body.append("] .joined(separator: \"\\n\")")
                    body.append("throw VMError.typeError(\"\(escapeForSwiftLiteral(method.displayName)) has no specialization for receiver \\(receiverName). Supported specializations:\\(supported)\")")
                }
            }
        }
        lines.append(contentsOf: indentLines(body, indent: 1))
        lines.append("}")
        return (base, lines.joined(separator: "\n"))
    }

    private func renderVMArrayDispatch(thunks: [HostMethodRender.Thunk], methodAvailCond: String?) -> [String] {
        var lines: [String] = []

        // Handle empty array edge case
        lines.append("guard !box.elements.isEmpty else {")
        lines.append("    throw VMError.typeError(\"Cannot infer element type from empty array\")")
        lines.append("}")

        lines.append("// Infer element type from first element")
        lines.append("let sampleElement = box.elements[0]")
        lines.append("")

        // Group thunks by element type, keeping all return type specializations
        var thunksByElementType: [String: [HostMethodRender.Thunk]] = [:]
        for thunk in thunks {
            if let elementType = extractArrayElementType(from: thunk.receiverTypeName) {
                thunksByElementType[elementType, default: []].append(thunk)
            }
        }

        // Generate element type checks with inline conversion
        for (elementType, elementThunks) in thunksByElementType.sorted(by: { $0.key < $1.key }) {
            let valueCheck = generateValueTypeCheck(for: elementType)
            lines.append("if \(valueCheck) {")
            lines.append("    // Convert VM array to host array of \(elementType) inline")

            // Generate inline conversion code based on element type (use first thunk as template)
            guard let firstThunk = elementThunks.first else { continue }
            let conversionCode = generateArrayConversionCode(elementType: elementType, receiverTypeName: firstThunk.receiverTypeName)
            lines.append(contentsOf: conversionCode.map { "    \($0)" })

            lines.append("    var convertedArgs = args")
            lines.append("    convertedArgs[0] = hostArray")
            lines.append("")

            // Extract closure return type from closure argument (if present)
            lines.append("    // Dispatch to correct specialization based on closure return type")
            lines.append("    var closureReturnType: String? = nil")
            lines.append("    if args.count > 1, case let .closure(closureHandle) = args[1] {")
            lines.append("        closureReturnType = try vm.makeHostClosureHandle(from: args[1], expectedArity: 1).returnType")
            lines.append("    }")
            lines.append("")

            // Generate return type dispatch for this element type
            let returnTypeDispatch = renderClosureReturnTypeDispatch(
                thunks: elementThunks,
                methodAvailCond: methodAvailCond
            )
            lines.append(contentsOf: returnTypeDispatch.map { "    \($0)" })

            lines.append("}")
        }

        lines.append("throw VMError.typeError(\"Array element type not supported for \(escapeForSwiftLiteral(method.displayName))\")")

        return lines
    }

    private func renderClosureReturnTypeDispatch(thunks: [HostMethodRender.Thunk], methodAvailCond: String?) -> [String] {
        var lines: [String] = []

        // Extract return type from each thunk's specialization (the "T" generic parameter)
        var thunksByReturnType: [String: HostMethodRender.Thunk] = [:]
        for thunk in thunks {
            // Look for generic parameter "T" which represents the closure return type
            if let returnTypeName = thunk.specialization.argumentsByName["T"] {
                let canonicalReturnType = returnTypeName.canonicalDescription()
                thunksByReturnType[canonicalReturnType] = thunk
            }
        }

        // Generate if/else chain for each return type
        var isFirst = true
        for (returnType, thunk) in thunksByReturnType.sorted(by: { $0.key < $1.key }) {
            let prefix = isFirst ? "if" : "else if"
            lines.append("\(prefix) closureReturnType == \"\(returnType)\" {")

            if methodAvailCond != nil {
                let cond = methodAvailCond!
                lines.append("    if \(cond) {")
                lines.append("        return try \(thunk.name)(vm: &vm, convertedArgs)")
                lines.append("    } else {")
                lines.append("        throw VMError.unsupported(\"\(escapeForSwiftLiteral(method.displayName)) is not available on this platform\")")
                lines.append("    }")
            } else {
                lines.append("    return try \(thunk.name)(vm: &vm, convertedArgs)")
            }

            lines.append("}")
            isFirst = false
        }

        // Fallback for when return type doesn't match or is nil
        if !thunksByReturnType.isEmpty {
            lines.append("else {")
            let supportedTypesList = thunksByReturnType.keys.sorted().map { "\"\($0)\"" }.joined(separator: ", ")
            lines.append("    let supported = [\(supportedTypesList)].map { \"    - \\($0)\" }.joined(separator: \"\\n\")")
            lines.append("    throw VMError.typeError(\"Closure return type '\\(closureReturnType ?? \"nil\")' not supported for \(escapeForSwiftLiteral(method.displayName)). Supported return types:\\n\\(supported)\")")
            lines.append("}")
        } else {
            // No thunks with return type info - call first thunk as fallback
            if let firstThunk = thunks.first {
                if methodAvailCond != nil {
                    let cond = methodAvailCond!
                    lines.append("if \(cond) {")
                    lines.append("    return try \(firstThunk.name)(vm: &vm, convertedArgs)")
                    lines.append("} else {")
                    lines.append("    throw VMError.unsupported(\"\(escapeForSwiftLiteral(method.displayName)) is not available on this platform\")")
                    lines.append("}")
                } else {
                    lines.append("return try \(firstThunk.name)(vm: &vm, convertedArgs)")
                }
            }
        }

        return lines
    }

    private func renderVMDictDispatch(thunks: [HostMethodRender.Thunk], methodAvailCond: String?) -> [String] {
        var lines: [String] = []

        // VM dictionaries always have String keys (DictBox.storage is [String: Value])
        // We can only support host dictionaries with String keys

        lines.append("// VM dictionaries always have String keys")
        lines.append("guard !box.storage.isEmpty else {")
        lines.append("    throw VMError.typeError(\"Cannot infer value type from empty dictionary\")")
        lines.append("}")

        lines.append("// Infer value type from first value")
        lines.append("let sampleValue = box.storage.values.first!")
        lines.append("")

        // Deduplicate thunks by value type (key type must be String)
        var seenValueTypes = Set<String>()
        var uniqueThunks: [(keyType: String, valueType: String, thunk: HostMethodRender.Thunk)] = []
        for thunk in thunks {
            if let (keyType, valueType) = extractDictionaryKeyValueTypes(from: thunk.receiverTypeName) {
                // Only support String keys since VM dictionaries have String keys
                if keyType == "Swift.String" && !seenValueTypes.contains(valueType) {
                    seenValueTypes.insert(valueType)
                    uniqueThunks.append((keyType, valueType, thunk))
                }
            }
        }

        // Generate value type checks with inline conversion
        for (keyType, valueType, thunk) in uniqueThunks {
            let valueCheck = generateValueTypeCheck(for: valueType, varName: "sampleValue")
            lines.append("if \(valueCheck) {")
            lines.append("    // Convert VM dictionary to host dictionary of <\(keyType), \(valueType)> inline")

            // Generate inline conversion code
            let conversionCode = generateDictConversionCode(keyType: keyType, valueType: valueType, receiverTypeName: thunk.receiverTypeName)
            lines.append(contentsOf: conversionCode.map { "    \($0)" })

            lines.append("    var convertedArgs = args")
            lines.append("    convertedArgs[0] = hostDictValue")
            if methodAvailCond != nil {
                let cond = methodAvailCond!
                lines.append("    if \(cond) {")
                lines.append("        return try \(thunk.name)(vm: &vm, convertedArgs)")
                lines.append("    } else {")
                lines.append("        throw VMError.unsupported(\"\(escapeForSwiftLiteral(method.displayName)) is not available on this platform\")")
                lines.append("    }")
            } else {
                lines.append("    return try \(thunk.name)(vm: &vm, convertedArgs)")
            }
            lines.append("}")
        }

        lines.append("throw VMError.typeError(\"Dictionary value type not supported for \(escapeForSwiftLiteral(method.displayName))\")")

        return lines
    }

    private func renderVMOptionalDispatch(thunks: [HostMethodRender.Thunk], methodAvailCond: String?, isNil: Bool) -> [String] {
        var lines: [String] = []

        if isNil {
            // For nil case, we can call any specialization since nil works for all Optional<T>
            if let firstThunk = thunks.first {
                lines.append("// Nil value - works for any Optional<T>")
                if methodAvailCond != nil {
                    let cond = methodAvailCond!
                    lines.append("if \(cond) {")
                    lines.append("    return try \(firstThunk.name)(vm: &vm, args)")
                    lines.append("} else {")
                    lines.append("    throw VMError.unsupported(\"\(escapeForSwiftLiteral(method.displayName)) is not available on this platform\")")
                    lines.append("}")
                } else {
                    lines.append("return try \(firstThunk.name)(vm: &vm, args)")
                }
            }
        } else {
            // For non-nil wrapped values, infer type and convert to host Optional
            lines.append("// Infer wrapped type from value")

            // Deduplicate thunks by wrapped type
            var seenWrappedTypes = Set<String>()
            var uniqueThunks: [(wrappedType: String, thunk: HostMethodRender.Thunk)] = []
            for thunk in thunks {
                if let wrappedType = extractOptionalWrappedType(from: thunk.receiverTypeName) {
                    if !seenWrappedTypes.contains(wrappedType) {
                        seenWrappedTypes.insert(wrappedType)
                        uniqueThunks.append((wrappedType, thunk))
                    }
                }
            }

            // Generate wrapped type checks with inline conversion
            for (wrappedType, thunk) in uniqueThunks {
                let valueCheck = generateValueTypeCheck(for: wrappedType, varName: "args[0]")
                lines.append("if \(valueCheck) {")
                lines.append("    // Convert VM value to host Optional<\(wrappedType)> inline")

                // Generate inline conversion code
                let conversionCode = generateOptionalConversionCode(wrappedType: wrappedType, receiverTypeName: thunk.receiverTypeName)
                lines.append(contentsOf: conversionCode.map { "    \($0)" })

                lines.append("    var convertedArgs = args")
                lines.append("    convertedArgs[0] = hostOptional")
                if methodAvailCond != nil {
                    let cond = methodAvailCond!
                    lines.append("    if \(cond) {")
                    lines.append("        return try \(thunk.name)(vm: &vm, convertedArgs)")
                    lines.append("    } else {")
                    lines.append("        throw VMError.unsupported(\"\(escapeForSwiftLiteral(method.displayName)) is not available on this platform\")")
                    lines.append("    }")
                } else {
                    lines.append("    return try \(thunk.name)(vm: &vm, convertedArgs)")
                }
                lines.append("}")
            }

            lines.append("throw VMError.typeError(\"Optional wrapped type not supported for \(escapeForSwiftLiteral(method.displayName))\")")
        }

        return lines
    }

    private func extractArrayElementType(from receiverTypeName: String) -> String? {
        // Extract "Swift.Int" from "Swift.Array<Swift.Int>"
        guard receiverTypeName.hasPrefix("Swift.Array<"), receiverTypeName.hasSuffix(">") else {
            return nil
        }
        let start = receiverTypeName.index(receiverTypeName.startIndex, offsetBy: "Swift.Array<".count)
        let end = receiverTypeName.index(before: receiverTypeName.endIndex)
        return String(receiverTypeName[start..<end])
    }

    private func extractDictionaryKeyValueTypes(from receiverTypeName: String) -> (String, String)? {
        // Extract ("Swift.String", "Swift.Int") from "Swift.Dictionary<Swift.String,Swift.Int>"
        guard receiverTypeName.hasPrefix("Swift.Dictionary<"), receiverTypeName.hasSuffix(">") else {
            return nil
        }
        let start = receiverTypeName.index(receiverTypeName.startIndex, offsetBy: "Swift.Dictionary<".count)
        let end = receiverTypeName.index(before: receiverTypeName.endIndex)
        let typeArgs = String(receiverTypeName[start..<end])

        // Split on comma (simple approach - assumes no nested generics)
        let parts = typeArgs.split(separator: ",").map { $0.trimmingCharacters(in: .whitespaces) }
        guard parts.count == 2 else { return nil }
        return (parts[0], parts[1])
    }

    private func extractOptionalWrappedType(from receiverTypeName: String) -> String? {
        // Extract "Swift.Int" from "Swift.Optional<Swift.Int>"
        guard receiverTypeName.hasPrefix("Swift.Optional<"), receiverTypeName.hasSuffix(">") else {
            return nil
        }
        let start = receiverTypeName.index(receiverTypeName.startIndex, offsetBy: "Swift.Optional<".count)
        let end = receiverTypeName.index(before: receiverTypeName.endIndex)
        return String(receiverTypeName[start..<end])
    }

    private func generateValueTypeCheck(for typeName: String, varName: String = "sampleElement") -> String {
        // Generate a pattern match check for the given type
        // Only special-case VM primitive types; all host types use the default path
        switch typeName {
        case "Swift.Int":
            return "case .int = \(varName)"
        case "Swift.String":
            return "case .string = \(varName)"
        case "Swift.Bool":
            return "case .bool = \(varName)"
        case "Swift.Double":
            return "case .double = \(varName)"
        case "Swift.UInt8":
            return "case .int = \(varName)" // UInt8 stored as int in VM
        default:
            // All host types (Foundation.Data, custom structs, etc.) use .host
            return "case let .host(_, tid) = \(varName), tid == vm.hostTypeID(named: \"\(typeName)\")"
        }
    }

    private func generateArrayConversionCode(elementType: String, receiverTypeName: String) -> [String] {
        var lines: [String] = []
        let swiftType = swiftTypeString(for: elementType)

        // Generate code to extract and convert each element from the VM array
        // Only special-case VM primitive types; all host types use the default path
        switch elementType {
        case "Swift.Int":
            lines.append("var hostElements: [\(swiftType)] = []")
            lines.append("for element in box.elements {")
            lines.append("    guard case let .int(i) = element else { throw VMError.typeError(\"Array element is not Int\") }")
            lines.append("    hostElements.append(i)")
            lines.append("}")
        case "Swift.String":
            lines.append("var hostElements: [\(swiftType)] = []")
            lines.append("for element in box.elements {")
            lines.append("    guard case let .string(s) = element else { throw VMError.typeError(\"Array element is not String\") }")
            lines.append("    hostElements.append(s)")
            lines.append("}")
        case "Swift.Bool":
            lines.append("var hostElements: [\(swiftType)] = []")
            lines.append("for element in box.elements {")
            lines.append("    guard case let .bool(b) = element else { throw VMError.typeError(\"Array element is not Bool\") }")
            lines.append("    hostElements.append(b)")
            lines.append("}")
        case "Swift.Double":
            lines.append("var hostElements: [\(swiftType)] = []")
            lines.append("for element in box.elements {")
            lines.append("    guard case let .double(d) = element else { throw VMError.typeError(\"Array element is not Double\") }")
            lines.append("    hostElements.append(d)")
            lines.append("}")
        case "Swift.UInt8":
            lines.append("var hostElements: [\(swiftType)] = []")
            lines.append("for element in box.elements {")
            lines.append("    guard case let .int(i) = element else { throw VMError.typeError(\"Array element is not UInt8 (Int)\") }")
            lines.append("    hostElements.append(UInt8(i))")
            lines.append("}")
        default:
            // All host types (Foundation.Data, custom structs, etc.)
            lines.append("var hostElements: [\(swiftType)] = []")
            lines.append("for element in box.elements {")
            lines.append("    guard case let .host(ref, _) = element else { throw VMError.typeError(\"Array element is not a host value\") }")
            lines.append("    guard let typedValue = ref.asAny() as? \(swiftType) else { throw VMError.typeError(\"Host value is not \(elementType)\") }")
            lines.append("    hostElements.append(typedValue)")
            lines.append("}")
        }

        // Wrap in HostRef with the correct type ID
        lines.append("let hostRef = HostRef(box: makeOpaqueBox(hostElements))")
        lines.append("let hostArray = Value.host(hostRef, vm.hostTypeID(named: \"\(receiverTypeName)\"))")

        return lines
    }

    private func generateDictConversionCode(keyType: String, valueType: String, receiverTypeName: String) -> [String] {
        var lines: [String] = []

        // Determine concrete Swift dictionary type
        let keySwiftType = swiftTypeString(for: keyType)
        let valueSwiftType = swiftTypeString(for: valueType)

        // This function should only be called for String-keyed dictionaries
        // since VM dictionaries (DictBox.storage) always have String keys
        assert(keyType == "Swift.String", "VM dictionaries only support String keys")

        lines.append("var hostDict: [\(keySwiftType): \(valueSwiftType)] = [:]")
        lines.append("for (keyString, valueVal) in box.storage {")

        // Extract value
        lines.append(contentsOf: generateValueExtraction(from: "valueVal", toVariable: "value", typeName: valueType).map { "    \($0)" })

        lines.append("    hostDict[keyString] = value")
        lines.append("}")

        // Wrap in HostRef
        lines.append("let hostDictRef = HostRef(box: makeOpaqueBox(hostDict))")
        lines.append("let hostDictValue = Value.host(hostDictRef, vm.hostTypeID(named: \"\(receiverTypeName)\"))")

        return lines
    }

    private func generateOptionalConversionCode(wrappedType: String, receiverTypeName: String) -> [String] {
        var lines: [String] = []

        let swiftType = swiftTypeString(for: wrappedType)

        lines.append(contentsOf: generateValueExtraction(from: "args[0]", toVariable: "wrapped", typeName: wrappedType))
        lines.append("let hostOptionalValue: \(swiftType)? = wrapped")
        lines.append("let hostRef = HostRef(box: makeOpaqueBox(hostOptionalValue))")
        lines.append("let hostOptional = Value.host(hostRef, vm.hostTypeID(named: \"\(receiverTypeName)\"))")

        return lines
    }

    private func swiftTypeString(for typeName: String) -> String {
        // Only special-case VM primitive types; all others use their canonical name
        switch typeName {
        case "Swift.Int": return "Int"
        case "Swift.String": return "String"
        case "Swift.Bool": return "Bool"
        case "Swift.Double": return "Double"
        case "Swift.UInt8": return "UInt8"
        default:
            // For host types (Foundation.Data, custom structs, etc.), use the canonical name
            return typeName
        }
    }

    private func generateValueExtraction(from sourceVar: String, toVariable destVar: String, typeName: String) -> [String] {
        // Only special-case VM primitive types; all host types use the default path
        switch typeName {
        case "Swift.Int":
            return ["guard case let .int(\(destVar)) = \(sourceVar) else { throw VMError.typeError(\"Expected Int\") }"]
        case "Swift.String":
            return ["guard case let .string(\(destVar)) = \(sourceVar) else { throw VMError.typeError(\"Expected String\") }"]
        case "Swift.Bool":
            return ["guard case let .bool(\(destVar)) = \(sourceVar) else { throw VMError.typeError(\"Expected Bool\") }"]
        case "Swift.Double":
            return ["guard case let .double(\(destVar)) = \(sourceVar) else { throw VMError.typeError(\"Expected Double\") }"]
        case "Swift.UInt8":
            return [
                "guard case let .int(intVal) = \(sourceVar) else { throw VMError.typeError(\"Expected UInt8 (Int)\") }",
                "let \(destVar) = UInt8(intVal)"
            ]
        default:
            // All host types (Foundation.Data, custom structs, etc.)
            return [
                "guard case let .host(ref, _) = \(sourceVar) else { throw VMError.typeError(\"Expected host value\") }",
                "guard let \(destVar) = ref.asAny() as? \(swiftTypeString(for: typeName)) else { throw VMError.typeError(\"Host value is not \(typeName)\") }"
            ]
        }
    }

    private func makeFunctionName(for specialization: GenericSpecialization) -> String {
        var name = sanitizeIdentifier("Host_\(method.receiver.canonicalDescription())_\(method.functionBaseName)")
        // Always include parameter type suffix to disambiguate overloads
        if !method.parameters.isEmpty {
            let paramSuffix = method.parameters.map { p in
                // Apply substitutions so generic params reflect concrete types when available
                let specialized = p.type.applyingSubstitutions(specialization.argumentsByName)
                return sanitizeIdentifier(specialized.canonicalDescription())
            }.joined(separator: "__")
            name += "__\(paramSuffix)"
        }
        // Retain specialization argument suffix for additional disambiguation if present
        if !specialization.arguments.isEmpty {
            let genSuffix = specialization.arguments.map { arg in
                sanitizeIdentifier("\(arg.name)_\(arg.type.canonicalDescription())")
            }.joined(separator: "__")
            name += "__" + genSuffix
        }
        return name
    }

    private func specializationSummary(for thunk: HostMethodRender.Thunk) -> String {
        specializationSummary(for: thunk.specialization, parameterTypes: thunk.parameterTypeDescriptions)
    }

    private func specializationSummary(for specialization: GenericSpecialization) -> String {
        let parameterTypes = parameterTypeDescriptions(for: specialization)
        return specializationSummary(for: specialization, parameterTypes: parameterTypes)
    }

    private func specializationSummary(for specialization: GenericSpecialization,
                                       parameterTypes: [String]) -> String {
        let parameterSummary: String
        if method.parameters.isEmpty {
            parameterSummary = "()"
        } else {
            var parts: [String] = []
            for (index, parameter) in method.parameters.enumerated() {
                let typeDesc = index < parameterTypes.count ? parameterTypes[index] : "<unknown>"
                if let label = parameter.label, label != "_" {
                    parts.append("\(label): \(typeDesc)")
                } else {
                    parts.append(typeDesc)
                }
            }
            parameterSummary = "(\(parts.joined(separator: ", ")))"
        }
        let specialReceiver = method.receiver.applyingSubstitutions(specialization.argumentsByName)
        return "  - \(specialReceiver.canonicalDescription()) \(parameterSummary)"
    }

    private func parameterTypeDescriptions(for specialization: GenericSpecialization) -> [String] {
        let substitutions = specialization.argumentsByName
        return method.parameters.map { parameter in
            parameter.type.applyingSubstitutions(substitutions).canonicalDescription()
        }
    }

    private func isVoid(_ type: TypeName) -> Bool {
        type.canonicalDescription() == "Swift.Void"
    }
    
    private func returnConversionLines(resultExpression: String,
                                       returnType: TypeName?,
                                       fallbackExpression: String,
                                       hostTypeIDExpression: String? = nil,
                                       runtimeTypeName: String? = nil) -> [String] {
        guard let returnType else { return ["return \(fallbackExpression)"] }
        if isVoid(returnType) { return ["return \(fallbackExpression)"] }
        return emitReturnValue(for: returnType,
                               expression: resultExpression,
                               hostTypeIDExpression: hostTypeIDExpression,
                               runtimeTypeName: runtimeTypeName,
                               primitiveAliases: primitiveTypeAliases)
    }
    
    private func returnHostRuntimeName(for type: TypeName?) -> String? {
        guard let type else { return nil }
        if let wrapped = optionalWrappedType(type) {
            return returnHostRuntimeName(for: wrapped)
        }
        switch typeCategory(for: type, primitiveAliases: primitiveTypeAliases) {
        case .host(var canonical):
            canonical = rewriteConfiguredAny(in: canonical, using: rewriteMap)
            return exportedTypeNameProvider(canonical)
        default:
            return nil
        }
    }

    private func emitReturnValue(for type: TypeName,
                                 expression: String,
                                 hostTypeIDExpression: String?,
                                 runtimeTypeName: String?,
                                 primitiveAliases: [String: String],
                                 vmIdentifier: String = "vm") -> [String] {
        if let wrapped = optionalWrappedType(type) {
            var lines: [String] = []
            lines.append("switch \(expression) {")
            lines.append("case .some(let unwrapped):")
            lines.append(contentsOf: indentLines(emitReturnValue(for: wrapped,
                                                                 expression: "unwrapped",
                                                                 hostTypeIDExpression: hostTypeIDExpression,
                                                                 runtimeTypeName: runtimeTypeName,
                                                                 primitiveAliases: primitiveAliases,
                                                                 vmIdentifier: vmIdentifier),
                                                    indent: 1))
            lines.append("case .none:")
            lines.append("    return .nilValue")
            lines.append("}")
            return lines
        }
        switch typeCategory(for: type, primitiveAliases: primitiveAliases) {
        case .primitive(let primitive):
            switch primitive {
            case .void:
                return ["return .bool(true)"]
            case .int:
                return ["return .int(\(expression))"]
            case .double:
                return ["return .double(\(expression))"]
            case .bool:
                return ["return .bool(\(expression))"]
            case .string:
                return ["return .string(\(expression))"]
            }
        case .host(var canonical):
            canonical = rewriteConfiguredAny(in: canonical, using: rewriteMap)
            if let hostTypeIDExpression {
                return ["return .host(HostRef(box: makeOpaqueBox(\(expression))), \(hostTypeIDExpression))"]
            }
            let runtimeName = runtimeTypeName ?? exportedTypeNameProvider(canonical)
            return ["return .host(HostRef(box: makeOpaqueBox(\(expression))), \(vmIdentifier).hostTypeID(named: \"\(runtimeName)\"))"]
        case .array, .dictionary:
            let stem = sanitizeIdentifier("\(expression)_value")
            if let synthesis = emitValueSynthesisExpression(resultNameStem: stem,
                                                           type: type,
                                                           expression: expression,
                                                           rewriteMap: rewriteMap,
                                                           exportedTypeNameProvider: exportedTypeNameProvider,
                                                           primitiveAliases: primitiveAliases,
                                                           vmIdentifier: vmIdentifier,
                                                           hostTypeIDExpression: hostTypeIDExpression,
                                                           runtimeTypeName: runtimeTypeName) {
                var lines = synthesis.lines
                lines.append("return \(synthesis.expression)")
                return lines
            }
            let canonical = rewriteConfiguredAny(in: type.canonicalDescription(), using: rewriteMap)
            if let hostTypeIDExpression {
                return ["return .host(HostRef(box: makeOpaqueBox(\(expression))), \(hostTypeIDExpression))"]
            }
            let runtimeName = runtimeTypeName ?? exportedTypeNameProvider(canonical)
            return ["return .host(HostRef(box: makeOpaqueBox(\(expression))), \(vmIdentifier).hostTypeID(named: \"\(runtimeName)\"))"]
        case .closure:
            fatalError("closure return types are not supported yet")
        }
    }

    private func hostTypeIDOverride() -> String? {
        switch method.kind {
        case .initializer(_):
            return "tid"
        default:
            return nil
        }
    }
}

struct HostPropertyRender {
    let getter: HostMethodRender
    let setter: HostMethodRender?
}

struct HostPropertyGenerator {
    let property: HostPropertyDescriptor
    let rewriteMap: [String: String]
    let exportedTypeNameProvider: (String) -> String
    let primitiveTypeAliases: [String: String]
    let genericPlaceholderMap: [String: [String]]

    private func normalize(_ type: TypeName, substitutions: [String: TypeName]) -> TypeName {
        type.resolvingPlaceholders(using: genericPlaceholderMap, substitutions: substitutions)
    }

    private func normalizeOptional(_ type: TypeName?, substitutions: [String: TypeName]) -> TypeName? {
        type.map { normalize($0, substitutions: substitutions) }
    }

    func render(allowedSpecializations: [String: [TypeName]] = [:]) throws -> HostPropertyRender {
        let specializations = try planSpecializations(allowed: allowedSpecializations)
        let getterRender = try renderGetter(specializations: specializations)
        let setterRender: HostMethodRender?
        if property.isSettable {
            setterRender = try renderSetter(specializations: specializations)
        } else {
            setterRender = nil
        }
        return HostPropertyRender(getter: getterRender, setter: setterRender)
    }

    private func renderGetter(specializations: [GenericSpecialization]) throws -> HostMethodRender {
        var thunks: [HostMethodRender.Thunk] = []
        thunks.reserveCapacity(specializations.count)
        for specialization in specializations {
            let thunk = try renderGetterThunk(for: specialization)
            thunks.append(thunk)
        }
        let dispatcher = renderGetterDispatcher(for: thunks)
        return HostMethodRender(dispatcherName: dispatcher.name,
                                 dispatcherSource: dispatcher.source,
                                 thunks: thunks,
                                 fallback: nil)
    }

    private func renderGetterThunk(for specialization: GenericSpecialization) throws -> HostMethodRender.Thunk {
        let substitutions = specialization.argumentsByName
        let specializedReceiver = property.receiver.applyingSubstitutions(substitutions)
        let canonicalReceiverName = specializedReceiver.canonicalDescription()
        let receiverTypeName = rewriteConfiguredAny(in: canonicalReceiverName, using: rewriteMap)
        let runtimeTypeName = exportedTypeNameProvider(canonicalReceiverName)
        let specializedReturnType = property.type.applyingSubstitutions(substitutions)
        let normalizedReturnType = normalize(specializedReturnType, substitutions: substitutions)
        var functionName = sanitizeIdentifier("Host_\(receiverTypeName)_\(property.name)")
        // Disambiguate static vs instance getters to avoid redeclarations when both exist.
        switch property.kind {
        case .instance: functionName += "_instance"
        case .static:   functionName += "_static"
        }

        var lines: [String] = []
        for attr in property.availability { lines.append(attr) }
        lines.append("public func \(functionName)(vm: inout VM, _ args: [Value]) throws -> Value {")
        var body: [String] = []
        // One receiver arg only
        body.append("guard args.count == 1 else { throw VMError.typeError(\"\(property.displayName) expects 0 args\") }")
        switch property.kind {
        case .instance:
            body.append("guard case let .host(_, tid) = args[0] else { throw VMError.typeError(\"receiver must be \(runtimeTypeName) host\") }")
            body.append("let receiver = try vm.withHost(args[0], typeName: \"\(runtimeTypeName)\", as: \(receiverTypeName).self) { host in")
            body.append("    host")
            body.append("}")
            body.append("let result = receiver.\(property.name)")
        case .static:
            body.append("guard case let .metatype(tid) = args[0] else { throw VMError.typeError(\"receiver must be \(runtimeTypeName) metatype\") }")
            body.append("let result = \(receiverTypeName).\(property.name)")
        }
        // Return conversion
        body.append(contentsOf: emitReturnValueForProperty(type: normalizedReturnType,
                                                           expression: "result",
                                                           rewriteMap: rewriteMap,
                                                           exportedTypeNameProvider: exportedTypeNameProvider,
                                                           primitiveAliases: primitiveTypeAliases,
                                                           runtimeTypeName: runtimeTypeName))
        lines.append(contentsOf: indentLines(body, indent: 1))
        lines.append("}")

        return HostMethodRender.Thunk(name: functionName,
                                      receiverTypeName: receiverTypeName,
                                      runtimeTypeName: runtimeTypeName,
                                      specialization: specialization,
                                      parameterTypeDescriptions: [],
                                      source: lines.joined(separator: "\n"))
    }

    private func renderGetterDispatcher(for thunks: [HostMethodRender.Thunk]) -> (name: String, source: String) {
        let base = sanitizeIdentifier("HostDispatcher_\(property.receiver.canonicalDescription())_\(property.name)")
        var lines: [String] = []
        for attr in property.availability { lines.append(attr) }
        lines.append("public func \(base)(vm: inout VM, _ args: [Value]) throws -> Value {")
        var body: [String] = []
        body.append("guard !args.isEmpty else { throw VMError.typeError(\"missing receiver\") }")
        body.append("let tid: TypeID")
        switch property.kind {
        case .instance:
            body.append("guard case let .host(_, receiverTid) = args[0] else { throw VMError.typeError(\"receiver must be host\") }")
            body.append("tid = receiverTid")
        case .static:
            body.append("guard case let .metatype(receiverTid) = args[0] else { throw VMError.typeError(\"receiver must be metatype\") }")
            body.append("tid = receiverTid")
        }
        let propAvailCond = strictAvailabilityConditionFromAttributes(property.availability)
        for thunk in thunks {
            body.append("if tid == vm.hostTypeID(named: \"\(thunk.runtimeTypeName)\") {")
            if propAvailCond != nil {
                let cond = propAvailCond!
                body.append(contentsOf: indentLines(["if \(cond) {", "    return try \(thunk.name)(vm: &vm, args)", "} else {", "    throw VMError.unsupported(\"Getter for \(escapeForSwiftLiteral(property.displayName)) is not available on this platform\")", "}"], indent: 1))
            } else {
                body.append(contentsOf: indentLines(["return try \(thunk.name)(vm: &vm, args)"], indent: 1))
            }
            body.append("}")
        }
        body.append("let receiverName = vm.debugTypeName(tid)")
        body.append("let supported = [")
        for thunk in thunks {
            let summary = getterSummary(for: thunk)
            body.append("    \"\(escapeForSwiftLiteral(summary))\",")
        }
        body.append("] .joined(separator: \"\\n\")")
        body.append("throw VMError.typeError(\"Getter for \(escapeForSwiftLiteral(property.displayName)) has no specialization for receiver \\(receiverName). Supported specializations:\\(supported)\")")
        lines.append(contentsOf: indentLines(body, indent: 1))
        lines.append("}")
        return (base, lines.joined(separator: "\n"))
    }

    private func getterSummary(for thunk: HostMethodRender.Thunk) -> String {
        return "  - \(thunk.receiverTypeName) ()"
    }

    private func renderSetter(specializations: [GenericSpecialization]) throws -> HostMethodRender {
        var thunks: [HostMethodRender.Thunk] = []
        thunks.reserveCapacity(specializations.count)
        for specialization in specializations {
            let thunk = try renderSetterThunk(for: specialization)
            thunks.append(thunk)
        }
        let dispatcher = renderSetterDispatcher(for: thunks)
        return HostMethodRender(dispatcherName: dispatcher.name,
                                 dispatcherSource: dispatcher.source,
                                 thunks: thunks,
                                 fallback: nil)
    }

    private func renderSetterThunk(for specialization: GenericSpecialization) throws -> HostMethodRender.Thunk {
        let substitutions = specialization.argumentsByName
        let specializedReceiver = property.receiver.applyingSubstitutions(substitutions)
        let canonicalReceiverName = specializedReceiver.canonicalDescription()
        let receiverTypeName = rewriteConfiguredAny(in: canonicalReceiverName, using: rewriteMap)
        let runtimeTypeName = exportedTypeNameProvider(canonicalReceiverName)
        let specializedValueType = property.type.applyingSubstitutions(substitutions)
        let normalizedValueType = normalize(specializedValueType, substitutions: substitutions)
        let functionName = makeSetterFunctionName(for: specialization, receiverTypeName: receiverTypeName)

        var lines: [String] = []
        for attr in property.availability { lines.append(attr) }
        lines.append("public func \(functionName)(vm: inout VM, _ args: [Value]) throws -> Value {")
        var body: [String] = []
        body.append("guard args.count == 2 else { throw VMError.typeError(\"\(property.displayName) setter expects value\") }")
        switch property.kind {
        case .instance:
            body.append("guard case .host = args[0] else { throw VMError.typeError(\"receiver must be \(runtimeTypeName) host\") }")
        case .static:
            body.append("guard case .metatype = args[0] else { throw VMError.typeError(\"receiver must be \(runtimeTypeName) metatype\") }")
        }
        let bindingLines = setterValueBindingLines(for: normalizedValueType,
                                                   argIndex: 1,
                                                   label: property.name,
                                                   rewriteMap: rewriteMap)
        body.append(contentsOf: bindingLines)
        switch property.kind {
        case .instance:
            body.append("return try vm.withHost(args[0], typeName: \"\(runtimeTypeName)\", as: \(receiverTypeName).self) { receiver in")
            var inner: [String] = []
            inner.append("receiver.\(property.name) = newValue")
            inner.append("return .nilValue")
            body.append(contentsOf: indentLines(inner, indent: 1))
            body.append("}")
        case .static:
            body.append("\(receiverTypeName).\(property.name) = newValue")
            body.append("return .nilValue")
        }
        lines.append(contentsOf: indentLines(body, indent: 1))
        lines.append("}")

        let parameterTypes = [normalizedValueType.canonicalDescription()]
        return HostMethodRender.Thunk(name: functionName,
                                      receiverTypeName: receiverTypeName,
                                      runtimeTypeName: runtimeTypeName,
                                      specialization: specialization,
                                      parameterTypeDescriptions: parameterTypes,
                                      source: lines.joined(separator: "\n"))
    }

    private func renderSetterDispatcher(for thunks: [HostMethodRender.Thunk]) -> (name: String, source: String) {
        let base = sanitizeIdentifier("HostSetter_\(property.receiver.canonicalDescription())_\(property.name)")
        var lines: [String] = []
        for attr in property.availability { lines.append(attr) }
        lines.append("public func \(base)(vm: inout VM, _ args: [Value]) throws -> Value {")
        var body: [String] = []
        body.append("guard !args.isEmpty else { throw VMError.typeError(\"missing receiver\") }")
        body.append("let tid: TypeID")
        switch property.kind {
        case .instance:
            body.append("guard case let .host(_, receiverTid) = args[0] else { throw VMError.typeError(\"receiver must be host\") }")
            body.append("tid = receiverTid")
        case .static:
            body.append("guard case let .metatype(receiverTid) = args[0] else { throw VMError.typeError(\"receiver must be metatype\") }")
            body.append("tid = receiverTid")
        }
        let propAvailCond = strictAvailabilityConditionFromAttributes(property.availability)
        for thunk in thunks {
            body.append("if tid == vm.hostTypeID(named: \"\(thunk.runtimeTypeName)\") {")
            if propAvailCond != nil {
                let cond = propAvailCond!
                body.append(contentsOf: indentLines(["if \(cond) {", "    return try \(thunk.name)(vm: &vm, args)", "} else {", "    throw VMError.unsupported(\"Setter for \(escapeForSwiftLiteral(property.displayName)) is not available on this platform\")", "}"], indent: 1))
            } else {
                body.append(contentsOf: indentLines(["return try \(thunk.name)(vm: &vm, args)"], indent: 1))
            }
            body.append("}")
        }
        body.append("let receiverName = vm.debugTypeName(tid)")
        body.append("let supported = [")
        for thunk in thunks {
            let summary = setterSummary(for: thunk)
            body.append("    \"\(escapeForSwiftLiteral(summary))\",")
        }
        body.append("] .joined(separator: \"\\n\")")
        body.append("throw VMError.typeError(\"Setter for \(escapeForSwiftLiteral(property.displayName)) has no specialization for receiver \\(receiverName). Supported specializations:\\(supported)\")")
        lines.append(contentsOf: indentLines(body, indent: 1))
        lines.append("}")
        return (base, lines.joined(separator: "\n"))
    }

    // Availability helpers moved to file scope

    private func makeSetterFunctionName(for specialization: GenericSpecialization,
                                        receiverTypeName: String) -> String {
        var name = sanitizeIdentifier("HostSet_\(receiverTypeName)_\(property.name)")
        // Disambiguate static vs instance setters to avoid collisions.
        switch property.kind {
        case .instance: name += "_instance"
        case .static:   name += "_static"
        }
        return name
    }

    private func setterValueBindingLines(for type: TypeName,
                                         argIndex: Int,
                                         label: String,
                                         rewriteMap: [String: String]) -> [String] {
        if let wrapped = optionalWrappedType(type) {
            // Normalize optional declaration types (e.g., Swift.Any? -> Any?)
            var declarationType = optionalTypeDescription(for: type)
            declarationType = rewriteConfiguredAny(in: declarationType, using: rewriteMap)
            var lines: [String] = []
            lines.append("var newValue: \(declarationType) = nil")
            lines.append("if case .nilValue = args[\(argIndex)] {")
            lines.append("    newValue = nil")
            lines.append("} else {")
            lines.append(contentsOf: indentLines(bindingLinesAssigning(name: "newValue",
                                                                       type: wrapped,
                                                                       source: "args[\(argIndex)]",
                                                                       label: label,
                                                                       rewriteMap: rewriteMap,
                                                                       primitiveAliases: primitiveTypeAliases),
                                                indent: 1))
            lines.append("}")
            return lines
        }
        var lines = bindingLinesDeclaring(name: "newValue",
                                          type: type,
                                          source: "args[\(argIndex)]",
                                          label: label,
                                          rewriteMap: rewriteMap,
                                          primitiveAliases: primitiveTypeAliases)
        if lines.isEmpty {
            lines.append("let newValue = args[\(argIndex)]")
        }
        return lines
    }

    private func setterSummary(for thunk: HostMethodRender.Thunk) -> String {
        let typeDescription = thunk.parameterTypeDescriptions.first ?? "<unknown>"
        return "  - \(thunk.receiverTypeName) (value: \(typeDescription))"
    }

    private func planSpecializations(allowed: [String: [TypeName]]) throws -> [GenericSpecialization] {
        let planner = GenericSpecializationPlanner(
            parameters: property.genericParameters,
            requirements: property.requirements,
            conformanceTable: property.conformanceTable,
            placeholderMap: genericPlaceholderMap,
            receiverTemplate: property.receiver
        )
        let domain = property.genericParameters.isEmpty ? [:] : allowed
        return try planner.planSpecializations(from: domain)
    }
}

private enum PrimitiveBridgeKind: Equatable {
    case void
    case int
    case double
    case bool
    case string
}

private enum TypeCategory: Equatable {
    case primitive(PrimitiveBridgeKind)
    case array(TypeName)
    case dictionary(key: TypeName, value: TypeName)
    case host(String)
    case closure(TypeName.FunctionSignature)
}

fileprivate func arrayElementType(_ type: TypeName) -> TypeName? {
    guard type.path == ["Swift", "Array"], type.genericArguments.count == 1 else { return nil }
    return type.genericArguments[0]
}

fileprivate func dictionaryKeyValueTypes(_ type: TypeName) -> (TypeName, TypeName)? {
    guard type.path == ["Swift", "Dictionary"], type.genericArguments.count == 2 else { return nil }
    return (type.genericArguments[0], type.genericArguments[1])
}

fileprivate func optionalWrappedType(_ type: TypeName) -> TypeName? {
    guard type.path == ["Swift", "Optional"], type.genericArguments.count == 1 else { return nil }
    return type.genericArguments[0]
}

fileprivate func optionalTypeDescription(for type: TypeName) -> String {
    guard let wrapped = optionalWrappedType(type) else { return type.canonicalDescription() }
    return "\(wrapped.canonicalDescription())?"
}

fileprivate func resolvedCanonical(_ canonical: String,
                                   primitiveAliases: [String: String]) -> String {
    if let mapped = primitiveAliases[canonical] {
        return mapped
    }
    if let simple = canonical.split(separator: ".").last.map(String.init),
       let mapped = primitiveAliases[simple] {
        return mapped
    }
    return canonical
}

fileprivate func primitiveKind(for type: TypeName,
                               primitiveAliases: [String: String]) -> PrimitiveBridgeKind? {
    let resolved = resolvedCanonical(type.canonicalDescription(), primitiveAliases: primitiveAliases)
    switch resolved {
    case "Swift.Void": return .void
    case "Swift.Int": return .int
    case "Swift.Double": return .double
    case "Swift.Bool": return .bool
    case "Swift.String": return .string
    default: return nil
    }
}

fileprivate func typeCategory(for type: TypeName,
                              primitiveAliases: [String: String]) -> TypeCategory {
    if let signature = type.functionSignature {
        return .closure(signature)
    }
    if let arrayElement = arrayElementType(type) {
        return .array(arrayElement)
    }
    if let (key, value) = dictionaryKeyValueTypes(type) {
        return .dictionary(key: key, value: value)
    }
    if let primitive = primitiveKind(for: type, primitiveAliases: primitiveAliases) {
        return .primitive(primitive)
    }
    let canonical = resolvedCanonical(type.canonicalDescription(), primitiveAliases: primitiveAliases)
    return .host(canonical)
}

fileprivate func bindingLinesDeclaring(name: String,
                                       type: TypeName,
                                       source: String,
                                       label: String,
                                       rewriteMap: [String: String],
                                       primitiveAliases: [String: String],
                                       vmIdentifier: String = "vm") -> [String] {
    switch typeCategory(for: type, primitiveAliases: primitiveAliases) {
    case .primitive(let primitive):
        switch primitive {
        case .int:
            return ["let \(name) = try \(source).expectInt(\"\(label)\")"]
        case .double:
            return ["let \(name) = try \(source).expectDouble(\"\(label)\")"]
        case .bool:
            return ["let \(name) = try \(source).expectBool(\"\(label)\")"]
        case .string:
            return ["let \(name) = try \(source).expectString(\"\(label)\")"]
        case .void:
            return []
        }
    case .array(let element):
        if let conversion = emitArrayConversionExpression(resultNameStem: name,
                                                          arrayType: type,
                                                          elementType: element,
                                                          source: source,
                                                          label: label,
                                                          rewriteMap: rewriteMap,
                                                          primitiveAliases: primitiveAliases,
                                                          vmIdentifier: vmIdentifier) {
            var lines = conversion.lines
            lines.append("let \(name) = \(conversion.expression)")
            return lines
        }
        let fallbackArray = rewriteConfiguredAny(in: type.canonicalDescription(), using: rewriteMap)
        return [
            "let \(name): \(fallbackArray) = try \(vmIdentifier).withHost(\(source), typeName: \"\(fallbackArray)\", as: \(fallbackArray).self) { host in",
            "    host",
            "}"
        ]
    case .dictionary(let key, let value):
        if let conversion = emitDictionaryConversionExpression(resultNameStem: name,
                                                               dictionaryType: type,
                                                               keyType: key,
                                                               valueType: value,
                                                               source: source,
                                                               label: label,
                                                               rewriteMap: rewriteMap,
                                                               primitiveAliases: primitiveAliases,
                                                               vmIdentifier: vmIdentifier) {
            var lines = conversion.lines
            lines.append("let \(name) = \(conversion.expression)")
            return lines
        }
        let canonicalDict = rewriteConfiguredAny(in: type.canonicalDescription(), using: rewriteMap)
        return [
            "let \(name): \(canonicalDict) = try \(vmIdentifier).withHost(\(source), typeName: \"\(canonicalDict)\", as: \(canonicalDict).self) { host in",
            "    host",
            "}"
        ]
    case .host(var canonical):
        canonical = rewriteConfiguredAny(in: canonical, using: rewriteMap)
        if canonical == "Any" {
            return [
                "let \(name): Any = try \(vmIdentifier).convertToAny(\(source))"
            ]
        }
        return [
            "let \(name): \(canonical) = try \(vmIdentifier).withHost(\(source), typeName: \"\(canonical)\", as: \(canonical).self) { host in",
            "    host",
            "}"
        ]
    case .closure:
        fatalError("closure types should be handled before bindingLinesDeclaring")
    }
}

fileprivate func bindingLinesAssigning(name: String,
                                       type: TypeName,
                                       source: String,
                                       label: String,
                                       rewriteMap: [String: String],
                                       primitiveAliases: [String: String],
                                       vmIdentifier: String = "vm") -> [String] {
    switch typeCategory(for: type, primitiveAliases: primitiveAliases) {
    case .primitive(let primitive):
        switch primitive {
        case .int:
            return ["\(name) = try \(source).expectInt(\"\(label)\")"]
        case .double:
            return ["\(name) = try \(source).expectDouble(\"\(label)\")"]
        case .bool:
            return ["\(name) = try \(source).expectBool(\"\(label)\")"]
        case .string:
            return ["\(name) = try \(source).expectString(\"\(label)\")"]
        case .void:
            return []
        }
    case .array(let element):
        if let conversion = emitArrayConversionExpression(resultNameStem: name,
                                                          arrayType: type,
                                                          elementType: element,
                                                          source: source,
                                                          label: label,
                                                          rewriteMap: rewriteMap,
                                                          primitiveAliases: primitiveAliases,
                                                          vmIdentifier: vmIdentifier) {
            var lines = conversion.lines
            lines.append("\(name) = \(conversion.expression)")
            return lines
        }
        let fallbackArray = rewriteConfiguredAny(in: type.canonicalDescription(), using: rewriteMap)
        return [
            "\(name) = try \(vmIdentifier).withHost(\(source), typeName: \"\(fallbackArray)\", as: \(fallbackArray).self) { host in",
            "    host",
            "}"
        ]
    case .dictionary(let key, let value):
        if let conversion = emitDictionaryConversionExpression(resultNameStem: name,
                                                               dictionaryType: type,
                                                               keyType: key,
                                                               valueType: value,
                                                               source: source,
                                                               label: label,
                                                               rewriteMap: rewriteMap,
                                                               primitiveAliases: primitiveAliases,
                                                               vmIdentifier: vmIdentifier) {
            var lines = conversion.lines
            lines.append("\(name) = \(conversion.expression)")
            return lines
        }
        let canonicalDict = rewriteConfiguredAny(in: type.canonicalDescription(), using: rewriteMap)
        return [
            "\(name) = try \(vmIdentifier).withHost(\(source), typeName: \"\(canonicalDict)\", as: \(canonicalDict).self) { host in",
            "    host",
            "}"
        ]
    case .host(var canonical):
        canonical = rewriteConfiguredAny(in: canonical, using: rewriteMap)
        if canonical == "Any" {
            return ["\(name) = try \(vmIdentifier).convertToAny(\(source))"]
        }
        return [
            "\(name) = try \(vmIdentifier).withHost(\(source), typeName: \"\(canonical)\", as: \(canonical).self) { host in",
            "    host",
            "}"
        ]
    case .closure:
        fatalError("closure types should be handled before bindingLinesAssigning")
    }
}

fileprivate func emitValueConversion(source: String,
                                      type: TypeName,
                                      label: String,
                                      tempNameStem: String,
                                      rewriteMap: [String: String],
                                      primitiveAliases: [String: String],
                                      vmIdentifier: String) -> (lines: [String], expression: String)? {
    if let wrapped = optionalWrappedType(type) {
        let optionalName = sanitizeIdentifier("\(tempNameStem)_optional")
        let optionalTypeDescription = rewriteConfiguredAny(in: type.canonicalDescription(), using: rewriteMap)
        var lines: [String] = []
        lines.append("let \(optionalName): \(optionalTypeDescription)")
        lines.append("if case .nilValue = \(source) {")
        lines.append("    \(optionalName) = nil")
        lines.append("} else {")
        guard let inner = emitValueConversion(source: source,
                                              type: wrapped,
                                              label: label,
                                              tempNameStem: "\(tempNameStem)_wrapped",
                                              rewriteMap: rewriteMap,
                                              primitiveAliases: primitiveAliases,
                                              vmIdentifier: vmIdentifier) else { return nil }
        lines.append(contentsOf: indentLines(inner.lines, indent: 1))
        lines.append("    \(optionalName) = \(inner.expression)")
        lines.append("}")
        return (lines, optionalName)
    }
    let tempName = sanitizeIdentifier(tempNameStem)
    var conversion = bindingLinesDeclaring(name: tempName,
                                           type: type,
                                           source: source,
                                           label: label,
                                           rewriteMap: rewriteMap,
                                           primitiveAliases: primitiveAliases,
                                           vmIdentifier: vmIdentifier)
    if conversion.isEmpty {
        conversion = ["let \(tempName) = \(source)"]
    }
    return (conversion, tempName)
}

fileprivate func emitValueSynthesisExpression(resultNameStem: String,
                                              type: TypeName,
                                              expression: String,
                                              rewriteMap: [String: String],
                                              exportedTypeNameProvider: (String) -> String,
                                              primitiveAliases: [String: String],
                                              vmIdentifier: String,
                                              hostTypeIDExpression: String? = nil,
                                              runtimeTypeName: String? = nil) -> (lines: [String], expression: String)? {
    if let wrapped = optionalWrappedType(type) {
        let optionalName = sanitizeIdentifier("\(resultNameStem)_optionalValue")
        var lines: [String] = []
        lines.append("var \(optionalName): Value")
        lines.append("if let unwrapped = \(expression) {")
        guard let inner = emitValueSynthesisExpression(resultNameStem: "\(resultNameStem)_wrapped",
                                                       type: wrapped,
                                                       expression: "unwrapped",
                                                       rewriteMap: rewriteMap,
                                                       exportedTypeNameProvider: exportedTypeNameProvider,
                                                       primitiveAliases: primitiveAliases,
                                                       vmIdentifier: vmIdentifier,
                                                       hostTypeIDExpression: hostTypeIDExpression,
                                                       runtimeTypeName: runtimeTypeName) else { return nil }
        lines.append(contentsOf: indentLines(inner.lines, indent: 1))
        lines.append("    \(optionalName) = \(inner.expression)")
        lines.append("} else {")
        lines.append("    \(optionalName) = .nilValue")
        lines.append("}")
        return (lines, optionalName)
    }
    switch typeCategory(for: type, primitiveAliases: primitiveAliases) {
    case .primitive(let primitive):
        switch primitive {
        case .void:
            return ([], ".bool(true)")
        case .int:
            return ([], ".int(\(expression))")
        case .double:
            return ([], ".double(\(expression))")
        case .bool:
            return ([], ".bool(\(expression))")
        case .string:
            return ([], ".string(\(expression))")
        }
    case .host(var canonical):
        canonical = rewriteConfiguredAny(in: canonical, using: rewriteMap)
        if let hostTypeIDExpression {
            return ([], ".host(HostRef(box: makeOpaqueBox(\(expression))), \(hostTypeIDExpression))")
        }
        let runtimeName = runtimeTypeName ?? exportedTypeNameProvider(canonical)
        return ([], ".host(HostRef(box: makeOpaqueBox(\(expression))), \(vmIdentifier).hostTypeID(named: \"\(runtimeName)\"))")
    case .array(let element):
        let elementsVar = sanitizeIdentifier("\(resultNameStem)_elements")
        let elementVar = sanitizeIdentifier("\(resultNameStem)_element")
        var lines: [String] = []
        lines.append("var \(elementsVar): [Value] = []")
        lines.append("\(elementsVar).reserveCapacity(\(expression).count)")
        lines.append("for \(elementVar) in \(expression) {")
        guard let elementSynthesis = emitValueSynthesisExpression(resultNameStem: "\(resultNameStem)_element",
                                                                  type: element,
                                                                  expression: elementVar,
                                                                  rewriteMap: rewriteMap,
                                                                  exportedTypeNameProvider: exportedTypeNameProvider,
                                                                  primitiveAliases: primitiveAliases,
                                                                  vmIdentifier: vmIdentifier) else { return nil }
        lines.append(contentsOf: indentLines(elementSynthesis.lines, indent: 1))
        lines.append("    \(elementsVar).append(\(elementSynthesis.expression))")
        lines.append("}")
        let boxName = sanitizeIdentifier("\(resultNameStem)_arrayBox")
        lines.append("let \(boxName) = ArrBox(\(elementsVar))")
        return (lines, ".array(\(boxName))")
    case .dictionary(let key, let value):
        guard case .primitive(.string) = typeCategory(for: key, primitiveAliases: primitiveAliases) else { return nil }
        let storageVar = sanitizeIdentifier("\(resultNameStem)_storage")
        let keyVar = sanitizeIdentifier("\(resultNameStem)_key")
        let valueVar = sanitizeIdentifier("\(resultNameStem)_value")
        var lines: [String] = []
        lines.append("var \(storageVar): [String: Value] = [:]")
        lines.append("for (\(keyVar), \(valueVar)) in \(expression) {")
        guard let valueSynthesis = emitValueSynthesisExpression(resultNameStem: "\(resultNameStem)_value",
                                                                type: value,
                                                                expression: valueVar,
                                                                rewriteMap: rewriteMap,
                                                                exportedTypeNameProvider: exportedTypeNameProvider,
                                                                primitiveAliases: primitiveAliases,
                                                                vmIdentifier: vmIdentifier) else { return nil }
        lines.append(contentsOf: indentLines(valueSynthesis.lines, indent: 1))
        lines.append("    \(storageVar)[\(keyVar)] = \(valueSynthesis.expression)")
        lines.append("}")
        let dictBoxName = sanitizeIdentifier("\(resultNameStem)_dictBox")
        lines.append("let \(dictBoxName) = DictBox(\(storageVar))")
        return (lines, ".dict(\(dictBoxName))")
    case .closure:
        return nil
    }
}

fileprivate func emitArrayConversionExpression(resultNameStem: String,
                                               arrayType: TypeName,
                                               elementType: TypeName,
                                               source: String,
                                               label: String,
                                               rewriteMap: [String: String],
                                               primitiveAliases: [String: String],
                                               vmIdentifier: String) -> (lines: [String], expression: String)? {
    let canonicalLabel = escapeForSwiftLiteral(label)
    let arrayTypeDescription = rewriteConfiguredAny(in: arrayType.canonicalDescription(), using: rewriteMap)
    let arrayBoxName = sanitizeIdentifier("\(resultNameStem)_arrayBox")
    let resultVar = sanitizeIdentifier("\(resultNameStem)_arrayValue")
    var lines: [String] = []
    lines.append("guard case let .array(\(arrayBoxName)) = \(source) else { throw VMError.typeError(\"\(canonicalLabel) expects \(arrayTypeDescription)\") }")
    lines.append("var \(resultVar): \(arrayTypeDescription) = []")
    lines.append("\(resultVar).reserveCapacity(\(arrayBoxName).elements.count)")
    lines.append("for (index, elementValue) in \(arrayBoxName).elements.enumerated() {")
    let elementLabel = "\(canonicalLabel)[\\(index)]"
    guard let elementConversion = emitValueConversion(source: "elementValue",
                                                      type: elementType,
                                                      label: elementLabel,
                                                      tempNameStem: "\(resultNameStem)_element",
                                                      rewriteMap: rewriteMap,
                                                      primitiveAliases: primitiveAliases,
                                                      vmIdentifier: vmIdentifier) else { return nil }
    lines.append(contentsOf: indentLines(elementConversion.lines, indent: 1))
    lines.append("    \(resultVar).append(\(elementConversion.expression))")
    lines.append("}")
    return (lines, resultVar)
}

fileprivate func emitDictionaryConversionExpression(resultNameStem: String,
                                                    dictionaryType: TypeName,
                                                    keyType: TypeName,
                                                    valueType: TypeName,
                                                    source: String,
                                                    label: String,
                                                    rewriteMap: [String: String],
                                                    primitiveAliases: [String: String],
                                                    vmIdentifier: String) -> (lines: [String], expression: String)? {
    guard case .primitive(.string) = typeCategory(for: keyType, primitiveAliases: primitiveAliases) else {
        return nil
    }
    let canonicalLabel = escapeForSwiftLiteral(label)
    let dictionaryTypeDescription = rewriteConfiguredAny(in: dictionaryType.canonicalDescription(), using: rewriteMap)
    let dictBoxName = sanitizeIdentifier("\(resultNameStem)_dictBox")
    let resultVar = sanitizeIdentifier("\(resultNameStem)_dictValue")
    var lines: [String] = []
    lines.append("guard case let .dict(\(dictBoxName)) = \(source) else { throw VMError.typeError(\"\(canonicalLabel) expects \(dictionaryTypeDescription)\") }")
    lines.append("var \(resultVar): \(dictionaryTypeDescription) = [:]")
    lines.append("for (rawKey, rawValue) in \(dictBoxName).storage {")
    let valueLabel = "\(canonicalLabel)[\\(rawKey)]"
    guard let valueConversion = emitValueConversion(source: "rawValue",
                                                    type: valueType,
                                                    label: valueLabel,
                                                    tempNameStem: "\(resultNameStem)_value",
                                                    rewriteMap: rewriteMap,
                                                    primitiveAliases: primitiveAliases,
                                                    vmIdentifier: vmIdentifier) else { return nil }
    lines.append(contentsOf: indentLines(valueConversion.lines, indent: 1))
    lines.append("    \(resultVar)[rawKey] = \(valueConversion.expression)")
    lines.append("}")
    return (lines, resultVar)
}

fileprivate func escapeForSwiftLiteral(_ text: String) -> String {
    text
        .replacingOccurrences(of: "\\", with: "\\\\")
        .replacingOccurrences(of: "\"", with: "\\\"")
}

// Property getter return conversion (no initializer host type override needed)
fileprivate func emitReturnValueForProperty(type: TypeName,
                                            expression: String,
                                            rewriteMap: [String: String],
                                            exportedTypeNameProvider: (String) -> String,
                                            primitiveAliases: [String: String],
                                            runtimeTypeName: String? = nil,
                                            vmIdentifier: String = "vm") -> [String] {
    if let wrapped = optionalWrappedType(type) {
        var lines: [String] = []
        lines.append("switch \(expression) {")
        lines.append("case .some(let unwrapped):")
        lines.append(contentsOf: indentLines(emitReturnValueForProperty(type: wrapped,
                                                                        expression: "unwrapped",
                                                                        rewriteMap: rewriteMap,
                                                                        exportedTypeNameProvider: exportedTypeNameProvider,
                                                                        primitiveAliases: primitiveAliases,
                                                                        runtimeTypeName: runtimeTypeName),
                                             indent: 1))
        lines.append("case .none:")
        lines.append("    return .nilValue")
        lines.append("}")
        return lines
    }
    switch typeCategory(for: type, primitiveAliases: primitiveAliases) {
    case .primitive(let primitive):
        switch primitive {
        case .void:
            return ["return .bool(true)"]
        case .int:
            return ["return .int(\(expression))"]
        case .double:
            return ["return .double(\(expression))"]
        case .bool:
            return ["return .bool(\(expression))"]
        case .string:
            return ["return .string(\(expression))"]
        }
    case .host(var canonical):
        canonical = rewriteConfiguredAny(in: canonical, using: rewriteMap)
        let runtimeName = runtimeTypeName ?? exportedTypeNameProvider(canonical)
        return ["return .host(HostRef(box: makeOpaqueBox(\(expression))), \(vmIdentifier).hostTypeID(named: \"\(runtimeName)\"))"]
    case .array, .dictionary:
        let stem = sanitizeIdentifier("\(expression)_value")
        if let synthesis = emitValueSynthesisExpression(resultNameStem: stem,
                                                       type: type,
                                                       expression: expression,
                                                       rewriteMap: rewriteMap,
                                                       exportedTypeNameProvider: exportedTypeNameProvider,
                                                       primitiveAliases: primitiveAliases,
                                                       vmIdentifier: vmIdentifier,
                                                       runtimeTypeName: runtimeTypeName) {
            var lines = synthesis.lines
            lines.append("return \(synthesis.expression)")
            return lines
        }
        let canonical = rewriteConfiguredAny(in: type.canonicalDescription(), using: rewriteMap)
        let runtimeName = runtimeTypeName ?? exportedTypeNameProvider(canonical)
        return ["return .host(HostRef(box: makeOpaqueBox(\(expression))), \(vmIdentifier).hostTypeID(named: \"\(runtimeName)\"))"]
    case .closure:
        fatalError("closure return types are not supported in property getters yet")
    }
}

private func sanitizeIdentifier(_ raw: String) -> String {
    guard !raw.isEmpty else { return "" }
    let allowed = CharacterSet.alphanumerics.union(CharacterSet(charactersIn: "_"))
    var result = ""
    for scalar in raw.unicodeScalars {
        if allowed.contains(scalar) {
            result.append(Character(scalar))
        } else {
            let code = String(format: "%04X", scalar.value)
            result.append("_u\(code)_")
        }
    }
    if let first = result.first, first.isNumber {
        result = "_" + result
    }
    return result
}

// Escape reserved Swift keywords used as identifiers in generated source.
private func escapeIdentifierIfNeeded(_ name: String) -> String {
    // Subset of Swift keywords and commonly conflicting identifiers.
    let keywords: Set<String> = [
        "associatedtype","class","deinit","enum","extension","fileprivate","func","import","init","inout","internal",
        "let","open","operator","private","protocol","public","rethrows","static","struct","subscript","typealias","var",
        "break","case","continue","default","defer","do","else","fallthrough","for","guard","if","in","repeat","return",
        "switch","where","while","as","Any","catch","false","is","nil","super","self","Self","throw","throws","true","try",
        "some","any"
    ]
    if keywords.contains(name) { return "`\(name)`" }
    return name
}

private func memberAccessIdentifier(for name: String) -> String {
    if isOperatorName(name) { return "`\(name)`" }
    let allowed = CharacterSet.alphanumerics.union(CharacterSet(charactersIn: "_"))
    if name.unicodeScalars.contains(where: { !allowed.contains($0) }) {
        return "`\(name)`"
    }
    return escapeIdentifierIfNeeded(name)
}

private func indentLines(_ lines: [String], indent: Int) -> [String] {
    guard indent > 0 else { return lines }
    let prefix = String(repeating: "    ", count: indent)
    return lines.map { prefix + $0 }
}

// Replace interface placeholder generics with configured defaults.
// We currently map Foundation.FloatingPointFormatStyle<Swift.Any> → <Swift.Double>.
// Rewrite placeholder `<Swift.Any>` occurrences to configured default
// concrete types for known generic surfaces (e.g., `T: BinaryFloatingPoint`),
// when the config provides a single-type domain.
private func rewriteConfiguredAny(in canonical: String, using map: [String: String]) -> String {
    var out = canonical
    // First, apply configured whole-type rewrites (e.g., Foo<Swift.Any> -> Foo<Swift.Double>)
    for (needle, replacement) in map {
        if out.contains(needle) {
            out = out.replacingOccurrences(of: needle, with: replacement)
        }
    }
    // Then, normalize bare Swift.Any occurrences to Any to avoid parser complaints in generics
    // and typed binding sites (e.g., Dictionary<..., Swift.Any> -> Dictionary<..., Any>).
    if out.contains("Swift.Any") {
        out = out.replacingOccurrences(of: "Swift.Any", with: "Any")
    }
    return out
}

// Detect whether a method base name is an operator (composed solely of
// operator characters). This helps us decide to emit infix/prefix forms.
private func isOperatorName(_ name: String) -> Bool {
    // Minimal, safe superset of common Swift operator characters.
    // Enough to catch ==, !=, <, >, <=, >=, &&, ||, !, +, -, *, /, %, ^, ~, |=, etc.
    let operatorChars = CharacterSet(charactersIn: "/=+-!*%<>&|^~")
    // All scalars must be operator characters
    return !name.isEmpty && name.unicodeScalars.allSatisfy { operatorChars.contains($0) }
}

// Encode an operator token into a stable, identifier-friendly suffix.
// We encode per-character to avoid needing a complete operator table.
private func encodeOperatorToken(_ op: String) -> String {
    var out: [String] = []
    for ch in op {
        switch ch {
        case "+": out.append("plus")
        case "-": out.append("minus")
        case "*": out.append("star")
        case "/": out.append("slash")
        case "%": out.append("percent")
        case "=": out.append("eq")
        case "!": out.append("bang")
        case "<": out.append("lt")
        case ">": out.append("gt")
        case "&": out.append("amp")
        case "|": out.append("pipe")
        case "^": out.append("caret")
        case "~": out.append("tilde")
        case ".": out.append("dot")
        case "?": out.append("q")
        default:
            // Fallback to unicode scalar value if an unexpected operator char appears
            let s = String(ch).unicodeScalars.map { String($0.value) }.joined(separator: "_")
            out.append("u_\(s)")
        }
    }
    return out.joined(separator: "_")
}

// MARK: - Availability helpers (file scope)
fileprivate func strictAvailabilityConditionFromAttributes(_ attributes: [String]) -> String? {
    if attributes.isEmpty { return nil }
    var maxVersions: [String: String] = [:]
    let platforms = ["macOS", "iOS", "tvOS", "watchOS", "visionOS"]
    for attr in attributes {
        guard let open = attr.firstIndex(of: "("), let close = attr.lastIndex(of: ")"), open < close else { continue }
        let inside = String(attr[attr.index(after: open)..<close])
        let parts = inside.split(separator: ",")
        for raw in parts {
            let token = raw.trimmingCharacters(in: .whitespaces)
            for plat in platforms {
                if token.hasPrefix(plat + " ") {
                    let rest = token.dropFirst((plat + " ").count)
                    let ver = rest.split(whereSeparator: { $0 == " " || $0 == ":" }).first.map(String.init)
                    if let ver, Double(ver.filter({ $0 == "." || ($0 >= "0" && $0 <= "9") })) != nil {
                        if let existing = maxVersions[plat] {
                            if compareVersionStrings(ver, existing) == .orderedDescending { maxVersions[plat] = ver }
                        } else {
                            maxVersions[plat] = ver
                        }
                    }
                }
            }
        }
    }
    if maxVersions.isEmpty { return nil }
    var clauses: [String] = []
    for plat in platforms { if let ver = maxVersions[plat] { clauses.append("\(plat) \(ver)") } }
    clauses.append("*")
    return "#available(\(clauses.joined(separator: ", ")))"
}

fileprivate func compareVersionStrings(_ a: String, _ b: String) -> ComparisonResult {
    func split(_ s: String) -> [Int] { s.split(separator: ".").compactMap { Int($0) } }
    let va = split(a), vb = split(b)
    let n = max(va.count, vb.count)
    for i in 0..<n {
        let ai = i < va.count ? va[i] : 0
        let bi = i < vb.count ? vb[i] : 0
        if ai != bi { return ai < bi ? .orderedAscending : .orderedDescending }
    }
    return .orderedSame
}
