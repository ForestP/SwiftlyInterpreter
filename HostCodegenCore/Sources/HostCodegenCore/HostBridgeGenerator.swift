//
//  HostBridgeGenerator.swift
//  HostCodegenCore
//
//  Created by Forest Plasencia on 9/19/25.
//

import HostSurfaceKit
import Foundation

public struct HostBridgeGenerationConfig {
    /// Map of receiver canonical type name → generic parameter name → allowed TypeNames.
    var specializationsByType: [String: [String: [TypeName]]]
    /// Map of receiver canonical type name → property config key → generic parameter name → allowed TypeNames.
    var propertySpecializationsByType: [String: [String: [String: [TypeName]]]]
    /// Map of canonical type name → set of protocol canonical names it conforms to.
    var conformanceTable: [String: Set<String>]
    /// Map of canonical type name → exported runtime registration name.
    var exportedTypeNames: [String: String]
    /// Map of protocol canonical names → concrete witness type canonical names.
    var protocolWitnesses: [String: [String]]
    /// Maximum number of emitted thunks per selector before trimming.
    var maxSpecializationsPerSelector: Int
    /// Maximum entries allowed in a single generic parameter domain before trimming.
    var maxDomainPerGenericParam: Int
    /// Set of selectors (keyed by canonical receiver name) exempt from trimming caps.
    var forceSpecializations: [String: Set<String>]
    /// Map of canonical type alias → canonical primitive target (e.g. Foundation.TimeInterval → Swift.Double).
    var primitiveTypeAliases: [String: String]
    /// Map of canonical type names → declared generic placeholder names (from interface inventory).
    var genericPlaceholderMap: [String: [String]]

    public static let defaultMaxSpecializationsPerSelector = 12
    public static let defaultMaxDomainPerGenericParam = 6

    public init(
        specializationsByType: [String: [String: [TypeName]]] = [:],
        propertySpecializationsByType: [String: [String: [String: [TypeName]]]] = [:],
        conformanceTable: [String: Set<String>] = [:],
        exportedTypeNames: [String: String] = [:],
        protocolWitnesses: [String: [String]] = [:],
        maxSpecializationsPerSelector: Int = HostBridgeGenerationConfig.defaultMaxSpecializationsPerSelector,
        maxDomainPerGenericParam: Int = HostBridgeGenerationConfig.defaultMaxDomainPerGenericParam,
        forceSpecializations: [String: Set<String>] = [:],
        primitiveTypeAliases: [String: String] = [:],
        genericPlaceholderMap: [String: [String]] = [:]
    ) {
        self.specializationsByType = specializationsByType
        self.propertySpecializationsByType = propertySpecializationsByType
        self.conformanceTable = conformanceTable
        self.exportedTypeNames = exportedTypeNames
        self.protocolWitnesses = protocolWitnesses
        self.maxSpecializationsPerSelector = maxSpecializationsPerSelector
        self.maxDomainPerGenericParam = maxDomainPerGenericParam
        self.forceSpecializations = forceSpecializations
        self.primitiveTypeAliases = primitiveTypeAliases
        self.genericPlaceholderMap = genericPlaceholderMap
    }

    func isForced(_ descriptor: HostMethodDescriptor) -> Bool {
        let receiver = descriptor.receiver.canonicalDescription()
        guard let forcedSelectors = forceSpecializations[receiver] else { return false }
        if forcedSelectors.contains("*") { return true }
        return forcedSelectors.contains(descriptor.selector)
    }
}

struct HostBridgeMethodArtifact {
    let descriptor: HostMethodDescriptor
    let render: HostMethodRender
    let origin: HostBridgeSpecializationOrigin
    let domainSizes: [String: Int]
}

struct HostBridgePropertyArtifact {
    let descriptor: HostPropertyDescriptor
    let render: HostPropertyRender
    let origin: HostBridgeSpecializationOrigin
    let domainSizes: [String: Int]
}

public struct HostBridgeGenerationResult {
    let methodDescriptors: [HostMethodDescriptor]
    let propertyDescriptors: [HostPropertyDescriptor]
    let methodArtifacts: [HostBridgeMethodArtifact]
    let propertyArtifacts: [HostBridgePropertyArtifact]
    public let diagnostics: [HostSurfaceDiagnostic]
}

struct HostBridgeGenerator {
    var config: HostBridgeGenerationConfig
    var autoSpecializationStrategies: [AutoSpecializationStrategy]
    var closureSelectorAllowlist: [String: Set<String>]
    private let unsupportedHostTypeNames: Set<String> = ["UTF8Span", "Swift.UTF8Span"]

    init(config: HostBridgeGenerationConfig = .init(),
         autoSpecializationStrategies: [AutoSpecializationStrategy] = [],
         closureSelectorAllowlist: [String: Set<String>] = [:]) {
        self.config = config
        self.autoSpecializationStrategies = autoSpecializationStrategies
        self.closureSelectorAllowlist = closureSelectorAllowlist
    }

    func generate(from surface: ApiSurface) -> HostBridgeGenerationResult {
        var diagnostics: [HostSurfaceDiagnostic] = []

        let builder = HostMethodDescriptorBuilder()
        var (builtMethods, propertyDescriptorsRaw, builderDiagnostics) = builder.build(from: surface,
                                                                                        conformanceTable: config.conformanceTable)
        diagnostics.append(contentsOf: builderDiagnostics)

        if !config.protocolWitnesses.isEmpty {
            let expansion = applyProtocolWitnesses(methods: builtMethods,
                                                   properties: propertyDescriptorsRaw)
            builtMethods = expansion.methods
            propertyDescriptorsRaw = expansion.properties
            diagnostics.append(contentsOf: expansion.diagnostics)
        }

        // Filter out methods that require a KeyPath rooted in the receiver type.
        // These signatures typically look like `KeyPath<Self, Value>` in protocol extensions
        // and aren’t invocable on existential receivers in generated thunks.
        // Also drop Objective‑C bridging-only surfaces (e.g., _bridgeToObjectiveC, NS* types)
        let filteredMethods = builtMethods.filter { descriptor in
            if requiresReceiverRootedKeyPath(descriptor) { return false }
            if let reason = unsupportedParameterReason(for: descriptor) {
                diagnostics.append(.init(kind: .unsupported("Skipping \(descriptor.displayName): \(reason)"), line: nil))
                return false
            }
            if let unsupported = unsupportedHostTypeUsage(in: descriptor) {
                diagnostics.append(.init(kind: .unsupported("Skipping \(descriptor.displayName): references unsupported host type \(unsupported)"), line: nil))
                return false
            }
            if isObjCBridgeOnly(descriptor) { return false }
            return true
        }
        // Deduplicate identical method signatures to avoid duplicate symbol emission
        var seenMethodKeys: Set<String> = []
        var methodDescriptors: [HostMethodDescriptor] = []
        methodDescriptors.reserveCapacity(filteredMethods.count)
        for m in filteredMethods {
            let receiver = m.receiver.canonicalDescription()
            let kindStr: String = {
                switch m.kind {
                case .initializer: return "init"
                case .instance: return "instance"
                case .static: return "static"
                }
            }()
            let paramTypes = m.parameters.map { $0.type.canonicalDescription() }.joined(separator: "|")
            let key = "\(receiver)::\(kindStr)::\(m.selector)::\(paramTypes)"
            if !seenMethodKeys.contains(key) {
                seenMethodKeys.insert(key)
                methodDescriptors.append(m)
            } else {
                diagnostics.append(.init(kind: .unsupported("Deduplicated duplicate method signature: \(m.displayName)"), line: nil))
            }
        }

        let (availableMethods, methodAvailabilityDiagnostics) = removeUnavailableMethods(from: methodDescriptors)
        diagnostics.append(contentsOf: methodAvailabilityDiagnostics)
        methodDescriptors = availableMethods

        // Build a rewrite map from config: for any generic surface that has a
        // single-type domain per generic parameter, emit a canonical rewrite
        // from `<Swift.Any>` to that concrete type so typed binding sites that
        // surfaced as `…<Swift.Any>` can be normalized.
        let rewriteMap = defaultAnyRewriteMap(from: config)

        let exportedNameProvider: (String) -> String = { canonical in
            return self.exportedTypeName(for: canonical)
        }

        var methodArtifacts: [HostBridgeMethodArtifact] = []
        methodArtifacts.reserveCapacity(methodDescriptors.count)

        for descriptor in methodDescriptors {
            if descriptor.baseName.contains(".") {
                diagnostics.append(.init(kind: .unsupported("Skipping dotted operator surface \(descriptor.displayName)"), line: nil))
                continue
            }
            let isForced = config.isForced(descriptor)
            let allowedResult = allowedSpecializations(for: descriptor,
                                                       surface: surface,
                                                       isForced: isForced)
            diagnostics.append(contentsOf: allowedResult.diagnostics)
            guard !allowedResult.shouldSkip else { continue }
            let effectiveForced = isForced || allowedResult.shouldForce

            let generator = HostMethodGenerator(method: descriptor,
                                                rewriteMap: rewriteMap,
                                                exportedTypeNameProvider: exportedNameProvider,
                                                primitiveTypeAliases: config.primitiveTypeAliases,
                                                genericPlaceholderMap: config.genericPlaceholderMap)
            do {
                let planned = try generator.planSpecializations(allowed: allowedResult.map)
                let trimming = trimSpecializations(planned,
                                                   descriptor: descriptor,
                                                   isForced: effectiveForced)
                diagnostics.append(contentsOf: trimming.diagnostics)
                guard !trimming.primary.isEmpty || !trimming.overflow.isEmpty else { continue }
                let render = try generator.render(specializations: trimming.primary,
                                                  overflow: trimming.overflow)
                if render.fallback != nil {
                    let msg = "Emitting fallback thunk for \(descriptor.displayName) due to cap \(config.maxSpecializationsPerSelector)"
                    diagnostics.append(.init(kind: .unsupported(msg), line: nil))
                }
                let domainSizes = Dictionary(uniqueKeysWithValues: allowedResult.map.map { ($0.key, $0.value.count) })
                methodArtifacts.append(.init(descriptor: descriptor,
                                             render: render,
                                             origin: allowedResult.origin,
                                             domainSizes: domainSizes))
            } catch let plannerError as GenericSpecializationPlannerError {
                if plannerError == .noValidSpecializations {
                    diagnostics.append(.init(kind: .unsupported("Skipping \(descriptor.displayName): constraints have no valid specialization"), line: nil))
                    continue
                }
                diagnostics.append(.init(kind: .unsupported("Failed to render host method \(descriptor.displayName): \(plannerError)"), line: nil))
            } catch {
                diagnostics.append(.init(kind: .unsupported("Failed to render host method \(descriptor.displayName): \(error)"),
                                         line: nil))
            }
        }

        // Deduplicate properties by receiver/kind/name/type
        var seenPropertyKeys: Set<String> = []
        // Drop properties that reference Objective‑C-only types as well
        var propertyDescriptors: [HostPropertyDescriptor] = propertyDescriptorsRaw.filter { p in
            guard !isObjCOnlyType(p.receiver) && !isObjCOnlyType(p.type) else {
                diagnostics.append(.init(kind: .unsupported("Skipping Objective-C-only property surface: \(p.displayName)"), line: nil))
                return false
            }
            if let unsupported = unsupportedHostTypeDescription(for: p.type) {
                diagnostics.append(.init(kind: .unsupported("Skipping property \(p.displayName): references unsupported host type \(unsupported)"), line: nil))
                return false
            }
            let receiver = p.receiver.canonicalDescription()
            let kindStr = (p.kind == .static) ? "static" : "instance"
            let key = "\(receiver)::\(kindStr)::\(p.name)::\(p.type.canonicalDescription())"
            if seenPropertyKeys.contains(key) {
                diagnostics.append(.init(kind: .unsupported("Deduplicated duplicate property signature: \(receiver).\(p.name)"), line: nil))
                return false
            }
            seenPropertyKeys.insert(key)
            return true
        }

        let (availableProperties, propertyAvailabilityDiagnostics) = removeUnavailableProperties(from: propertyDescriptors)
        diagnostics.append(contentsOf: propertyAvailabilityDiagnostics)
        propertyDescriptors = availableProperties

        var propertyArtifacts: [HostBridgePropertyArtifact] = []
        propertyArtifacts.reserveCapacity(propertyDescriptors.count)

        for descriptor in propertyDescriptors {
            let allowed = allowedSpecializations(for: descriptor,
                                                  surface: surface)
            diagnostics.append(contentsOf: allowed.diagnostics)
            if allowed.shouldSkip { continue }

            let generator = HostPropertyGenerator(property: descriptor,
                                                  rewriteMap: rewriteMap,
                                                  exportedTypeNameProvider: exportedNameProvider,
                                                  primitiveTypeAliases: config.primitiveTypeAliases,
                                                  genericPlaceholderMap: config.genericPlaceholderMap)
            do {
                let render = try generator.render(allowedSpecializations: allowed.map)
                let usesGetterFallback = render.getter.fallback != nil
                let usesSetterFallback = render.setter?.fallback != nil
                if usesGetterFallback || usesSetterFallback {
                    let msg = "Emitting fallback thunk for property \(descriptor.displayName) due to cap \(config.maxSpecializationsPerSelector)"
                    diagnostics.append(.init(kind: .unsupported(msg), line: nil))
                }
                let domainSizes = Dictionary(uniqueKeysWithValues: allowed.map.map { ($0.key, $0.value.count) })
                propertyArtifacts.append(.init(descriptor: descriptor,
                                               render: render,
                                               origin: allowed.origin,
                                               domainSizes: domainSizes))
            } catch let plannerError as GenericSpecializationPlannerError {
                if plannerError == .noValidSpecializations {
                    diagnostics.append(.init(kind: .unsupported("Skipping property \(descriptor.displayName): constraints have no valid specialization"), line: nil))
                    continue
                }
                diagnostics.append(.init(kind: .unsupported("Failed to render host property \(descriptor.displayName): \(plannerError)"), line: nil))
            } catch {
                diagnostics.append(.init(kind: .unsupported("Failed to render host property \(descriptor.displayName): \(error)"),
                                         line: nil))
            }
        }

        return HostBridgeGenerationResult(methodDescriptors: methodDescriptors,
                                          propertyDescriptors: propertyDescriptors,
                                          methodArtifacts: methodArtifacts,
                                          propertyArtifacts: propertyArtifacts,
                                          diagnostics: diagnostics)
    }

    private func applyProtocolWitnesses(methods: [HostMethodDescriptor],
                                        properties: [HostPropertyDescriptor]) -> (methods: [HostMethodDescriptor], properties: [HostPropertyDescriptor], diagnostics: [HostSurfaceDiagnostic]) {
        var diagnostics: [HostSurfaceDiagnostic] = []

        var resultMethods = methods
        var resultProperties = properties
        func signatureKey(receiver: TypeName, selector: String) -> String {
            receiver.canonicalDescription() + "::" + selector
        }
        var existingSignatures = Set(resultMethods.map { signatureKey(receiver: $0.receiver, selector: $0.selector) })
        func receiverGenerics(for type: TypeName) -> [GenericParameter] {
            var generics: [GenericParameter] = []
            var seen: Set<String> = []
            for argument in type.genericArguments {
                guard argument.genericArguments.isEmpty,
                      argument.functionSignature == nil,
                      argument.path.count == 1 else { continue }
                let name = argument.path[0]
                if seen.insert(name).inserted {
                    generics.append(GenericParameter(name: name, origin: .receiver))
                }
            }
            return generics
        }

        func makeDisplayName(receiver: TypeName, selector: String) -> String {
            let shortName = receiver.path.last ?? receiver.canonicalDescription()
            return "\(shortName).\(selector)"
        }

        func substitute(_ requirement: GenericRequirement, with substitutions: [String: TypeName]) -> GenericRequirement {
            switch requirement {
            case .sameType(let param, let type):
                return .sameType(param: param, type.applyingSubstitutions(substitutions))
            case .sameTypeParameters(let lhs, let rhs):
                return .sameTypeParameters(lhs: lhs, rhs: rhs)
            case .conformsTo(let param, let proto):
                return .conformsTo(param: param, protocolName: proto)
            case .sameTypeResolved(let lhs, let rhs):
                return .sameTypeResolved(lhs: lhs.applyingSubstitutions(substitutions),
                                         rhs: rhs.applyingSubstitutions(substitutions))
            case .conformsResolved(let type, let proto):
                return .conformsResolved(type: type.applyingSubstitutions(substitutions),
                                         protocolName: proto)
            }
        }

        func sanitize(_ type: TypeName, witness: TypeName) -> TypeName {
            let canonical = type.canonicalDescription()
            let target = witness.canonicalDescription()
            if !canonical.contains("Self") { return type }
            let replaced = canonical.replacingOccurrences(of: "Self", with: target)
            if let parsed = try? TypeNameParser.parse(replaced) {
                return parsed
            }
            return type
        }

        func duplicate(_ descriptor: HostMethodDescriptor,
                       to witness: TypeName) -> HostMethodDescriptor {
            let substitutions: [String: TypeName] = ["Self": witness]
            let substitutedParams = descriptor.parameters.map { param -> HostMethodParameter in
                let substitutedType = param.type.applyingSubstitutions(substitutions)
                let sanitized = sanitize(substitutedType, witness: witness)
                return HostMethodParameter(label: param.label,
                                           name: param.name,
                                           type: sanitized,
                                           isInout: param.isInout,
                                           isVariadic: param.isVariadic)
            }
            let substitutedReturn = descriptor.returnType.map {
                sanitize($0.applyingSubstitutions(substitutions), witness: witness)
            }
            let substitutedRequirements = descriptor.requirements.map { requirement -> GenericRequirement in
                let substituted = substitute(requirement, with: substitutions)
                switch substituted {
                case .sameType(let param, let type):
                    return .sameType(param: param, sanitize(type, witness: witness))
                case .sameTypeParameters:
                    return substituted
                case .conformsTo:
                    return substituted
                case .sameTypeResolved(let lhs, let rhs):
                    return .sameTypeResolved(lhs: sanitize(lhs, witness: witness),
                                             rhs: sanitize(rhs, witness: witness))
                case .conformsResolved(let type, let proto):
                    return .conformsResolved(type: sanitize(type, witness: witness),
                                             protocolName: proto)
                }
            }

            let receiverParameters = receiverGenerics(for: witness)
            var combinedParameters: [GenericParameter] = []
            var seen: Set<String> = []
            for param in receiverParameters + descriptor.genericParameters {
                if seen.insert(param.name).inserted {
                    combinedParameters.append(param)
                }
            }

            return HostMethodDescriptor(receiver: witness,
                                        kind: descriptor.kind,
                                        baseName: descriptor.baseName,
                                        selector: descriptor.selector,
                                        displayName: makeDisplayName(receiver: witness, selector: descriptor.selector),
                                        returnType: substitutedReturn,
                                        parameters: substitutedParams,
                                        genericParameters: combinedParameters,
                                        requirements: substitutedRequirements,
                                        conformanceTable: descriptor.conformanceTable,
                                        throwsKind: descriptor.throwsKind,
                                        availability: descriptor.availability)
        }

        if !config.protocolWitnesses.isEmpty {
            for (protocolName, witnessList) in config.protocolWitnesses {
                let protocolMethods = methods.filter { $0.receiver.canonicalDescription() == protocolName }
                guard !protocolMethods.isEmpty else { continue }
                var witnessTargets: [TypeName] = []
                var seenWitnessTargets: Set<String> = []
                for rawWitness in witnessList {
                    do {
                        let witnessType = try TypeNameParser.parse(rawWitness)
                        let canonical = witnessType.canonicalDescription()
                        if seenWitnessTargets.insert(canonical).inserted {
                            witnessTargets.append(witnessType)
                        }
                    } catch {
                        diagnostics.append(.init(kind: .unsupported("Failed to parse protocol witness '\(rawWitness)' for protocol \(protocolName): \(error)"), line: nil))
                    }
                }
                guard !witnessTargets.isEmpty else { continue }

                for witness in witnessTargets {
                    for descriptor in protocolMethods {
                        let key = signatureKey(receiver: witness, selector: descriptor.selector)
                        if existingSignatures.contains(key) { continue }
                        let cloned = duplicate(descriptor, to: witness)
                        resultMethods.append(cloned)
                        existingSignatures.insert(key)
                    }
                }
            }

            let protocolReceivers = Set(config.protocolWitnesses.keys)
            resultMethods.removeAll { protocolReceivers.contains($0.receiver.canonicalDescription()) }
            resultProperties.removeAll { protocolReceivers.contains($0.receiver.canonicalDescription()) }
        }

        return (resultMethods, resultProperties, diagnostics)
    }

    private func removeUnavailableMethods(from descriptors: [HostMethodDescriptor]) -> ([HostMethodDescriptor], [HostSurfaceDiagnostic]) {
        var kept: [HostMethodDescriptor] = []
        var diagnostics: [HostSurfaceDiagnostic] = []
        kept.reserveCapacity(descriptors.count)
        for descriptor in descriptors {
            if let reason = availabilityExclusionReason(descriptor.availability) {
                let message = "Skipping \(descriptor.displayName) due to availability \(reason)"
                diagnostics.append(.init(kind: .unsupported(message), line: nil))
                continue
            }
            kept.append(descriptor)
        }
        return (kept, diagnostics)
    }

    private func removeUnavailableProperties(from descriptors: [HostPropertyDescriptor]) -> ([HostPropertyDescriptor], [HostSurfaceDiagnostic]) {
        var kept: [HostPropertyDescriptor] = []
        var diagnostics: [HostSurfaceDiagnostic] = []
        kept.reserveCapacity(descriptors.count)
        for descriptor in descriptors {
            if let reason = availabilityExclusionReason(descriptor.availability) {
                let message = "Skipping property \(descriptor.displayName) due to availability \(reason)"
                diagnostics.append(.init(kind: .unsupported(message), line: nil))
                continue
            }
            kept.append(descriptor)
        }
        return (kept, diagnostics)
    }

    private func availabilityExclusionReason(_ attributes: [String]) -> String? {
        for attr in attributes {
            let trimmed = attr.trimmingCharacters(in: .whitespacesAndNewlines)
            let lower = trimmed.lowercased()
            guard lower.contains("@available") else { continue }
            if lower.contains("@available(*") && lower.contains("unavailable") {
                return trimmed
            }
            if lower.contains("@available(swift") && lower.contains("obsoleted:") {
                return trimmed
            }
        }
        return nil
    }

    /// Heuristic: skip methods that take a `KeyPath<Receiver, ...>` (or writable/reference variants).
    /// Such methods typically encode `Self` in the signature and cannot be called on
    /// existential-typed receivers produced by the VM bridging layer.
    private func requiresReceiverRootedKeyPath(_ descriptor: HostMethodDescriptor) -> Bool {
        let receiverName = descriptor.receiver.canonicalDescription()
        func isReceiverRootedKeyPath(_ type: TypeName) -> Bool {
            let p = type.path
            guard (p == ["Swift", "KeyPath"]) || (p == ["Swift", "WritableKeyPath"]) || (p == ["Swift", "ReferenceWritableKeyPath"]) else {
                return false
            }
            guard let root = type.genericArguments.first else { return false }
            return root.canonicalDescription() == receiverName
        }
        return descriptor.parameters.contains { isReceiverRootedKeyPath($0.type) }
    }

    // MARK: - Objective‑C bridging filters (WASM compatibility)
    // Treat members that reference NS* types or *_ObjectiveC bridging shims as Apple-only and skip them.
    private func isObjCBridgeOnly(_ descriptor: HostMethodDescriptor) -> Bool {
        // Drop explicit ObjectiveC bridge entry points
        if descriptor.baseName.contains("ObjectiveC") || descriptor.selector.contains("ObjectiveC") {
            return true
        }
        if let ret = descriptor.returnType, isObjCOnlyType(ret) { return true }
        for p in descriptor.parameters { if isObjCOnlyType(p.type) { return true } }
        return false
    }

    private func isObjCOnlyType(_ type: TypeName) -> Bool {
        let c = type.canonicalDescription()
        if c.hasPrefix("Foundation.NS") { return true }
        if c.contains("ObjectiveC") { return true }
        if c.contains("Swift.Optional<Foundation.NS") { return true }
        return false
    }

    private func supportsClosureParameter(descriptor: HostMethodDescriptor,
                                          parameter _: HostMethodParameter) -> Bool {
        let receiverKey = canonicalReceiverBaseName(descriptor.receiver)
        guard let allowedSelectors = closureSelectorAllowlist[receiverKey] else { return false }
        return allowedSelectors.contains(descriptor.selector)
    }

    /// Skip signatures we cannot correctly bridge yet:
    /// - variadic parameters (e.g., `init(objects: Any...)`, `init(arrayLiteral:)`)
    /// - host `inout` parameters (pending proper lifetime handling)
    private func unsupportedParameterReason(for descriptor: HostMethodDescriptor) -> String? {
        for p in descriptor.parameters {
            if p.isVariadic { return "variadic parameters are not supported yet" }
            // Placeholder emitted when closure types (e.g., () -> Void) failed to parse
            if p.type.canonicalDescription() == "-" { return "parameter type placeholder '-' found" }
            if p.type.isFunctionType {
                if supportsClosureParameter(descriptor: descriptor, parameter: p) {
                    continue
                }
                return "closure parameter '\(p.name)' is not yet bridged"
            }
            if p.isInout && !isPrimitiveInoutSupported(p.type) {
                return "inout parameter '\(p.name)' of type \(p.type.canonicalDescription()) is not yet bridged"
            }
        }
        return nil
    }

    private func canonicalReceiverBaseName(_ receiver: TypeName) -> String {
        var base = receiver
        base.genericArguments = []
        base.genericOwnerIndex = nil
        return base.canonicalDescription()
    }

    private struct AllowedSpecializationsResult {
        let map: [String: [TypeName]]
        let diagnostics: [HostSurfaceDiagnostic]
        let shouldSkip: Bool
        let origin: HostBridgeSpecializationOrigin
        let shouldForce: Bool
    }

    private func allowedSpecializations(for descriptor: HostMethodDescriptor,
                                        surface: ApiSurface,
                                        isForced: Bool) -> AllowedSpecializationsResult {
        guard !descriptor.genericParameters.isEmpty else {
            return AllowedSpecializationsResult(map: [:],
                                                diagnostics: [],
                                                shouldSkip: false,
                                                origin: .notApplicable,
                                                shouldForce: false)
        }

        let receiverName = descriptor.receiver.canonicalDescription()
        if let mapping = config.specializationsByType[receiverName] {
            return validateMethodDomains(mapping,
                                         descriptor: descriptor,
                                         isForced: isForced,
                                         origin: .config,
                                         missingMessage: { param in
                                             "Missing specialization list for generic parameter '\(param)' on \(descriptor.displayName)"
                                         })
        }

        for strategy in autoSpecializationStrategies {
            let domains = strategy.methodDomains(for: descriptor, surface: surface)
            guard !domains.isEmpty else { continue }
            return validateMethodDomains(domains,
                                         descriptor: descriptor,
                                         isForced: isForced,
                                         origin: .auto(strategyIdentifier: strategy.identifier),
                                         missingMessage: { param in
                                             "Auto-specialization strategy \(strategy.identifier) did not provide domain for generic parameter '\(param)' on \(descriptor.displayName)"
                                         })
        }

        let diag = HostSurfaceDiagnostic(kind: .unsupported("Missing specialization config for \(receiverName) (selector: \(descriptor.selector))"),
                                         line: nil)
        return AllowedSpecializationsResult(map: [:],
                                            diagnostics: [diag],
                                            shouldSkip: true,
                                            origin: .none,
                                            shouldForce: false)
    }

    private func validateMethodDomains(_ mapping: [String: [TypeName]],
                                       descriptor: HostMethodDescriptor,
                                       isForced: Bool,
                                       origin: HostBridgeSpecializationOrigin,
                                       missingMessage: (String) -> String) -> AllowedSpecializationsResult {
        var allowed: [String: [TypeName]] = [:]
        var diags: [HostSurfaceDiagnostic] = []
        var skip = false
        var autoForce = false
        for parameter in descriptor.genericParameters {
            let name = parameter.name
            guard let entries = mapping[name], !entries.isEmpty else {
                diags.append(.init(kind: .unsupported(missingMessage(name)), line: nil))
                skip = true
                continue
            }
            if !isForced && entries.count > config.maxDomainPerGenericParam {
                autoForce = true
                allowed[name] = entries
                let message = "Auto-expanded specialization domain for \(descriptor.displayName) generic parameter '\(name)' (needed \(entries.count) > cap \(config.maxDomainPerGenericParam))."
                diags.append(.init(kind: .unsupported(message), line: nil))
            } else {
                allowed[name] = entries
            }
        }
        if skip {
            return AllowedSpecializationsResult(map: [:],
                                                diagnostics: diags,
                                                shouldSkip: true,
                                                origin: origin,
                                                shouldForce: false)
        }
        return AllowedSpecializationsResult(map: allowed,
                                            diagnostics: diags,
                                            shouldSkip: false,
                                            origin: origin,
                                            shouldForce: autoForce)
    }

    private func propertyConfigKey(for descriptor: HostPropertyDescriptor) -> String {
        let prefix = descriptor.kind == .static ? "static" : "instance"
        return "\(prefix)::\(descriptor.name)"
    }

    private func allowedSpecializations(for descriptor: HostPropertyDescriptor,
                                        surface: ApiSurface) -> AllowedSpecializationsResult {
        guard !descriptor.genericParameters.isEmpty else {
            return AllowedSpecializationsResult(map: [:],
                                                diagnostics: [],
                                                shouldSkip: false,
                                                origin: .notApplicable,
                                                shouldForce: false)
        }

        let receiverName = descriptor.receiver.canonicalDescription()
        let key = propertyConfigKey(for: descriptor)

        if let propertyMap = config.propertySpecializationsByType[receiverName],
           let mapping = propertyMap[key] {
            return validatePropertyDomains(mapping,
                                           descriptor: descriptor,
                                           origin: .config,
                                           missingMessage: { param in
                                               "Missing specialization list for property \(descriptor.displayName) generic parameter '\(param)'"
                                           })
        }

        for strategy in autoSpecializationStrategies {
            let domains = strategy.propertyDomains(for: descriptor, surface: surface)
            guard !domains.isEmpty else { continue }
            return validatePropertyDomains(domains,
                                           descriptor: descriptor,
                                           origin: .auto(strategyIdentifier: strategy.identifier),
                                           missingMessage: { param in
                                               "Auto-specialization strategy \(strategy.identifier) did not provide domain for property \(descriptor.displayName) generic parameter '\(param)'"
                                           })
        }

        let diag = HostSurfaceDiagnostic(kind: .unsupported("Missing property specialization config for \(descriptor.displayName)"),
                                         line: nil)
        return AllowedSpecializationsResult(map: [:],
                                            diagnostics: [diag],
                                            shouldSkip: true,
                                            origin: .none,
                                            shouldForce: false)
    }

    private func validatePropertyDomains(_ mapping: [String: [TypeName]],
                                         descriptor: HostPropertyDescriptor,
                                         origin: HostBridgeSpecializationOrigin,
                                         missingMessage: (String) -> String) -> AllowedSpecializationsResult {
        var diagnostics: [HostSurfaceDiagnostic] = []
        var skip = false
        var allowed: [String: [TypeName]] = [:]
        var autoForce = false
        for parameter in descriptor.genericParameters {
            let name = parameter.name
            guard let entries = mapping[name], !entries.isEmpty else {
                diagnostics.append(.init(kind: .unsupported(missingMessage(name)), line: nil))
                skip = true
                continue
            }
            if entries.count > config.maxDomainPerGenericParam {
                autoForce = true
                allowed[name] = entries
                let message = "Auto-expanded specialization domain for property \(descriptor.displayName) generic parameter '\(name)' (needed \(entries.count) > cap \(config.maxDomainPerGenericParam))."
                diagnostics.append(.init(kind: .unsupported(message), line: nil))
            } else {
                allowed[name] = entries
            }
        }

        if skip {
            return AllowedSpecializationsResult(map: [:],
                                                diagnostics: diagnostics,
                                                shouldSkip: true,
                                                origin: origin,
                                                shouldForce: false)
        }
        return AllowedSpecializationsResult(map: allowed,
                                            diagnostics: diagnostics,
                                            shouldSkip: false,
                                            origin: origin,
                                            shouldForce: autoForce)
    }

    private struct TrimResult {
        let primary: [GenericSpecialization]
        let overflow: [GenericSpecialization]
        let diagnostics: [HostSurfaceDiagnostic]
    }

    private func trimSpecializations(_ planned: [GenericSpecialization],
                                     descriptor: HostMethodDescriptor,
                                     isForced: Bool) -> TrimResult {
        guard !descriptor.genericParameters.isEmpty else {
            return TrimResult(primary: planned, overflow: [], diagnostics: [])
        }
        if isForced || planned.count <= config.maxSpecializationsPerSelector {
            return TrimResult(primary: planned, overflow: [], diagnostics: [])
        }
        let message = "Auto-expanded specialization combinations for \(descriptor.displayName) (needed \(planned.count) > cap \(config.maxSpecializationsPerSelector))."
        let diag = HostSurfaceDiagnostic(kind: .unsupported(message), line: nil)
        return TrimResult(primary: planned, overflow: [], diagnostics: [diag])
    }

    /// Builds a rewrite map from `<Swift.Any>` to concrete types based on the
    /// configured specialization domains. Only includes entries where all
    /// generic parameters have exactly one configured type.
    private func defaultAnyRewriteMap(from config: HostBridgeGenerationConfig) -> [String: String] {
        var map: [String: String] = [:]
        for (receiver, paramDomains) in config.specializationsByType {
            // Require a single concrete type per generic parameter
            let singletons: [(String, TypeName)] = paramDomains.compactMap { (param, types) in
                guard types.count == 1, let only = types.first else { return nil }
                return (param, only)
            }
            guard singletons.count == paramDomains.count else { continue }
            guard let parsed = try? TypeNameParser.parse(receiver) else { continue }
            var anySubs: [String: TypeName] = [:]
            var concreteSubs: [String: TypeName] = [:]
            for (param, only) in singletons {
                anySubs[param] = TypeName(path: ["Swift", "Any"]) // placeholder
                concreteSubs[param] = only
            }
            let anyType = parsed.applyingSubstitutions(anySubs).canonicalDescription()
            let concreteType = parsed.applyingSubstitutions(concreteSubs).canonicalDescription()
            map[anyType] = concreteType
        }
        return map
    }

    private func exportedTypeName(for canonical: String) -> String {
        if let override = config.exportedTypeNames[canonical] {
            return override
        }
        if let angle = canonical.firstIndex(of: "<") {
            let base = String(canonical[..<angle])
            let suffix = canonical[angle...]
            let exportedBase = exportedTypeName(for: base)
            return exportedBase + suffix
        }
        if let dot = canonical.lastIndex(of: ".") {
            let nextIndex = canonical.index(after: dot)
            return String(canonical[nextIndex...])
        }
        return canonical
    }

    private func resolvedPrimitiveCanonical(for canonical: String) -> String {
        if let mapped = config.primitiveTypeAliases[canonical] { return mapped }
        if let simple = canonical.split(separator: ".").last {
            let key = String(simple)
            if let mapped = config.primitiveTypeAliases[key] {
                return mapped
            }
        }
        return canonical
    }

    private func unsupportedHostTypeUsage(in descriptor: HostMethodDescriptor) -> String? {
        if let returnType = descriptor.returnType,
           let unsupported = unsupportedHostTypeDescription(for: returnType) {
            return "return type \(unsupported)"
        }
        for parameter in descriptor.parameters {
            if let unsupported = unsupportedHostTypeDescription(for: parameter.type) {
                return "parameter '\(parameter.name)' of type \(unsupported)"
            }
        }
        return nil
    }

    private func unsupportedHostTypeDescription(for type: TypeName) -> String? {
        let canonical = type.canonicalDescription()
        if canonical.contains("Builtin.") || canonical == "Builtin" {
            return canonical
        }
        if canonical.contains("Span") {
            return canonical
        }
        if unsupportedHostTypeNames.contains(canonical) {
            return canonical
        }
        if let signature = type.functionSignature {
            for parameter in signature.parameters {
                if let nested = unsupportedHostTypeDescription(for: parameter) {
                    return nested
                }
            }
            if let nested = unsupportedHostTypeDescription(for: signature.returnType) {
                return nested
            }
        }
        for argument in type.genericArguments {
            if let nested = unsupportedHostTypeDescription(for: argument) {
                return nested
            }
        }
        return nil
    }

    private func isPrimitiveInoutSupported(_ type: TypeName) -> Bool {
        let canonical = resolvedPrimitiveCanonical(for: type.canonicalDescription())
        switch canonical {
        case "Swift.Int", "Swift.Double", "Swift.Bool", "Swift.String":
            return true
        default:
            return false
        }
    }

}
