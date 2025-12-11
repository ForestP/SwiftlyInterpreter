//
//  HostBridgetGeneratorController.swift
//  HostCodegenCore
//
//  Created by Forest Plasencia on 9/19/25.
//

import HostSurfaceKit
import Foundation

public struct HostBridgeGenerationRequest {
    let moduleName: String
    let interfaceURL: URL
    let outputDirectory: URL
    var config: HostBridgeGenerationConfig
    var autoSpecializationStrategies: [AutoSpecializationStrategy]
    var pipelineOptions: HostSurfacePipelineOptions
    var filePrefix: String
    var installerBasename: String
    var autoRegisterInstaller: Bool
    var reportURL: URL?
    var enableSwiftFormat: Bool
    var swiftFormatPath: String?
    var enableTypeAliasStrategy: Bool
    var paletteStrategyKind: PaletteSpecializationStrategy.Kind?
    var hostRegistrationTypes: [String]?
    var allowedTypeNames: [String]?
    var allowedSelectorsByType: [String: Set<String>]?

    public init(
        moduleName: String,
        interfaceURL: URL,
        outputDirectory: URL,
        config: HostBridgeGenerationConfig = .init(),
        autoSpecializationStrategies: [AutoSpecializationStrategy] = [],
        pipelineOptions: HostSurfacePipelineOptions = .init(),
        filePrefix: String = "HostBridges",
        installerBasename: String = "GeneratedHostBridges",
        autoRegisterInstaller: Bool = true,
        reportURL: URL? = nil,
        enableSwiftFormat: Bool = false,
        swiftFormatPath: String? = nil,
        enableTypeAliasStrategy: Bool = false,
        paletteStrategyKind: PaletteSpecializationStrategy.Kind? = nil,
        hostRegistrationTypes: [String]? = nil,
        allowedTypeNames: [String]? = nil,
        allowedSelectorsByType: [String: Set<String>]? = nil
    ) {
        self.moduleName = moduleName
        self.interfaceURL = interfaceURL
        self.outputDirectory = outputDirectory
        self.config = config
        self.autoSpecializationStrategies = autoSpecializationStrategies
        self.pipelineOptions = pipelineOptions
        self.filePrefix = filePrefix
        self.installerBasename = installerBasename
        self.autoRegisterInstaller = autoRegisterInstaller
        self.reportURL = reportURL
        self.enableSwiftFormat = enableSwiftFormat
        self.swiftFormatPath = swiftFormatPath
        self.enableTypeAliasStrategy = enableTypeAliasStrategy
        self.paletteStrategyKind = paletteStrategyKind
        self.hostRegistrationTypes = hostRegistrationTypes
        self.allowedTypeNames = allowedTypeNames
        self.allowedSelectorsByType = allowedSelectorsByType
    }
}

public struct HostBridgeGenerationSummary {
    public let surfaceResult: HostSurfaceResolveResult
    public let generationResult: HostBridgeGenerationResult
    public let files: [HostBridgeFile]
    public let writtenFiles: [URL]
    public let report: HostBridgeGenerationReport
    public let formatDiagnostics: [HostSurfaceDiagnostic]
}

public struct HostBridgeGenerationController {
    var request: HostBridgeGenerationRequest

    public init(request: HostBridgeGenerationRequest) {
        self.request = request
    }

    public func run(fileManager: FileManager = .default) throws -> HostBridgeGenerationSummary {
        let interfaceContents = try String(contentsOf: request.interfaceURL, encoding: .utf8)

        var pipeline = HostSurfacePipeline(
            moduleName: request.moduleName,
            interfaceContents: interfaceContents,
            options: request.pipelineOptions
        )
        let surfaceResult = try pipeline.run()

        var strategies = request.autoSpecializationStrategies
        // Precedence: config > alias > registrations > palette (first non-empty wins)
        if request.enableTypeAliasStrategy {
            let aliasStrategy = TypeAliasDomainStrategy(typeAliases: surfaceResult.typeAliases)
            strategies.append(aliasStrategy)
        }
        if let registered = request.hostRegistrationTypes, !registered.isEmpty {
            strategies.append(HostRegistrationDomainStrategy(registeredTypeNames: registered))
        }
        if let paletteKind = request.paletteStrategyKind {
            let paletteStrategy = PaletteSpecializationStrategy(kind: paletteKind)
            strategies.append(paletteStrategy)
        }

        // Derive primitive alias mappings (e.g., TimeInterval → Double) from surface typealiases
        func canonicalPrimitiveTarget(for canonical: String) -> String? {
            switch canonical {
            case "Swift.Int", "Swift.Int8", "Swift.Int16", "Swift.Int32", "Swift.Int64",
                 "Swift.UInt", "Swift.UInt8", "Swift.UInt16", "Swift.UInt32", "Swift.UInt64",
                 "Swift.Double", "Swift.Float",
                 "Swift.Bool", "Swift.String", "Swift.Void":
                return canonical
            default:
                return nil
            }
        }
        var primitiveAliasMap = request.config.primitiveTypeAliases
        for alias in surfaceResult.typeAliases {
            guard let primitive = canonicalPrimitiveTarget(for: alias.canonicalTarget) else { continue }
            primitiveAliasMap[alias.qualifiedName] = primitive
            primitiveAliasMap[alias.name] = primitive
        }

        // Ensure common Foundation aliases bridge to primitives even when surfaced under module-qualified names.
        func seedPrimitiveAlias(_ alias: String, target: String) {
            if primitiveAliasMap[alias] == nil {
                primitiveAliasMap[alias] = target
            }
        }
        seedPrimitiveAlias("Foundation.TimeInterval", target: "Swift.Double")
        seedPrimitiveAlias("Swift.TimeInterval", target: "Swift.Double")
        seedPrimitiveAlias("TimeInterval", target: "Swift.Double")

        var config = request.config
        if !primitiveAliasMap.isEmpty {
            config.primitiveTypeAliases.merge(primitiveAliasMap) { existing, _ in existing }
        }
        if !surfaceResult.genericParameterInventory.isEmpty {
            config.genericPlaceholderMap.merge(surfaceResult.genericParameterInventory) { existing, _ in existing }
        }

        // Apply type allowlist filter if provided
        let generationSurface: ApiSurface = {
            guard let allow = request.allowedTypeNames, !allow.isEmpty else { return surfaceResult.apiSurface }
            let matcher = TypeAllowlistMatcher(rawEntries: allow,
                                               moduleName: request.moduleName)
            let filteredTypes = surfaceResult.apiSurface.types.filter { matcher.contains(canonicalName: $0.canonicalName) }
            return ApiSurface(types: filteredTypes)
        }()

        let closureAllowlist = request.allowedSelectorsByType ?? [:]
        let generator = HostBridgeGenerator(config: config,
                                            autoSpecializationStrategies: strategies,
                                            closureSelectorAllowlist: closureAllowlist)
        let generationResult = generator.generate(from: generationSurface)

        let emitter = HostBridgeEmitter(filePrefix: request.filePrefix,
                                        exportedTypeNames: config.exportedTypeNames,
                                        installerBasename: request.installerBasename,
                                        autoRegisterInstaller: request.autoRegisterInstaller)
        let files = emitter.emitFiles(methodArtifacts: generationResult.methodArtifacts,
                                      propertyArtifacts: generationResult.propertyArtifacts)

        let written = try write(files: files,
                                to: request.outputDirectory,
                                fileManager: fileManager)

        let formatDiagnostics = formatIfNeeded(files: written)

        let report = HostBridgeGenerationReport(moduleName: request.moduleName,
                                                methodDescriptors: generationResult.methodDescriptors,
                                                propertyDescriptors: generationResult.propertyDescriptors,
                                                methodArtifacts: generationResult.methodArtifacts,
                                                propertyArtifacts: generationResult.propertyArtifacts,
                                                diagnostics: surfaceResult.diagnostics + generationResult.diagnostics + formatDiagnostics,
                                                typeAliases: surfaceResult.typeAliases,
                                                collectorFallbackPlaceholderHitCount: surfaceResult.collectorFallbackPlaceholderHitCount,
                                                resolverPlaceholderDropHitCount: surfaceResult.resolverPlaceholderDropHitCount)

        if let reportURL = request.reportURL {
            try write(report: report, to: reportURL, fileManager: fileManager)
        }

        return HostBridgeGenerationSummary(surfaceResult: surfaceResult,
                                           generationResult: generationResult,
                                           files: files,
                                           writtenFiles: written,
                                           report: report,
                                           formatDiagnostics: formatDiagnostics)
    }

    private func write(files: [HostBridgeFile],
                       to baseDirectory: URL,
                       fileManager: FileManager) throws -> [URL] {
        var written: [URL] = []
        if !fileManager.fileExists(atPath: baseDirectory.path) {
            try fileManager.createDirectory(at: baseDirectory,
                                            withIntermediateDirectories: true,
                                            attributes: nil)
        }
        for file in files {
            let fileURL = baseDirectory.appendingPathComponent(file.relativePath)
            let parentDirectory = fileURL.deletingLastPathComponent()
            if !fileManager.fileExists(atPath: parentDirectory.path) {
                try fileManager.createDirectory(at: parentDirectory,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
            }
            try file.contents.write(to: fileURL, atomically: true, encoding: .utf8)
            written.append(fileURL)
        }
        return written
    }

    private func write(report: HostBridgeGenerationReport,
                       to url: URL,
                       fileManager: FileManager) throws {
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
        let data = try encoder.encode(report)
        let directory = url.deletingLastPathComponent()
        if !directory.path.isEmpty && !fileManager.fileExists(atPath: directory.path) {
            try fileManager.createDirectory(at: directory,
                                            withIntermediateDirectories: true,
                                            attributes: nil)
        }
        try data.write(to: url, options: .atomic)
    }

    private func formatIfNeeded(files: [URL]) -> [HostSurfaceDiagnostic] {
        guard request.enableSwiftFormat else { return [] }
        guard !files.isEmpty else { return [] }

        guard let toolPath = resolveSwiftFormatExecutable(explicitPath: request.swiftFormatPath) else {
            return [HostSurfaceDiagnostic(kind: .unsupported("swift-format not found; skipped formatting"), line: nil)]
        }
#if os(macos)
        let task = Process()
        task.executableURL = URL(fileURLWithPath: toolPath)
        task.arguments = ["--in-place"] + files.map { $0.path }
        do {
            try task.run()
            task.waitUntilExit()
            if task.terminationStatus != 0 {
                return [HostSurfaceDiagnostic(kind: .unsupported("swift-format exited with status \(task.terminationStatus)"), line: nil)]
            }
        } catch {
            return [HostSurfaceDiagnostic(kind: .unsupported("Failed to run swift-format: \(error)"), line: nil)]
        }
#endif
        return []
    }

    private func resolveSwiftFormatExecutable(explicitPath: String?) -> String? {
        if let explicitPath, !explicitPath.isEmpty {
            return explicitPath
        }
        
#if os(macos)
        let which = Process()
        which.executableURL = URL(fileURLWithPath: "/usr/bin/env")
        which.arguments = ["which", "swift-format"]
        let pipe = Pipe()
        which.standardOutput = pipe
        which.standardError = Pipe()
        do {
            try which.run()
            which.waitUntilExit()
            guard which.terminationStatus == 0 else { return nil }
            let data = pipe.fileHandleForReading.readDataToEndOfFile()
            guard let path = String(data: data, encoding: .utf8)?.trimmingCharacters(in: .whitespacesAndNewlines), !path.isEmpty else {
                return nil
            }
            return path
        } catch {
            return nil
        }
#else
        return nil
#endif
    }
}

struct TypeAllowlistMatcher {
    private let exactMatches: Set<String>
    private let baseMatches: Set<String>
    private let rawFallbacks: Set<String>
    private let moduleName: String

    init(rawEntries: [String], moduleName: String) {
        self.moduleName = moduleName
        var exact: Set<String> = []
        var bases: Set<String> = []
        var fallbacks: Set<String> = []
        for entry in rawEntries {
            guard let parsed = try? TypeNameParser.parse(entry) else {
                fallbacks.insert(entry)
                continue
            }
            let canonical = CanonicalTypeNameHelper.canonicalize(type: parsed,
                                                                 defaultModule: moduleName)
            let canonicalDescription = canonical.canonicalDescription()
            exact.insert(canonicalDescription)

            var baseType = canonical
            baseType.genericArguments = []
            baseType.genericOwnerIndex = nil
            bases.insert(baseType.canonicalDescription())
        }
        self.exactMatches = exact
        self.baseMatches = bases
        self.rawFallbacks = fallbacks
    }

    func contains(canonicalName: String) -> Bool {
        if exactMatches.contains(canonicalName) || rawFallbacks.contains(canonicalName) {
            return true
        }
        guard let parsed = try? TypeNameParser.parse(canonicalName) else {
            return false
        }
        let canonical = CanonicalTypeNameHelper.canonicalize(type: parsed,
                                                             defaultModule: moduleName)
        let canonicalDescription = canonical.canonicalDescription()
        if exactMatches.contains(canonicalDescription) || rawFallbacks.contains(canonicalDescription) {
            return true
        }
        var baseType = canonical
        baseType.genericArguments = []
        baseType.genericOwnerIndex = nil
        let baseDescription = baseType.canonicalDescription()
        return baseMatches.contains(baseDescription)
    }
}

public struct HostBridgeGenerationReport: Codable {
    public struct Metrics: Codable {
        public let collectorFallbackPlaceholderHitCount: Int
        public let resolverPlaceholderDropHitCount: Int
    }

    public struct Method: Codable {
        public let selector: String
        public let baseName: String
        public let specializationCount: Int
        public let genericParameters: [String]
        public let requirements: [String]
        public let specializationSource: String
        public let usesFallback: Bool
        public let genericParameterDomainSizes: [String: Int]
    }

    public struct Property: Codable {
        public let name: String
        public let isSettable: Bool
        public let getterSpecializationCount: Int
        public let setterSpecializationCount: Int
        public let kind: String
        public let specializationSource: String
        public let usesFallback: Bool
        public let genericParameterDomainSizes: [String: Int]
    }

    public struct TypeEntry: Codable {
        public let canonicalName: String
        public let methods: [Method]
        public let properties: [Property]
    }

    public struct TypeAliasEntry: Codable {
        public let name: String
        public let qualifiedName: String
        public let canonicalTarget: String
        public let sourceLine: Int
    }

    public let moduleName: String
    public let types: [TypeEntry]
    public let diagnostics: [String]
    public let typeAliases: [TypeAliasEntry]
    public let metrics: Metrics

    init(moduleName: String,
         methodDescriptors: [HostMethodDescriptor],
         propertyDescriptors: [HostPropertyDescriptor],
         methodArtifacts: [HostBridgeMethodArtifact],
         propertyArtifacts: [HostBridgePropertyArtifact],
         diagnostics: [HostSurfaceDiagnostic],
         typeAliases: [ResolvedTypeAlias],
         collectorFallbackPlaceholderHitCount: Int,
         resolverPlaceholderDropHitCount: Int) {
        self.moduleName = moduleName
        self.diagnostics = diagnostics.map { $0.description }
        self.metrics = Metrics(collectorFallbackPlaceholderHitCount: collectorFallbackPlaceholderHitCount,
                               resolverPlaceholderDropHitCount: resolverPlaceholderDropHitCount)

        struct MethodKey: Hashable {
            let typeName: String
            let selector: String
            let signature: String
        }

        func kindToken(for kind: HostMethodKind) -> String {
            switch kind {
            case .initializer(let isFailable):
                return "initializer:\(isFailable ? "failable" : "nonfailable")"
            case .instance(let isMutating):
                return "instance:\(isMutating ? "mutating" : "nonmutating")"
            case .static:
                return "static"
            }
        }

        func parameterSignature(for descriptor: HostMethodDescriptor) -> String {
            guard !descriptor.parameters.isEmpty else { return "()" }
            return descriptor.parameters.map { param in
                let label = param.label ?? "_"
                let typeName = param.type.canonicalDescription()
                let flags = [
                    param.isInout ? "inout" : "value",
                    param.isVariadic ? "variadic" : "fixed"
                ].joined(separator: ":")
                return "\(label):\(typeName):\(flags)"
            }.joined(separator: "|")
        }

        func methodKey(for descriptor: HostMethodDescriptor) -> MethodKey {
            let typeName = descriptor.receiver.canonicalDescription()
            let signatureParts = [
                kindToken(for: descriptor.kind),
                parameterSignature(for: descriptor),
                descriptor.returnType?.canonicalDescription() ?? "Void"
            ]
            return MethodKey(typeName: typeName,
                             selector: descriptor.selector,
                             signature: signatureParts.joined(separator: "->"))
        }

        var methodArtifactMap: [MethodKey: HostBridgeMethodArtifact] = [:]
        methodArtifacts.forEach { artifact in
            let key = methodKey(for: artifact.descriptor)
            if methodArtifactMap[key] == nil {
                methodArtifactMap[key] = artifact
            }
        }

        var methodsByType: [String: [Method]] = [:]
        var reportedMethodKeys: Set<MethodKey> = []
        for artifact in methodArtifacts {
            let descriptor = artifact.descriptor
            let typeName = descriptor.receiver.canonicalDescription()
            let key = methodKey(for: descriptor)
            let requirements = descriptor.requirements.map { requirementString($0) }
            let source = specializationSource(for: descriptor,
                                              artifact: artifact,
                                              defaultSource: descriptor.genericParameters.isEmpty ? "not_applicable" : "config")
            let method = Method(selector: descriptor.selector,
                                baseName: descriptor.baseName,
                                specializationCount: artifact.render.thunks.count,
                                genericParameters: descriptor.genericParameters.map(\.name),
                                requirements: requirements,
                                specializationSource: source.label,
                                usesFallback: source.usesFallback,
                                genericParameterDomainSizes: artifact.domainSizes)
            methodsByType[typeName, default: []].append(method)
            reportedMethodKeys.insert(key)
        }
        for descriptor in methodDescriptors {
            let typeName = descriptor.receiver.canonicalDescription()
            let key = methodKey(for: descriptor)
            if reportedMethodKeys.contains(key) { continue }
            let requirements = descriptor.requirements.map { requirementString($0) }
            let artifact = methodArtifactMap[key]
            let source = specializationSource(for: descriptor,
                                              artifact: artifact,
                                              defaultSource: descriptor.genericParameters.isEmpty ? "not_applicable" : "none")
            let method = Method(selector: descriptor.selector,
                                baseName: descriptor.baseName,
                                specializationCount: 0,
                                genericParameters: descriptor.genericParameters.map(\.name),
                                requirements: requirements,
                                specializationSource: source.label,
                                usesFallback: source.usesFallback,
                                genericParameterDomainSizes: [:])
            methodsByType[typeName, default: []].append(method)
            reportedMethodKeys.insert(key)
        }

        struct PropertyKey: Hashable { let typeName: String; let name: String; let kind: HostPropertyKind }
        let propertyArtifactMap: [PropertyKey: HostBridgePropertyArtifact] = Dictionary(uniqueKeysWithValues: propertyArtifacts.map { artifact in
            let descriptor = artifact.descriptor
            let key = PropertyKey(typeName: descriptor.receiver.canonicalDescription(), name: descriptor.name, kind: descriptor.kind)
            return (key, artifact)
        })

        var propertiesByType: [String: [Property]] = [:]
        for artifact in propertyArtifacts {
            let descriptor = artifact.descriptor
            let typeName = descriptor.receiver.canonicalDescription()
            let source = specializationSource(for: descriptor,
                                              artifact: artifact,
                                              defaultSource: descriptor.genericParameters.isEmpty ? "not_applicable" : "config")
            let property = Property(name: descriptor.name,
                                    isSettable: descriptor.isSettable,
                                    getterSpecializationCount: artifact.render.getter.thunks.count,
                                    setterSpecializationCount: artifact.render.setter?.thunks.count ?? 0,
                                    kind: descriptor.kind == .static ? "static" : "instance",
                                    specializationSource: source.label,
                                    usesFallback: source.usesFallback,
                                    genericParameterDomainSizes: artifact.domainSizes)
            propertiesByType[typeName, default: []].append(property)
        }
        for descriptor in propertyDescriptors {
            let typeName = descriptor.receiver.canonicalDescription()
            if propertiesByType[typeName]?.contains(where: { $0.name == descriptor.name }) == true { continue }
            let artifact = propertyArtifactMap[PropertyKey(typeName: typeName, name: descriptor.name, kind: descriptor.kind)]
            let source = specializationSource(for: descriptor,
                                              artifact: artifact,
                                              defaultSource: descriptor.genericParameters.isEmpty ? "not_applicable" : "none")
            let property = Property(name: descriptor.name,
                                    isSettable: descriptor.isSettable,
                                    getterSpecializationCount: 0,
                                    setterSpecializationCount: 0,
                                    kind: descriptor.kind == .static ? "static" : "instance",
                                    specializationSource: source.label,
                                    usesFallback: source.usesFallback,
                                    genericParameterDomainSizes: [:])
            propertiesByType[typeName, default: []].append(property)
        }

        let typeNames = Set(methodsByType.keys).union(propertiesByType.keys)
        self.types = typeNames.sorted().map { typeName in
            let methods = methodsByType[typeName]?.sorted { $0.selector < $1.selector } ?? []
            let properties = propertiesByType[typeName]?.sorted { $0.name < $1.name } ?? []
            return TypeEntry(canonicalName: typeName,
                             methods: methods,
                             properties: properties)
        }

        self.typeAliases = typeAliases.sorted { $0.qualifiedName < $1.qualifiedName }.map { alias in
            TypeAliasEntry(name: alias.name,
                           qualifiedName: alias.qualifiedName,
                           canonicalTarget: alias.canonicalTarget,
                           sourceLine: alias.sourceLine)
        }
    }
}

private func specializationSource(for descriptor: HostMethodDescriptor,
                                  artifact: HostBridgeMethodArtifact?,
                                  defaultSource: String) -> (label: String, usesFallback: Bool) {
    guard !descriptor.genericParameters.isEmpty else {
        return (label: "not_applicable", usesFallback: false)
    }
    guard let artifact else {
        return (label: defaultSource, usesFallback: false)
    }
    let label = artifact.origin.reportLabel
    let usesFallback = artifact.render.fallback != nil
    return (label: label, usesFallback: usesFallback)
}

private func specializationSource(for descriptor: HostPropertyDescriptor,
                                  artifact: HostBridgePropertyArtifact?,
                                  defaultSource: String) -> (label: String, usesFallback: Bool) {
    guard !descriptor.genericParameters.isEmpty else {
        return (label: "not_applicable", usesFallback: false)
    }
    guard let artifact else {
        return (label: defaultSource, usesFallback: false)
    }
    let label = artifact.origin.reportLabel
    let getterFallback = artifact.render.getter.fallback != nil
    let setterFallback = artifact.render.setter?.fallback != nil
    return (label: label, usesFallback: getterFallback || setterFallback)
}
private func requirementString(_ requirement: GenericRequirement) -> String {
    switch requirement {
    case .sameType(let param, let type):
        return "\(param) == \(type.canonicalDescription())"
    case .sameTypeParameters(let lhs, let rhs):
        return "\(lhs) == \(rhs)"
    case .conformsTo(let param, let protocolName):
        return "\(param): \(protocolName)"
    case .sameTypeResolved(let lhs, let rhs):
        return "\(lhs.canonicalDescription()) == \(rhs.canonicalDescription())"
    case .conformsResolved(let type, let protocolName):
        return "\(type.canonicalDescription()): \(protocolName)"
    }
}
