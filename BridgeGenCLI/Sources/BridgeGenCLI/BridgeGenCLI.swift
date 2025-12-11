//
//  BridgeGenCLI.swift
//  BridgeGenCLI
//
//  Created by Forest Plasencia on 9/19/25.
//

import HostSurfaceKit
import HostCodegenCore
import Foundation

public struct BridgeGenCLI {
    public enum Error: Swift.Error, CustomStringConvertible {
        case missingValue(String)
        case unknownFlag(String)
        case missingRequired(String)
        case configReadFailed(String)

        public var description: String {
            switch self {
            case .missingValue(let flag):
                return "Missing value for flag \(flag)"
            case .unknownFlag(let flag):
                return "Unknown flag \(flag)"
            case .missingRequired(let name):
                return "Missing required option: \(name)"
            case .configReadFailed(let message):
                return "Failed to read config: \(message)"
            }
        }
    }

    struct Options {
        let moduleName: String
        let interfacePath: URL
        let outputDirectory: URL
        let configPath: URL?
        let filePrefix: String
        let installerBasename: String
        let autoRegisterInstaller: Bool
        let reportPath: URL?
        let dumpSurfacePath: URL?
        let dumpInventoryPath: URL?
        let enableSwiftFormat: Bool
        let swiftFormatPath: String?
        let autoSpecializeAliases: Bool
        let paletteKind: PaletteSpecializationStrategy.Kind?
        let hostRegistrationsPath: URL?
        let allowlistPath: URL?
        let allowlistData: AllowlistData?
    }

    public init() {}

    // Exposed helper for CLI tests and runners: formats summary lines like the runner does
    public static func specializationSummaryLines(from types: [HostBridgeGenerationReport.TypeEntry]) -> [String] {
        var lines: [String] = []
        for type in types {
            let methodSummaries = type.methods.filter { !$0.genericParameters.isEmpty }
            let propertySummaries = type.properties.filter { $0.specializationSource != "not_applicable" }
            guard !methodSummaries.isEmpty || !propertySummaries.isEmpty else { continue }
            lines.append("  \(type.canonicalName):")
            for method in methodSummaries {
                let fallback = method.usesFallback ? ", uses fallback" : ""
                lines.append("    method \(method.selector): source=\(method.specializationSource), thunks=\(method.specializationCount)\(fallback)")
            }
            for property in propertySummaries {
                let fallback = property.usesFallback ? ", uses fallback" : ""
                lines.append("    property \(property.name): source=\(property.specializationSource), getterThunks=\(property.getterSpecializationCount), setterThunks=\(property.setterSpecializationCount)\(fallback)")
            }
        }
        return lines
    }

    public func run(arguments: [String]) throws -> HostBridgeGenerationSummary {
        let options = try parse(arguments: arguments)
        let config = try loadConfig(at: options.configPath)
        let request = HostBridgeGenerationRequest(
            moduleName: options.moduleName,
            interfaceURL: options.interfacePath,
            outputDirectory: options.outputDirectory,
            config: config,
            pipelineOptions: .init(dumpModelURL: options.dumpSurfacePath,
                                   dumpInventoryURL: options.dumpInventoryPath,
                                   dumpPrettyPrint: true),
            filePrefix: options.filePrefix,
            installerBasename: options.installerBasename,
            autoRegisterInstaller: options.autoRegisterInstaller,
            reportURL: options.reportPath,
            enableSwiftFormat: options.enableSwiftFormat,
            swiftFormatPath: options.swiftFormatPath,
            enableTypeAliasStrategy: options.autoSpecializeAliases,
            paletteStrategyKind: options.paletteKind,
            hostRegistrationTypes: loadHostRegistrations(at: options.hostRegistrationsPath),
            allowedTypeNames: options.allowlistData?.typeNames,
            allowedSelectorsByType: options.allowlistData?.selectorsByType
        )
        let controller = HostBridgeGenerationController(request: request)
        let summary = try controller.run()
        report(summary: summary)
        return summary
    }

    private func parse(arguments: [String]) throws -> Options {
        var moduleName: String?
        var interfacePath: URL?
        var outputDirectory: URL?
        var configPath: URL?
        var filePrefix = "HostBridges"
        var reportPath: URL?
        var enableSwiftFormat = false
        var swiftFormatPath: String?
        var dumpSurfacePath: URL? = nil
        var dumpInventoryPath: URL? = nil
        var autoSpecializeAliases = false
        var paletteKind: PaletteSpecializationStrategy.Kind?
        var hostRegistrationsPath: URL?
        var aggregatorRequestedRegistrations = false
        var allowlistPath: URL?
        var allowlistData: AllowlistData?
        var installerBasename = "GeneratedHostBridges"
        var autoRegisterInstaller = true

        var iterator = arguments.dropFirst().makeIterator()
        while let arg = iterator.next() {
            switch arg {
            case "--module":
                guard let value = iterator.next() else { throw Error.missingValue(arg) }
                moduleName = value
            case "--interface":
                guard let value = iterator.next() else { throw Error.missingValue(arg) }
                interfacePath = URL(fileURLWithPath: value)
            case "--out":
                guard let value = iterator.next() else { throw Error.missingValue(arg) }
                outputDirectory = URL(fileURLWithPath: value, isDirectory: true)
            case "--config":
                guard let value = iterator.next() else { throw Error.missingValue(arg) }
                configPath = URL(fileURLWithPath: value)
            case "--file-prefix":
                guard let value = iterator.next() else { throw Error.missingValue(arg) }
                filePrefix = value
            case "--installer-basename":
                guard let value = iterator.next() else { throw Error.missingValue(arg) }
                installerBasename = value
            case "--skip-installer-registration":
                autoRegisterInstaller = false
            case "--report":
                guard let value = iterator.next() else { throw Error.missingValue(arg) }
                reportPath = URL(fileURLWithPath: value)
            case "--dump-surface":
                guard let value = iterator.next() else { throw Error.missingValue(arg) }
                dumpSurfacePath = URL(fileURLWithPath: value)
            case "--dump-inventory":
                guard let value = iterator.next() else { throw Error.missingValue(arg) }
                dumpInventoryPath = URL(fileURLWithPath: value)
            case "--swift-format":
                guard let value = iterator.next() else { throw Error.missingValue(arg) }
                swiftFormatPath = value
                enableSwiftFormat = true
            case "--format":
                enableSwiftFormat = true
            case "--auto-specialize-aliases":
                autoSpecializeAliases = true
            case "--auto-specialize-palette":
                guard let value = iterator.next() else { throw Error.missingValue(arg) }
                let normalized = value.lowercased()
                switch normalized {
                case "standard":
                    paletteKind = .standard
                case "none":
                    paletteKind = nil
                default:
                    throw Error.unknownFlag("--auto-specialize-palette \(value)")
                }
            case "--auto-specialize-registrations":
                guard let value = iterator.next() else { throw Error.missingValue(arg) }
                hostRegistrationsPath = URL(fileURLWithPath: value)
            case "--auto-specialize":
                // Aggregator: accepts comma/plus-separated list of strategies: aliases, palette, registrations, all
                guard let value = iterator.next() else { throw Error.missingValue(arg) }
                let tokens = value
                    .replacingOccurrences(of: "+", with: ",")
                    .split(separator: ",")
                    .map { $0.trimmingCharacters(in: .whitespacesAndNewlines).lowercased() }
                    .filter { !$0.isEmpty }
                for t in tokens {
                    switch t {
                    case "aliases":
                        autoSpecializeAliases = true
                    case "palette":
                        // Default to standard palette unless explicitly overridden elsewhere
                        if paletteKind == nil { paletteKind = .standard }
                    case "registrations":
                        // Mark that registrations were requested via aggregator; requires manifest via --auto-specialize-registrations
                        aggregatorRequestedRegistrations = true
                    case "all":
                        autoSpecializeAliases = true
                        if paletteKind == nil { paletteKind = .standard }
                        aggregatorRequestedRegistrations = true
                    default:
                        throw Error.unknownFlag("--auto-specialize \(t)")
                    }
                }
            default:
                if arg == "--allow-types" {
                    guard let value = iterator.next() else { throw Error.missingValue(arg) }
                    let path = URL(fileURLWithPath: value)
                    allowlistPath = path
                    if let moduleName {
                        allowlistData = loadAllowlist(at: path, moduleName: moduleName)
                    }
                    continue
                }
                throw Error.unknownFlag(arg)
            }
        }

        // If aggregator requested registrations but no manifest path was supplied, warn and continue
        if aggregatorRequestedRegistrations && hostRegistrationsPath == nil {
            FileHandle.standardError.write(Data(("warning: --auto-specialize requested 'registrations' but no manifest provided. Supply --auto-specialize-registrations <FILE> to enable.\n").utf8))
        }

        guard let moduleName else { throw Error.missingRequired("--module") }
        guard let interfacePath else { throw Error.missingRequired("--interface") }
        guard let outputDirectory else { throw Error.missingRequired("--out") }

        if let path = allowlistPath, allowlistData == nil {
            allowlistData = loadAllowlist(at: path, moduleName: moduleName)
        }

        return Options(moduleName: moduleName,
                       interfacePath: interfacePath,
                       outputDirectory: outputDirectory,
                       configPath: configPath,
                       filePrefix: filePrefix,
                       installerBasename: installerBasename,
                       autoRegisterInstaller: autoRegisterInstaller,
                       reportPath: reportPath,
                       dumpSurfacePath: dumpSurfacePath,
                       dumpInventoryPath: dumpInventoryPath,
                       enableSwiftFormat: enableSwiftFormat,
                       swiftFormatPath: swiftFormatPath,
                       autoSpecializeAliases: autoSpecializeAliases,
                       paletteKind: paletteKind,
                       hostRegistrationsPath: hostRegistrationsPath,
                       allowlistPath: allowlistPath,
                       allowlistData: allowlistData)
    }

    private func loadConfig(at url: URL?) throws -> HostBridgeGenerationConfig {
        guard let url else { return HostBridgeGenerationConfig() }
        do {
            let data = try Data(contentsOf: url)
            let decoder = JSONDecoder()
            let file = try decoder.decode(BridgeGenConfigFile.self, from: data)
            return try file.makeConfig()
        } catch {
            throw Error.configReadFailed(error.localizedDescription)
        }
    }

    private func loadHostRegistrations(at url: URL?) -> [String]? {
        guard let url else { return nil }
        struct File: Decodable { let registered_types: [String] }
        do {
            let data = try Data(contentsOf: url)
            let file = try JSONDecoder().decode(File.self, from: data)
            return file.registered_types
        } catch {
            FileHandle.standardError.write(Data(("Failed to read host registrations: \(error)\n").utf8))
            return nil
        }
    }

    private func loadAllowlist(at url: URL?, moduleName: String) -> AllowlistData? {
        guard let url else { return nil }
        let defaultModule = moduleName.isEmpty ? nil : moduleName
        if url.pathExtension.lowercased() == "json" {
            struct File: Decodable {
                let types: [String]?
                let selectors: [String: [String]]?
            }
            do {
                let data = try Data(contentsOf: url)
                let file = try JSONDecoder().decode(File.self, from: data)
                return canonicalizeAllowlist(types: file.types ?? [],
                                             selectors: file.selectors ?? [:],
                                             defaultModule: defaultModule)
            } catch {
                FileHandle.standardError.write(Data(("Failed to read allow types: \(error)\n").utf8))
                return canonicalizeAllowlist(types: [], selectors: [:], defaultModule: defaultModule)
            }
        } else {
            do {
                let raw = try String(contentsOf: url)
                let lines = raw.split(separator: "\n").map { $0.trimmingCharacters(in: .whitespacesAndNewlines) }
                    .filter { !$0.isEmpty && !$0.hasPrefix("#") }
                return canonicalizeAllowlist(types: lines, selectors: [:], defaultModule: defaultModule)
            } catch {
                FileHandle.standardError.write(Data(("Failed to read allow types: \(error)\n").utf8))
                return canonicalizeAllowlist(types: [], selectors: [:], defaultModule: defaultModule)
            }
        }
    }

    private func canonicalizeAllowlist<S: Sequence>(types: S,
                                                    selectors: [String: [String]],
                                                    defaultModule: String?) -> AllowlistData where S.Element == String {
        var canonicalTypes: [String] = []
        canonicalTypes.reserveCapacity(16)
        let environment = ProcessInfo.processInfo.environment
        let previewOnly = environment["BRIDGEGEN_CANONICALIZE_ALLOWLIST_PREVIEW"] != nil
        let applyCanonicalization = environment["BRIDGEGEN_CANONICALIZE_ALLOWLIST_APPLY"] != nil
        for t in types {
            let raw = String(t)
            do {
                let parsed = try TypeNameParser.parse(raw)
                let baseline = parsed.canonicalDescription()
                if let canonical = CanonicalTypeNameHelper.canonicalizedAllowlistEntry(raw,
                                                                                       defaultModule: defaultModule) {
                    if applyCanonicalization {
                        if previewOnly && canonical != baseline {
                            FileHandle.standardError.write(Data(("allowlist canonicalization (applying): \(baseline) -> \(canonical)\n").utf8))
                        }
                        canonicalTypes.append(canonical)
                    } else if previewOnly && canonical != baseline {
                        FileHandle.standardError.write(Data(("allowlist canonicalization: \(baseline) -> \(canonical)\n").utf8))
                        canonicalTypes.append(baseline)
                    } else {
                        canonicalTypes.append(baseline)
                    }
                } else {
                    canonicalTypes.append(baseline)
                }
            } catch {
                canonicalTypes.append(raw)
            }
        }

        var canonicalSelectors: [String: Set<String>] = [:]
        canonicalSelectors.reserveCapacity(selectors.count)

        for (rawKey, valueList) in selectors {
            let canonicalKey: String
            if let parsed = try? TypeNameParser.parse(rawKey) {
                var base = parsed
                base.genericArguments = []
                base.genericOwnerIndex = nil
                canonicalKey = base.canonicalDescription()
            } else if let alt = CanonicalTypeNameHelper.canonicalizedAllowlistEntry(rawKey,
                                                                                    defaultModule: defaultModule) {
                canonicalKey = alt
            } else {
                canonicalKey = rawKey
            }
            var set = canonicalSelectors[canonicalKey] ?? Set<String>()
            for selector in valueList {
                if !selector.isEmpty {
                    set.insert(selector)
                }
            }
            if !set.isEmpty {
                canonicalSelectors[canonicalKey] = set
            }
        }

        return AllowlistData(typeNames: canonicalTypes,
                             selectorsByType: canonicalSelectors)
    }

    private func report(summary: HostBridgeGenerationSummary) {
        let diagnostics = summary.surfaceResult.diagnostics + summary.generationResult.diagnostics + summary.formatDiagnostics
        if !diagnostics.isEmpty {
            for diagnostic in diagnostics {
                FileHandle.standardError.write(Data((diagnostic.description + "\n").utf8))
            }
        }
    }
}

struct AllowlistData {
    let typeNames: [String]
    let selectorsByType: [String: Set<String>]
}

private struct BridgeGenConfigFile: Decodable {
    let specializations: [String: [String: [String]]]
    let propertySpecializations: [String: [String: [String: [String]]]]?
    let protocolWitnesses: [String: [String]]?
    let conformances: [String: [String]]
    let exportedTypeNames: [String: String]?
    let maxSpecializationsPerSelector: Int?
    let maxDomainPerGenericParam: Int?
    let forceSpecializations: [String: [String]]?

    init(specializations: [String: [String: [String]]] = [:],
         propertySpecializations: [String: [String: [String: [String]]]]? = nil,
         protocolWitnesses: [String: [String]]? = nil,
         conformances: [String: [String]] = [:],
         exportedTypeNames: [String: String]? = nil,
         maxSpecializationsPerSelector: Int? = nil,
         maxDomainPerGenericParam: Int? = nil,
         forceSpecializations: [String: [String]]? = nil) {
        self.specializations = specializations
        self.propertySpecializations = propertySpecializations
        self.protocolWitnesses = protocolWitnesses
        self.conformances = conformances
        self.exportedTypeNames = exportedTypeNames
        self.maxSpecializationsPerSelector = maxSpecializationsPerSelector
        self.maxDomainPerGenericParam = maxDomainPerGenericParam
        self.forceSpecializations = forceSpecializations
    }

    private enum CodingKeys: String, CodingKey {
        case specializations
        case propertySpecializations = "property_specializations"
        case conformances
        case protocolWitnesses = "protocol_witnesses"
        case exportedTypeNames = "exported_type_names"
        case maxSpecializationsPerSelector
        case maxDomainPerGenericParam
        case forceSpecializations
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        let specValues = try container.decodeIfPresent([String: [String: [String]]].self, forKey: .specializations) ?? [:]
        let propertyValues = try container.decodeIfPresent([String: [String: [String: [String]]]].self, forKey: .propertySpecializations)
        let confValues = try container.decodeIfPresent([String: [String]].self, forKey: .conformances) ?? [:]
        let witnessValues = try container.decodeIfPresent([String: [String]].self, forKey: .protocolWitnesses)
        self.specializations = specValues
        self.propertySpecializations = propertyValues
        self.protocolWitnesses = witnessValues
        self.conformances = confValues
        self.exportedTypeNames = try container.decodeIfPresent([String: String].self, forKey: .exportedTypeNames)
        self.maxSpecializationsPerSelector = try container.decodeIfPresent(Int.self, forKey: .maxSpecializationsPerSelector)
        self.maxDomainPerGenericParam = try container.decodeIfPresent(Int.self, forKey: .maxDomainPerGenericParam)
        self.forceSpecializations = try container.decodeIfPresent([String: [String]].self, forKey: .forceSpecializations)
    }

    func makeConfig() throws -> HostBridgeGenerationConfig {
        var mapping: [String: [String: [TypeName]]] = [:]
        for (typeName, paramMap) in specializations {
            var typeMapping: [String: [TypeName]] = [:]
            for (param, typeStrings) in paramMap {
                typeMapping[param] = try typeStrings.map { try TypeNameParser.parse($0) }
            }
            mapping[typeName] = typeMapping
        }
        var propertyMapping: [String: [String: [String: [TypeName]]]] = [:]
        if let propertySpecializations {
            for (typeName, propertyMap) in propertySpecializations {
                let canonicalType = try TypeNameParser.parse(typeName).canonicalDescription()
                var propertyEntries: [String: [String: [TypeName]]] = [:]
                for (propertyKey, paramMap) in propertyMap {
                    var parameterEntries: [String: [TypeName]] = [:]
                    for (param, typeStrings) in paramMap {
                        parameterEntries[param] = try typeStrings.map { try TypeNameParser.parse($0) }
                    }
                    propertyEntries[propertyKey] = parameterEntries
                }
                propertyMapping[canonicalType] = propertyEntries
            }
        }
        var conformanceTable: [String: Set<String>] = [:]
        for (typeName, protocols) in conformances {
            let canonicalType = try TypeNameParser.parse(typeName).canonicalDescription()
            var set: Set<String> = []
            for proto in protocols {
                let canonicalProto = try TypeNameParser.parse(proto).canonicalDescription()
                set.insert(canonicalProto)
            }
            conformanceTable[canonicalType] = set
        }
        var witnessMap: [String: [String]] = [:]
        if let protocolWitnesses {
            for (proto, witnesses) in protocolWitnesses {
                let canonicalProto = try TypeNameParser.parse(proto).canonicalDescription()
                var canonicalWitnesses: [String] = []
                for witness in witnesses {
                    let canonicalWitness = try TypeNameParser.parse(witness).canonicalDescription()
                    canonicalWitnesses.append(canonicalWitness)
                }
                if !canonicalWitnesses.isEmpty {
                    witnessMap[canonicalProto] = canonicalWitnesses
                }
            }
        }
        var forceMap: [String: Set<String>] = [:]
        if let forceSpecializations {
            for (typeName, selectors) in forceSpecializations {
                let canonicalType = try TypeNameParser.parse(typeName).canonicalDescription()
                forceMap[canonicalType] = Set(selectors)
            }
        }
        let maxPerSelector = maxSpecializationsPerSelector ?? HostBridgeGenerationConfig.defaultMaxSpecializationsPerSelector
        let maxPerGeneric = maxDomainPerGenericParam ?? HostBridgeGenerationConfig.defaultMaxDomainPerGenericParam
        var exportedNames: [String: String] = [:]
        if let exportedTypeNames {
            for (rawCanonical, exported) in exportedTypeNames {
                let canonicalType = try TypeNameParser.parse(rawCanonical).canonicalDescription()
                exportedNames[canonicalType] = exported
            }
        }

        return HostBridgeGenerationConfig(specializationsByType: mapping,
                                          propertySpecializationsByType: propertyMapping,
                                          conformanceTable: conformanceTable,
                                          exportedTypeNames: exportedNames,
                                          protocolWitnesses: witnessMap,
                                          maxSpecializationsPerSelector: maxPerSelector,
                                          maxDomainPerGenericParam: maxPerGeneric,
                                          forceSpecializations: forceMap)
    }
}
