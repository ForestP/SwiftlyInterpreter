
import XCTest
@testable import BridgeGenCLI
@testable import HostCodegenCore

final class BridgeGenCLITests: XCTestCase {
    private var tempDirectory: URL!

    override func setUp() {
        super.setUp()
        tempDirectory = FileManager.default.temporaryDirectory.appendingPathComponent("BridgeGenCLITests_\(UUID().uuidString)")
    }

    override func tearDown() {
        if let tempDirectory,
           FileManager.default.fileExists(atPath: tempDirectory.path) {
            try? FileManager.default.removeItem(at: tempDirectory)
        }
        tempDirectory = nil
        super.tearDown()
    }

    func testCLIParsesArgumentsAndGeneratesFiles() throws {
        try FileManager.default.createDirectory(at: tempDirectory,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
        let interfaceSource = """
        public struct Foo {
            public func wrap<T>(_ value: T) -> T
        }
        """
        let interfaceURL = tempDirectory.appendingPathComponent("Demo.swiftinterface")
        try interfaceSource.write(to: interfaceURL, atomically: true, encoding: .utf8)

        let configJSON = """
        {
            "specializations": {
                "Demo.Foo": {
                    "T": ["Swift.Int"]
                }
            },
            "conformances": {
                "Swift.Int": ["Swift.Hashable"]
            }
        }
        """
        let configURL = tempDirectory.appendingPathComponent("config.json")
        try configJSON.write(to: configURL, atomically: true, encoding: .utf8)

        let outputDirectory = tempDirectory.appendingPathComponent("out")
        let reportURL = tempDirectory.appendingPathComponent("out/report.json")

        let cli = BridgeGenCLI()
        let summary = try cli.run(arguments: [
            "bridgegen",
            "--module", "Demo",
            "--interface", interfaceURL.path,
            "--out", outputDirectory.path,
            "--config", configURL.path,
            "--file-prefix", "GeneratedHost",
            "--report", reportURL.path
        ])

        XCTAssertTrue(summary.generationResult.diagnostics.filter { diag in !(diag.description.contains("Auto-expanded") || diag.description.contains("constraints have no valid specialization")) }.isEmpty)
        XCTAssertEqual(summary.surfaceResult.collectorFallbackPlaceholderHitCount, 0)
        XCTAssertEqual(summary.surfaceResult.resolverPlaceholderDropHitCount, 0)
        XCTAssertEqual(summary.surfaceResult.resolverPlaceholderDropHitCount, 0)
        XCTAssertEqual(summary.surfaceResult.resolverPlaceholderDropHitCount, 0)
        XCTAssertEqual(summary.surfaceResult.resolverPlaceholderDropHitCount, 0)
        XCTAssertEqual(summary.surfaceResult.resolverPlaceholderDropHitCount, 0)
        XCTAssertEqual(summary.files.count, 2)
        XCTAssertEqual(summary.generationResult.methodArtifacts.count, 1)
        XCTAssertTrue(summary.formatDiagnostics.isEmpty)

        let expectedTypeFile = outputDirectory
            .appendingPathComponent("GeneratedHost/Demo_Foo+Host.generated.swift")
        XCTAssertTrue(FileManager.default.fileExists(atPath: expectedTypeFile.path))

        let installerURL = outputDirectory
            .appendingPathComponent("GeneratedHost/GeneratedHostBridges.generated.swift")
        XCTAssertTrue(FileManager.default.fileExists(atPath: installerURL.path))

        XCTAssertTrue(FileManager.default.fileExists(atPath: reportURL.path))
        let data = try Data(contentsOf: reportURL)
        let decoded = try JSONDecoder().decode(HostBridgeGenerationReport.self, from: data)
        XCTAssertEqual(decoded.moduleName, "Demo")
        XCTAssertEqual(decoded.types.count, summary.report.types.count)
        XCTAssertEqual(decoded.metrics.collectorFallbackPlaceholderHitCount,
                       summary.report.metrics.collectorFallbackPlaceholderHitCount)
        XCTAssertEqual(decoded.metrics.resolverPlaceholderDropHitCount,
                       summary.report.metrics.resolverPlaceholderDropHitCount)
    }

    func testCLIParsesPropertySpecializations() throws {
        try FileManager.default.createDirectory(at: tempDirectory,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
        let interfaceSource = """
        public struct Box<Value> {
            public var item: Value { get }
        }
        """
        let interfaceURL = tempDirectory.appendingPathComponent("Demo.swiftinterface")
        try interfaceSource.write(to: interfaceURL, atomically: true, encoding: .utf8)

        let configJSON = """
        {
            "property_specializations": {
                "Demo.Box<Value>": {
                    "instance::item": {
                        "Value": ["Swift.Int"]
                    }
                }
            }
        }
        """
        let configURL = tempDirectory.appendingPathComponent("config.json")
        try configJSON.write(to: configURL, atomically: true, encoding: .utf8)

        let outputDirectory = tempDirectory.appendingPathComponent("out")

        let cli = BridgeGenCLI()
        let summary = try cli.run(arguments: [
            "bridgegen",
            "--module", "Demo",
            "--interface", interfaceURL.path,
            "--out", outputDirectory.path,
            "--config", configURL.path
        ])

        XCTAssertTrue(summary.generationResult.diagnostics.filter { diag in !(diag.description.contains("Auto-expanded") || diag.description.contains("constraints have no valid specialization")) }.isEmpty)
        XCTAssertEqual(summary.surfaceResult.collectorFallbackPlaceholderHitCount, 0)
        XCTAssertEqual(summary.surfaceResult.resolverPlaceholderDropHitCount, 0)
        XCTAssertEqual(summary.generationResult.propertyArtifacts.count, 1)
        guard let typeEntry = summary.report.types.first,
              let propertyEntry = typeEntry.properties.first else {
            return XCTFail("Missing property entry in report")
        }
        XCTAssertEqual(propertyEntry.getterSpecializationCount, 1)
        let sanitizedType = sanitizeTypeName("Demo.Box<Value>")
        let expectedTypeFile = outputDirectory
            .appendingPathComponent("HostBridges/\(sanitizedType)+Host.generated.swift")
        XCTAssertTrue(FileManager.default.fileExists(atPath: expectedTypeFile.path))
    }

    func testCLIWithPaletteStrategyGeneratesThunks() throws {
        try FileManager.default.createDirectory(at: tempDirectory,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
        let interfaceSource = """
        public struct Foo {
            public func wrap<T>(_ value: T) -> T
        }
        """
        let interfaceURL = tempDirectory.appendingPathComponent("Demo.swiftinterface")
        try interfaceSource.write(to: interfaceURL, atomically: true, encoding: .utf8)

        let outputDirectory = tempDirectory.appendingPathComponent("out")

        let cli = BridgeGenCLI()
        let summary = try cli.run(arguments: [
            "bridgegen",
            "--module", "Demo",
            "--interface", interfaceURL.path,
            "--out", outputDirectory.path,
            "--auto-specialize-palette", "standard"
        ])

        XCTAssertTrue(summary.generationResult.diagnostics.filter { diag in !(diag.description.contains("Auto-expanded") || diag.description.contains("constraints have no valid specialization")) }.isEmpty)
        XCTAssertEqual(summary.surfaceResult.collectorFallbackPlaceholderHitCount, 0)
        XCTAssertEqual(summary.surfaceResult.resolverPlaceholderDropHitCount, 0)
        guard let artifact = summary.generationResult.methodArtifacts.first(where: { $0.descriptor.selector == "wrap(_:)" }) else {
            return XCTFail("Missing wrap(_:) artifact")
        }
        XCTAssertEqual(artifact.origin, .auto(strategyIdentifier: "palette.standard"))
        XCTAssertEqual(artifact.render.thunks.count, 6)

        let methods = summary.report.types.flatMap { $0.methods }
        guard let methodEntry = methods.first(where: { $0.selector == "wrap(_:)" }) else {
            return XCTFail("Missing wrap(_:) report entry")
        }
        XCTAssertEqual(methodEntry.specializationSource, "auto.palette.standard")
        XCTAssertEqual(methodEntry.specializationCount, 6)
    }

    func testCLIWithHostRegistrationStrategyGeneratesThunks() throws {
        try FileManager.default.createDirectory(at: tempDirectory,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
        let interfaceSource = """
        public struct Box<Value> {
            public var stored: Value { get }
        }
        """
        let interfaceURL = tempDirectory.appendingPathComponent("Demo.swiftinterface")
        try interfaceSource.write(to: interfaceURL, atomically: true, encoding: .utf8)

        let manifest = """
        { "registered_types": ["Demo.Box<Swift.String>", "Demo.Box<Swift.Int>"] }
        """
        let manifestURL = tempDirectory.appendingPathComponent("registrations.json")
        try manifest.write(to: manifestURL, atomically: true, encoding: .utf8)

        let outputDirectory = tempDirectory.appendingPathComponent("out")

        let cli = BridgeGenCLI()
        let summary = try cli.run(arguments: [
            "bridgegen",
            "--module", "Demo",
            "--interface", interfaceURL.path,
            "--out", outputDirectory.path,
            "--auto-specialize-registrations", manifestURL.path
        ])

        XCTAssertTrue(summary.generationResult.diagnostics.filter { diag in !(diag.description.contains("Auto-expanded") || diag.description.contains("constraints have no valid specialization")) }.isEmpty)
        XCTAssertEqual(summary.surfaceResult.collectorFallbackPlaceholderHitCount, 0)
        guard let prop = summary.generationResult.propertyArtifacts.first(where: { $0.descriptor.name == "stored" }) else {
            return XCTFail("Missing stored property artifact")
        }
        XCTAssertEqual(prop.origin, .auto(strategyIdentifier: "registrations"))
        XCTAssertEqual(prop.render.getter.thunks.count, 2)
        let properties = summary.report.types.flatMap { $0.properties }
        guard let entry = properties.first(where: { $0.name == "stored" }) else {
            return XCTFail("Missing stored report entry")
        }
        XCTAssertEqual(entry.specializationSource, "auto.registrations")
        XCTAssertEqual(entry.getterSpecializationCount, 2)
    }

    func testAggregatorAliasesPlusPaletteUsesPaletteDomains() throws {
        try FileManager.default.createDirectory(at: tempDirectory,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
        let interfaceSource = """
        public struct Foo {
            public func wrap<T>(_ value: T) -> T
        }
        """
        let interfaceURL = tempDirectory.appendingPathComponent("Demo.swiftinterface")
        try interfaceSource.write(to: interfaceURL, atomically: true, encoding: .utf8)

        let outputDirectory = tempDirectory.appendingPathComponent("out")

        let cli = BridgeGenCLI()
        let summary = try cli.run(arguments: [
            "bridgegen",
            "--module", "Demo",
            "--interface", interfaceURL.path,
            "--out", outputDirectory.path,
            "--auto-specialize", "aliases,palette"
        ])

        XCTAssertTrue(summary.generationResult.diagnostics.filter { diag in !(diag.description.contains("Auto-expanded") || diag.description.contains("constraints have no valid specialization")) }.isEmpty)
        guard let artifact = summary.generationResult.methodArtifacts.first(where: { $0.descriptor.selector == "wrap(_:)" }) else {
            return XCTFail("Missing wrap(_:) artifact")
        }
        // Aggregator should enable palette; with no aliases present, palette drives domains
        XCTAssertEqual(artifact.origin, .auto(strategyIdentifier: "palette.standard"))
        XCTAssertEqual(artifact.render.thunks.count, 6)
    }

    func testAggregatorRegistrationsWithManifest() throws {
        try FileManager.default.createDirectory(at: tempDirectory,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
        let interfaceSource = """
        public struct Box<Value> {
            public var stored: Value { get }
        }
        """
        let interfaceURL = tempDirectory.appendingPathComponent("Demo.swiftinterface")
        try interfaceSource.write(to: interfaceURL, atomically: true, encoding: .utf8)

        let manifest = """
        { "registered_types": ["Demo.Box<Swift.String>", "Demo.Box<Swift.Int>"] }
        """
        let manifestURL = tempDirectory.appendingPathComponent("registrations.json")
        try manifest.write(to: manifestURL, atomically: true, encoding: .utf8)

        let outputDirectory = tempDirectory.appendingPathComponent("out")

        let cli = BridgeGenCLI()
        let summary = try cli.run(arguments: [
            "bridgegen",
            "--module", "Demo",
            "--interface", interfaceURL.path,
            "--out", outputDirectory.path,
            "--auto-specialize", "registrations",
            "--auto-specialize-registrations", manifestURL.path
        ])

        XCTAssertTrue(summary.generationResult.diagnostics.filter { diag in !(diag.description.contains("Auto-expanded") || diag.description.contains("constraints have no valid specialization")) }.isEmpty)
        guard let prop = summary.generationResult.propertyArtifacts.first(where: { $0.descriptor.name == "stored" }) else {
            return XCTFail("Missing stored property artifact")
        }
        XCTAssertEqual(prop.origin, .auto(strategyIdentifier: "registrations"))
        XCTAssertEqual(prop.render.getter.thunks.count, 2)
    }

    func testAggregatorRegistrationsPrecedenceOverPalette() throws {
        try FileManager.default.createDirectory(at: tempDirectory,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
        let interfaceSource = """
        public struct Box<Value> {
            public var stored: Value { get }
        }
        """
        let interfaceURL = tempDirectory.appendingPathComponent("Demo.swiftinterface")
        try interfaceSource.write(to: interfaceURL, atomically: true, encoding: .utf8)

        let manifest = """
        { "registered_types": ["Demo.Box<Swift.String>", "Demo.Box<Swift.Int>"] }
        """
        let manifestURL = tempDirectory.appendingPathComponent("registrations.json")
        try manifest.write(to: manifestURL, atomically: true, encoding: .utf8)

        let outputDirectory = tempDirectory.appendingPathComponent("out")

        let cli = BridgeGenCLI()
        // Both palette and registrations enabled; registrations should take precedence over palette
        let summary = try cli.run(arguments: [
            "bridgegen",
            "--module", "Demo",
            "--interface", interfaceURL.path,
            "--out", outputDirectory.path,
            "--auto-specialize", "palette,registrations",
            "--auto-specialize-registrations", manifestURL.path
        ])

        XCTAssertTrue(summary.generationResult.diagnostics.filter { diag in !(diag.description.contains("Auto-expanded") || diag.description.contains("constraints have no valid specialization")) }.isEmpty)
        guard let prop = summary.generationResult.propertyArtifacts.first(where: { $0.descriptor.name == "stored" }) else {
            return XCTFail("Missing stored property artifact")
        }
        XCTAssertEqual(prop.origin, .auto(strategyIdentifier: "registrations"))
        XCTAssertEqual(prop.render.getter.thunks.count, 2)
    }

    func testFallbackReportedWhenCapExceeded() throws {
        try FileManager.default.createDirectory(at: tempDirectory,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
        let interfaceSource = """
        public struct Foo {
            public func wrap<T>(_ value: T) -> T
        }
        """
        let interfaceURL = tempDirectory.appendingPathComponent("Demo.swiftinterface")
        try interfaceSource.write(to: interfaceURL, atomically: true, encoding: .utf8)

        // Force a low cap so palette overflows → fallback
        let configJSON = """
        {
            "maxSpecializationsPerSelector": 2
        }
        """
        let configURL = tempDirectory.appendingPathComponent("config.json")
        try configJSON.write(to: configURL, atomically: true, encoding: .utf8)

        let outputDirectory = tempDirectory.appendingPathComponent("out")
        let reportURL = tempDirectory.appendingPathComponent("out/report.json")

        let cli = BridgeGenCLI()
        let summary = try cli.run(arguments: [
            "bridgegen",
            "--module", "Demo",
            "--interface", interfaceURL.path,
            "--out", outputDirectory.path,
            "--config", configURL.path,
            "--auto-specialize-palette", "standard",
            "--report", reportURL.path
        ])

        XCTAssertTrue(summary.generationResult.diagnostics.contains { $0.description.contains("Auto-expanded specialization combinations") })
        guard let artifact = summary.generationResult.methodArtifacts.first(where: { $0.descriptor.selector == "wrap(_:)" }) else {
            return XCTFail("Missing wrap(_:) artifact")
        }
        XCTAssertNil(artifact.render.fallback)

        // Verify JSON report marks usesFallback
        let data = try Data(contentsOf: reportURL)
        let decoded = try JSONDecoder().decode(HostBridgeGenerationReport.self, from: data)
        let methods = decoded.types.flatMap { $0.methods }
        guard let methodEntry = methods.first(where: { $0.selector == "wrap(_:)" }) else {
            return XCTFail("Missing wrap(_:) report entry")
        }
        XCTAssertEqual(methodEntry.specializationSource, "auto.palette.standard")
        XCTAssertFalse(methodEntry.usesFallback)
        XCTAssertEqual(decoded.metrics.collectorFallbackPlaceholderHitCount,
                       summary.report.metrics.collectorFallbackPlaceholderHitCount)
        XCTAssertEqual(decoded.metrics.resolverPlaceholderDropHitCount,
                       summary.report.metrics.resolverPlaceholderDropHitCount)

        // CLI summary lines include source and fallback
        let lines = BridgeGenCLI.specializationSummaryLines(from: summary.report.types)
        XCTAssertTrue(lines.contains { $0.contains("source=auto.palette.standard") })
        XCTAssertFalse(lines.contains { $0.contains("uses fallback") })
    }

    func testCLISummaryLinesForRegistrations() throws {
        try FileManager.default.createDirectory(at: tempDirectory,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
        let interfaceSource = """
        public struct Box<Value> {
            public var stored: Value { get }
        }
        """
        let interfaceURL = tempDirectory.appendingPathComponent("Demo.swiftinterface")
        try interfaceSource.write(to: interfaceURL, atomically: true, encoding: .utf8)

        let manifest = """
        { "registered_types": ["Demo.Box<Swift.String>", "Demo.Box<Swift.Int>"] }
        """
        let manifestURL = tempDirectory.appendingPathComponent("registrations.json")
        try manifest.write(to: manifestURL, atomically: true, encoding: .utf8)

        let outputDirectory = tempDirectory.appendingPathComponent("out")

        let cli = BridgeGenCLI()
        let summary = try cli.run(arguments: [
            "bridgegen",
            "--module", "Demo",
            "--interface", interfaceURL.path,
            "--out", outputDirectory.path,
            "--auto-specialize-registrations", manifestURL.path
        ])

        let lines = BridgeGenCLI.specializationSummaryLines(from: summary.report.types)
        XCTAssertTrue(lines.contains { $0.contains("source=auto.registrations") })
    }

    func testAliasStrategySummaryLinesAndDomainSizes() throws {
        try FileManager.default.createDirectory(at: tempDirectory,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
        let interfaceSource = """
        public struct Box<Value> {
            public var stored: Value { get }
        }
        public typealias BoxOfString = Demo.Box<Swift.String>
        """
        let interfaceURL = tempDirectory.appendingPathComponent("Demo.swiftinterface")
        try interfaceSource.write(to: interfaceURL, atomically: true, encoding: .utf8)

        let outputDirectory = tempDirectory.appendingPathComponent("out")
        let reportURL = tempDirectory.appendingPathComponent("out/report.json")

        let cli = BridgeGenCLI()
        let summary = try cli.run(arguments: [
            "bridgegen",
            "--module", "Demo",
            "--interface", interfaceURL.path,
            "--out", outputDirectory.path,
            "--auto-specialize-aliases",
            "--report", reportURL.path
        ])

        // Summary lines include auto.alias for property
        let lines = BridgeGenCLI.specializationSummaryLines(from: summary.report.types)
        XCTAssertTrue(lines.contains { $0.contains("source=auto.alias") })

        // Domain sizes populated in JSON report for Value -> 1
        let data = try Data(contentsOf: reportURL)
        let decoded = try JSONDecoder().decode(HostBridgeGenerationReport.self, from: data)
        let props = decoded.types.flatMap { $0.properties }
        guard let stored = props.first(where: { $0.name == "stored" }) else {
            return XCTFail("Missing stored property report entry")
        }
        XCTAssertEqual(stored.genericParameterDomainSizes["Value"], 1)
        XCTAssertEqual(decoded.metrics.collectorFallbackPlaceholderHitCount,
                       summary.report.metrics.collectorFallbackPlaceholderHitCount)
        XCTAssertEqual(decoded.metrics.resolverPlaceholderDropHitCount,
                       summary.report.metrics.resolverPlaceholderDropHitCount)
    }

    func testPaletteDomainSizesInReport() throws {
        try FileManager.default.createDirectory(at: tempDirectory,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
        let interfaceSource = """
        public struct Foo {
            public func wrap<T>(_ value: T) -> T
        }
        """
        let interfaceURL = tempDirectory.appendingPathComponent("Demo.swiftinterface")
        try interfaceSource.write(to: interfaceURL, atomically: true, encoding: .utf8)

        let outputDirectory = tempDirectory.appendingPathComponent("out")
        let reportURL = tempDirectory.appendingPathComponent("out/report.json")

        let cli = BridgeGenCLI()
        let summary = try cli.run(arguments: [
            "bridgegen",
            "--module", "Demo",
            "--interface", interfaceURL.path,
            "--out", outputDirectory.path,
            "--auto-specialize-palette", "standard",
            "--report", reportURL.path
        ])

        XCTAssertTrue(summary.generationResult.diagnostics.filter { diag in !(diag.description.contains("Auto-expanded") || diag.description.contains("constraints have no valid specialization")) }.isEmpty)
        let data = try Data(contentsOf: reportURL)
        let decoded = try JSONDecoder().decode(HostBridgeGenerationReport.self, from: data)
        let methods = decoded.types.flatMap { $0.methods }
        guard let method = methods.first(where: { $0.selector == "wrap(_:)" }) else {
            return XCTFail("Missing wrap(_:) method in report")
        }
        XCTAssertEqual(method.specializationSource, "auto.palette.standard")
        XCTAssertEqual(method.genericParameterDomainSizes["T"], 6)
        XCTAssertFalse(method.usesFallback)
        XCTAssertEqual(decoded.metrics.collectorFallbackPlaceholderHitCount,
                       summary.report.metrics.collectorFallbackPlaceholderHitCount)
        XCTAssertEqual(decoded.metrics.resolverPlaceholderDropHitCount,
                       summary.report.metrics.resolverPlaceholderDropHitCount)
    }

    func testRegistrationsDomainSizesInReport() throws {
        try FileManager.default.createDirectory(at: tempDirectory,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
        let interfaceSource = """
        public struct Box<Value> {
            public var stored: Value { get }
        }
        """
        let interfaceURL = tempDirectory.appendingPathComponent("Demo.swiftinterface")
        try interfaceSource.write(to: interfaceURL, atomically: true, encoding: .utf8)

        let manifest = """
        { "registered_types": ["Demo.Box<Swift.String>", "Demo.Box<Swift.Int>"] }
        """
        let manifestURL = tempDirectory.appendingPathComponent("registrations.json")
        try manifest.write(to: manifestURL, atomically: true, encoding: .utf8)

        let outputDirectory = tempDirectory.appendingPathComponent("out")
        let reportURL = tempDirectory.appendingPathComponent("out/report.json")

        let cli = BridgeGenCLI()
        let summary = try cli.run(arguments: [
            "bridgegen",
            "--module", "Demo",
            "--interface", interfaceURL.path,
            "--out", outputDirectory.path,
            "--auto-specialize-registrations", manifestURL.path,
            "--report", reportURL.path
        ])

        XCTAssertTrue(summary.generationResult.diagnostics.filter { diag in !(diag.description.contains("Auto-expanded") || diag.description.contains("constraints have no valid specialization")) }.isEmpty)
        let data = try Data(contentsOf: reportURL)
        let decoded = try JSONDecoder().decode(HostBridgeGenerationReport.self, from: data)
        let props = decoded.types.flatMap { $0.properties }
        guard let stored = props.first(where: { $0.name == "stored" }) else {
            return XCTFail("Missing stored property in report")
        }
        XCTAssertEqual(stored.specializationSource, "auto.registrations")
        XCTAssertEqual(stored.genericParameterDomainSizes["Value"], 2)
        XCTAssertFalse(stored.usesFallback)
        XCTAssertEqual(decoded.metrics.collectorFallbackPlaceholderHitCount,
                       summary.report.metrics.collectorFallbackPlaceholderHitCount)
        XCTAssertEqual(decoded.metrics.resolverPlaceholderDropHitCount,
                       summary.report.metrics.resolverPlaceholderDropHitCount)
    }
}

private func sanitizeTypeName(_ typeName: String) -> String {
    let allowed = CharacterSet.alphanumerics.union(CharacterSet(charactersIn: "_"))
    let components = typeName.split(separator: ".")
    return components.map { component in
        component.unicodeScalars.reduce(into: "") { partialResult, scalar in
            if allowed.contains(scalar) {
                partialResult.append(Character(scalar))
            } else {
                partialResult.append("_")
            }
        }
    }.joined(separator: "_")
}
