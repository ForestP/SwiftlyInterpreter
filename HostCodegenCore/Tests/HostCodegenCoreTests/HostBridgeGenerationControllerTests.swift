//
//  HostBridgeGenerationControllerTests.swift
//  HostCodegenCore
//
//  Created by Forest Plasencia on 9/19/25.
//

import XCTest
@testable import HostSurfaceKit
@testable import HostCodegenCore

final class HostBridgeGenerationControllerTests: XCTestCase {

    private var tempDirectory: URL!

    override func setUp() {
        super.setUp()
        tempDirectory = FileManager.default.temporaryDirectory.appendingPathComponent("HostBridgeGenerationControllerTests_\(UUID().uuidString)")
    }

    override func tearDown() {
        if let tempDirectory,
           FileManager.default.fileExists(atPath: tempDirectory.path) {
            try? FileManager.default.removeItem(at: tempDirectory)
        }
        tempDirectory = nil
        super.tearDown()
    }

    func testControllerWritesFilesToDisk() throws {
        let interfaceSource = """
        public struct Foo {
            public init()
            public func latest() -> Foo
        }
        """
        let interfaceURL = tempDirectory.appendingPathComponent("Demo.swiftinterface")
        try FileManager.default.createDirectory(at: tempDirectory,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
        try interfaceSource.write(to: interfaceURL, atomically: true, encoding: .utf8)

        let outputDirectory = tempDirectory.appendingPathComponent("out")
        let reportURL = tempDirectory.appendingPathComponent("report/report.json")
        let request = HostBridgeGenerationRequest(
            moduleName: "Demo",
            interfaceURL: interfaceURL,
            outputDirectory: outputDirectory,
            reportURL: reportURL
        )
        let controller = HostBridgeGenerationController(request: request)
        let summary = try controller.run()

        XCTAssertTrue(summary.surfaceResult.diagnostics.isEmpty)
        XCTAssertEqual(summary.surfaceResult.collectorFallbackPlaceholderHitCount, 0)
        XCTAssertTrue(summary.generationResult.diagnostics.isEmpty)
        XCTAssertEqual(summary.files.count, 2)
        XCTAssertEqual(summary.writtenFiles.count, 2)
        XCTAssertEqual(summary.report.moduleName, "Demo")
        XCTAssertFalse(summary.report.types.isEmpty)
        XCTAssertEqual(summary.report.metrics.collectorFallbackPlaceholderHitCount,
                       summary.surfaceResult.collectorFallbackPlaceholderHitCount)
        XCTAssertEqual(summary.report.metrics.resolverPlaceholderDropHitCount,
                       summary.surfaceResult.resolverPlaceholderDropHitCount)

        let installerURL = summary.writtenFiles.first { $0.lastPathComponent == "GeneratedHostBridges.generated.swift" }
        XCTAssertNotNil(installerURL)
        if let installerURL {
            let contents = try String(contentsOf: installerURL)
            XCTAssertTrue(contents.contains("installGeneratedHostBridges"))
        }

        XCTAssertTrue(FileManager.default.fileExists(atPath: reportURL.path))
        let data = try Data(contentsOf: reportURL)
        let decoded = try JSONDecoder().decode(HostBridgeGenerationReport.self, from: data)
        XCTAssertEqual(decoded.types.count, summary.report.types.count)
        XCTAssertEqual(decoded.metrics.collectorFallbackPlaceholderHitCount,
                       summary.report.metrics.collectorFallbackPlaceholderHitCount)
        XCTAssertEqual(decoded.metrics.resolverPlaceholderDropHitCount,
                       summary.report.metrics.resolverPlaceholderDropHitCount)
    }

    func testControllerAppliesAliasStrategyWhenEnabled() throws {
        let interfaceSource = """
        public struct Box<Value> {
            public var stored: Value { get }
        }
        public typealias StringBox = Box<Swift.String>
        public typealias IntBox = Box<Swift.Int>
        """
        let interfaceURL = tempDirectory.appendingPathComponent("DemoAlias.swiftinterface")
        try FileManager.default.createDirectory(at: tempDirectory,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
        try interfaceSource.write(to: interfaceURL, atomically: true, encoding: .utf8)

        let outputDirectory = tempDirectory.appendingPathComponent("out-auto")
        let reportURL = tempDirectory.appendingPathComponent("report/auto_report.json")
        let request = HostBridgeGenerationRequest(
            moduleName: "Demo",
            interfaceURL: interfaceURL,
            outputDirectory: outputDirectory,
            reportURL: reportURL,
            enableTypeAliasStrategy: true
        )
        let controller = HostBridgeGenerationController(request: request)
        let summary = try controller.run()

        XCTAssertTrue(summary.generationResult.diagnostics.isEmpty)
        XCTAssertEqual(summary.surfaceResult.collectorFallbackPlaceholderHitCount, 0)
        guard let propertyArtifact = summary.generationResult.propertyArtifacts.first(where: { $0.descriptor.name == "stored" }) else {
            return XCTFail("Missing stored property artifact")
        }
        XCTAssertEqual(propertyArtifact.origin, .auto(strategyIdentifier: "alias"))
        XCTAssertEqual(propertyArtifact.render.getter.thunks.count, 2)

        let properties = summary.report.types.flatMap { $0.properties }
        guard let storedProperty = properties.first(where: { $0.name == "stored" }) else {
            return XCTFail("Missing stored report entry")
        }
        XCTAssertEqual(storedProperty.specializationSource, "auto.alias")
        XCTAssertEqual(storedProperty.getterSpecializationCount, 2)
        XCTAssertFalse(storedProperty.usesFallback)
    }
    
    func testControllerAppliesAliasStrategyWhenEnabled2() throws {
        let interfaceSource = """
            public struct Box<Value> {
                public var stored: Value { get }
            }
            public typealias StringBox = Box<Swift.String>
            public typealias IntBox = Box<Swift.Int>
            """
        let interfaceURL = tempDirectory.appendingPathComponent("DemoAlias.swiftinterface")
        try FileManager.default.createDirectory(at: tempDirectory,
                                                withIntermediateDirectories: true,
                                                attributes: nil)
        try interfaceSource.write(to: interfaceURL, atomically: true, encoding: .utf8)
        
        let outputDirectory = tempDirectory.appendingPathComponent("out-auto")
        let reportURL = tempDirectory.appendingPathComponent("report/auto_report.json")
        let request = HostBridgeGenerationRequest(
            moduleName: "Demo",
            interfaceURL: interfaceURL,
            outputDirectory: outputDirectory,
            reportURL: reportURL,
            enableTypeAliasStrategy: true
        )
        let controller = HostBridgeGenerationController(request: request)
        let summary = try controller.run()
        
        XCTAssertTrue(summary.generationResult.diagnostics.isEmpty)
        guard let propertyArtifact = summary.generationResult.propertyArtifacts.first(where: { $0.descriptor.name == "stored" }) else {
            return XCTFail("Missing stored property artifact")
        }
        XCTAssertEqual(propertyArtifact.origin, .auto(strategyIdentifier: "alias"))
        XCTAssertEqual(propertyArtifact.render.getter.thunks.count, 2)
        
        let properties = summary.report.types.flatMap { $0.properties }
        guard let storedProperty = properties.first(where: { $0.name == "stored" }) else {
            return XCTFail("Missing stored report entry")
        }
        XCTAssertEqual(storedProperty.specializationSource, "auto.alias")
        XCTAssertEqual(storedProperty.getterSpecializationCount, 2)
        XCTAssertFalse(storedProperty.usesFallback)
        XCTAssertEqual(summary.report.metrics.collectorFallbackPlaceholderHitCount,
                       summary.surfaceResult.collectorFallbackPlaceholderHitCount)
        XCTAssertEqual(summary.report.metrics.resolverPlaceholderDropHitCount,
                       summary.surfaceResult.resolverPlaceholderDropHitCount)
    }

    func testAllowlistMatcherCanonicallyMatchesGenericReceivers() throws {
        let matcher = TypeAllowlistMatcher(rawEntries: ["Swift.Array"], moduleName: "Swift")
        XCTAssertTrue(matcher.contains(canonicalName: "Swift.Array<Swift.Int>"))
        XCTAssertTrue(matcher.contains(canonicalName: "Swift.Array<Swift.Array<Swift.String>>"))
        XCTAssertFalse(matcher.contains(canonicalName: "Swift.Dictionary<Swift.String,Swift.Int>"))
    }

}
