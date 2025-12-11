import XCTest
@testable import BridgeGenCLI

final class ExamplesSnapshotTests: XCTestCase {
    // Write embedded example content to a temp file and return its path
    private func writeExample(named name: String) throws -> String {
        let contents: String
        switch name {
        case "DemoAliases.swiftinterface":
            contents = """
            public struct Box<Value> {
                public init(item: Value)
                public var item: Value { get }
            }

            public typealias StringBox = Box<Swift.String>
            public typealias IntBox = Box<Swift.Int>
            """
        case "Demo.swiftinterface":
            contents = """
            public struct Foo {
                public init(value: Swift.Int)
                public func doubled() -> Swift.Int
                public mutating func increment(by amount: Swift.Int)
                public func wrap<T>(_ value: T) -> T
                public static func makeDefault() -> Foo
            }

            public struct Box<Value> {
                public init(item: Value)
                public var item: Value { get }
            }
            """
        case "Registrations.json":
            contents = """
            {"registered_types":["Demo.Box<Swift.String>","Demo.Box<Swift.Int>"]}
            """
        default:
            XCTFail("Unknown example name: \(name)")
            throw NSError(domain: "Examples", code: 1)
        }
        let url = FileManager.default.temporaryDirectory
            .appendingPathComponent("BridgeGenCLI_Examples_\(UUID().uuidString)")
        try FileManager.default.createDirectory(at: url, withIntermediateDirectories: true)
        let fileURL = url.appendingPathComponent(name)
        try contents.trimmingCharacters(in: .whitespacesAndNewlines).appending("\n").write(to: fileURL, atomically: true, encoding: .utf8)
        return fileURL.path
    }

    func testExamples_Aliases_AutoSpecializeAliases() throws {
        let interfacePath = try writeExample(named: "DemoAliases.swiftinterface")
        let outDir = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        try FileManager.default.createDirectory(at: outDir, withIntermediateDirectories: true)

        let cli = BridgeGenCLI()
        let summary = try cli.run(arguments: [
            "bridgegen",
            "--module", "Demo",
            "--interface", interfacePath,
            "--out", outDir.path,
            "--auto-specialize-aliases"
        ])

        XCTAssertTrue(summary.generationResult.diagnostics.filter { $0.description.contains("error") }.isEmpty)
        XCTAssertEqual(summary.surfaceResult.collectorFallbackPlaceholderHitCount, 0)
        XCTAssertEqual(summary.surfaceResult.resolverPlaceholderDropHitCount, 0)
        // Installer present
        let installer = summary.writtenFiles.first { $0.lastPathComponent == "GeneratedHostBridges.generated.swift" }
        XCTAssertNotNil(installer)
        // At least one type file
        XCTAssertTrue(summary.writtenFiles.contains { $0.lastPathComponent.hasSuffix("+Host.generated.swift") })
        // Report provenance
        let lines = BridgeGenCLI.specializationSummaryLines(from: summary.report.types)
        XCTAssertTrue(lines.contains { $0.contains("source=auto.alias") })
    }

    func testExamples_Palette_AutoSpecializePalette() throws {
        let interfacePath = try writeExample(named: "Demo.swiftinterface")
        let outDir = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        try FileManager.default.createDirectory(at: outDir, withIntermediateDirectories: true)

        let cli = BridgeGenCLI()
        let summary = try cli.run(arguments: [
            "bridgegen",
            "--module", "Demo",
            "--interface", interfacePath,
            "--out", outDir.path,
            "--auto-specialize-palette", "standard"
        ])

        XCTAssertTrue(summary.generationResult.diagnostics.filter { $0.description.contains("error") }.isEmpty)
        XCTAssertEqual(summary.surfaceResult.collectorFallbackPlaceholderHitCount, 0)
        XCTAssertEqual(summary.surfaceResult.resolverPlaceholderDropHitCount, 0)
        XCTAssertTrue(summary.writtenFiles.contains { $0.lastPathComponent == "GeneratedHostBridges.generated.swift" })
        XCTAssertTrue(summary.writtenFiles.contains { $0.lastPathComponent.hasSuffix("+Host.generated.swift") })
        let lines = BridgeGenCLI.specializationSummaryLines(from: summary.report.types)
        XCTAssertTrue(lines.contains { $0.contains("source=auto.palette.standard") })
    }

    func testExamples_Registrations_AutoSpecializeRegistrations() throws {
        let interfacePath = try writeExample(named: "Demo.swiftinterface")
        let manifestPath = try writeExample(named: "Registrations.json")
        let outDir = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        try FileManager.default.createDirectory(at: outDir, withIntermediateDirectories: true)

        let cli = BridgeGenCLI()
        let summary = try cli.run(arguments: [
            "bridgegen",
            "--module", "Demo",
            "--interface", interfacePath,
            "--out", outDir.path,
            "--auto-specialize-registrations", manifestPath
        ])

        XCTAssertTrue(summary.generationResult.diagnostics.filter { $0.description.contains("error") }.isEmpty)
        XCTAssertEqual(summary.surfaceResult.collectorFallbackPlaceholderHitCount, 0)
        XCTAssertEqual(summary.surfaceResult.resolverPlaceholderDropHitCount, 0)
        XCTAssertTrue(summary.writtenFiles.contains { $0.lastPathComponent == "GeneratedHostBridges.generated.swift" })
        XCTAssertTrue(summary.writtenFiles.contains { $0.lastPathComponent.hasSuffix("+Host.generated.swift") })
        let lines = BridgeGenCLI.specializationSummaryLines(from: summary.report.types)
        XCTAssertTrue(lines.contains { $0.contains("source=auto.registrations") })
    }
}
