import XCTest
@testable import HostSurfaceKit
@testable import HostCodegenCore

final class SelfFormatInputGuardTests: XCTestCase {

    private let iface: String = """
    public protocol DiscreteFormatStyle {
        associatedtype FormatInput
        func input(after input: Self.FormatInput) -> Self.FormatInput?
    }

    // Protocol extension constrained on an associated type; should NOT bind `Self` to the RHS type.
    extension Foundation.DiscreteFormatStyle where Self.FormatInput == Foundation.Date {
        public func input(after input: Self.FormatInput) -> Self.FormatInput? { fatalError() }
    }
    """

    func testCollectorDoesNotBindSelfForAssociatedTypeConstraint() throws {
        var pipeline = HostSurfacePipeline(moduleName: "Foundation", interfaceContents: iface)
        let result = try pipeline.run()

        // Expect the surface to include the protocol type, not Foundation.Date.
        let typeNames = Set(result.apiSurface.types.map { $0.canonicalName })
        XCTAssertTrue(typeNames.contains("Foundation.DiscreteFormatStyle"), "Expected Foundation.DiscreteFormatStyle in surface")
        XCTAssertFalse(typeNames.contains("Foundation.Date"), "Did not expect Foundation.Date to be used as receiver for Self.FormatInput constraint")

        // Ensure the collected member sits under DiscreteFormatStyle and is not rewritten to Date.
        guard let discrete = result.apiSurface.types.first(where: { $0.canonicalName == "Foundation.DiscreteFormatStyle" }) else {
            return XCTFail("Missing DiscreteFormatStyle type entry")
        }
        XCTAssertTrue(discrete.members.contains { member in
            switch member {
            case .instanceMethod(let info):
                return info.signature.selector == "input(after:)"
            default: return false
            }
        }, "Expected input(after:) method under DiscreteFormatStyle")
    }

    func testGeneratorFiltersErroneousDateInputEmission() throws {
        // Run full generation with an allowlist that only permits Foundation.Date
        // The Self.FormatInput-constrained extension must NOT produce Date thunks.
        var pipeline = HostSurfacePipeline(moduleName: "Foundation", interfaceContents: iface)
        let surfaceResult = try pipeline.run()

        // Apply allowlist filtering like the controller does
        let allowed: Set<String> = ["Foundation.Date"]
        let filteredTypes = surfaceResult.apiSurface.types.filter { allowed.contains($0.canonicalName) }
        let filteredSurface = ApiSurface(types: filteredTypes)

        let generator = HostBridgeGenerator()
        let gen = generator.generate(from: filteredSurface)

        // No methods/properties should be generated (only installer if emitted).
        XCTAssertTrue(gen.methodArtifacts.isEmpty, "Expected no Date methods generated from Self.FormatInput constraint")
        XCTAssertTrue(gen.propertyArtifacts.isEmpty)

        // Emit files and ensure there is only an installer, and no Date+Host file for input(after:)
        let emitter = HostBridgeEmitter()
        let files = emitter.emitFiles(methodArtifacts: gen.methodArtifacts,
                                      propertyArtifacts: gen.propertyArtifacts)
        // Only installer file should exist when there are no artifacts
        XCTAssertEqual(files.count, 1, "Only installer file expected when there are no artifacts")
        let installer = files.first!
        XCTAssertTrue(installer.relativePath.hasSuffix("GeneratedHostBridges.generated.swift"))
    }
}

