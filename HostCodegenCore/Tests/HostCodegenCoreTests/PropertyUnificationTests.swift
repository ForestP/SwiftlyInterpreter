//
//  PropertyUnificationTests.swift
//  HostCodegenCore
//
//  Verifies that when a type has both an instance and a static property with
//  the same name, the emitter generates disambiguated thunks and a single
//  unified dispatcher, avoiding duplicate symbol names and registration.
//

import XCTest
@testable import HostSurfaceKit
@testable import HostCodegenCore

final class PropertyUnificationTests: XCTestCase {
    func testUnifiesStaticAndInstancePropertyDispatcher() throws {
        let source = """
        public struct Foo {
            public static var stamp: Swift.Int { get }
            public var stamp: Swift.Int { get }
        }
        """
        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolved = try pipeline.run()
        let generator = HostBridgeGenerator()
        let result = generator.generate(from: resolved.apiSurface)
        XCTAssertTrue(result.diagnostics.isEmpty)

        let emitter = HostBridgeEmitter()
        let files = emitter.emitFiles(methodArtifacts: result.methodArtifacts,
                                      propertyArtifacts: result.propertyArtifacts)
        guard let typeFile = files.first(where: { $0.relativePath.contains("Demo_Foo+") }) else {
            return XCTFail("Missing type file")
        }
        let contents = typeFile.contents
        // Expect disambiguated getter thunk names for instance and static
        XCTAssertTrue(contents.contains("Host_Demo_Foo_stamp_instance"), contents)
        XCTAssertTrue(contents.contains("Host_Demo_Foo_stamp_static"), contents)
        // Expect a single unified dispatcher
        XCTAssertTrue(contents.contains("public func HostDispatcher_Demo_Foo_stamp(vm: inout VM, _ args: [Value]) throws -> Value"), contents)
        // Ensure installer registers the unified getter once
        guard let installer = files.first(where: { $0.relativePath.hasSuffix("GeneratedHostBridges.generated.swift") }) else {
            return XCTFail("Missing installer file")
        }
        let installerSrc = installer.contents
        let token = "vm.registerHostProperty(type: \"Demo.Foo\", name: \"stamp\", get:"
        let occurrences = installerSrc.components(separatedBy: token).count - 1
        XCTAssertEqual(occurrences, 1, installerSrc) // exactly one registration
        // Verify it references our unified dispatcher
        XCTAssertTrue(installerSrc.contains("HostDispatcher_Demo_Foo_stamp"), installerSrc)
    }
}
