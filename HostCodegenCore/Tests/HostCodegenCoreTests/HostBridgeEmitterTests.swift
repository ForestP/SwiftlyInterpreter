//
//  HostBridgeEmitterTests.swift
//  HostCodegenCore
//
//  Created by Forest Plasencia on 9/19/25.
//

import XCTest
@testable import HostSurfaceKit
@testable import HostCodegenCore

final class HostBridgeEmitterTests: XCTestCase {

    func testEmitterProducesTypeAndInstallerFiles() throws {
        let source = """
        public struct Foo {
            public init()
            public func latest() -> Foo
            public var current: Swift.Int { get }
        }
        """
        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        let generator = HostBridgeGenerator()
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.methodArtifacts.count, 2)

        let emitter = HostBridgeEmitter()
        let files = emitter.emitFiles(methodArtifacts: result.methodArtifacts,
                                      propertyArtifacts: result.propertyArtifacts)
        XCTAssertEqual(files.count, 2)

        guard let typeFile = files.first(where: { $0.relativePath.contains("Demo_Foo") }) else {
            XCTFail("Missing type file")
            return
        }
        XCTAssertTrue(typeFile.contents.contains("HostDispatcher_Demo_Foo"))
        XCTAssertTrue(typeFile.contents.hasSuffix("\n"))

        guard let installer = files.first(where: { $0.relativePath.hasSuffix("GeneratedHostBridges.generated.swift") }) else {
            XCTFail("Missing installer file")
            return
        }
        XCTAssertTrue(installer.contents.contains("installGeneratedHostBridges"))
        XCTAssertTrue(installer.contents.contains("vm.registerHostMethods(type: \"Demo.Foo\""))
        XCTAssertTrue(installer.contents.contains("vm.registerHostProperty(type: \"Demo.Foo\", name: \"current\""))
        XCTAssertTrue(installer.contents.hasSuffix("\n"))
    }

    func testEmitterRegistersSettableProperty() throws {
        let source = """
        public struct Foo {
            public var value: Swift.Int { get set }
        }
        """
        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        let generator = HostBridgeGenerator()
        let result = generator.generate(from: resolveResult.apiSurface)
        let emitter = HostBridgeEmitter()
        let files = emitter.emitFiles(methodArtifacts: result.methodArtifacts,
                                      propertyArtifacts: result.propertyArtifacts)
        guard let installer = files.first(where: { $0.relativePath.hasSuffix("GeneratedHostBridges.generated.swift") }) else {
            return XCTFail("Missing installer file")
        }
        XCTAssertTrue(installer.contents.contains("set:"))
        XCTAssertTrue(installer.contents.contains("vm.registerHostProperty(type: \"Demo.Foo\", name: \"value\", get:"))
    }

    func testEmitterWrapsAvailabilityGatedMethod() throws {
        let source = """
        public struct Foo {
            @available(macOS 26.0, *)
            public func gated(_ x: Swift.Int) -> Swift.Int
        }
        """
        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        let generator = HostBridgeGenerator()
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertTrue(result.diagnostics.isEmpty)

        let emitter = HostBridgeEmitter()
        let files = emitter.emitFiles(methodArtifacts: result.methodArtifacts,
                                      propertyArtifacts: result.propertyArtifacts)
        guard let typeFile = files.first(where: { $0.relativePath.contains("Demo_Foo") }) else {
            return XCTFail("Missing type file")
        }
        // Expect an availability wrapper and #available guard with strict version
        XCTAssertTrue(typeFile.contents.contains("public func HostAvailWrap_"), typeFile.contents)
        XCTAssertTrue(typeFile.contents.contains("#available("), typeFile.contents)
        XCTAssertTrue(typeFile.contents.contains("macOS 26.0"), typeFile.contents)
        // Installer should still be generated
        XCTAssertTrue(files.contains { $0.relativePath.hasSuffix("GeneratedHostBridges.generated.swift") })
    }

    func testEmitterWrapsAvailabilityGatedPropertyGetter() throws {
        let source = """
        public struct Foo {
            @available(macOS 26.0, *)
            public static var gated: Swift.Int { get }
        }
        """
        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        let generator = HostBridgeGenerator()
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertTrue(result.diagnostics.isEmpty)

        let emitter = HostBridgeEmitter()
        let files = emitter.emitFiles(methodArtifacts: result.methodArtifacts,
                                      propertyArtifacts: result.propertyArtifacts)
        guard let typeFile = files.first(where: { $0.relativePath.contains("Demo_Foo") }) else {
            return XCTFail("Missing type file")
        }
        // Dispatcher for property getter should carry @available and contain an inner #available guard
        XCTAssertTrue(typeFile.contents.contains("@available(macOS 26.0, *)\npublic func HostDispatcher_Demo_Foo_gated"), typeFile.contents)
        XCTAssertTrue(typeFile.contents.contains("#available("), typeFile.contents)
        XCTAssertTrue(typeFile.contents.contains("Getter for Demo.Foo.gated is not available on this platform"), typeFile.contents)
    }
}
