//
//  HostMethodDescriptorBuilderTests.swift
//  HostCodegenCore
//
//  Created by Forest Plasencia on 9/19/25.
//

import XCTest
@testable import HostSurfaceKit
@testable import HostCodegenCore

final class HostMethodDescriptorBuilderTests: XCTestCase {

    func testBuildsDescriptorsForGenericMethod() throws {
        let source = """
        public struct Foo {
            public func transform<T>(_ value: T) -> T
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        XCTAssertTrue(resolveResult.diagnostics.isEmpty)

        let builder = HostMethodDescriptorBuilder()
        let (descriptors, _, diagnostics) = builder.build(from: resolveResult.apiSurface)
        XCTAssertTrue(diagnostics.isEmpty)
        XCTAssertEqual(descriptors.count, 1)
        let descriptor = try XCTUnwrap(descriptors.first)
        XCTAssertEqual(descriptor.genericParameters.map(\.name), ["T"])
        XCTAssertEqual(descriptor.genericParameters.map(\.origin), [.method])
        XCTAssertEqual(descriptor.selector, "transform(_:)")
        XCTAssertEqual(descriptor.parameters.first?.name, "value")
    }

    func testParsesWhereClauseIntoRequirements() throws {
        let source = """
        public struct Foo {
            public func specialize<T>(_ value: T) -> T where T == Swift.Int
            public func check<T>(_ value: T) -> Bool where T: Swift.Equatable & Swift.Hashable
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        XCTAssertTrue(resolveResult.diagnostics.isEmpty)

        let builder = HostMethodDescriptorBuilder()
        let (descriptors, _, diagnostics) = builder.build(from: resolveResult.apiSurface)
        XCTAssertTrue(diagnostics.isEmpty)
        XCTAssertEqual(descriptors.count, 2)

        let specialize = try XCTUnwrap(descriptors.first { $0.baseName == "specialize" })
        let intType = try TypeNameParser.parse("Swift.Int")
        XCTAssertEqual(specialize.requirements, [.sameType(param: "T", intType)])

        let check = try XCTUnwrap(descriptors.first { $0.baseName == "check" })
        XCTAssertEqual(check.requirements, [
            .conformsTo(param: "T", protocolName: "Swift.Equatable"),
            .conformsTo(param: "T", protocolName: "Swift.Hashable")
        ])
    }

    func testOptionalMapGenericParametersIncludeMethodAndReceiver() throws {
        let source = """
        public enum Optional<Wrapped> {
            case none
            case some(Wrapped)

            public func map<U>(_ transform: (Wrapped) -> U) -> Swift.Optional<U>
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Swift", interfaceContents: source)
        let resolveResult = try pipeline.run()
        XCTAssertTrue(resolveResult.diagnostics.isEmpty)

        let builder = HostMethodDescriptorBuilder()
        let (descriptors, _, diagnostics) = builder.build(from: resolveResult.apiSurface)
        XCTAssertTrue(diagnostics.isEmpty)

        let mapDescriptor = try XCTUnwrap(descriptors.first { $0.selector == "map(_:)" })
        XCTAssertEqual(mapDescriptor.genericParameters.map(\.name), ["U", "Wrapped"])
        XCTAssertEqual(mapDescriptor.genericParameters.map(\.origin), [.method, .receiver])
    }
}
