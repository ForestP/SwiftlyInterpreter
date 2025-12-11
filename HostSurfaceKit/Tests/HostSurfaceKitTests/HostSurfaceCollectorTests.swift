//
//  HostSurfaceCollectorTests.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/19/25.
//

import XCTest
@testable import HostSurfaceKit

final class HostSurfaceCollectorTests: XCTestCase {

    private func collect(_ source: String) -> HostSurfaceCollection {
        let collector = HostSurfaceCollector()
        return collector.collect(moduleName: "Demo", contents: source)
    }

    func testCollectsStructMembers() throws {
        let source = """
        public struct Foo {
            public init(value: Swift.Int)
            public mutating func increment(by amount: Swift.Int)
            public func doubled() -> Swift.Int
            public static func makeDefault() -> Foo
        }
        """

        let result = collect(source)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.methods.count, 4)
        XCTAssertEqual(result.properties.count, 0)

        guard let initializer = result.methods.first(where: {
            if case .initializer(let _) = $0.kind {
                return true
            }
            return false
        }) else {
            XCTFail("Expected initializer")
            return
        }
        XCTAssertEqual(initializer.selector, "init(value:)")
        XCTAssertEqual(initializer.baseName, "value")
        XCTAssertEqual(initializer.parameters.count, 1)
        XCTAssertEqual(initializer.parameters[0].type.canonicalDescription(), "Swift.Int")
        XCTAssertEqual(initializer.returnType?.canonicalDescription(), "Demo.Foo")

        guard let mutatingMethod = result.methods.first(where: {
            if case .instance(let isMutating) = $0.kind { return isMutating }
            return false
        }) else {
            XCTFail("Expected mutating method")
            return
        }
        XCTAssertEqual(mutatingMethod.baseName, "increment")
        XCTAssertEqual(mutatingMethod.selector, "increment(by:)")

        guard let staticMethod = result.methods.first(where: {
            if case .static = $0.kind { return true }
            return false
        }) else {
            XCTFail("Expected static method")
            return
        }
        XCTAssertEqual(staticMethod.selector, "makeDefault()")
    }

    func testCollectsExtensionMembersAndAvailability() throws {
        let source = """
        public struct Foo {}

        @available(iOS 15, *)
        extension Foo {
            public func latest() -> Foo
        }
        """

        let result = collect(source)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.methods.count, 1)

        guard let method = result.methods.first else {
            XCTFail("Expected method")
            return
        }
        XCTAssertEqual(method.type.canonicalDescription(), "Demo.Foo")
        XCTAssertEqual(method.selector, "latest()")
        XCTAssertEqual(method.availability, ["@available(iOS 15, *)"])
    }

    func testCollectsProperties() throws {
        let source = """
        public struct Foo {
            public var current: Swift.Int { get }
            public var editable: Swift.String { get set }
            public static var shared: Foo { get }
        }
        """

        let result = collect(source)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.properties.count, 3)

        guard let instanceProp = result.properties.first(where: { !$0.isStatic && !$0.isSettable }) else {
            XCTFail("Expected current property")
            return
        }
        XCTAssertEqual(instanceProp.name, "current")
        XCTAssertEqual(instanceProp.propertyType.canonicalDescription(), "Swift.Int")

        guard let editable = result.properties.first(where: { $0.name == "editable" }) else {
            XCTFail("Expected editable property")
            return
        }
        XCTAssertTrue(editable.isSettable)

        guard let staticProp = result.properties.first(where: { $0.isStatic }) else {
            XCTFail("Expected static property")
            return
        }
        XCTAssertEqual(staticProp.propertyType.canonicalDescription(), "Demo.Foo")
    }

#if DEBUG
    func testCollectorWithoutInventoryLeavesReceiverNonGeneric() throws {
        let source = """
        public extension Array {
            public func customDescription() -> Swift.String
        }
        """

        let collector = HostSurfaceCollector()
        let result = collector.collect(moduleName: "Swift", contents: source)
        XCTAssertEqual(result.methods.count, 1)
        XCTAssertEqual(result.methods.first?.type.canonicalDescription(), "Swift.Array")
        XCTAssertEqual(result.fallbackPlaceholderHitCount, 0)
        XCTAssertTrue(result.diagnostics.isEmpty)
    }

    func testInventoryPopulatesGenericReceiver() throws {
        let source = """
        public extension Array {
            public func customDescription() -> Swift.String
        }
        """

        let collector = HostSurfaceCollector(genericParameterInventory: ["Swift.Array": ["Element"]])
        let result = collector.collect(moduleName: "Swift", contents: source)
        XCTAssertEqual(result.methods.first?.type.canonicalDescription(), "Swift.Array<Element>")
        XCTAssertEqual(result.fallbackPlaceholderHitCount, 0)
        XCTAssertTrue(result.diagnostics.isEmpty)
    }
#endif

    // MARK: - Conformance Tracking Tests

    func testCapturesConformancesFromStructDeclaration() throws {
        let source = """
        public protocol Sequence {}
        public protocol Collection {}

        public struct MyArray: Sequence, Collection {
            public func contains() -> Bool
        }
        """

        let result = collect(source)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertNotNil(result.conformances["Demo.MyArray"])
        XCTAssertEqual(result.conformances["Demo.MyArray"]?.count, 2)
        XCTAssertTrue(result.conformances["Demo.MyArray"]?.contains("Demo.Sequence") ?? false)
        XCTAssertTrue(result.conformances["Demo.MyArray"]?.contains("Demo.Collection") ?? false)
    }

    func testCapturesConformancesFromExtension() throws {
        let source = """
        public protocol Hashable {}
        public protocol Equatable {}

        public struct MyType {
            public func foo() -> Swift.Int
        }

        extension MyType: Hashable, Equatable {}
        """

        let result = collect(source)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertNotNil(result.conformances["Demo.MyType"])
        XCTAssertEqual(result.conformances["Demo.MyType"]?.count, 2)
        XCTAssertTrue(result.conformances["Demo.MyType"]?.contains("Demo.Hashable") ?? false)
        XCTAssertTrue(result.conformances["Demo.MyType"]?.contains("Demo.Equatable") ?? false)
    }

    func testMergesConformancesFromMultipleSources() throws {
        let source = """
        public protocol Sequence {}
        public protocol Collection {}
        public protocol Hashable {}

        public struct MyArray: Sequence {
            public func contains() -> Bool
        }

        extension MyArray: Collection {}
        extension MyArray: Hashable {}
        """

        let result = collect(source)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertNotNil(result.conformances["Demo.MyArray"])
        XCTAssertEqual(result.conformances["Demo.MyArray"]?.count, 3)
        XCTAssertTrue(result.conformances["Demo.MyArray"]?.contains("Demo.Sequence") ?? false)
        XCTAssertTrue(result.conformances["Demo.MyArray"]?.contains("Demo.Collection") ?? false)
        XCTAssertTrue(result.conformances["Demo.MyArray"]?.contains("Demo.Hashable") ?? false)
    }

    func testCapturesModuleQualifiedConformances() throws {
        let source = """
        public struct MyArray: Swift.Sequence {
            public func contains() -> Bool
        }
        """

        let result = collect(source)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertNotNil(result.conformances["Demo.MyArray"])
        XCTAssertTrue(result.conformances["Demo.MyArray"]?.contains("Swift.Sequence") ?? false)
    }

    func testCollectsStructMethodsAsBaseline() throws {
        let source = """
        public struct MyType {
            public func map() -> Swift.Array<Swift.Int>
        }
        """

        let result = collect(source)
        print("[BASELINE] Diagnostics: \(result.diagnostics)")
        print("[BASELINE] Methods count: \(result.methods.count)")

        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.methods.count, 1)
    }

    func testCollectsProtocolMethods() throws {
        let source = """
        public protocol Sequence {
            func map() -> Swift.Array<Swift.Int>
            func filter() -> Swift.Array<Swift.Int>
        }
        """

        let result = collect(source)
        print("[PROTOCOL] Diagnostics: \(result.diagnostics)")
        print("[PROTOCOL] Methods count: \(result.methods.count)")
        print("[PROTOCOL] Properties count: \(result.properties.count)")

        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.methods.count, 2, "Expected 2 protocol methods")

        let selectors = result.methods.map { $0.selector }.sorted()
        XCTAssertEqual(selectors, ["filter()", "map()"])

        // Verify the methods are associated with the protocol
        for method in result.methods {
            XCTAssertEqual(method.type.canonicalDescription(), "Demo.Sequence")
        }
    }
}
