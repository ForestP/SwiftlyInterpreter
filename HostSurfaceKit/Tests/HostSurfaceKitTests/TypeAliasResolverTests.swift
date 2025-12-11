//
//  TypeAliasResolverTests.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/19/25.
//

import XCTest
@testable import HostSurfaceKit


final class TypeAliasResolverTests: XCTestCase {
    func testParsesSimpleAliases() throws {
        let contents = """
        public typealias TimeInterval = Double
        public typealias CFTimeInterval = TimeInterval
        """
        let result = TypeAliasParser.parse(moduleName: "Foundation", contents: contents)
        XCTAssertEqual(result.aliases.count, 2)
        var resolver = try TypeAliasResolver(aliases: result.aliases)
        let canonical = try resolver.canonicalName(for: "Foundation.CFTimeInterval")
        XCTAssertEqual(canonical, "Swift.Double")
    }

    func testAliasChainWithOverrides() throws {
        let contents = """
        public typealias CGFloat = Double
        public typealias TimeInterval = CGFloat
        """
        let parserResult = TypeAliasParser.parse(moduleName: "CoreGraphics", contents: contents)
        var resolver = try TypeAliasResolver(
            aliases: parserResult.aliases,
            overrides: ["CoreGraphics.CGFloat": "Swift.Double"]
        )
        let canonical = try resolver.canonicalName(for: "CoreGraphics.TimeInterval")
        XCTAssertEqual(canonical, "Swift.Double")
    }

    func testDictionaryCanonicalization() throws {
        let contents = """
        public typealias HTTPHeaders = [String: String]
        """
        let parserResult = TypeAliasParser.parse(moduleName: "MyNetworking", contents: contents)
        var resolver = try TypeAliasResolver(aliases: parserResult.aliases)
        let canonical = try resolver.canonicalName(for: "MyNetworking.HTTPHeaders")
        XCTAssertEqual(canonical, "Swift.Dictionary<Swift.String,Swift.String>")
    }

    func testOptionalCanonicalization() throws {
        let contents = """
        public typealias MaybeString = String?
        """
        let parserResult = TypeAliasParser.parse(moduleName: "Util", contents: contents)
        var resolver = try TypeAliasResolver(aliases: parserResult.aliases)
        let canonical = try resolver.canonicalName(for: "MaybeString")
        XCTAssertEqual(canonical, "Swift.Optional<Swift.String>")
    }

    func testAnyKeywordIsStripped() throws {
        let contents = """
        public typealias AnyErr = any Error
        """
        let parserResult = TypeAliasParser.parse(moduleName: "Foundation", contents: contents)
        var resolver = try TypeAliasResolver(aliases: parserResult.aliases)
        let canonical = try resolver.canonicalName(for: "AnyErr")
        XCTAssertEqual(canonical, "Swift.Error")
    }

    func testCycleDetection() throws {
        let contents = """
        public typealias A = B
        public typealias B = A
        """
        let parserResult = TypeAliasParser.parse(moduleName: "Cycle", contents: contents)
        var resolver = try TypeAliasResolver(aliases: parserResult.aliases)
        XCTAssertThrowsError(try resolver.canonicalName(for: "Cycle.A")) { error in
            guard case TypeAliasResolverError.cycleDetected(let chain) = error else {
                return XCTFail("Unexpected error: \(error)")
            }
            XCTAssertEqual(chain, ["Cycle.A", "Cycle.B", "Cycle.A"])
        }
    }

    func testGenericParameterInventoryBuilderCapturesNominalTypes() throws {
        let contents = """
        public struct Array<Element> {}
        public struct Dictionary<Key, Value> {}
        public struct Container {
            public struct Nested<Item> {}
        }
        public enum Optional<Wrapped> {}
        """

        let inventory = GenericParameterInventoryBuilder.build(moduleName: "Demo", contents: contents)
        XCTAssertEqual(inventory["Demo.Array"], ["Element"])
        XCTAssertEqual(inventory["Demo.Dictionary"], ["Key", "Value"])
        XCTAssertEqual(inventory["Demo.Container.Nested"], ["Item"])
        XCTAssertEqual(inventory["Demo.Optional"], ["Wrapped"])
    }

    func testTypeAliasResolverExposesGenericInventory() throws {
        let decl = TypeAliasDecl(moduleName: "MyModule",
                                 name: "MyDictionary",
                                 qualifiedName: "MyModule.MyDictionary",
                                 target: TypeName(path: ["Swift", "Dictionary"],
                                                 genericArguments: [TypeName(path: ["Key"]),
                                                                    TypeName(path: ["Value"])]),
                                 line: 1)
        var resolver = try TypeAliasResolver(aliases: [decl], genericParameterInventory: ["Swift.Array": ["Element"]])
        XCTAssertEqual(resolver.genericParameters(for: "Swift.Array"), ["Element"])
        XCTAssertEqual(resolver.genericParameters(for: "Swift.Dictionary"), ["Key", "Value"])
    }

    func testSpecializedAliasDoesNotProtectConcreteTypes() throws {
        let contents = """
        public typealias DataArray = [UInt8]
        """
        let parserResult = TypeAliasParser.parse(moduleName: "Demo", contents: contents)
        var resolver = try TypeAliasResolver(aliases: parserResult.aliases)
        let canonical = try resolver.canonicalName(for: "Demo.DataArray")
        XCTAssertEqual(canonical, "Swift.Array<Swift.UInt8>")
    }

    func testGenericParametersAreNotResolvedAsAliases() throws {
        let elementAlias = TypeAliasDecl(moduleName: "Swift",
                                         name: "Element",
                                         qualifiedName: "Swift.Element",
                                         target: TypeName(path: ["Swift", "UInt8"]),
                                         line: 1)
        var resolver = try TypeAliasResolver(aliases: [elementAlias],
                                             genericParameterInventory: ["Swift.Array": ["Element"]])
        let type = try TypeNameParser.parse("Swift.Array<Element>")
        let canonical = try resolver.canonicalName(for: type)
        XCTAssertEqual(canonical, "Swift.Array<Element>")
    }
}
