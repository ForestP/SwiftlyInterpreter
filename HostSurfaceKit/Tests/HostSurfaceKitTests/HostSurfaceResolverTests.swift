//
//  HostSurfaceResolverTests.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/19/25.
//

import XCTest
@testable import HostSurfaceKit

final class HostSurfaceResolverTests: XCTestCase {

    private func makeAliasResolver(_ source: String, module: String = "Demo") throws -> TypeAliasResolver {
        let aliasParse = TypeAliasParser.parse(moduleName: module, contents: source)
        let inventory = GenericParameterInventoryBuilder.build(moduleName: module, contents: source)
        return try TypeAliasResolver(aliases: aliasParse.aliases,
                                     diagnostics: aliasParse.diagnostics,
                                     genericParameterInventory: inventory)
    }

    func testCanonicalizesParametersAndReturnTypes() throws {
        let source = """
        public typealias TimeInterval = Double

        public struct Foo {
            public init(period: TimeInterval)
            public func advance(by delta: TimeInterval) -> TimeInterval
        }
        """

        let surface = HostSurfaceCollector().collect(moduleName: "Demo", contents: source)
        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.apiSurface.types.count, 1)
        XCTAssertEqual(result.collectorFallbackPlaceholderHitCount, 0)
        XCTAssertEqual(result.resolverPlaceholderDropHitCount, 0)

        guard let type = result.apiSurface.types.first else {
            XCTFail("Expected surface to include Demo.Foo")
            return
        }

        XCTAssertEqual(type.canonicalName, "Demo.Foo")
        XCTAssertEqual(type.members.count, 2)

        guard let initializer = type.members.compactMap({ member -> ResolvedInitializer? in
            if case let .initializer(info) = member { return info }
            return nil
        }).first else {
            XCTFail("Missing initializer")
            return
        }

        XCTAssertEqual(initializer.signature.parameters.count, 1)
        XCTAssertEqual(initializer.signature.parameters[0].type.canonicalDescription(), "Swift.Double")

        guard let advance = type.members.compactMap({ member -> ResolvedInstanceMethod? in
            if case let .instanceMethod(info) = member { return info }
            return nil
        }).first else {
            XCTFail("Missing instance method")
            return
        }
        XCTAssertEqual(advance.signature.returnType?.canonicalDescription(), "Swift.Double")
        XCTAssertEqual(advance.signature.parameters[0].type.canonicalDescription(), "Swift.Double")
    }

    func testExtensionAvailabilityPropagates() throws {
        let source = """
        public struct Foo {}

        @available(iOS 15, *)
        extension Foo {
            public func latest() -> Foo
        }
        """

        let surface = HostSurfaceCollector().collect(moduleName: "Demo", contents: source)
        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.apiSurface.types.count, 1)
        XCTAssertEqual(result.collectorFallbackPlaceholderHitCount, 0)
        XCTAssertEqual(result.resolverPlaceholderDropHitCount, 0)

        guard let type = result.apiSurface.types.first else {
            XCTFail("Missing type")
            return
        }

        XCTAssertEqual(type.canonicalName, "Demo.Foo")
        XCTAssertEqual(type.members.count, 1)
        XCTAssertEqual(type.availability, ["@available(iOS 15, *)"])

        guard let member = type.members.first else {
            XCTFail("Missing member")
            return
        }

        switch member {
        case .instanceMethod(let info):
            XCTAssertEqual(info.signature.availability, ["@available(iOS 15, *)"])
            XCTAssertEqual(info.signature.returnType?.canonicalDescription(), "Demo.Foo")
        default:
            XCTFail("Expected instance method")
        }
    }

    func testResolvedTypeAliasesSurfaceInResult() throws {
        let source = """
        public typealias FooInt = Foo<Int>

        public struct Foo {
            public func echo(_ value: FooInt) -> FooInt
        }
        """

        let surface = HostSurfaceCollector().collect(moduleName: "Demo", contents: source)
        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        guard let alias = result.typeAliases.first(where: { $0.qualifiedName == "Demo.FooInt" }) else {
            return XCTFail("Expected Demo.FooInt alias metadata")
        }
        XCTAssertEqual(alias.name, "FooInt")
        XCTAssertEqual(alias.canonicalTarget, "Foo<Swift.Int>")
        XCTAssertEqual(alias.sourceLine, 1)
        XCTAssertEqual(result.collectorFallbackPlaceholderHitCount, 0)
        XCTAssertEqual(result.resolverPlaceholderDropHitCount, 0)
    }

#if DEBUG
    func testExtensionFirstKeepsPlaceholdersAndResolverDoesNotDrop() throws {
        let source = """
        public extension Bag where Element: Equatable {
            public func firstMatch(_ value: Element) -> Element?
        }

        public struct Bag<Element> {
            public init(items: Swift.Array<Element>)
        }
        """

        let inventory = GenericParameterInventoryBuilder.build(moduleName: "Demo", contents: source)
        let surface = HostSurfaceCollector(genericParameterInventory: inventory).collect(moduleName: "Demo",
                                                                                          contents: source)
        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        XCTAssertEqual(result.collectorFallbackPlaceholderHitCount, 0)
        XCTAssertEqual(result.resolverPlaceholderDropHitCount, 0)

        guard let type = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.Bag<Element>" }) else {
            return XCTFail("Missing Demo.Bag<Element> type")
        }
        XCTAssertEqual(type.members.count, 2)

        guard let method = type.members.compactMap({ member -> ResolvedInstanceMethod? in
            if case let .instanceMethod(info) = member { return info }
            return nil
        }).first(where: { $0.signature.selector == "firstMatch(_:)" }) else {
            return XCTFail("Missing firstMatch method")
        }

        XCTAssertEqual(method.signature.parameters.count, 1)
        XCTAssertEqual(method.signature.parameters[0].type.canonicalDescription(), "Element")
        XCTAssertEqual(method.signature.returnType?.canonicalDescription(), "Swift.Optional<Element>")
    }
#endif

    // MARK: - Protocol Method Flattening Tests

    func testReceiverPlaceholdersNotOverwrittenByExternalAliases() throws {
        let source = """
        public struct AliasBox<K> {
            public typealias Key = K
        }

        public struct MyDictionary<Key, Value> {
            public func value(forKey key: Key) -> Value?
        }
        """

        let inventory = GenericParameterInventoryBuilder.build(moduleName: "Demo", contents: source)
        let surface = HostSurfaceCollector(genericParameterInventory: inventory).collect(moduleName: "Demo",
                                                                                          contents: source)
        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        guard let type = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.MyDictionary<Key,Value>" }) else {
            return XCTFail("Missing Demo.MyDictionary<Key,Value> type")
        }

        guard let method = type.members.compactMap({ member -> ResolvedInstanceMethod? in
            if case let .instanceMethod(info) = member { return info }
            return nil
        }).first(where: { $0.signature.selector == "value(forKey:)" }) else {
            return XCTFail("Missing value(forKey:) method")
        }

        XCTAssertEqual(method.signature.parameters.count, 1)
        XCTAssertEqual(method.signature.parameters[0].type.canonicalDescription(), "Key")
        XCTAssertEqual(method.signature.returnType?.canonicalDescription(), "Swift.Optional<Value>")
    }

    func testFlattensProtocolMethodsIntoConcreteTypes() throws {
        let source = """
        public protocol Sequence {
            func map() -> Swift.Array<Swift.Int>
            func filter() -> Swift.Array<Swift.Int>
        }

        public struct MyArray: Sequence {
            public func contains() -> Swift.Bool
        }
        """

        let surface = HostSurfaceCollector().collect(moduleName: "Demo", contents: source)
        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        XCTAssertTrue(result.diagnostics.isEmpty)

        guard let myArrayType = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.MyArray" }) else {
            XCTFail("Expected Demo.MyArray in result")
            return
        }

        // Should have the original method plus the two protocol methods
        XCTAssertEqual(myArrayType.members.count, 3)

        let selectors = myArrayType.members.map { $0.selector }.sorted()
        XCTAssertTrue(selectors.contains("contains()"))
        XCTAssertTrue(selectors.contains("map()"))
        XCTAssertTrue(selectors.contains("filter()"))
    }

    func testDoesNotDuplicateMethodsAlreadyPresentInConcreteType() throws {
        let source = """
        public protocol Sequence {
            func contains() -> Swift.Bool
            func map() -> Swift.Array<Swift.Int>
        }

        public struct MyArray: Sequence {
            public func contains() -> Swift.Bool
        }
        """

        let surface = HostSurfaceCollector().collect(moduleName: "Demo", contents: source)
        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        XCTAssertTrue(result.diagnostics.isEmpty)

        guard let myArrayType = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.MyArray" }) else {
            XCTFail("Expected Demo.MyArray in result")
            return
        }

        // Should have contains() once (not duplicated) and map() from protocol
        XCTAssertEqual(myArrayType.members.count, 2)

        let containsCount = myArrayType.members.filter { $0.selector == "contains()" }.count
        XCTAssertEqual(containsCount, 1, "contains() should appear exactly once")

        let selectors = myArrayType.members.map { $0.selector }.sorted()
        XCTAssertTrue(selectors.contains("contains()"))
        XCTAssertTrue(selectors.contains("map()"))
    }

    func testFlattensMethodsFromMultipleProtocols() throws {
        let source = """
        public protocol Sequence {
            func map() -> Swift.Array<Swift.Int>
        }

        public protocol Collection {
            func filter() -> Swift.Array<Swift.Int>
        }

        public struct MyArray: Sequence, Collection {
            public func contains() -> Swift.Bool
        }
        """

        let surface = HostSurfaceCollector().collect(moduleName: "Demo", contents: source)
        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        XCTAssertTrue(result.diagnostics.isEmpty)

        guard let myArrayType = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.MyArray" }) else {
            XCTFail("Expected Demo.MyArray in result")
            return
        }

        // Should have all three methods
        XCTAssertEqual(myArrayType.members.count, 3)

        let selectors = myArrayType.members.map { $0.selector }.sorted()
        XCTAssertTrue(selectors.contains("contains()"))
        XCTAssertTrue(selectors.contains("map()"))
        XCTAssertTrue(selectors.contains("filter()"))
    }

    func testFlattensMethodsFromExtensionConformances() throws {
        let source = """
        public protocol Hashable {
            func hash() -> Swift.Int
        }

        public struct MyType {
            public func foo() -> Swift.Int
        }

        extension MyType: Hashable {}
        """

        let surface = HostSurfaceCollector().collect(moduleName: "Demo", contents: source)
        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        XCTAssertTrue(result.diagnostics.isEmpty)

        guard let myType = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.MyType" }) else {
            XCTFail("Expected Demo.MyType in result")
            return
        }

        // Should have both methods
        XCTAssertEqual(myType.members.count, 2)

        let selectors = myType.members.map { $0.selector }.sorted()
        XCTAssertTrue(selectors.contains("foo()"))
        XCTAssertTrue(selectors.contains("hash()"))
    }

    func testSubstitutesSelfWhenFlatteningProtocolMethods() throws {
        let source = """
        public protocol Comparable {
            func isLessThan(_ other: Self) -> Swift.Bool
            func compareTo(_ other: Self) -> Self
        }

        public struct MyNumber: Comparable {
            public func getValue() -> Swift.Int
        }
        """

        let surface = HostSurfaceCollector().collect(moduleName: "Demo", contents: source)
        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        XCTAssertTrue(result.diagnostics.isEmpty)

        guard let myNumber = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.MyNumber" }) else {
            XCTFail("Expected Demo.MyNumber in result")
            return
        }

        // Should have 3 methods: getValue + 2 from protocol
        XCTAssertEqual(myNumber.members.count, 3)

        // Find the flattened protocol methods
        guard let isLessThan = myNumber.members.compactMap({ member -> ResolvedInstanceMethod? in
            if case let .instanceMethod(info) = member, info.signature.selector == "isLessThan(_:)" {
                return info
            }
            return nil
        }).first else {
            XCTFail("Missing isLessThan method")
            return
        }

        // Verify Self was substituted with Demo.MyNumber
        XCTAssertEqual(isLessThan.signature.parameters.count, 1)
        XCTAssertEqual(isLessThan.signature.parameters[0].type.canonicalDescription(), "Demo.MyNumber",
                      "Self should be substituted with Demo.MyNumber in parameter")
        XCTAssertEqual(isLessThan.signature.returnType?.canonicalDescription(), "Swift.Bool")

        guard let compareTo = myNumber.members.compactMap({ member -> ResolvedInstanceMethod? in
            if case let .instanceMethod(info) = member, info.signature.selector == "compareTo(_:)" {
                return info
            }
            return nil
        }).first else {
            XCTFail("Missing compareTo method")
            return
        }

        // Verify Self was substituted in both parameter and return type
        XCTAssertEqual(compareTo.signature.parameters.count, 1)
        XCTAssertEqual(compareTo.signature.parameters[0].type.canonicalDescription(), "Demo.MyNumber",
                      "Self should be substituted with Demo.MyNumber in parameter")
        XCTAssertEqual(compareTo.signature.returnType?.canonicalDescription(), "Demo.MyNumber",
                      "Self should be substituted with Demo.MyNumber in return type")
    }

    func testSubstitutesAssociatedTypesWhenFlatteningProtocolMethods() throws {
        let source = """
        public protocol Collection {
            func swapAt(_ i: Self.Index, _ j: Self.Index)
            func index(after i: Self.Index) -> Self.Index
        }

        public struct MyArray: Collection {
            public func count() -> Swift.Int
        }
        """

        let surface = HostSurfaceCollector().collect(moduleName: "Demo", contents: source)
        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        XCTAssertTrue(result.diagnostics.isEmpty)

        guard let myArray = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.MyArray" }) else {
            XCTFail("Expected Demo.MyArray in result")
            return
        }

        // Should have 3 methods: count + 2 from protocol
        XCTAssertEqual(myArray.members.count, 3)

        // Find swapAt method
        guard let swapAt = myArray.members.compactMap({ member -> ResolvedInstanceMethod? in
            if case let .instanceMethod(info) = member, info.signature.selector == "swapAt(_:_:)" {
                return info
            }
            return nil
        }).first else {
            XCTFail("Missing swapAt method")
            return
        }

        // Verify Self.Index was substituted with Demo.MyArray.Index
        XCTAssertEqual(swapAt.signature.parameters.count, 2)
        XCTAssertEqual(swapAt.signature.parameters[0].type.canonicalDescription(), "Demo.MyArray.Index",
                      "Self.Index should be substituted with Demo.MyArray.Index")
        XCTAssertEqual(swapAt.signature.parameters[1].type.canonicalDescription(), "Demo.MyArray.Index",
                      "Self.Index should be substituted with Demo.MyArray.Index")

        // Find index(after:) method
        guard let indexAfter = myArray.members.compactMap({ member -> ResolvedInstanceMethod? in
            if case let .instanceMethod(info) = member, info.signature.selector == "index(after:)" {
                return info
            }
            return nil
        }).first else {
            XCTFail("Missing index(after:) method")
            return
        }

        // Verify Self.Index was substituted in both parameter and return type
        XCTAssertEqual(indexAfter.signature.parameters.count, 1)
        XCTAssertEqual(indexAfter.signature.parameters[0].type.canonicalDescription(), "Demo.MyArray.Index",
                      "Self.Index should be substituted with Demo.MyArray.Index in parameter")
        XCTAssertEqual(indexAfter.signature.returnType?.canonicalDescription(), "Demo.MyArray.Index",
                      "Self.Index should be substituted with Demo.MyArray.Index in return type")
    }

    func testPreservesGenericArgumentsWhenSubstitutingAssociatedTypes() throws {
        let source = """
        public protocol Collection {
            func swapAt(_ i: Self.Index, _ j: Self.Index)
        }

        public struct MyArray<Element>: Collection {
            public func count() -> Swift.Int
        }
        """

        let surface = HostSurfaceCollector().collect(moduleName: "Demo", contents: source)
        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        XCTAssertTrue(result.diagnostics.isEmpty)

        // Find the specialized MyArray<Element> type
        guard let myArrayElement = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.MyArray<Element>" }) else {
            XCTFail("Expected Demo.MyArray<Element> in result")
            return
        }

        // Should have 2 methods: count + swapAt from protocol
        XCTAssertEqual(myArrayElement.members.count, 2)

        // Find swapAt method
        guard let swapAt = myArrayElement.members.compactMap({ member -> ResolvedInstanceMethod? in
            if case let .instanceMethod(info) = member, info.signature.selector == "swapAt(_:_:)" {
                return info
            }
            return nil
        }).first else {
            XCTFail("Missing swapAt method")
            return
        }

        // Verify Self.Index was substituted with Demo.MyArray<Element>.Index
        XCTAssertEqual(swapAt.signature.parameters.count, 2)
        XCTAssertEqual(swapAt.signature.parameters[0].type.canonicalDescription(), "Demo.MyArray<Element>.Index",
                      "Self.Index should be substituted with Demo.MyArray<Element>.Index preserving generic arguments")
        XCTAssertEqual(swapAt.signature.parameters[1].type.canonicalDescription(), "Demo.MyArray<Element>.Index",
                      "Self.Index should be substituted with Demo.MyArray<Element>.Index preserving generic arguments")
    }

    func testFlattensMethodsWhenWhereClauseConstraintsAreSatisfied() throws {
        let source = """
        public protocol Sequence {
            func map() -> Swift.Array<Swift.Int>
        }

        extension Sequence where Element: Swift.Comparable {
            public func sorted() -> Swift.Array<Element>
        }

        public struct NumberSequence: Sequence {
            public func customMethod() -> Swift.Int
        }
        """

        // Register that Int conforms to Comparable
        let surface = HostSurfaceCollector(genericParameterInventory: ["Demo.NumberSequence": ["Element"]]).collect(moduleName: "Demo", contents: source)

        // We need to manually add conformances for this test
        var modifiedSurface = surface
        modifiedSurface.conformances["Swift.Int"] = ["Swift.Comparable"]

        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: modifiedSurface)

        XCTAssertTrue(result.diagnostics.isEmpty)

        // Find NumberSequence with Element type
        guard let numberSeq = result.apiSurface.types.first(where: { $0.canonicalName.contains("NumberSequence") }) else {
            XCTFail("Expected Demo.NumberSequence in result")
            return
        }

        // Should have customMethod and map from the protocol
        // Note: sorted() won't be included because we don't know what Element is for NumberSequence
        let selectors = numberSeq.members.map { $0.selector }.sorted()
        XCTAssert(selectors.contains("customMethod()"))
        XCTAssert(selectors.contains("map()"))
    }

    func testDoesNotFlattenMethodsWhenWhereClauseConstraintsNotSatisfied() throws {
        let source = """
        public protocol Sequence {
            func map() -> Swift.Array<Swift.Int>
        }

        extension Sequence where Self.Element: Swift.Comparable {
            public func sorted() -> Swift.Array<Self.Element>
        }

        public struct MySequence: Sequence {
            public func customMethod() -> Swift.Int
        }
        """

        let surface = HostSurfaceCollector().collect(moduleName: "Demo", contents: source)
        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        XCTAssertTrue(result.diagnostics.isEmpty)

        // Find MySequence
        guard let mySequence = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.MySequence" }) else {
            XCTFail("Expected Demo.MySequence in result")
            return
        }

        // Should have 2 methods: customMethod + map (unconstrained protocol method)
        // sorted() should NOT be flattened because we can't verify the Element: Comparable constraint
        XCTAssertEqual(mySequence.members.count, 2, "Should only have customMethod and map, not constrained methods")

        let selectors = mySequence.members.map { $0.selector }.sorted()
        XCTAssertEqual(selectors, ["customMethod()", "map()"])

        // Verify sorted() is NOT present
        XCTAssertFalse(mySequence.members.contains(where: { $0.selector == "sorted()" }),
                      "sorted() should not be flattened because Element: Comparable constraint cannot be verified")
    }

    func testFlattensConstrainedMethodsWhenGenericArgumentSatisfiesConstraint() throws {
        let source = """
        public protocol Comparable {}

        public protocol Sequence {
            func map() -> Swift.Array<Swift.Int>
        }

        extension Sequence where Element: Comparable {
            public func sorted() -> Swift.Array<Element>
        }

        public struct IntArray: Sequence {
            public func count() -> Swift.Int
        }
        """

        let inventory = ["Demo.IntArray": ["Element"]]
        let surface = HostSurfaceCollector(genericParameterInventory: inventory).collect(moduleName: "Demo", contents: source)

        // Add conformances: IntArray.Element (Int) conforms to Comparable
        var modifiedSurface = surface
        modifiedSurface.conformances["Swift.Int"] = ["Demo.Comparable"]
        // Set IntArray's Element typealias to Int
        modifiedSurface.typeAliases["Demo.IntArray"] = ["Element": "Swift.Int"]

        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: modifiedSurface)

        XCTAssertTrue(result.diagnostics.isEmpty)

        // Find IntArray
        guard let intArray = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.IntArray" }) else {
            XCTFail("Expected Demo.IntArray in result")
            return
        }

        // Should have count(), map(), and sorted() because Element (Int) conforms to Comparable
        let selectors = intArray.members.map { $0.selector }.sorted()
        XCTAssert(selectors.contains("count()"), "Should have count()")
        XCTAssert(selectors.contains("map()"), "Should have map() from unconstrained protocol")
        XCTAssert(selectors.contains("sorted()"), "Should have sorted() because Element (Int) conforms to Comparable")
    }

    func testSubstitutesAllProtocolReferencesInMethodSignatures() throws {
        let source = """
        public protocol Sequence {
            func map() -> Swift.Array<Swift.Int>
        }

        public protocol Collection: Sequence {
            func count() -> Swift.Int
        }

        extension Sequence {
            public func filter(_ isIncluded: (Collection.Element) throws -> Swift.Bool) rethrows -> Swift.Array<Collection.Element>
        }

        public struct MyArray<Element>: Collection {
            public func customMethod() -> Swift.Int
        }
        """

        let inventory = ["Demo.MyArray": ["Element"]]
        let surface = HostSurfaceCollector(genericParameterInventory: inventory).collect(moduleName: "Demo", contents: source)

        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        XCTAssertTrue(result.diagnostics.isEmpty)

        // Find MyArray<Element>
        guard let myArray = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.MyArray<Element>" }) else {
            XCTFail("Expected Demo.MyArray<Element> in result")
            return
        }

        // Find filter method
        guard let filterMethod = myArray.members.compactMap({ member -> ResolvedInstanceMethod? in
            if case let .instanceMethod(info) = member, info.signature.selector == "filter(_:)" {
                return info
            }
            return nil
        }).first else {
            XCTFail("Expected filter(_:) method to be flattened from Sequence")
            return
        }

        // Verify that Collection.Element was substituted with MyArray<Element>.Element (which becomes just Element)
        // The parameter should be (Element) throws -> Bool, not (Collection.Element) throws -> Bool
        XCTAssertEqual(filterMethod.signature.parameters.count, 1)
        let param = filterMethod.signature.parameters[0]

        guard let funcSig = param.type.functionSignature else {
            XCTFail("Expected parameter to be a function type")
            return
        }

        XCTAssertEqual(funcSig.parameters.count, 1)
        let closureParam = funcSig.parameters[0]

        // The closure parameter should be "Element" or "Demo.MyArray<Element>.Element", not "Collection.Element"
        let paramDesc = closureParam.canonicalDescription()
        XCTAssertFalse(paramDesc.contains("Collection"),
                      "Collection.Element should be substituted, got: \(paramDesc)")
        XCTAssertTrue(paramDesc.contains("Element"),
                     "Should contain Element reference, got: \(paramDesc)")

        // Verify return type also has Collection.Element substituted
        guard let returnType = filterMethod.signature.returnType else {
            XCTFail("Expected filter to have a return type")
            return
        }

        let returnDesc = returnType.canonicalDescription()
        XCTAssertFalse(returnDesc.contains("Collection.Element"),
                      "Collection.Element in return type should be substituted, got: \(returnDesc)")
    }

    func testSubstitutesFullyQualifiedProtocolReferencesAcrossModules() throws {
        let source = """
        public protocol Sequence {
            func map() -> Swift.Array<Swift.Int>
        }

        public protocol Collection: Sequence {}

        extension Sequence {
            public func filter(_ isIncluded: (Swift.Collection.Element) throws -> Swift.Bool) rethrows -> Swift.Array<Swift.Collection.Element>
        }

        public struct MyArray<Element>: Collection {
            public func customMethod() -> Swift.Int
        }
        """

        let inventory = ["Demo.MyArray": ["Element"]]
        let surface = HostSurfaceCollector(genericParameterInventory: inventory).collect(moduleName: "Demo", contents: source)

        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        XCTAssertTrue(result.diagnostics.isEmpty)

        // Find MyArray<Element>
        guard let myArray = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.MyArray<Element>" }) else {
            XCTFail("Expected Demo.MyArray<Element> in result")
            return
        }

        // Find filter method
        guard let filterMethod = myArray.members.compactMap({ member -> ResolvedInstanceMethod? in
            if case let .instanceMethod(info) = member, info.signature.selector == "filter(_:)" {
                return info
            }
            return nil
        }).first else {
            XCTFail("Expected filter(_:) method to be flattened from Sequence")
            return
        }

        // Verify that Swift.Collection.Element was substituted correctly
        XCTAssertEqual(filterMethod.signature.parameters.count, 1)
        let param = filterMethod.signature.parameters[0]

        guard let funcSig = param.type.functionSignature else {
            XCTFail("Expected parameter to be a function type")
            return
        }

        XCTAssertEqual(funcSig.parameters.count, 1)
        let closureParam = funcSig.parameters[0]

        // The closure parameter should not contain "Swift.Collection" or "Collection"
        let paramDesc = closureParam.canonicalDescription()
        XCTAssertFalse(paramDesc.contains("Collection"),
                      "Swift.Collection.Element should be substituted, got: \(paramDesc)")
        XCTAssertTrue(paramDesc.contains("Element"),
                     "Should contain Element reference, got: \(paramDesc)")

        // Verify return type also has Swift.Collection.Element substituted
        guard let returnType = filterMethod.signature.returnType else {
            XCTFail("Expected filter to have a return type")
            return
        }

        let returnDesc = returnType.canonicalDescription()
        XCTAssertFalse(returnDesc.contains("Collection.Element"),
                      "Swift.Collection.Element in return type should be substituted, got: \(returnDesc)")
    }

    func testFlattensMethodsWithMethodLevelGenericConstraints() throws {
        // Methods with where clauses on method-level generics should still be flattened
        // e.g., func map<T, E>(_:) where E: Error should be flattened into all conforming types
        let source = """
        public protocol Sequence {
            associatedtype Element
        }

        extension Sequence {
            public func map<T, E>(_ transform: (Element) throws -> T) rethrows where E: Swift.Error -> Swift.Array<T>
        }

        public struct MyArray<Element>: Sequence {
            public func count() -> Swift.Int
        }
        """

        let inventory = ["Demo.MyArray": ["Element"]]
        let surface = HostSurfaceCollector(genericParameterInventory: inventory).collect(moduleName: "Demo", contents: source)

        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        XCTAssertTrue(result.diagnostics.isEmpty)

        // Find MyArray<Element>
        guard let myArray = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.MyArray<Element>" }) else {
            XCTFail("Expected Demo.MyArray<Element> in result")
            return
        }

        // Should have both count() and map(_:)
        // The where clause "where E: Swift.Error" is on a method-level generic parameter
        // and should not prevent flattening
        let selectors = myArray.members.map { $0.selector }.sorted()
        XCTAssert(selectors.contains("count()"), "Should have count()")
        XCTAssert(selectors.contains("map(_:)"), "Should have map(_:) despite method-level generic constraint")
    }

    func testSwiftArrayFlattensSequenceMap() throws {
        // Validate that Swift.Array gets Sequence.map(_:) flattened into it
        // This is a critical test for closure type inference
        // This matches the actual Swift stdlib structure where Collection has:
        // func map<T, E>(_ transform: (Self.Element) throws(E) -> T) throws(E) -> [T] where E : Swift.Error
        let source = """
        public protocol Collection {
            associatedtype Element
        }

        extension Collection {
            public func map<T, E>(_ transform: (Self.Element) throws -> T) throws -> Swift.Array<T> where E: Swift.Error
        }

        public struct Array<Element>: Collection {
            public func withUnsafeBytes<R>(_ body: (Swift.UnsafeRawBufferPointer) throws -> R) rethrows -> R
        }
        """

        let inventory = ["Swift.Array": ["Element"]]
        let surface = HostSurfaceCollector(genericParameterInventory: inventory).collect(moduleName: "Swift", contents: source)

        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source, module: "Swift"))
        let result = resolver.resolve(surface: surface)

        print("[TEST] Diagnostics: \(result.diagnostics)")
        print("[TEST] Types in result: \(result.apiSurface.types.map { $0.canonicalName })")

        XCTAssertTrue(result.diagnostics.isEmpty)

        // Find Array<Element>
        guard let swiftArray = result.apiSurface.types.first(where: { $0.canonicalName == "Swift.Array<Element>" }) else {
            XCTFail("Expected Swift.Array<Element> in result")
            return
        }

        print("[TEST] Swift.Array members: \(swiftArray.members.map { $0.selector }.sorted())")

        // Should have both withUnsafeBytes(_:) and map(_:)
        let selectors = swiftArray.members.map { $0.selector }.sorted()
        XCTAssert(selectors.contains("withUnsafeBytes(_:)"), "Should have withUnsafeBytes(_:)")
        XCTAssert(selectors.contains("map(_:)"), "Should have map(_:) flattened from Collection despite where E: Error constraint")

        // Now test JSON serialization round-trip using the same encoder used in production
        let encoder = ApiSurfaceJSONEncoder(prettyPrinted: true)
        let jsonData = try encoder.encode(result.apiSurface)

        print("[TEST] JSON size: \(jsonData.count) bytes")

        // Decode it back
        let decoder = ApiSurfaceJSONDecoder()
        let decodedSurface = try decoder.decode(jsonData)

        // Find Array<Element> in decoded surface
        guard let decodedArray = decodedSurface.types.first(where: { $0.canonicalName == "Swift.Array<Element>" }) else {
            XCTFail("Expected Swift.Array<Element> in decoded JSON")
            return
        }

        print("[TEST] Decoded Swift.Array members: \(decodedArray.members.map { $0.selector }.sorted())")

        // Verify map(_:) survived JSON round-trip
        let decodedSelectors = decodedArray.members.map { $0.selector }.sorted()
        XCTAssert(decodedSelectors.contains("map(_:)"), "map(_:) should survive JSON serialization round-trip")

        guard let decodedMap = decodedArray.members.compactMap({ member -> ResolvedInstanceMethod? in
            if case let .instanceMethod(info) = member { return info }
            return nil
        }).first(where: { $0.signature.selector == "map(_:)" }) else {
            return XCTFail("Decoded surface missing map(_:) details")
        }

        guard let transformParam = decodedMap.signature.parameters.first else {
            return XCTFail("map(_:) should have a transform parameter")
        }
        XCTAssertNotNil(transformParam.type.functionSignature, "map(_:) transform parameter should be a closure type")
    }

    func testConstrainedExtensionWitnessesMissing() throws {
        let source = """
        public protocol MyRangeReplaceableCollection {
            associatedtype Element
            mutating func append(_ newElement: Element)
            mutating func removeLast() -> Element
        }

        extension MyRangeReplaceableCollection where Element: Swift.Equatable {
            public mutating func popMatching(_ value: Element) -> Element? { return removeLast() }
        }

        public struct EquatableBuffer<Element: Swift.Equatable> {
            public init() {}
        }

        extension EquatableBuffer: MyRangeReplaceableCollection where Element: Swift.Equatable {
            public mutating func append(_ newElement: Element) {}
            public mutating func removeLast() -> Element { fatalError() }
        }
        """

        let inventory = ["Demo.EquatableBuffer": ["Element"]]
        let surface = HostSurfaceCollector(genericParameterInventory: inventory).collect(moduleName: "Demo",
                                                                                         contents: source)
        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        guard let buffer = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.EquatableBuffer<Element>" }) else {
            return XCTFail("Expected Demo.EquatableBuffer<Element> in resolved surface")
        }

        let selectors = Set(buffer.members.map { $0.selector })

        XCTAssertTrue(selectors.contains("popMatching(_:)")
                      && selectors.contains("append(_:)")
                      && selectors.contains("removeLast()"),
                      "EquatableBuffer should expose popMatching(_:) plus base requirements. Members: \(selectors.sorted())")
    }

    func testSelfConformanceConstraintAllowsWitnessFlattening() throws {
        let source = """
        public protocol MyBidirectionalCollection {}

        public protocol MyRangeReplaceableCollection {
            associatedtype Element
            mutating func append(_ newElement: Element)
        }

        extension MyRangeReplaceableCollection where Self: MyBidirectionalCollection {
            public mutating func popLastValue() -> Element? { return nil }
        }

        public struct IntDeque: MyRangeReplaceableCollection, MyBidirectionalCollection {
            public typealias Element = Swift.Int
            public init() {}
            public mutating func append(_ newElement: Swift.Int) {}
        }
        """

        let surface = HostSurfaceCollector().collect(moduleName: "Demo", contents: source)
        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        guard let deque = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.IntDeque" }) else {
            return XCTFail("Expected Demo.IntDeque in resolved surface")
        }

        let selectors = Set(deque.members.map { $0.selector })
        XCTAssertTrue(selectors.contains("popLastValue()"),
                      "Expected popLastValue() witness on Demo.IntDeque. Members: \(selectors.sorted())")
    }

    func testRangeReplaceableCollectionWitnessesFlattened() throws {
        let source = """
        public protocol MyRangeReplaceableCollection {
            associatedtype Element
            init()
            mutating func append(_ newElement: Element)
            @discardableResult
            mutating func removeLast() -> Element
        }

        extension MyRangeReplaceableCollection {
            public mutating func push(_ value: Element) { _ = value }
            public mutating func popHead() -> Element { return removeLast() }
        }

        public struct IntBag: MyRangeReplaceableCollection {
            public typealias Element = Swift.Int
            public init() {}
            public mutating func append(_ newElement: Swift.Int) {}
            public mutating func removeLast() -> Swift.Int { return 0 }
        }
        """

        let inventory = GenericParameterInventoryBuilder.build(moduleName: "Demo", contents: source)
        var surface = HostSurfaceCollector(genericParameterInventory: inventory).collect(moduleName: "Demo",
                                                                                        contents: source)

        // Simulate allowlist filtering that drops the protocol receiver surface.
        surface.methods.removeAll { method in
            let joined = method.type.path.joined(separator: ".")
            return joined == "Demo.MyRangeReplaceableCollection" || joined.hasSuffix(".MyRangeReplaceableCollection")
        }
        surface.protocolAssociatedTypes.removeValue(forKey: "Demo.MyRangeReplaceableCollection")

        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        guard let intBag = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.IntBag" }) else {
            return XCTFail("Expected Demo.IntBag in resolved surface")
        }

        let selectors = Set(intBag.members.map { $0.selector })

        XCTExpectFailure("RangeReplaceableCollection witnesses are not flattened into concrete types yet") {
            XCTAssertTrue(selectors.contains("push(_:)")
                          && selectors.contains("popHead()"),
                          "Expected push(_:)/popHead() witnesses on Demo.IntBag. Members: \(selectors)")
        }
    }

    func testDictionaryProtocolWitnesses() throws {
        let source = """
        public protocol MyDictionaryProtocol {
            associatedtype Key
            associatedtype Value
            mutating func updateValue(_ value: Value, forKey key: Key) -> Value?
        }

        extension MyDictionaryProtocol where Key: Swift.Hashable {
            public mutating func merge(_ other: Self, combining: (Value, Value) -> Value) {
                _ = (other, combining)
            }
        }

        public struct KeyValueStore<Key: Swift.Hashable, Value>: MyDictionaryProtocol {
            public init() {}
            public mutating func updateValue(_ value: Value, forKey key: Key) -> Value? { return value }
        }
        """

        let inventory = GenericParameterInventoryBuilder.build(moduleName: "Demo", contents: source)
        var surface = HostSurfaceCollector(genericParameterInventory: inventory).collect(moduleName: "Demo",
                                                                                        contents: source)

        surface.methods.removeAll { method in
            let joined = method.type.path.joined(separator: ".")
            return joined == "Demo.MyDictionaryProtocol" || joined.hasSuffix(".MyDictionaryProtocol")
        }
        surface.protocolAssociatedTypes.removeValue(forKey: "Demo.MyDictionaryProtocol")

        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        guard let store = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.KeyValueStore<Key,Value>" }) else {
            return XCTFail("Expected Demo.KeyValueStore<Key,Value> in resolved surface")
        }

        let selectors = Set(store.members.map { $0.selector })

        XCTExpectFailure("DictionaryProtocol-style witnesses with where clauses are not flattened yet") {
            XCTAssertTrue(selectors.contains("merge(_:combining:)"),
                          "Expected merge(_:combining:) witness on Demo.KeyValueStore. Members: \(selectors)")
        }
    }

    func testConformanceRecordedWhenProtocolNotInAllowlist() throws {
        let source = """
        public protocol OrderedSequence {
            associatedtype Element
        }

        extension OrderedSequence where Element == Swift.String {
            public func sortedAlphabetically() -> Swift.Array<Element> { return [] }
        }

        public struct WordList: OrderedSequence {
            public typealias Element = Swift.String
            public func description() -> Swift.String { "" }
        }
        """

        let inventory = GenericParameterInventoryBuilder.build(moduleName: "Demo", contents: source)
        var surface = HostSurfaceCollector(genericParameterInventory: inventory).collect(moduleName: "Demo",
                                                                                        contents: source)

        // Simulate allowlist filtering by dropping the protocol surface.
        surface.methods.removeAll { method in
            let joined = method.type.path.joined(separator: ".")
            return joined == "Demo.OrderedSequence" || joined.hasSuffix(".OrderedSequence")
        }
        surface.protocolAssociatedTypes.removeValue(forKey: "Demo.OrderedSequence")

        var resolver = HostSurfaceResolver(aliasResolver: try makeAliasResolver(source))
        let result = resolver.resolve(surface: surface)

        guard let wordList = result.apiSurface.types.first(where: { $0.canonicalName == "Demo.WordList" }) else {
            return XCTFail("Expected Demo.WordList in resolved surface")
        }

        let selectors = Set(wordList.members.map { $0.selector })

        XCTExpectFailure("Protocol witnesses disappear if the protocol surface is filtered out") {
            XCTAssertTrue(selectors.contains("sortedAlphabetically()"),
                          "Expected sortedAlphabetically() witness to survive even when protocol was filtered. Members: \(selectors)")
        }
    }
}
