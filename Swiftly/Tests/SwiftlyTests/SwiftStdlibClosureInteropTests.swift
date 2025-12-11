import XCTest
import Swiftly
import InterpreterCompiler
import InterpreterVM
import HostSurfaceKit

final class SwiftStdlibClosureInteropTests: XCTestCase {
    
    @MainActor
    static let generatedAPISurface = loadGeneratedApiSurface()

    // Load actual generated API surface from bridgegen output
    private static func loadGeneratedApiSurface() -> (ApiSurface, [String: [String]])? {
#if os(WASI)
        let surfaceData = SwiftStdlibHostBridgeEmbeddedResources.surfaceJSONData
#else
        // Load from test bundle resources (added via Package.swift)
        guard let surfaceURL = Bundle.module.url(forResource: "bridgegen-swiftstdlib-surface", withExtension: "json") else {
            print("⚠️  Generated surface file not found in test bundle")
            return nil
        }

        guard let surfaceData = try? Data(contentsOf: surfaceURL) else {
            print("⚠️  Failed to read data from: \(surfaceURL.path)")
            return nil
        }
#endif

#if os(WASI)
        guard let apiSurface = try? ApiSurfaceJSONDecoder().decode(surfaceData) else {
            print("⚠️  Failed to decode embedded WASI API surface")
            return nil
        }
#else
        guard let apiSurface = try? ApiSurfaceJSONDecoder().decode(surfaceData) else {
            print("⚠️  Failed to decode API surface from: \(surfaceURL.path)")
            return nil
        }
#endif

#if os(WASI)
        print("✅ Loaded generated API surface from embedded WASI resources")
#else
        print("✅ Loaded generated API surface from test bundle")
#endif

#if os(WASI)
        let inventoryData = SwiftStdlibHostBridgeEmbeddedResources.inventoryJSONData
#else
        guard let inventoryURL = Bundle.module.url(forResource: "bridgegen-swiftstdlib-inventory", withExtension: "json") else {
            print("⚠️  Generated inventory file not found in test bundle")
            return nil
        }

        let inventoryData: Data
        do {
            inventoryData = try Data(contentsOf: inventoryURL)
        } catch {
            print("⚠️  Failed to read inventory data: \(error)")
            return nil
        }
#endif

        let inventory: [String: [String]]
        do {
            inventory = try JSONDecoder().decode([String: [String]].self, from: inventoryData)
        } catch {
            print("⚠️  Failed to decode inventory JSON: \(error)")
            return nil
        }

        return (apiSurface, inventory)
    }

    // Create minimal API surface for closure type inference testing (fallback/mock)
    private static func createTestApiSurface() -> (ApiSurface, [String: [String]]) {
        // Array.map method signature
        let elementType = TypeName(path: ["Element"])
        let genericT = TypeName(path: ["T"])
        let closureSignature = TypeName.FunctionSignature(
            parameters: [elementType],
            returnType: genericT,
            isAsync: false,
            throwsKind: .throws
        )
        let closureType = TypeName(path: [], functionSignature: closureSignature)

        let transformParam = ResolvedParameter(
            label: nil,
            name: "transform",
            type: closureType,
            isInout: false,
            isVariadic: false
        )

        let returnType = TypeName(path: ["Swift", "Array"], genericArguments: [genericT])

        let mapSignature = ResolvedCallable(
            selector: "map(_:)",
            baseName: "map",
            returnType: returnType,
            parameters: [transformParam],
            genericParameters: [],
            whereClause: nil,
            availability: [],
            attributes: [],
            throwsKind: .throws,
            isAsync: false,
            sourceLine: 1
        )

        let mapMethod = ResolvedInstanceMethod(signature: mapSignature, isMutating: false)

        let arrayType = HostTypeSurface(
            canonicalName: "Swift.Array",
            availability: [],
            members: [.instanceMethod(mapMethod)]
        )

        let apiSurface = ApiSurface(types: [arrayType])
        let inventory = ["Swift.Array": ["Element"]]

        return (apiSurface, inventory)
    }

    private func run(
        _ src: String,
        file: String = "stdlib-closures.swift",
        hostSurface: (ApiSurface, [String : [String]])? = createTestApiSurface()
    ) throws -> String {
        guard let hostSurface else {
            throw NSError(domain: "HostSurface", code: 1)
        }
        // Prefer generated API surface, fallback to mock
        let (apiSurface, inventory) = hostSurface
        let compiler = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: src, fileName: file)
        var vm = VM()
        let _ = foundationHostBridgeRegistration
        let _ = swiftStdlibHostBridgeRegistration
        try vm.run(program)
        return vm.output
    }

    private func runSlot(
        _ src: String,
        file: String = "stdlib-closures.swift",
        hostSurface: (ApiSurface, [String : [String]])? = createTestApiSurface()
    ) throws -> String {
        guard let hostSurface else {
            throw NSError(domain: "HostSurface", code: 1)
        }
        // Prefer generated API surface, fallback to mock
        let (apiSurface, inventory) = hostSurface
        let compiler = Compiler(options: .init(mode: .slot), hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: src, fileName: file)
        var vm = VM()
        let _ = foundationHostBridgeRegistration
        let _ = swiftStdlibHostBridgeRegistration
        try vm.run(program)
        return vm.output
    }

    @MainActor
    private func runWithGeneratedSurface(_ src: String, file: String = "stdlib-closures.swift") throws -> String {
        // Explicitly require generated API surface
        guard let (apiSurface, inventory) = Self.generatedAPISurface else {
            throw XCTSkip("Generated API surface not available - run gen_swiftstdlib_bridges.sh first")
        }
        let compiler = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: src, fileName: file)
        var vm = VM()
        let _ = foundationHostBridgeRegistration
        let _ = swiftStdlibHostBridgeRegistration
        try vm.run(program)
        return vm.output
    }

    @MainActor
    private func runSlotWithGeneratedSurface(_ src: String, file: String = "stdlib-closures.swift") throws -> String {
        // Explicitly require generated API surface
        guard let (apiSurface, inventory) = Self.generatedAPISurface else {
            throw XCTSkip("Generated API surface not available - run gen_swiftstdlib_bridges.sh first")
        }
        let compiler = Compiler(options: .init(mode: .slot), hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: src, fileName: file)
        var vm = VM()
        let _ = foundationHostBridgeRegistration
        let _ = swiftStdlibHostBridgeRegistration
        try vm.run(program)
        return vm.output
    }
    
    @MainActor
    func testDiagnosticArrayType() throws {
        // Diagnostic test to understand what type the array literal creates
        let src = """
        let strings = ["1", "2", "3"]
        print(strings.count)
        """
        // This should work with VM-native array
        XCTAssertEqual(try run(src, hostSurface: Self.generatedAPISurface), "3\n")
        XCTAssertEqual(try run(src), "3\n")
        XCTAssertEqual(try runSlot(src, hostSurface: Self.generatedAPISurface), "3\n")
        XCTAssertEqual(try runSlot(src), "3\n")
    }

    @MainActor
    func testGeneratedInventoryIncludesArrayPlaceholder() throws {
        guard let (_, inventory) = Self.generatedAPISurface else {
            XCTFail("Generated inventory not available")
            return
        }
        XCTAssertNotNil(inventory["Swift.Array"], "Inventory should contain Swift.Array entry")
    }

    func testStringArrayMapTransformsValues() throws {
        let src = """
        let strings = ["1", "2", "3"]
        let doubled = strings.map({ value in value })
        print(doubled.count)
        """
        XCTAssertEqual(try run(src), "3\n")
//        XCTAssertEqual(try runSlot(src, hostSurface: apiSurface), "3\n")
//        XCTAssertEqual(try runSlot(src), "3\n")
    }
    
  
    @MainActor
    func testStringArrayMapTransformsValuesGenSurface() throws {
        
        let src = """
        let strings = ["1", "2", "3"]
        let doubled = strings.map({ value in value })
        print(doubled.count)
        """
        XCTAssertEqual(try run(src, hostSurface: Self.generatedAPISurface), "3\n")
//        XCTAssertEqual(try runSlot(src, hostSurface: apiSurface), "3\n")
//        XCTAssertEqual(try runSlot(src), "3\n")
    }
    
    @MainActor
    func testStringArrayFilterGenSurface() throws {
        
        let src = """
        let strings = ["1", "2", "3"]
        let doubled = strings.filter({ value in value == "1" })
        print(doubled.count)
        """
        XCTAssertEqual(try run(src, hostSurface: Self.generatedAPISurface), "1\n")
//        XCTAssertEqual(try runSlot(src, hostSurface: apiSurface), "3\n")
//        XCTAssertEqual(try runSlot(src), "3\n")
    }


    @MainActor
    func testArrayIntegerMap() throws {
        let src = """
        let numbers = [1, 2, 3]
        let doubled = numbers.map({ value in value })
        print(doubled.count)
        """
        XCTAssertEqual(try run(src, hostSurface: Self.generatedAPISurface), "3\n")
        XCTAssertEqual(try runSlot(src, hostSurface: Self.generatedAPISurface), "3\n")
    }
    
    
    @MainActor
    func testArrayMapTransformsValues() throws {
        let src = """
        let numbers = [1, 2, 3]
        let doubled = numbers.map({ value in value * 2 })
        print(doubled.count)
        """
        XCTAssertEqual(try run(src, hostSurface: Self.generatedAPISurface), "3\n")
        XCTAssertEqual(try runSlot(src, hostSurface: Self.generatedAPISurface), "3\n")
    }
    
    @MainActor
    func testArrayFilterSelectsSubset() throws {
        let src = """
        let numbers = [1, 2, 3, 4]
        let evens = numbers.filter({ value in value % 2 == 0 })
        print(evens.count)
        """
        XCTAssertEqual(try run(src, hostSurface: Self.generatedAPISurface), "2\n")
        XCTAssertEqual(try runSlot(src, hostSurface: Self.generatedAPISurface), "2\n")
    }
    
    @MainActor
    func testArrayFilterTrailingSelectsSubset() throws {
        let src = """
        let numbers = [1, 2, 3, 4]
        let evens = numbers.filter { value in value % 2 == 0 }
        print(evens.count)
        """
        XCTAssertEqual(try run(src, hostSurface: Self.generatedAPISurface), "2\n")
        XCTAssertEqual(try runSlot(src, hostSurface: Self.generatedAPISurface), "2\n")
    }
    
    
    func testArrayMapTrailingClosure() throws {
        let src = """
        let numbers = [1, 2, 3, 4]
        let evens = numbers.map { value in value }
        print(evens.count)
        """
        XCTAssertEqual(try run(src), "4\n")
        XCTAssertEqual(try runSlot(src), "4\n")
    }

//    func testArrayFilterSelectsSubset() throws {
//        let src = """
//        let numbers = [1, 2, 3, 4]
//        let evens = numbers.filter { value in value }
//        print(evens.count)
//        """
//        XCTAssertEqual(try run(src), "2\n")
//        XCTAssertEqual(try runSlot(src), "2\n")
//    }

//    func testArrayContainsWhereFindsMatch() throws {
//        let src = """
//        let numbers = [1, 3, 5, 8]
//        let hasEven = numbers.contains { candidate in candidate % 2 == 0 }
//        print(hasEven)
//        """
//        XCTAssertEqual(try run(src), "true\n")
//        XCTAssertEqual(try runSlot(src), "true\n")
//    }

//    func testOptionalMapTransformsWrappedValue() throws {
//        let src = """
//        let value: Int? = 5
//        if let doubled = value.map({ candidate in candidate * 2 }) {
//            print(doubled)
//        }
//        """
//        XCTAssertEqual(try run(src), "10\n")
//        XCTAssertEqual(try runSlot(src), "10\n")
//    }
//
//    func testOptionalFlatMapFlattensResult() throws {
//        let src = """
//        let value: Int? = 7
//        if let doubled = value.flatMap({ candidate in Optional(candidate * 2) }) {
//            print(doubled)
//        }
//        """
//        XCTAssertEqual(try run(src), "14\n")
//        XCTAssertEqual(try runSlot(src), "14\n")
//    }

//    func testDictionaryMergeCombinesValues() throws {
//        let src = """
//        var counts = ["a": 1, "b": 2]
//        let incoming = ["a": 3, "c": 4]
//        counts.merge(incoming) { current, incoming in current + incoming }
//        print(counts.count)
//        print(counts["a"]!)
//        print(counts["c"]!)
//        """
//        XCTAssertEqual(try run(src), "3\n4\n4\n")
//        XCTAssertEqual(try runSlot(src), "3\n4\n4\n")
//    }
}
