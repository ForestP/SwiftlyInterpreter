import XCTest
@testable import InterpreterCompiler
import InterpreterModels
import HostSurfaceKit

final class ClosureTypeInferenceTests: XCTestCase {

    // MARK: - Helper: Create mock host surface for testing

    /// Creates a comprehensive ApiSurface with Array.map and property types for testing
    private func createMockArrayMapSurface() -> (ApiSurface, [String: [String]]) {
        // Create String type with count property
        let countProperty = ResolvedProperty(
            name: "count",
            type: TypeName(path: ["Swift", "Int"]),
            isSettable: false,
            availability: [],
            attributes: [],
            sourceLine: 1
        )

        let isEmptyProperty = ResolvedProperty(
            name: "isEmpty",
            type: TypeName(path: ["Swift", "Bool"]),
            isSettable: false,
            availability: [],
            attributes: [],
            sourceLine: 1
        )

        let stringType = HostTypeSurface(
            canonicalName: "Swift.String",
            availability: [],
            members: [
                .instanceProperty(countProperty),
                .instanceProperty(isEmptyProperty)
            ]
        )

        // Create Int type with description property
        let descriptionProperty = ResolvedProperty(
            name: "description",
            type: TypeName(path: ["Swift", "String"]),
            isSettable: false,
            availability: [],
            attributes: [],
            sourceLine: 1
        )

        let intType = HostTypeSurface(
            canonicalName: "Swift.Int",
            availability: [],
            members: [
                .instanceProperty(descriptionProperty)
            ]
        )

        // Create Array.map<T> signature: func map<T>(_ transform: (Element) throws -> T) -> [T]
        // Closure parameter type: (Element) -> T
        let elementType = TypeName(path: ["Element"])
        let genericT = TypeName(path: ["T"])
        let closureParamType = (try? TypeNameParser.parse("Swift.Array<Element>.Element")) ?? elementType

        let closureSignature = TypeName.FunctionSignature(
            parameters: [closureParamType],
            returnType: genericT,
            isAsync: false,
            throwsKind: .none
        )
        let closureType = TypeName(
            path: [],
            functionSignature: closureSignature
        )

        // Method parameter
        let transformParam = ResolvedParameter(
            label: nil,
            name: "transform",
            type: closureType,
            isInout: false,
            isVariadic: false
        )

        // Return type: [T]
        let returnType = TypeName(path: ["Swift", "Array"], genericArguments: [genericT])

        // Method signature
        let mapSignature = ResolvedCallable(
            selector: "map(_:)",
            baseName: "map",
            returnType: returnType,
            parameters: [transformParam],
            genericParameters: [],
            whereClause: nil,
            availability: [],
            attributes: [],
            throwsKind: .none,
            isAsync: false,
            sourceLine: 1
        )

        let mapMethod = ResolvedInstanceMethod(signature: mapSignature, isMutating: false)

        // Array type with first property (generic)
        let firstProperty = ResolvedProperty(
            name: "first",
            type: TypeName(path: ["Swift", "Optional"], genericArguments: [TypeName(path: ["Element"])]),
            isSettable: false,
            availability: [],
            attributes: [],
            sourceLine: 1
        )

        // Array type surface with map method and first property
        let arrayType = HostTypeSurface(
            canonicalName: "Swift.Array",
            availability: [],
            members: [
                .instanceMethod(mapMethod),
                .instanceProperty(firstProperty)
            ]
        )

        let apiSurface = ApiSurface(types: [arrayType, stringType, intType])

        // Generic parameter inventory
        let inventory = [
            "Swift.Array": ["Element"],
            "Swift.Optional": ["Wrapped"]
        ]

        return (apiSurface, inventory)
    }

    // MARK: - Tests: Array Literal Type Inference

    func testArrayLiteralTypeInference_IntArray() throws {
        let source = """
        let nums = [1, 2, 3]
        """

        let (apiSurface, inventory) = createMockArrayMapSurface()
        let compiler = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: source, fileName: "test.swift")

        // Verify compilation succeeds
        XCTAssertGreaterThan(program.ops.count, 0)
    }

    func testArrayLiteralTypeInference_StringArray() throws {
        let source = """
        let strings = ["a", "b", "c"]
        """

        let (apiSurface, inventory) = createMockArrayMapSurface()
        let compiler = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: source, fileName: "test.swift")

        // Verify compilation succeeds
        XCTAssertGreaterThan(program.ops.count, 0)
    }

    func testArrayLiteralTypeInference_DoubleArray() throws {
        let source = """
        let floats = [1.0, 2.0, 3.0]
        """

        let (apiSurface, inventory) = createMockArrayMapSurface()
        let compiler = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: source, fileName: "test.swift")

        // Verify compilation succeeds
        XCTAssertGreaterThan(program.ops.count, 0)
    }

    // MARK: - Tests: Closure Type Inference in Bytecode

    func testClosureTypeInference_IntArrayIdentity() throws {
        let source = """
        let nums = [1, 2, 3]
        let result = nums.map({ x in x })
        """

        let (apiSurface, inventory) = createMockArrayMapSurface()
        let compiler = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: source, fileName: "test.swift")

        // Find the makeClosure opcode
        var foundClosure = false
        var closureParamTypes: [String]?
        var closureReturnType: String?

        for op in program.ops {
            if case let .makeClosure(params, paramTypes, returnType, _, _) = op {
                foundClosure = true
                closureParamTypes = paramTypes
                closureReturnType = returnType
                break
            }
        }

        XCTAssertTrue(foundClosure, "Expected to find makeClosure opcode")
        XCTAssertNotNil(closureParamTypes, "Expected closure to have parameter types")
        XCTAssertNotNil(closureReturnType, "Expected closure to have return type")

        // Verify the closure has type (Swift.Int) -> Swift.Int
        if let paramTypes = closureParamTypes {
            XCTAssertEqual(paramTypes.count, 1, "Expected one parameter type")
            XCTAssertEqual(paramTypes.first, "Swift.Int", "Expected parameter type to be Swift.Int")
        }
        XCTAssertEqual(closureReturnType, "Swift.Int", "Expected return type to be Swift.Int")
    }

    func testClosureTypeInference_IntArrayNonIdentity() throws {
        let source = """
        let nums = [1, 2, 3]
        let doubled = nums.map({ value in value * 2 })
        """

        let (apiSurface, inventory) = createMockArrayMapSurface()
        let compiler = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: source, fileName: "test.swift")

        var foundClosure = false
        var closureParamTypes: [String]?
        var closureReturnType: String?

        for op in program.ops {
            if case let .makeClosure(params, paramTypes, returnType, _, _) = op {
                foundClosure = true
                closureParamTypes = paramTypes
                closureReturnType = returnType
                break
            }
        }

        XCTAssertTrue(foundClosure, "Expected makeClosure opcode in compiled program")
        XCTAssertEqual(closureParamTypes, ["Swift.Int"])
        XCTAssertEqual(closureReturnType, "Swift.Int")
    }

    func testClosureTypeInference_StringArrayIdentity() throws {
        let source = """
        let strings = ["a", "b", "c"]
        let result = strings.map({ x in x })
        """

        let (apiSurface, inventory) = createMockArrayMapSurface()
        let compiler = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: source, fileName: "test.swift")

        // Find the makeClosure opcode
        var closureParamTypes: [String]?
        var closureReturnType: String?

        for op in program.ops {
            if case let .makeClosure(params, paramTypes, returnType, _, _) = op {
                closureParamTypes = paramTypes
                closureReturnType = returnType
                break
            }
        }

        XCTAssertNotNil(closureParamTypes)
        XCTAssertNotNil(closureReturnType)

        // Verify the closure has type (Swift.String) -> Swift.String
        if let paramTypes = closureParamTypes {
            XCTAssertEqual(paramTypes.count, 1)
            XCTAssertEqual(paramTypes.first, "Swift.String", "Expected parameter type to be Swift.String")
        }
        XCTAssertEqual(closureReturnType, "Swift.String", "Expected return type to be Swift.String")
    }

    func testClosureTypeInference_DoubleArrayIdentity() throws {
        let source = """
        let floats = [1.0, 2.0, 3.0]
        let result = floats.map({ x in x })
        """

        let (apiSurface, inventory) = createMockArrayMapSurface()
        let compiler = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: source, fileName: "test.swift")

        // Find the makeClosure opcode
        var closureParamTypes: [String]?
        var closureReturnType: String?

        for op in program.ops {
            if case let .makeClosure(params, paramTypes, returnType, _, _) = op {
                closureParamTypes = paramTypes
                closureReturnType = returnType
                break
            }
        }

        XCTAssertNotNil(closureParamTypes)
        XCTAssertNotNil(closureReturnType)

        // Verify the closure has type (Swift.Double) -> Swift.Double
        if let paramTypes = closureParamTypes {
            XCTAssertEqual(paramTypes.count, 1)
            XCTAssertEqual(paramTypes.first, "Swift.Double", "Expected parameter type to be Swift.Double")
        }
        XCTAssertEqual(closureReturnType, "Swift.Double", "Expected return type to be Swift.Double")
    }

    // MARK: - Tests: Closure Return Type Inference

    func testClosureReturnTypeInference_StringCount() throws {
        let source = """
        let strings = ["hello", "world"]
        let lengths = strings.map({ s in s.count })
        """

        let (apiSurface, inventory) = createMockArrayMapSurface()
        let compiler = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: source, fileName: "test.swift")

        // Find the makeClosure opcode
        var closureParamTypes: [String]?
        var closureReturnType: String?

        for op in program.ops {
            if case let .makeClosure(params, paramTypes, returnType, _, _) = op {
                closureParamTypes = paramTypes
                closureReturnType = returnType
                break
            }
        }

        XCTAssertNotNil(closureParamTypes)
        XCTAssertNotNil(closureReturnType)

        // Verify the closure has type (Swift.String) -> Swift.Int
        if let paramTypes = closureParamTypes {
            XCTAssertEqual(paramTypes.first, "Swift.String")
        }
        XCTAssertEqual(closureReturnType, "Swift.Int", "Expected return type to be Swift.Int for .count property")
    }

    // MARK: - Tests: Backward Compatibility

    func testClosureWithoutHostSurface_FallsBackGracefully() throws {
        let source = """
        let nums = [1, 2, 3]
        let result = nums.map({ x in x })
        """

        // Compile without host surface - should still work but closures have empty types
        let compiler = Compiler()
        let program = try compiler.compileProgram(source: source, fileName: "test.swift")

        // Find the makeClosure opcode
        var foundClosure = false
        var closureParamTypes: [String]?

        for op in program.ops {
            if case let .makeClosure(params, paramTypes, returnType, _, _) = op {
                foundClosure = true
                closureParamTypes = paramTypes
                break
            }
        }

        XCTAssertTrue(foundClosure, "Expected to find makeClosure opcode")

        // Without host surface, types should be empty (graceful degradation)
        if let paramTypes = closureParamTypes {
            XCTAssertEqual(paramTypes.count, 0, "Expected empty param types without host surface")
        }
    }

    // MARK: - Tests: Generic Substitution Edge Cases

    func testMultipleClosuresInSameProgram() throws {
        let source = """
        let nums = [1, 2, 3]
        let doubled = nums.map({ x in x })

        let strings = ["a", "b"]
        let lengths = strings.map({ s in s.count })
        """

        let (apiSurface, inventory) = createMockArrayMapSurface()
        let compiler = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: source, fileName: "test.swift")

        // Find all makeClosure opcodes
        var closures: [(paramTypes: [String], returnType: String?)] = []

        for op in program.ops {
            if case let .makeClosure(_, paramTypes, returnType, _, _) = op {
                closures.append((paramTypes, returnType))
            }
        }

        XCTAssertEqual(closures.count, 2, "Expected two closures")

        // First closure: (Int) -> Int
        if closures.count >= 1 {
            XCTAssertEqual(closures[0].paramTypes.first, "Swift.Int")
            XCTAssertEqual(closures[0].returnType, "Swift.Int")
        }

        // Second closure: (String) -> Int
        if closures.count >= 2 {
            XCTAssertEqual(closures[1].paramTypes.first, "Swift.String")
            XCTAssertEqual(closures[1].returnType, "Swift.Int")
        }
    }

    // MARK: - Tests: Property Type Inference

    func testPropertyTypeInference_IntDescription() throws {
        let source = """
        let nums = [1, 2, 3]
        let descriptions = nums.map({ n in n.description })
        """

        let (apiSurface, inventory) = createMockArrayMapSurface()
        let compiler = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: source, fileName: "test.swift")

        // Find the makeClosure opcode
        var closureReturnType: String?

        for op in program.ops {
            if case let .makeClosure(_, paramTypes, returnType, _, _) = op {
                closureReturnType = returnType
                break
            }
        }

        // Verify closure has type (Int) -> String (via Int.description property)
        XCTAssertEqual(closureReturnType, "Swift.String", "Expected Int.description to return String")
    }

    func testPropertyTypeInference_StringIsEmpty() throws {
        let source = """
        let strings = ["hello", "world", ""]
        let empties = strings.map({ s in s.isEmpty })
        """

        let (apiSurface, inventory) = createMockArrayMapSurface()
        let compiler = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: source, fileName: "test.swift")

        // Find the makeClosure opcode
        var closureReturnType: String?

        for op in program.ops {
            if case let .makeClosure(_, _, returnType, _, _) = op {
                closureReturnType = returnType
                break
            }
        }

        // Verify closure has type (String) -> Bool (via String.isEmpty property)
        XCTAssertEqual(closureReturnType, "Swift.Bool", "Expected String.isEmpty to return Bool")
    }

    func testGenericPropertyTypeSubstitution_ArrayFirst() throws {
        let source = """
        let arrays = [[1, 2], [3, 4]]
        let firsts = arrays.map({ arr in arr.first })
        """

        let (apiSurface, inventory) = createMockArrayMapSurface()
        let compiler = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: source, fileName: "test.swift")

        // Find the makeClosure opcode
        var closureParamTypes: [String]?
        var closureReturnType: String?

        for op in program.ops {
            if case let .makeClosure(_, paramTypes, returnType, _, _) = op {
                closureParamTypes = paramTypes
                closureReturnType = returnType
                break
            }
        }

        // Verify parameter type is Array<Int>
        if let paramTypes = closureParamTypes {
            XCTAssertEqual(paramTypes.first, "Swift.Array<Swift.Int>", "Expected parameter type to be Array<Int>")
        }

        // Verify return type is Int? (Array<Int>.first has type Element? which is Int?)
        XCTAssertEqual(closureReturnType, "Swift.Optional<Swift.Int>", "Expected Array<Int>.first to return Int?")
    }

    func testChainedPropertyAccess() throws {
        let source = """
        let nums = [1, 2, 3]
        let descCounts = nums.map({ n in n.description.count })
        """

        let (apiSurface, inventory) = createMockArrayMapSurface()
        let compiler = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: source, fileName: "test.swift")

        // Find the makeClosure opcode
        var closureReturnType: String?

        for op in program.ops {
            if case let .makeClosure(_, _, returnType, _, _) = op {
                closureReturnType = returnType
                break
            }
        }

        // Verify: Int.description → String, String.count → Int
        XCTAssertEqual(closureReturnType, "Swift.Int", "Expected chained property access to infer Int")
    }

    func testPropertyAccessWithoutRegisteredType_ReturnsNil() throws {
        // Test that unknown properties don't crash, just return nil (conservative)
        let source = """
        let nums = [1, 2, 3]
        let unknowns = nums.map({ n in n.unknownProperty })
        """

        let (apiSurface, inventory) = createMockArrayMapSurface()
        let compiler = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let program = try compiler.compileProgram(source: source, fileName: "test.swift")

        // Find the makeClosure opcode
        var closureReturnType: String?

        for op in program.ops {
            if case let .makeClosure(_, _, returnType, _, _) = op {
                closureReturnType = returnType
                break
            }
        }

        // Unknown property should result in nil return type (graceful degradation)
        XCTAssertNil(closureReturnType, "Expected nil return type for unknown property")
    }
}
