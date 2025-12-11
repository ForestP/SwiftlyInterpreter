import XCTest
import Swiftly
import InterpreterCompiler
import InterpreterVM

final class SwiftStdlibClosureInteropWasmTests: XCTestCase {

    private func run(_ src: String, file: String = "stdlib-closures-wasm.swift") throws -> String {
        let compiler = Compiler()
        let program = try compiler.compileProgram(source: src, fileName: file)
        var vm = VM()
        let _ = foundationHostBridgeRegistration
        let _ = swiftStdlibHostBridgeRegistration
        try vm.prepare(program)
        try vm.run(program)
        return vm.output
    }

    func testArrayMapTransformsValuesOnWasm() throws {
        let src = """
        let numbers = [1, 2, 3]
        let doubled = numbers.map { value in value * 2 }
        print(doubled.count)
        """
        XCTAssertEqual(try run(src), "3\n")
    }

    func testArrayFilterSelectsSubsetOnWasm() throws {
        let src = """
        let numbers = [1, 2, 3, 4]
        let evens = numbers.filter { value in value % 2 == 0 }
        print(evens.count)
        """
        XCTAssertEqual(try run(src), "2\n")
    }

    func testArrayContainsWhereFindsMatchOnWasm() throws {
        let src = """
        let numbers = [1, 3, 5, 8]
        let hasEven = numbers.contains { candidate in candidate % 2 == 0 }
        print(hasEven)
        """
        XCTAssertEqual(try run(src), "true\n")
    }

    func testOptionalMapTransformsWrappedValueOnWasm() throws {
        let src = """
        let value: Int? = 5
        if let doubled = value.map({ candidate in candidate * 2 }) {
            print(doubled)
        }
        """
        XCTAssertEqual(try run(src), "10\n")
    }

    func testOptionalFlatMapFlattensResultOnWasm() throws {
        let src = """
        let value: Int? = 7
        if let doubled = value.flatMap({ candidate in Optional(candidate * 2) }) {
            print(doubled)
        }
        """
        XCTAssertEqual(try run(src), "14\n")
    }

    func testDictionaryMergeCombinesValuesOnWasm() throws {
        let src = """
        var counts = ["a": 1, "b": 2]
        let incoming = ["a": 3, "c": 4]
        counts.merge(incoming) { current, incoming in current + incoming }
        print(counts.count)
        print(counts["a"]!)
        print(counts["c"]!)
        """
        XCTAssertEqual(try run(src), "3\n4\n4\n")
    }
}
