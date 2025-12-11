import InterpreterModels
import InterpreterCompiler
import InterpreterVM

import XCTest

final class StringInterpolationIntegrationTests: XCTestCase {

    private func run(_ src: String, file: String = "interp.swift") throws -> String {
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: file)
        var vm = VM()
        try vm.run(p)
        return vm.output
    }

    func testFunctionInterpolation_VM() throws {
        let src = """
        func greet(name: String) -> String {
          return "Hello \\(name)"
        }
        print(greet(name: "Forest"))
        """
        let out = try run(src)
        XCTAssertEqual(out, "Hello Forest\n")
    }

    func testMethodImplicitSelfInterpolation_VM() throws {
        let src = """
        struct Person {
          var name: String
          func hello() -> String { return "Hello \\(name)" }
        }
        print(Person(name: "X").hello())
        """
        let out = try run(src)
        XCTAssertEqual(out, "Hello X\n")
    }

    func testMultipleInterpolations_VM() throws {
        let src = """
        func greet(first: String, last: String) -> String {
          return "Hello \\(first) \\(last)!"
        }
        print(greet(first: "Forest", last: "Plasencia"))
        """
        let out = try run(src)
        XCTAssertEqual(out, "Hello Forest Plasencia!\n")
    }

    func testInterpolationWithNumericExpression_VM() throws {
        let src = """
        print("Sum is \\(1 + 2)")
        print("Val is \\((3.5))")
        """
        let out = try run(src)
        XCTAssertEqual(out, "Sum is 3\nVal is 3.5\n")
    }
    func testMultipleInterpolationsWithArithmetic_VM() throws {
        let src = """
        print("A \\((1 + 2)) and \\((3 + 4))")
        """
        let out = try run(src)
        XCTAssertEqual(out, "A 3 and 7\n")
    }

    func testNestedParenthesesInterpolation_VM() throws {
        let src = """
        print("V \\(((1 + 2) * (3)))")
        """
        let out = try run(src)
        XCTAssertEqual(out, "V 9\n")
    }
}
