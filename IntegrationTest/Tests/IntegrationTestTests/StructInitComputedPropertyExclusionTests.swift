import InterpreterModels
import InterpreterCompiler
import InterpreterVM

import XCTest

final class StructInitComputedPropertyExclusionTests: XCTestCase {

    private func run(_ src: String, file: String = "struct_init.swift") throws -> String {
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: file)
        var vm = VM()
        try vm.run(p)
        return vm.output
    }

    private func runExpectError(_ src: String, file: String = "struct_init_err.swift") -> String {
        do {
            _ = try run(src, file: file)
            XCTFail("Expected error, but program ran successfully")
            return ""
        } catch let e as VMExecutionError {
            return e.description
        } catch {
            return String(describing: error)
        }
    }

    func testMemberwiseInitIgnoresComputedProperties() throws {
        let src = """
        struct Person {
          let name: String
          var greet: String { return "Hello \\((name))" }
        }
        let p = Person(name: "Forest")
        print(p.name)
        """
        let out = try run(src)
        XCTAssertEqual(out, "Forest\n")
    }

    func testInitRejectsUnknownLabelsFromComputedProps() {
        let src = """
        struct Person {
          let name: String
          var greet: String { return "Hello \\((name))" }
        }
        // Should not accept 'greet' as an initializer label, since it's computed.
        let a = Person(name: "Forest", greet: "X")
        """
        let msg = runExpectError(src)
        // Expected should only list the stored field(s)
        XCTAssertTrue(msg.contains("Expected labeled args [\"name\"]"), msg)
        // And the 'got' should reflect the call site including the extraneous 'greet'
        XCTAssertTrue(msg.contains("got [\"name\", \"greet\"]"), msg)
    }
}
