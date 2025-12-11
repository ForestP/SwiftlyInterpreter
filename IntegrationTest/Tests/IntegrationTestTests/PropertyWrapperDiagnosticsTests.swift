import InterpreterModels
import InterpreterCompiler
import InterpreterVM

import XCTest

final class PropertyWrapperDiagnosticsTests: XCTestCase {

    func testWrapperRequiresVarOnField() {
        let src = """
        struct S {
          @State let x: Int = 0
        }
        """
        let c = Compiler()
        do {
            _ = try c.compileProgram(source: src, fileName: "diag1.swift")
            XCTFail("expected error")
        } catch {
            let msg = String(describing: error)
            XCTAssertTrue(msg.contains("requires 'var'"), msg)
        }
    }

    func testWrapperRequiresVarOnLocal() {
        let src = """
        func f() {
          @State let x: Int = 1
        }
        """
        let c = Compiler()
        do {
            _ = try c.compileProgram(source: src, fileName: "diag2.swift")
            XCTFail("expected error")
        } catch {
            let msg = String(describing: error)
            XCTAssertTrue(msg.contains("requires 'var'"), msg)
        }
    }
}

