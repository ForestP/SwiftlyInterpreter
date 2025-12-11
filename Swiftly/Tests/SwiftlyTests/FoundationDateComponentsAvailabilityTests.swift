import XCTest
import Swiftly
import InterpreterModels
import InterpreterCompiler
import InterpreterVM

final class FoundationDateComponentsAvailabilityTests: XCTestCase {

    private func run(_ src: String, file: String = "wasm.swift") throws -> String {
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: file)
        var vm = VM()
        try vm.prepare(p)
        try vm.run(p)
        return vm.output
    }

    func testDateComponentsIsRepeatedDayAvailabilityGuard() throws {
        let src = """
        var c = DateComponents()
        print(c.isRepeatedDay == nil)
        """
        do {
            let out = try run(src)
            XCTAssertEqual(out.trimmingCharacters(in: .whitespacesAndNewlines), "true")
        } catch let e as VMError {
            switch e {
            case .unsupported(let msg):
                XCTAssertTrue(msg.contains("DateComponents.isRepeatedDay"))
            default:
                XCTFail("Unexpected VM error: \(e)")
            }
        } catch {
            XCTFail("Unexpected error: \(error)")
        }
    }

    func testLocalePreferredLocalesAvailabilityGuard() throws {
        let src = """
        print(Locale.preferredLocales.count >= 0)
        """
        do {
            let out = try run(src)
            XCTAssertEqual(out.trimmingCharacters(in: .whitespacesAndNewlines), "true")
        } catch let e as VMError {
            switch e {
            case .unsupported(let msg):
                XCTAssertTrue(msg.contains("Locale.preferredLocales"))
            default:
                XCTFail("Unexpected VM error: \(e)")
            }
        } catch {
            XCTFail("Unexpected error: \(error)")
        }
    }
}
