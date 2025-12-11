import XCTest
@testable import InterpreterCompiler
import InterpreterModels

final class PropertyWrapperLoweringTests: XCTestCase {

    func testMemberwiseInitLowersWrappedFieldToBackingAndWrapperConstruction() throws {
        let source = """
        struct Counter {
          @State var count: Int
        }
        let a = Counter(count: 2)
        """

        let compiler = Compiler()
        let program = try compiler.compileProgram(source: source, fileName: "WrapperInit.swift")

        // 1) defineType should list only the backing field _count
        var typeFieldsOK = false
        for op in program.ops {
            if case let .defineType(name, fields) = op, name == "Counter" {
                typeFieldsOK = (fields == ["_count"]) || (Set(fields) == Set(["_count"]))
                break
            }
        }
        XCTAssertTrue(typeFieldsOK, "Expected Counter to define only backing field _count")

        // 2) Expect a call to State.init(wrappedValue:)
        let selName = "init(wrappedValue:)"
        let hasInitWrapped = program.selectorTable.contains(selName)
        XCTAssertTrue(hasInitWrapped, "Selector table should include \(selName)")

        let sawCallInitWrapped = program.ops.contains { op in
            if case let .callMethod(sel, argc) = op {
                return program.selectorTable[sel.raw] == selName && argc == 2
            }
            return false
        }
        XCTAssertTrue(sawCallInitWrapped, "Expected callMethod to State.init(wrappedValue:)")

        // 3) newObject should use backing label _count
        let sawNewObjectWithBacking = program.ops.contains { op in
            if case let .newObject(typeName, labels) = op, typeName == "Counter" {
                return labels.contains("_count") && !labels.contains("count")
            }
            return false
        }
        XCTAssertTrue(sawNewObjectWithBacking, "Expected newObject with backing label _count")
    }
}
