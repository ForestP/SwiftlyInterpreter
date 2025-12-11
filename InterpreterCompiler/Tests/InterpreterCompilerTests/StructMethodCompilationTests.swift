import XCTest
@testable import InterpreterCompiler
import InterpreterModels

final class StructMethodCompilationTests: XCTestCase {
    func testCompilesStructWithMethodAndUserMethodTable() throws {
        let source = """
        struct Person {
            let name: String
            func greet() -> String {
                return "Hello"
            }
        }
        // Top-level usage to force property access interning
        var p = Person(name: "A")
        let n = p.name
        """

        let compiler = Compiler()
        let program = try compiler.compileProgram(source: source, fileName: "Person.swift")

        // Type table includes entries
        XCTAssertTrue(program.typeTable.contains("Person"), "Expected type 'Person' to be interned")
        // Property table should include 'name' because we accessed p.name above
        XCTAssertTrue(program.propertyTable.contains("name"), "Expected property 'name' to be interned after member access")

        // Selector table should include zero-arg method selector
        XCTAssertTrue(program.selectorTable.contains("greet()"), "Expected selector 'greet()' to be interned")

        // Verify a defineType op for Person with field ["name"] exists
        let hasDefineType = program.ops.contains { op in
            if case let .defineType(name, fields) = op {
                return name == "Person" && fields == ["name"]
            }
            return false
        }
        XCTAssertTrue(hasDefineType, "Expected a defineType('Person', fields: ['name']) op")

        // Verify user method table contains Person.greet()
        let methodPairs = program.userMethods.keys.map { key in
            (program.typeTable[key.type.raw], program.selectorTable[key.selector.raw])
        }
        XCTAssertTrue(methodPairs.contains(where: { $0.0 == "Person" && $0.1 == "greet()" }),
                      "Expected user method entry for Person.greet()")
    }
}
