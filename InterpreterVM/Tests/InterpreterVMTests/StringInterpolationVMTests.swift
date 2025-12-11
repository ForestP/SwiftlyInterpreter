import XCTest
import InterpreterModels
@testable import InterpreterVM

final class StringInterpolationVMTests: XCTestCase {
    func testInterpolatedConcatenationProducesExpectedOutput() throws {
        // Tables
        let typeTable = ["Person"]
        let propertyTable = ["name"]

        // Helper IDs
        let tid = TypeID(raw: 0)
        let pidName = PropertyID(raw: 0)

        // Layout:
        // 0: jump past function body to top-level
        // 1..N: function body (entry)
        // A: defineFunction("hello", params:["self"], entry:1, localCount:0)
        // A+1..: top-level: construct Person(name: "Forest"), dup, printTop, callFunc hello, printTop
        var ops: [OpCode] = []
        var locs: [SourceLoc?] = []
        func emit(_ op: OpCode) { ops.append(op); locs.append(nil) }

        // Prologue jump (patched later)
        emit(.jump(0))                          // 0
        let entry = ops.count                   // 1

        // Function body: return "Hello \(self.name)"
        emit(.loadConst(.string("")))          // init acc
        emit(.loadConst(.string("Hello ")))    // segment
        emit(.add)                              // acc += "Hello "
        emit(.loadVar("self"))                 // push self
        emit(.getProp(pidName))                 // push self.name
        emit(.add)                              // acc += name
        emit(.ret)                              // return acc

        // After body: type def and function def
        let afterBody = ops.count
        // Patch prologue
        ops[0] = .jump(afterBody - (0 + 1))

        emit(.defineType(name: "Person", fields: ["name"]))
        emit(.defineFunction(name: "hello", params: ["self"], entry: entry, localCount: 0))

        // Top-level: f = Person(name: "Forest"); print(f); print(hello(f))
        emit(.loadConst(.string("Forest")))
        emit(.newObject(typeName: "Person", argLabels: ["name"]))
        emit(.dup)              // keep a copy for calling hello
        emit(.printTop)         // prints: Person { name: Forest }
        emit(.callFunc(name: "hello", argc: 1))
        emit(.printTop)         // prints: Hello Forest

        let program = Program(
            ops: ops,
            locs: locs,
            source: "<unit>",
            file: "<unit>",
            debug: nil,
            typeTable: typeTable,
            selectorTable: [],
            propertyTable: propertyTable,
            id: 42,
            userMethods: [:]
        )

        var vm = VM()
        try vm.run(program)

        let expected = """
        Person { name: Forest }
        Hello Forest
        
        """
        XCTAssertEqual(vm.output, expected)
    }
}

