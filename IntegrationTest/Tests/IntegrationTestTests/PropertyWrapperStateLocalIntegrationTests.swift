import InterpreterModels
import InterpreterCompiler
import InterpreterVM

import XCTest

final class PropertyWrapperStateLocalIntegrationTests: XCTestCase {
    private final class StateBox { var value: Value; init(_ v: Value) { self.value = v } }

    private func runWithState(_ src: String, file: String = "state_local.swift") throws -> String {
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: file)
            var vm = VM()
        vm.registerHostMethod(type: "State", selector: "init(wrappedValue:)") { vm, args in
            guard args.count == 2 else { throw VMError.typeError("State.init arity") }
            guard case let .metatype(tid) = args[0] else { throw VMError.typeError("missing metatype receiver") }
            return .host(HostRef(box: makeOpaqueBox(StateBox(args[1]))), tid)
        }
        vm.registerHostProperty(type: "State", name: "wrappedValue",
            get: { vm, args in try vm.withHost(args[0], typeName: "State", as: StateBox.self) { $0.value } },
            set: { vm, args in try vm.withHost(args[0], typeName: "State", as: StateBox.self) { box in box.value = args[1]; return args[0] } }
        )
        try vm.run(p)
        return vm.output
    }

    func testLocalStateVariableReadWrite() throws {
        let src = """
        func f() {
          @State var x: Int = 2
          print(x)
          x = 5
          print(x)
        }
        f()
        """
        let out = try runWithState(src)
        XCTAssertEqual(out, "2\n5\n")
    }

    func testTopLevelStateVariableReadWrite() throws {
        let src = """
        @State var g: Int = 10
        print(g)
        g = 15
        print(g)
        """
        let out = try runWithState(src)
        XCTAssertEqual(out, "10\n15\n")
    }
}

