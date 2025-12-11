import InterpreterModels
import InterpreterCompiler
import InterpreterVM

import XCTest

final class PropertyWrapperStateIntegrationTests: XCTestCase {

    private final class StateBox { var value: Value; init(_ v: Value) { self.value = v } }

    private func runWithState(_ src: String, file: String = "state.swift") throws -> String {
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: file)
        var vm = VM()
        // Register minimal @State host bridge
        vm.registerHostMethod(type: "State", selector: "init(wrappedValue:)") { vm, args in
            // args: [metatype(State), wrappedValue]
            guard args.count == 2 else { throw VMError.typeError("State.init arity") }
            guard case let .metatype(tid) = args[0] else { throw VMError.typeError("missing metatype receiver") }
            return .host(HostRef(box: makeOpaqueBox(StateBox(args[1]))), tid)
        }
        vm.registerHostProperty(type: "State", name: "wrappedValue",
            get: { vm, args in
                try vm.withHost(args[0], typeName: "State", as: StateBox.self) { box in
                    box.value
                }
            },
            set: { vm, args in
                guard args.count == 2 else { throw VMError.typeError("wrappedValue set arity") }
                return try vm.withHost(args[0], typeName: "State", as: StateBox.self) { box in
                    box.value = args[1]
                    return args[0]
                }
            }
        )
        vm.registerHostProperty(type: "State", name: "projectedValue", get: { vm, args in
            // Projected value: return self for now
            return args[0]
        })
        try vm.run(p)
        return vm.output
    }

    func testWrappedReadWriteThroughState() throws {
        let src = """
        struct Counter { @State var count: Int }
        var c = Counter(count: 2)
        print(c.count)
        c.count = 5
        print(c.count)
        """
        let out = try runWithState(src)
        XCTAssertEqual(out, "2\n5\n")
    }

    func testProjectionDollarAccess() throws {
        let src = """
        struct S { @State var x: Int }
        func f(_ s: S) {
          _ = s.$x // should compile (projects to projectedValue)
        }
        let s = S(x: 1)
        print(s.x)
        """
        let out = try runWithState(src)
        XCTAssertEqual(out, "1\n")
    }
}

