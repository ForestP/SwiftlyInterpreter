import InterpreterModels
import InterpreterCompiler
import InterpreterVM

import XCTest

final class PropertyWrapperDefaultsIntegrationTests: XCTestCase {

    private final class StateBox { var value: Value; init(_ v: Value) { self.value = v } }

    private func runWithState(_ src: String, file: String = "state_defaults.swift") throws -> String {
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: file)
        var vm = VM()
        // Register minimal @State host bridge
        vm.registerHostMethod(type: "State", selector: "init(wrappedValue:)") { vm, args in
            guard args.count == 2 else { throw VMError.typeError("State.init arity") }
            guard case let .metatype(tid) = args[0] else { throw VMError.typeError("missing metatype receiver") }
            return .host(HostRef(box: makeOpaqueBox(StateBox(args[1]))), tid)
        }
        vm.registerHostProperty(type: "State", name: "wrappedValue",
            get: { vm, args in
                try vm.withHost(args[0], typeName: "State", as: StateBox.self) { box in box.value }
            },
            set: { vm, args in
                return try vm.withHost(args[0], typeName: "State", as: StateBox.self) { box in box.value = args[1]; return args[0] }
            }
        )
        try vm.run(p)
        return vm.output
    }

    func testWrappedDefaultUsedWhenArgOmitted() throws {
        let src = """
        struct Counter { @State var count: Int = 1 }
        let c = Counter()
        print(c.count)
        """
        let out = try runWithState(src)
        XCTAssertEqual(out, "1\n")
    }

    func testMixedDefaultsPlainAndWrapped() throws {
        let src = """
        struct S {
          @State var x: Int = 1
          var y: Int = 2
        }
        let a = S()
        print(a.x)
        print(a.y)
        let b = S(x: 5)
        print(b.x)
        print(b.y)
        """
        let out = try runWithState(src)
        XCTAssertEqual(out, "1\n2\n5\n2\n")
    }

    func testMissingDefaultErrors() {
        let src = """
        struct P { var a: Int }
        let p = P()
        """
        do {
            _ = try runWithState(src)
            XCTFail("expected compile-time error for missing default")
        } catch {
            let msg = String(describing: error)
            XCTAssertTrue(msg.contains("Missing argument for property 'a'"), msg)
        }
    }
}

