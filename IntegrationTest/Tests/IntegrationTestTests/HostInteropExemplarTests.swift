//
//  HostInteropExemplarTests.swift
//  IntegrationTest
//
//  Created by Forest Plasencia on 9/19/25.
//

import InterpreterModels
import InterpreterCompiler
import InterpreterVM

import XCTest

final class HostInteropExemplarTests: XCTestCase {

    private final class UIImageStub {
        let name: String
        init(_ name: String) { self.name = name }
    }
    private final class TitleBox { var title: String; init(_ t: String) { self.title = t } }

    func testNestedClosuresWithHostCalls() throws {
        let src = """
        let name = "xyz"
        let outer = { () -> () -> Void in
            return {
                let img = UIImage(named: name)
                let d = img.pngData()
                print(d.count)
            }
        }
        let inner = outer()
        inner()
        """

        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: "nested.swift")

        var vm = VM()
        vm.registerHostMethod(type: "UIImage", selector: "init(named:)") { vm, args in
            guard args.count == 2, case let .metatype(imgTid) = args[0] else { throw VMError.typeError("init(named:) receiver") }
            let name: String
            if case let .string(s) = args[1] { name = s } else { throw VMError.typeError("init(named:) expects String") }
            return .host(HostRef(box: makeOpaqueBox(UIImageStub(name))), imgTid)
        }
        vm.registerHostMethod(type: "UIImage", selector: "pngData()") { vm, _ in
            let dataTid = vm.hostTypeID(named: "Data")
            return .host(HostRef(box: makeOpaqueBox(Data([0, 1, 2, 3]))), dataTid)
        }
        vm.registerHostProperty(type: "Data", name: "count", get: { vm, args in
            try vm.withHost(args[0], typeName: "Data", as: Data.self) { data in
                .int(data.count)
            }
        })

        try vm.run(p)
        XCTAssertEqual(vm.output, "4\n")
    }

    func testUppercaseShadowingDoesNotBreakHostCalls() throws {
        let src = """
        let UIImage = 42  // unrelated uppercase identifier in scope
        let f = {
            let img = UIImage(named: "a")
            let d = img.pngData()
            print(d.count)
        }
        f()
        """

        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: "shadow.swift")

        var vm = VM()
        vm.registerHostMethod(type: "UIImage", selector: "init(named:)") { vm, args in
            guard case let .metatype(imgTid) = args[0] else { throw VMError.typeError("missing metatype receiver") }
            let name: String
            if args.count >= 2, case let .string(s) = args[1] { name = s } else { throw VMError.typeError("init(named:) expects String") }
            return .host(HostRef(box: makeOpaqueBox(UIImageStub(name))), imgTid)
        }
        vm.registerHostMethod(type: "UIImage", selector: "pngData()") { vm, _ in
            let dataTid = vm.hostTypeID(named: "Data")
            return .host(HostRef(box: makeOpaqueBox(Data([0, 1, 2, 3, 4]))), dataTid)
        }
        vm.registerHostProperty(type: "Data", name: "count", get: { vm, args in
            try vm.withHost(args[0], typeName: "Data", as: Data.self) { data in
                .int(data.count)
            }
        })

        try vm.run(p)
        XCTAssertEqual(vm.output, "5\n")
    }

    func testNilValueFromHostAndEquality() throws {
        let src = """
        let img = UIImage(named: "missing")
        print(img)           // expect nil
        print(img == nil)    // true
        """

        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: "nilhost.swift")

        var vm = VM()
        // init(named:) returns nil
        vm.registerHostMethod(type: "UIImage", selector: "init(named:)") { vm, _ in .nilValue }

        try vm.run(p)
        XCTAssertEqual(vm.output, "nil\ntrue\n")
    }

    func testMethodCallOnNilErrors() {
        let src = """
        let img = UIImage(named: "missing")
        img.pngData()   // calling method on nil should error
        """
        let c = Compiler()
        let p = try! c.compileProgram(source: src, fileName: "nilmeth.swift")
        var vm = VM()
        vm.registerHostMethod(type: "UIImage", selector: "init(named:)") { _, _ in .nilValue }
        let msg: String
        do { try vm.run(p); XCTFail("expected error") ; return } catch let e as VMExecutionError { msg = e.description } catch { msg = String(describing: error) }
        XCTAssertTrue(msg.contains("Receiver has no methods") || msg.contains("Type error"), msg)
    }

    func testOptionalChainingShortCircuitsToNil() throws {
        let src = """
        let d = UIImage(named: "missing")?.pngData()
        print(d == nil)
        """
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: "optchain.swift")
        var vm = VM()
        vm.registerHostMethod(type: "UIImage", selector: "init(named:)") { _, _ in .nilValue }
        // Intentionally do NOT register pngData(); it should not be called when receiver is nil
        try vm.run(p)
        XCTAssertEqual(vm.output, "true\n")
    }

    func testOptionalChainingPassesThroughOnNonNil() throws {
        let src = """
        let d = UIImage(named: "ok")?.pngData()
        print(d.count)
        """
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: "optchain2.swift")
        var vm = VM()
        vm.registerHostMethod(type: "UIImage", selector: "init(named:)") { vm, args in
            guard case let .metatype(tid) = args[0] else { return .nilValue }
            let name: String
            if args.count >= 2, case let .string(s) = args[1] { name = s } else { throw VMError.typeError("init(named:) expects String") }
            return .host(HostRef(box: makeOpaqueBox(UIImageStub(name))), tid)
        }
        vm.registerHostMethod(type: "UIImage", selector: "pngData()") { vm, _ in
            let dataTid = vm.hostTypeID(named: "Data")
            return .host(HostRef(box: makeOpaqueBox(Data([0, 1, 2, 3, 4, 5, 6]))), dataTid)
        }
        vm.registerHostProperty(type: "Data", name: "count", get: { vm, args in
            try vm.withHost(args[0], typeName: "Data", as: Data.self) { data in
                .int(data.count)
            }
        })
        try vm.run(p)
        XCTAssertEqual(vm.output, "7\n")
    }

    func testOptionalPropertyChainingNilAndNonNil() throws {
        let src = """
        let a = [1, 2]
        print(a?.count)     // non-nil path
        let b = nil
        print(b?.count == nil)
        """
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: "optprop.swift")
        var vm = VM()
        try vm.run(p)
        XCTAssertEqual(vm.output, "2\ntrue\n")
    }

    func testOptionalSubscriptChainingNilAndNonNil() throws {
        let src = """
        let d = ["x": 1]
        print(d?["x"])      // non-nil path
        let e = nil
        print(e?["y"] == nil)
        """
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: "optsub.swift")
        var vm = VM()
        try vm.run(p)
        XCTAssertEqual(vm.output, "1\ntrue\n")
    }

    func testNilCoalescingBasicsAndWithChaining() throws {
        let src = """
        let a = nil
        let b = 5
        print((a ?? b) == 5)

        let img = UIImage(named: "missing")
        let d = img?.pngData() ?? Data()
        print(d.count)
        """
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: "coalesce.swift")
        var vm = VM()
        vm.registerHostMethod(type: "UIImage", selector: "init(named:)") { _, _ in .nilValue }
        vm.registerHostMethod(type: "Data", selector: "init()") { vm, _ in
            let tid = vm.hostTypeID(named: "Data")
            return .host(HostRef(box: makeOpaqueBox(Data())), tid)
        }
        vm.registerHostProperty(type: "Data", name: "count", get: { vm, args in
            try vm.withHost(args[0], typeName: "Data", as: Data.self) { data in
                .int(data.count)
            }
        })
        try vm.run(p)
        XCTAssertEqual(vm.output, "true\n0\n")
    }

    func testHostPropertyGetAndSet() throws {
        let src = """
        let t = Thing()
        print(t.title)
        t.title = "B"
        print(t.title)
        """
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: "hostprop.swift")
        var vm = VM()
        // init()
        vm.registerHostMethod(type: "Thing", selector: "init()") { vm, args in
            guard case let .metatype(tid) = args[0] else { throw VMError.typeError("missing metatype receiver") }
            return .host(HostRef(box: makeOpaqueBox(TitleBox("A"))), tid)
        }
        // title property get/set
        vm.registerHostProperty(type: "Thing", name: "title",
            get: { vm, args in
                try vm.withHost(args[0], typeName: "Thing", as: TitleBox.self) { host in
                    .string(host.title)
                }
            },
            set: { vm, args in
                guard case let .string(s) = args[1] else { throw VMError.typeError("title expects String") }
                return try vm.withHost(args[0], typeName: "Thing", as: TitleBox.self) { host in
                    host.title = s
                    return args[0]
                }
            }
        )
        try vm.run(p)
        XCTAssertEqual(vm.output, "A\nB\n")
    }

    func testHostPropertyAllowlistBlocksProperty() throws {
        let src = """
        let t = Thing()
        print(t.title)
        """
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: "hostprop_allow.swift")
        var vm = VM()
        // Allow type and init(), but block properties
        vm.setHostAllowlist(HostAllowlist(
            types: Set(["Thing"]),
            methodsByType: ["Thing": Set(["init()"])],
            propertiesByType: ["Thing": Set<String>([])]
        ))
        vm.registerHostMethod(type: "Thing", selector: "init()") { vm, args in
            guard case let .metatype(tid) = args[0] else { throw VMError.typeError("missing metatype receiver") }
            return .host(HostRef(box: makeOpaqueBox(TitleBox("A"))), tid)
        }
        // Register getter but allowlist will filter it out
        vm.registerHostProperty(type: "Thing", name: "title", get: { vm, args in
            try vm.withHost(args[0], typeName: "Thing", as: TitleBox.self) { host in
                .string(host.title)
            }
        })
        let msg: String
        do { try vm.run(p); XCTFail("expected missing property error"); return } catch let e as VMExecutionError { msg = e.description } catch { msg = String(describing: error) }
        XCTAssertTrue(msg.contains("Missing property Thing.title"), msg)
    }

    func testPropertyMethodCollisionIsRejected() throws {
        let src = """
        let x = Foo()
        print(x.p)
        """
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: "collision.swift")
        var vm = VM()
        // Register both a zero-arg method p() and a property p on Foo
        vm.registerHostMethod(type: "Foo", selector: "init()") { vm, args in
            guard case let .metatype(tid) = args[0] else { throw VMError.typeError("missing metatype receiver") }
            return .host(HostRef(box: makeOpaqueBox(TitleBox("X"))), tid)
        }
        vm.registerHostMethod(type: "Foo", selector: "p()") { _, _ in .string("method") }
        vm.registerHostProperty(type: "Foo", name: "p", get: { _, _ in .string("prop") })
        do {
            try vm.run(p)
            XCTFail("Expected collision error")
        } catch let e as VMExecutionError {
            XCTAssertTrue(e.description.contains("collision"), e.description)
        } catch {
            XCTFail("Unexpected error: \(error)")
        }
    }
}
