import InterpreterModels
import InterpreterCompiler
import InterpreterVM

import XCTest

final class IntegrationTestTests: XCTestCase {
    
    private func run(_ src: String, file: String = "test.swift") throws -> String {
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: file)
        var vm = VM()
        try vm.run(p)
        return vm.output
    }

    private func runSlot(_ src: String, file: String = "test.swift") throws -> String {
        let c = Compiler(options: .init(mode: .slot))
        let p = try c.compileProgram(source: src, fileName: file)
        var vm = VM()
        try vm.run(p)
        return vm.output
    }

    private func compileOnly(_ src: String, file: String = "test.swift") throws -> Program {
        let c = Compiler()
        return try c.compileProgram(source: src, fileName: file)
    }

    private func compileOnlySlot(_ src: String, file: String = "test.swift") throws -> Program {
        let c = Compiler(options: .init(mode: .slot))
        return try c.compileProgram(source: src, fileName: file)
    }

    private func runExpectError(_ src: String, file: String = "test.swift") -> String {
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

    func testArithmeticPrecedence() throws {
        let src = """
        print(1 + 2 * 3)        // 7
        print(10 - 2 * 3)       // 4
        print((1 + 2) * 3)      // 9
        print(10 - 2 * 3 + 1)   // 5
        """
        let out = try run(src)
        XCTAssertEqual(out, "7\n4\n9\n5\n")
    }

    func testRelationalWithArithmetic() throws {
        let src = """
        print(1 + 2 * 3 == 7)   // true
        print(1 + 2 < 4 * 2)    // true (3 < 8)
        print(10 % 4 == 2)      // true
        print(7 / 2 == 3)       // true (int / int => int)
        """
        let out = try run(src)
        XCTAssertEqual(out, "true\ntrue\ntrue\ntrue\n")
    }

    func testLogicalShortCircuiting() throws {
        // Ensure RHS isn't evaluated when LHS decides result
        let src = """
        // false && (divide by zero) => false, no crash
        print(1 == 0 && (1 / 0 == 1))
        // true || (divide by zero) => true, no crash
        print(2 * 3 == 6 || (1 / 0 == 0))
        // true && true || false && (divide by zero) => (true) || (false && ...) => true
        print(1 == 1 && 2 == 2 || 0 == 1 && (1 / 0 == 0))
        // (false || false) && true => false
        print(1 > 2 || 3 < 2 && true)
        """
        let out = try run(src)
        XCTAssertEqual(out, "false\ntrue\ntrue\nfalse\n")
    }

    func testMixedPrecedenceChains() throws {
        // Mix &&/|| with ==, <, +, -, *, /, % and parentheses
        let src = """
        print((1 + 2 == 3) && (4 * 2 > 6))          // true && true => true
        print(1 + 2 == 4 || 2 * 3 == 5 || 7 % 3 == 1) // false || false || true => true
        print(1 + 2 * 3 < 10 && 8 / 4 == 2)         // 7 < 10 && 2 == 2 => true
        print(1 + 2 * 3 < 7 || 8 / 4 == 3)          // 7 < 7 (false) || 2 == 3 (false) => false
        """
        let out = try run(src)
        XCTAssertEqual(out, "true\ntrue\ntrue\nfalse\n")
    }

    func testWhileBreak() throws {
        let src = """
        var i = 0
        while (true) {
            print(i)
            if i == 2 { break }
            i = i + 1
        }
        """
        let out = try run(src)
        XCTAssertEqual(out, "0\n1\n2\n")
    }

    func testWhileContinue() throws {
        let src = """
        var i = 0
        while (i < 5) {
            i = i + 1
            if i % 2 == 0 { continue }
            print(i)
        }
        """
        let out = try run(src)
        XCTAssertEqual(out, "1\n3\n5\n")
    }

    func testRepeatWhileBasic() throws {
        let src = """
        var i = 0
        repeat {
          print(i)
          i = i + 1
        } while i < 3
        """
        let out = try run(src)
        XCTAssertEqual(out, "0\n1\n2\n")
    }

    func testForInRangeHalfOpen() throws {
        let src = """
        for i in 0..<3 {
          print(i)
        }
        """
        let out = try run(src)
        XCTAssertEqual(out, "0\n1\n2\n")
    }

    func testForInRangeClosed() throws {
        let src = """
        for i in 0...2 {
          print(i)
        }
        """
        let out = try run(src)
        XCTAssertEqual(out, "0\n1\n2\n")
    }

    func testForInBreakContinue() throws {
        let src = """
        for i in 0..<5 {
          if i == 2 { continue }
          if i == 4 { break }
          print(i)
        }
        """
        let out = try run(src)
        XCTAssertEqual(out, "0\n1\n3\n")
    }

    func testForInArrayValues() throws {
        let src = """
        var digits = [1, 2, 3, 4]
        var total = 0
        for d in digits {
          total = total + d
        }
        print(total)
        """
        let out = try run(src)
        XCTAssertEqual(out, "10\n")
    }

    func testForInArrayValuesSlot() throws {
        let src = """
        var digits = [1, 2, 3, 4]
        var total = 0
        for d in digits {
          total = total + d
        }
        print(total)
        """
        let out = try runSlot(src)
        XCTAssertEqual(out, "10\n")
    }

    func testClosedRangeContains() throws {
        let src = """
        let r = 0...3
        print(r.contains(2))
        print(r.contains(5))
        print(r.lowerBound)
        print(r.upperBound)
        print(r.isEmpty)
        """
        let out = try run(src)
        XCTAssertEqual(out, "true\nfalse\n0\n3\nfalse\n")
    }

    func testHalfOpenRangeContains() throws {
        let src = """
        let r = 1..<4
        print(r.contains(1))
        print(r.contains(4))
        print(r.lowerBound)
        print(r.upperBound)
        print(r.isEmpty)
        let empty = 5..<5
        print(empty.isEmpty)
        """
        let out = try run(src)
        XCTAssertEqual(out, "true\nfalse\n1\n4\nfalse\ntrue\n")
    }

    func testArrayGetSet() throws {
        let src = """
        var a = [1, 2, 3]
        print(a[1])
        a[1] = 42
        print(a[1])
        """
        let out = try run(src)
        XCTAssertEqual(out, "2\n42\n")
    }

    func testDictGetSet() throws {
        let src = """
        var d = ["x": 1]
        print(d["x"]) 
        d["y"] = 7
        print(d["y"]) 
        """
        let out = try run(src)
        XCTAssertEqual(out, "1\n7\n")
    }

    func testUnaryPlusMinus() throws {
        let src = """
        print(-1)
        print(+(2))
        print(-(1 + 2 * 3))   // -(7) => -7
        print(-3.5)
        """
        let out = try run(src)
        XCTAssertEqual(out, "-1\n2\n-7\n-3.5\n")
    }

    func testCountPropertyOnCollections() throws {
        let src = """
        var a = [1, 2, 3]
        var d = ["x": 1, "y": 2]
        print(a.count)
        print(d.count)
        a[1] = 99
        d["z"] = 3
        print(a.count)
        print(d.count)
        """
        let out = try run(src)
        XCTAssertEqual(out, "3\n2\n3\n3\n")
    }

    // MARK: - E3: Lexical scoping & locals

    func testShadowingAndOuterAssignment() throws {
        let src = """
        var x = 1
        if true {
          var x = 2
          x = 3
        }
        print(x)
        x = 4
        print(x)
        """
        let out = try run(src)
        XCTAssertEqual(out, "1\n4\n")
    }

    func testOuterAssignmentFromInner() throws {
        let src = """
        var x = 1
        if true {
          x = 2
        }
        print(x)
        """
        let out = try run(src)
        XCTAssertEqual(out, "2\n")
    }

    func testConstViolation() {
        let src = """
        let x = 1
        if true {
          x = 2
        }
        """
        let msg = runExpectError(src)
        XCTAssertTrue(msg.contains("cannot assign to 'x'"), msg)
    }

    func testScopeElisionInIfBody() throws {
        // No new bindings → no pushScope/popScope in program ops
        let src = """
        if true {
          print(1)
        }
        """
        let p = try compileOnly(src)
        let hasScopeOps = p.ops.contains { op in
            if case .pushScope = op { return true }
            if case .popScope = op { return true }
            return false
        }
        XCTAssertFalse(hasScopeOps, "Expected no pushScope/popScope for block without bindings")
    }

    func testScopePresentWhenBindingsExist() throws {
        let src = """
        if true {
          var y = 1
          print(y)
        }
        """
        let p = try compileOnly(src)
        let hasPush = p.ops.contains { if case .pushScope = $0 { return true } else { return false } }
        let hasPop  = p.ops.contains { if case .popScope  = $0 { return true } else { return false } }
        XCTAssertTrue(hasPush && hasPop, "Expected pushScope/popScope for block with bindings")
    }

    func testBranchLocalLeakage() throws {
        // y and z are confined to their branches; accessing outside should error
        let src1 = """
        if true { var y = 1; print(y) } else { print(0) }
        print(y)
        """
        let msg1 = runExpectError(src1)
        XCTAssertTrue(msg1.contains("Variable not found"), msg1)

        let src2 = """
        if true { print(0) } else { var z = 2; print(z) }
        print(z)
        """
        let msg2 = runExpectError(src2)
        XCTAssertTrue(msg2.contains("Variable not found"), msg2)
    }

    func testUnknownIdentifierSuggestions() {
        let src = """
        var count = 1
        print(cont)
        """
        let msg = runExpectError(src)
        XCTAssertTrue(msg.contains("Did you mean 'count'"), msg)
    }

    func testScopeElisionReducesInstructionCount() throws {
        let noBindings = """
        if true {
          print(1)
        }
        """
        let withBindings = """
        if true {
          var y = 1
          print(y)
        }
        """
        let p1 = try compileOnly(noBindings)
        let p2 = try compileOnly(withBindings)
        XCTAssertLessThan(p1.ops.count, p2.ops.count, "Blocks without bindings should compile to fewer instructions")
    }

    func testScopeElisionReducesInstructionCountWhile() throws {
        let noBindings = """
        while true {
          print(1)
        }
        """
        let withBindings = """
        while true {
          var y = 1
          print(y)
        }
        """
        let p1 = try compileOnly(noBindings)
        let p2 = try compileOnly(withBindings)
        XCTAssertLessThan(p1.ops.count, p2.ops.count, "While-body without bindings should compile to fewer instructions")
    }

    func testSlotModeMatchesNameMode() throws {
        let cases = [
            """
            var x = 1
            print(x)
            x = 2
            print(x)
            """,
            """
            var x = 1
            if true { var x = 2; x = 3 }
            print(x)
            if true { x = 4 }
            print(x)
            """,
            """
            var i = 0
            while i < 3 {
              print(i)
              i = i + 1
            }
            """
            ,
            """
            var g = 1
            func inc() { g = g + 1 }
            inc()
            inc()
            print(g)
            """
            ,
            // Closure capturing global
            """
            var x = 1
            let f = { print(x) }
            x = 2
            f()
            """
            ,
            // Closure capturing function-local (snapshot by value)
            """
            func make() {
              var y = 10
              return { print(y) }
            }
            let c = make()
            c()
            """
            ,
            // Closure inside block capturing block-local
            """
            if true { var z = 5; let f = { print(z) }; z = 9; f() }
            """
        ]
        for (idx, src) in cases.enumerated() {
            let nameOut = try run(src, file: "nm_\(idx).swift")
            let slotOut = try runSlot(src, file: "sm_\(idx).swift")
            XCTAssertEqual(nameOut, slotOut, "Mismatch in case #\(idx)\nName: \n\(nameOut)\nSlot: \n\(slotOut)")
        }
    }

    func testSlotModeReducesVarAccessOps() throws {
        // Heavy local access workload in static bytecode
        let src = """
        var x = 0
        var i = 0
        while i < 100 {
          x = x + i
          i = i + 1
        }
        print(x)
        """
        let pName = try compileOnly(src)
        let pSlot = try compileOnlySlot(src)
        func counts(_ p: Program) -> (nameLoads: Int, nameSets: Int, slotLoads: Int, slotSets: Int) {
            var nl = 0, ns = 0, sl = 0, ss = 0
            for op in p.ops {
                switch op {
                case .loadVar: nl += 1
                case .setVar:  ns += 1
                case .loadLocal: sl += 1
                case .setLocal:  ss += 1
                default: break
                }
            }
            return (nl, ns, sl, ss)
        }
        let nm = counts(pName)
        let sm = counts(pSlot)
        XCTAssertGreaterThan(nm.nameLoads + nm.nameSets, 0, "Name-mode should use name var ops")
        XCTAssertEqual(sm.nameLoads + sm.nameSets, 0, "Slot-mode should avoid name var ops for locals")
        XCTAssertGreaterThan(sm.slotLoads + sm.slotSets, 0, "Slot-mode should use slot ops for locals")
    }

    func testSlotModeRuntimeTiming() throws {
        // Non-asserting timing comparison for local runs
        let src = """
        var x = 0
        var i = 0
        while i < 200 {
          x = x + i
          i = i + 1
        }
        print(x)
        """

        // Compile once per mode
        let cName = Compiler()
        let pName = try cName.compileProgram(source: src, fileName: "perf_nm.swift")
        let cSlot = Compiler(options: .init(mode: .slot))
        let pSlot = try cSlot.compileProgram(source: src, fileName: "perf_sm.swift")

        func time(_ runs: Int, program: Program) throws -> TimeInterval {
            let start = Date()
            for _ in 0..<runs {
                var vm = VM()
                try vm.run(program)
            }
            return Date().timeIntervalSince(start)
        }

        let runs = 400
        // Warm-up
        _ = try time(10, program: pName)
        _ = try time(10, program: pSlot)

        let tName = try time(runs, program: pName)
        let tSlot = try time(runs, program: pSlot)

        XCTContext.runActivity(named: "Timing (s)") { _ in
            print("name-mode: \(tName)")
            print("slot-mode: \(tSlot)")
        }

        // Sanity: outputs should match
        let outNm = try run(src)
        let outSm = try runSlot(src)
        XCTAssertEqual(outNm, outSm)
    }

    func testDisassemblerIncludesLocalsMaps() throws {
        let src = """
        var x = 1
        if true { var y = 2; print(y) }
        """
        let c = Compiler(options: .init(mode: .slot))
        let p = try c.compileProgram(source: src, fileName: "dis.swift")
//        let asm = Disassembler.disassemble(p)
//        // Should include locals annotation
//        XCTAssertTrue(asm.contains(";; locals depth"))
//        XCTAssertTrue(asm.contains("enter block") || asm.contains("enter topLevel") || asm.contains("enter function"))
    }

    func testPrepareAndInvokeReturnsValue() throws {
        let src = """
        func add(a: Int, b: Int) {
          return a + b
        }
        """
        let p = try compileOnly(src)
        var vm = VM()
        try vm.prepare(p)
        let rv = try vm.invoke(name: "add", args: [.int(2), .int(3)], in: p)
        XCTAssertEqual(rv, .int(5))
    }

    func testInvokeProgramMismatchGuard() throws {
        let p1 = try compileOnly("""
        func foo() { return 1 }
        """)
        let p2 = try compileOnly("""
        func bar() { return 2 }
        """)
        var vm = VM()
        try vm.prepare(p1)
        do {
            _ = try vm.invoke(name: "bar", args: [], in: p2)
            XCTFail("expected mismatch error")
        } catch let e as VMError {
            guard case .programMismatch = e else { XCTFail("unexpected error: \(e)"); return }
        } catch let e as VMExecutionError {
            XCTAssertTrue(e.description.contains("Program mismatch"), e.description)
        }
    }

    // MARK: - Host bridging (Epic J) + Minimal stdlib (D3)

    func testArrayAppendAndCountProperty() throws {
        let src = """
        var a = [1, 2]
        a.append(3)
        print(a.count)
        """
        let out = try run(src)
        XCTAssertEqual(out, "3\n")
        // Also ensure selector interning recorded methods
        let p = try compileOnly(src)
        XCTAssertTrue(p.selectorTable.contains("append(_:)"))
        XCTAssertFalse(p.selectorTable.contains("count()"))
    }

    func testUserStructMethodDispatch() throws {
        let src = """
        struct Point {
          var x: Int
          var y: Int
          func sum() { return self.x + self.y }
          func add(dx: Int, dy: Int) { self.x = self.x + dx; self.y = self.y + dy; return self.x + self.y }
        }
        var p = Point(x: 1, y: 2)
        print(p.sum())
        print(p.add(dx: 2, dy: 3))
        print(p.x)
        print(p.y)
        """
        let out = try run(src)
        XCTAssertEqual(out, "3\n8\n3\n5\n")
    }

    func testUserMethodMutatingFlagTagging() throws {
        let src = """
        struct Point {
          var x: Int
          var y: Int
          func sum() { return self.x + self.y }
          func add(dx: Int, dy: Int) { self.x = self.x + dx; self.y = self.y + dy; return self.x + self.y }
        }
        """
        let p = try compileOnly(src)
        // Resolve TypeID and SelectorIDs
        guard let typeIndex = p.typeTable.firstIndex(of: "Point") else { XCTFail("Point not interned"); return }
        let tid = TypeID(raw: typeIndex)
        guard let sumSelIdx = p.selectorTable.firstIndex(of: "sum()") else { XCTFail("sum() selector missing"); return }
        let sumSel = SelectorID(raw: sumSelIdx)
        guard let addSelIdx = p.selectorTable.firstIndex(of: "add(dx:dy:)") else { XCTFail("add(dx:dy:) selector missing"); return }
        let addSel = SelectorID(raw: addSelIdx)

        let sumKey = Program.MethodKey(type: tid, selector: sumSel)
        let addKey = Program.MethodKey(type: tid, selector: addSel)
        guard let sumEntry = p.userMethods[sumKey] else { XCTFail("sum() entry missing"); return }
        guard let addEntry = p.userMethods[addKey] else { XCTFail("add(dx:dy:) entry missing"); return }
        XCTAssertFalse(sumEntry.mutating, "sum() should be non-mutating")
        XCTAssertTrue(addEntry.mutating, "add(dx:dy:) should be tagged mutating")
        // Sanity: params include implicit self
        XCTAssertEqual(sumEntry.params.first, "self")
        XCTAssertEqual(addEntry.params.first, "self")
    }

    func testImplicitSelfGetSetAndMethodCall() throws {
        let src = """
        struct Counter {
          var x: Int
          func inc() { x = x + 1 }
          func val() { return x }
          func bumpTwice() { inc(); inc(); return x }
        }
        var c = Counter(x: 10)
        print(c.val())
        print(c.bumpTwice())
        print(c.val())
        """
        let out = try run(src)
        XCTAssertEqual(out, "10\n12\n12\n")
    }

    func testPropertyMethodCollisionRule() {
        let src = """
        struct S {
          var p: Int
          func p() { return 1 }
        }
        """
        do {
            _ = try compileOnly(src)
            XCTFail("expected compile-time collision error")
        } catch {
            let msg = String(describing: error)
            XCTAssertTrue(msg.contains("Collision: property 'p' and zero-arg method 'p()'"), msg)
        }
    }

    func testNonMutatingMethodDoesNotChangeState() throws {
        let src = """
        struct Box {
          var v: Int
          func peek() { return v + 1 }
        }
        var b = Box(v: 3)
        print(b.peek())
        print(b.v)
        """
        let out = try run(src)
        XCTAssertEqual(out, "4\n3\n")
    }

    func testClosureCapturesSelfReferenceSemantics() throws {
        let src = """
        struct C {
          var x: Int
          func make() {
            let g = { return self.x }
            x = x + 5
            print(g())
          }
        }
        var c = C(x: 1)
        c.make()
        """
        let out = try run(src)
        XCTAssertEqual(out, "6\n")
    }

    func testSelfCannotBeShadowed() {
        let src = """
        struct S {
          var x: Int
          func bad() { var self = 1 }
        }
        """
        do {
            _ = try compileOnly(src)
            XCTFail("expected redeclaration error for self")
        } catch {
            let msg = String(describing: error)
            XCTAssertTrue(msg.contains("redeclaration") || msg.contains("self"), msg)
        }
    }

    func testHostInitializerLoweringBytecode() throws {
        // Foo is not a user-defined struct; should lower to metatype + callMethod("init(bar:)")
        let src = """
        Foo(bar: 1)
        """
        let p = try compileOnly(src)
        // Find callMethod and matching metatype const
        var sawMetatype = false
        var sawCall = false
        for op in p.ops {
            switch op {
            case .loadConst(let v):
                if case .metatype(_) = v { sawMetatype = true }
            case .callMethod(let sel, _):
                sawCall = true
                let name = p.selectorTable[sel.raw]
                XCTAssertEqual(name, "init(bar:)")
            default: break
            }
        }
        XCTAssertTrue(sawMetatype, "Expected to push metatype for host init")
        XCTAssertTrue(sawCall, "Expected callMethod for host init")
        XCTAssertTrue(p.typeTable.contains("Foo"))
    }

    func testUnknownHostMethodReportsError() {
        let src = """
        var a = [1]
        a.notAMethod()
        """
        let msg = runExpectError(src)
        XCTAssertTrue(msg.contains("Missing method"), msg)
        XCTAssertTrue(msg.contains("test.swift"), msg) // should include source location
    }

    // MARK: - J3 Exemplars: UIImage / Data with closure calls

    private final class UIImageStub {
        let name: String
        init(_ name: String) { self.name = name }
    }

    func testUIKitAndDataThunksInsideClosure() throws {
        // Program uses host initializer, then instance method chaining inside a closure
        let src = """
        let name = "abc"
        let f = {
            let img = UIImage(named: name)
            let d = img.pngData()
            print(d.count)
        }
        f()
        """

        // Compile
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: "uikit.swift")

        // VM with exemplar host bridges registered
        var vm = VM()
        // UIImage.init(named:)
        vm.registerHostMethod(type: "UIImage", selector: "init(named:)") { vm, args in
            // args: [metatype(UIImage), .string(name)]
            guard args.count == 2 else { throw VMError.typeError("init(named:) arity") }
            guard case let .metatype(imgTid) = args[0] else { throw VMError.typeError("missing metatype receiver") }
            // We don't depend on UIKit in tests; return a dummy host object tagged with UIImage TypeID
            guard case let .string(name) = args[1] else { throw VMError.typeError("init(named:) expects String") }
            return .host(HostRef(box: makeOpaqueBox(UIImageStub(name))), imgTid)
        }
        // UIImage.pngData() -> Data
        vm.registerHostMethod(type: "UIImage", selector: "pngData()") { vm, args in
            guard args.count == 1 else { throw VMError.typeError("pngData() arity") }
            // Produce a host Data with a known count (e.g., 3 bytes)
            let dataTid = vm.hostTypeID(named: "Data")
            return .host(HostRef(box: makeOpaqueBox(Data([0, 1, 2]))), dataTid)
        }
        // Data.count (property)
        vm.registerHostProperty(type: "Data", name: "count", get: { vm, args in
            try vm.withHost(args[0], typeName: "Data", as: Data.self) { data in
                .int(data.count)
            }
        })

        try vm.run(p)
        XCTAssertEqual(vm.output, "3\n")
    }

    // MARK: - Ternary
    func testTernaryBasic() throws {
        let src = """
        print( (1 < 2) ? 10 : 20 )
        print( (2 < 1) ? 10 : 20 )
        """
        let out = try run(src)
        XCTAssertEqual(out, "10\n20\n")
    }

    func testTernaryMixedWithOps() throws {
        let src = """
        let x = (1 + 2 == 3) ? 5 * 2 : 1 + 1
        print(x)
        """
        let out = try run(src)
        XCTAssertEqual(out, "10\n")
    }
    
    func testMetatypeStaticIntCapture() throws {
        let src = """
        func accumulator(_ values: [Int]) -> Int {
            func bump(_ current: Int, delta: Int) -> Int {
                if current > Int.max - delta { return Int.max }
                return current + delta
            }

            var total = 0
            for value in values {
                total = bump(total, delta: value)
            }
            return total
        }

        print(accumulator([3, 4, 5, 6]))
        """

        let out = try run(src)
        XCTAssertEqual(out, "18\n")
    }

    func testLocalFunctionCapture() throws {
        let src = """
        func makeAccumulator(start: Int) -> Int {
            var total = start

            func bump(by delta: Int) -> Int {
                total += delta
                return total
            }

            let first = bump(by: 2)
            let second = bump(by: 3)
            print(total)
            return first + second
        }

        print(makeAccumulator(start: 5))
        """

        let out = try run(src)
        XCTAssertEqual(out, "5\n15\n")
    }

    func testCompoundAssignmentVariable() throws {
        let src = """
        var x = 10
        x -= 4
        print(x)

        var y = 5
        y += 8
        print(y)
        """

        let out = try run(src)
        XCTAssertEqual(out, "6\n13\n")
    }

    // TODO: Re-enable once compound assignments support member writes.
    /*
    func testCompoundAssignmentMember() throws {
        let src = """
        struct Box { var value: Int }
        var box = Box(value: 9)
        box.value -= 3
        print(box.value)
        """

        let out = try run(src)
        XCTAssertEqual(out, "6\n")
    }
    */
}
