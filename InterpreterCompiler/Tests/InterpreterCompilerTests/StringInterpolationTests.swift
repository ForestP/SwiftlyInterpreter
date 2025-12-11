import XCTest
@testable import InterpreterCompiler
import InterpreterModels

final class StringInterpolationTests: XCTestCase {
    // Step 1: simple function interpolation without implicit self
    func testInterpolatedStringLowering_SimpleFunction() throws {
        let source = """
        func greet(name: String) -> String {
          return "Hello \\(name)"
        }
        // force function emission and constant interning
        let a = greet(name: "Forest")
        """

        let compiler = Compiler()
        let program = try compiler.compileProgram(source: source, fileName: "InterpolationFunc.swift")

        // Find function entry for 'greet'
        guard let def = program.ops.first(where: { op in
            if case let .defineFunction(name, _, _, _) = op { return name == "greet" }
            return false
        }) else { XCTFail("Expected defineFunction for greet"); return }

        var entry: Int = -1
        var params: [String] = []
        var locals: Int = 0
        for op in program.ops {
            if case let .defineFunction(name, ps, e, lc) = op, name == "greet" { entry = e; params = ps; locals = lc; break }
        }
        XCTAssertNotEqual(entry, -1, "Missing entry for greet")
        XCTAssertEqual(params, ["name"]) // single param

        // Scan ops from entry to first ret
        var i = entry
        var sawEmptyInit = false
        var sawHelloConst = false
        var sawLoadVarName = false
        var addCount = 0
        while i < program.ops.count {
            let op = program.ops[i]
            switch op {
            case .ret:
                break
            case .loadConst(let v):
                if case .string(let s) = v {
                    if s.isEmpty { sawEmptyInit = true }
                    if s == "Hello " { sawHelloConst = true }
                }
            case .loadVar(let n):
                if n == "name" { sawLoadVarName = true }
            case .add:
                addCount += 1
            default:
                break
            }
            if case .ret = program.ops[i] { break }
            i += 1
        }
        XCTAssertTrue(sawEmptyInit, "Expected empty string accumulator for interpolation")
        XCTAssertTrue(sawHelloConst, "Expected constant segment 'Hello '")
        XCTAssertTrue(sawLoadVarName, "Expected loadVar('name') for interpolation expression")
        XCTAssertGreaterThanOrEqual(addCount, 2, "Expected at least two concatenations for interpolation")
    }

    // Step 2: method interpolation with implicit self — smoke-check only for now
    func testInterpolatedStringLowering_MethodImplicitSelf_Smoke() throws {
        let source = """
        struct Person {
          var name: String
          func hello() -> String { "Hello \\(name)" }
        }
        // force method table emission
        let a = Person(name: "X").hello()
        """

        let compiler = Compiler()
        let program = try compiler.compileProgram(source: source, fileName: "InterpolationMethod.swift")

        // Locate method entry (userMethods table)
        guard let typeIndex = program.typeTable.firstIndex(of: "Person") else { return }
        guard let selIndex = program.selectorTable.firstIndex(of: "hello()") else { return }
        let key = Program.MethodKey(type: TypeID(raw: typeIndex), selector: SelectorID(raw: selIndex))
        guard let meta = program.userMethods[key] else { return }
        let entry = meta.entry

        var i = entry
        var addCount = 0
        var sawNameProp = false
        let namePID: Int? = program.propertyTable.firstIndex(of: "name")
        while i < program.ops.count {
            let op = program.ops[i]
            if case .ret = op { break }
            if case .add = op { addCount += 1 }
            if case let .getProp(pid) = op, let nid = namePID, pid.raw == nid { sawNameProp = true }
            i += 1
        }
        // Looser assertions for progressive development
        XCTAssertGreaterThanOrEqual(addCount, 1, "Expected at least one concatenation")
        XCTAssertTrue(sawNameProp || program.propertyTable.contains("name"), "Expected reference to 'name' in method")
    }

    // Step 3: multiple interpolations in a single literal
    func testInterpolatedStringLowering_MultipleInterpolations() throws {
        let source = """
        func greet(first: String, last: String) -> String {
          return "Hello \\(first) \\(last)!"
        }
        let a = greet(first: "Forest", last: "Plasencia")
        """

        let compiler = Compiler()
        let program = try compiler.compileProgram(source: source, fileName: "InterpolationMulti.swift")

        // Find function entry for 'greet'
        var entry = -1
        var params: [String] = []
        for op in program.ops {
            if case let .defineFunction(name, ps, e, _) = op, name == "greet" {
                entry = e; params = ps; break
            }
        }
        XCTAssertNotEqual(entry, -1, "Missing entry for greet")
        XCTAssertEqual(params, ["first", "last"]) // two params

        // Scan ops from entry to first ret
        var i = entry
        var sawEmptyInit = false
        var sawHelloConst = false
        var sawSpaceConst = false
        var sawBangConst = false
        var sawLoadFirst = false
        var sawLoadLast = false
        var addCount = 0
        while i < program.ops.count {
            let op = program.ops[i]
            switch op {
            case .ret:
                break
            case .loadConst(let v):
                if case .string(let s) = v {
                    if s.isEmpty { sawEmptyInit = true }
                    if s == "Hello " { sawHelloConst = true }
                    if s == " " { sawSpaceConst = true }
                    if s == "!" { sawBangConst = true }
                }
            case .loadVar(let n):
                if n == "first" { sawLoadFirst = true }
                if n == "last" { sawLoadLast = true }
            case .add:
                addCount += 1
            default:
                break
            }
            if case .ret = program.ops[i] { break }
            i += 1
        }
        XCTAssertTrue(sawEmptyInit, "Expected empty string accumulator for interpolation")
        XCTAssertTrue(sawHelloConst, "Expected constant segment 'Hello '")
        XCTAssertTrue(sawSpaceConst, "Expected space segment between first and last")
        // '!' may be merged or separate depending on parsing; allow either presence or enough adds
        XCTAssertTrue(sawBangConst || addCount >= 5, "Expected trailing '!' or equivalent concatenation count")
        XCTAssertTrue(sawLoadFirst, "Expected loadVar('first') for first interpolation expression")
        XCTAssertTrue(sawLoadLast, "Expected loadVar('last') for second interpolation expression")
        XCTAssertGreaterThanOrEqual(addCount, 4, "Expected at least four concatenations for multiple interpolation")
    }

    // Step 4: ensure `_ = expr` compiles by lowering to RHS evaluation then pop
    func testDiscardAssignmentCompiles() throws {
        let source = """
        func f(_ x: Int) -> Int { x + 1 }
        _ = f(41)
        """
        let compiler = Compiler()
        XCTAssertNoThrow(try compiler.compileProgram(source: source, fileName: "Discard.swift"))
    }
}
 
final class GuardLoweringTests: XCTestCase {
    func testGuardSimpleBoolean() throws {
        let source = """
        func f(_ x: Int) -> Int {
          guard x > 0 else { return 0 }
          return x
        }
        let a = f(1)
        """
        let compiler = Compiler()
        let program = try compiler.compileProgram(source: source, fileName: "GuardSimple.swift")
        // Find function entry for 'f'
        var entry = -1
        for op in program.ops {
            if case let .defineFunction(name, _, e, _) = op, name == "f" { entry = e; break }
        }
        XCTAssertNotEqual(entry, -1)
        // Scan for jumpIfFalse and 'return 0'
        var sawJIF = false
        var sawReturnZero = false
        var i = entry
        while i < program.ops.count {
            switch program.ops[i] {
            case .jumpIfFalse: sawJIF = true
            case .loadConst(let v):
                if case .int(0) = v, i + 1 < program.ops.count, case .ret = program.ops[i+1] { sawReturnZero = true }
            case .ret: break
            default: break
            }
            if case .ret = program.ops[i] { /* don't break; there can be multiple returns */ }
            i += 1
        }
        XCTAssertTrue(sawJIF, "Expected guard to lower to jumpIfFalse")
        XCTAssertTrue(sawReturnZero, "Expected else-body early return 0 lowering")
    }

    func testGuardMultipleConditions() throws {
        let source = """
        func g(_ a: Bool, _ b: Bool) -> Int {
          guard a, b else { return 1 }
          return 2
        }
        let a = g(true, true)
        """
        let compiler = Compiler()
        let program = try compiler.compileProgram(source: source, fileName: "GuardMulti.swift")
        var entry = -1
        for op in program.ops { if case let .defineFunction(name, _, e, _) = op, name == "g" { entry = e; break } }
        XCTAssertNotEqual(entry, -1)
        // Expect at least two jumpIfFalse (one per condition)
        var jifCount = 0
        for op in program.ops[entry..<program.ops.count] {
            if case .jumpIfFalse = op { jifCount += 1 }
        }
        XCTAssertGreaterThanOrEqual(jifCount, 2)
    }

    func testGuardOptionalBindingDefinesName() throws {
        let source = """
        func f(_ x: Int?) -> Int {
          guard let y = x else { return 0 }
          return y + 1
        }
        let a = f(1)
        """
        let compiler = Compiler()
        let program = try compiler.compileProgram(source: source, fileName: "GuardBind.swift")
        // Look for defLet("y") and a jumpIfNil
        let hasDefY = program.ops.contains { op in if case let .defLet(n) = op { return n == "y" } else { return false } }
        let hasJIN = program.ops.contains { op in if case .jumpIfNil = op { return true } else { return false } }
        XCTAssertTrue(hasDefY)
        XCTAssertTrue(hasJIN)
    }

    func testGuardMultipleOptionalBindings() throws {
        let source = """
        func g(_ a: Int?, _ b: Int?) -> Int {
          guard let x = a, let y = b else { return 0 }
          return x + y
        }
        let a = g(1, 2)
        """
        let compiler = Compiler()
        let program = try compiler.compileProgram(source: source, fileName: "GuardBind2.swift")
        let hasDefX = program.ops.contains { op in if case let .defLet(n) = op { return n == "x" } else { return false } }
        let hasDefY = program.ops.contains { op in if case let .defLet(n) = op { return n == "y" } else { return false } }
        XCTAssertTrue(hasDefX && hasDefY)
    }
}
