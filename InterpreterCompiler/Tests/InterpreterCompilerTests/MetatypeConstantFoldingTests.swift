import XCTest
@testable import InterpreterCompiler
import InterpreterModels

final class MetatypeConstantFoldingTests: XCTestCase {

    func testIntStaticsFoldIntoConstants() throws {
        let source = """
        let maxValue = Int.max
        let minValue = Int.min
        let zeroValue = Int.zero
        let width = Int.bitWidth
        let signed = Int.isSigned
        """

        let compiler = Compiler()
        let program = try compiler.compileProgram(source: source, fileName: "IntStatics.swift")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.int(Int.max)) = $0 { return true }
            return false
        }, "Expected Int.max to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.int(Int.min)) = $0 { return true }
            return false
        }, "Expected Int.min to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.int(0)) = $0 { return true }
            return false
        }, "Expected Int.zero to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.int(Int.bitWidth)) = $0 { return true }
            return false
        }, "Expected Int.bitWidth to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.bool(Int.isSigned)) = $0 { return true }
            return false
        }, "Expected Int.isSigned to fold into a loadConst")

        XCTAssertFalse(program.ops.contains {
            if case .getProp = $0 { return true }
            return false
        }, "Metatype statics should not be lowered to property lookups when folding succeeds")

        XCTAssertFalse(program.propertyTable.contains("max"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("min"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("zero"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("bitWidth"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("isSigned"), "Folding should avoid interning property names for known statics")
    }

    func testDoubleStaticsFoldIntoConstants() throws {
        let source = """
        let max = Double.greatestFiniteMagnitude
        let normal = Double.leastNormalMagnitude
        let nonZero = Double.leastNonzeroMagnitude
        let infinite = Double.infinity
        let circle = Double.pi
        """

        let compiler = Compiler()
        let program = try compiler.compileProgram(source: source, fileName: "DoubleStatics.swift")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.double(Double.greatestFiniteMagnitude)) = $0 { return true }
            return false
        }, "Expected Double.greatestFiniteMagnitude to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.double(Double.leastNormalMagnitude)) = $0 { return true }
            return false
        }, "Expected Double.leastNormalMagnitude to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.double(Double.leastNonzeroMagnitude)) = $0 { return true }
            return false
        }, "Expected Double.leastNonzeroMagnitude to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.double(Double.infinity)) = $0 { return true }
            return false
        }, "Expected Double.infinity to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.double(Double.pi)) = $0 { return true }
            return false
        }, "Expected Double.pi to fold into a loadConst")

        XCTAssertFalse(program.ops.contains {
            if case .getProp = $0 { return true }
            return false
        }, "Metatype statics should not be lowered to property lookups when folding succeeds")

        XCTAssertFalse(program.propertyTable.contains("greatestFiniteMagnitude"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("leastNormalMagnitude"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("leastNonzeroMagnitude"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("infinity"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("pi"), "Folding should avoid interning property names for known statics")
    }

    func testUIntStaticsFoldIntoConstants() throws {
        let source = """
        let bigMax = UInt.max
        let bigMin = UInt.min
        let bigZero = UInt.zero
        let bigWidth = UInt.bitWidth
        let bigSigned = UInt.isSigned
        let byteMax = UInt8.max
        let byteMin = UInt8.min
        let wordMax = UInt16.max
        let dwordMax = UInt32.max
        let qwordMax = UInt64.max
        """

        let compiler = Compiler()
        let program = try compiler.compileProgram(source: source, fileName: "UIntStatics.swift")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.uint(UInt64(UInt.max))) = $0 { return true }
            return false
        }, "Expected UInt.max to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.uint(UInt64(UInt.min))) = $0 { return true }
            return false
        }, "Expected UInt.min to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.uint(0)) = $0 { return true }
            return false
        }, "Expected UInt.zero to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.int(UInt.bitWidth)) = $0 { return true }
            return false
        }, "Expected UInt.bitWidth to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.bool(false)) = $0 { return true }
            return false
        }, "Expected UInt.isSigned to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.uint(UInt64(UInt8.max))) = $0 { return true }
            return false
        }, "Expected UInt8.max to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.uint(UInt64(UInt8.min))) = $0 { return true }
            return false
        }, "Expected UInt8.min to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.uint(UInt64(UInt16.max))) = $0 { return true }
            return false
        }, "Expected UInt16.max to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.uint(UInt64(UInt32.max))) = $0 { return true }
            return false
        }, "Expected UInt32.max to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.uint(UInt64.max)) = $0 { return true }
            return false
        }, "Expected UInt64.max to fold into a loadConst")

        XCTAssertFalse(program.ops.contains {
            if case .getProp = $0 { return true }
            return false
        }, "Metatype statics should not be lowered to property lookups when folding succeeds")

        XCTAssertFalse(program.propertyTable.contains("max"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("min"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("zero"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("bitWidth"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("isSigned"), "Folding should avoid interning property names for known statics")
    }

    func testMetatypeStaticsFoldInsideNestedFunctions() throws {
        let source = """
        func outer() -> Int {
          func inner() -> Int {
            return Int.max
          }
          return inner() + Int.min
        }
        """

        let compiler = Compiler()
        let program = try compiler.compileProgram(source: source, fileName: "NestedFunctions.swift")

        let loadConsts = program.ops.filter {
            if case .loadConst(.int(Int.max)) = $0 { return true }
            if case .loadConst(.int(Int.min)) = $0 { return true }
            return false
        }
        XCTAssertEqual(loadConsts.count, 2, "Expected Int.max and Int.min to fold inside nested functions")

        XCTAssertFalse(program.ops.contains {
            if case .getProp = $0 { return true }
            return false
        }, "Metatype statics inside nested functions should not become property lookups")
    }

    func testMetatypeStaticsFoldInSlotMode() throws {
        let source = """
        var accumulator = UInt8.max
        accumulator = accumulator &+ UInt8.min
        let width = UInt.bitWidth
        """

        let compiler = Compiler(options: .init(mode: .slot))
        let program = try compiler.compileProgram(source: source, fileName: "SlotModeStatics.swift")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.uint(UInt64(UInt8.max))) = $0 { return true }
            return false
        }, "Expected UInt8.max to fold into a loadConst in slot mode")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.uint(UInt64(UInt8.min))) = $0 { return true }
            return false
        }, "Expected UInt8.min to fold into a loadConst in slot mode")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.int(UInt.bitWidth)) = $0 { return true }
            return false
        }, "Expected UInt.bitWidth to fold into a loadConst in slot mode")

        XCTAssertFalse(program.ops.contains {
            if case .getProp = $0 { return true }
            return false
        }, "Slot-mode compilation should not introduce property lookups for folded statics")
    }

    func testFloatStaticsFoldIntoConstants() throws {
        let source = """
        let max = Float.greatestFiniteMagnitude
        let normal = Float.leastNormalMagnitude
        let nonZero = Float.leastNonzeroMagnitude
        let infinite = Float.infinity
        let circle = Float.pi
        """

        let compiler = Compiler()
        let program = try compiler.compileProgram(source: source, fileName: "FloatStatics.swift")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.double(Double(Float.greatestFiniteMagnitude))) = $0 { return true }
            return false
        }, "Expected Float.greatestFiniteMagnitude to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.double(Double(Float.leastNormalMagnitude))) = $0 { return true }
            return false
        }, "Expected Float.leastNormalMagnitude to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.double(Double(Float.leastNonzeroMagnitude))) = $0 { return true }
            return false
        }, "Expected Float.leastNonzeroMagnitude to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.double(Double(Float.infinity))) = $0 { return true }
            return false
        }, "Expected Float.infinity to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.double(Double(Float.pi))) = $0 { return true }
            return false
        }, "Expected Float.pi to fold into a loadConst")

        XCTAssertFalse(program.ops.contains {
            if case .getProp = $0 { return true }
            return false
        }, "Metatype statics should not be lowered to property lookups when folding succeeds")

        XCTAssertFalse(program.propertyTable.contains("greatestFiniteMagnitude"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("leastNormalMagnitude"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("leastNonzeroMagnitude"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("infinity"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("pi"), "Folding should avoid interning property names for known statics")
    }

    func testFloat16StaticsFoldIntoConstants() throws {
        let source = """
        let max = Float16.greatestFiniteMagnitude
        let normal = Float16.leastNormalMagnitude
        let nonZero = Float16.leastNonzeroMagnitude
        let infinite = Float16.infinity
        let circle = Float16.pi
        """

        let compiler = Compiler()
        let program = try compiler.compileProgram(source: source, fileName: "Float16Statics.swift")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.double(Double(Float16.greatestFiniteMagnitude))) = $0 { return true }
            return false
        }, "Expected Float16.greatestFiniteMagnitude to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.double(Double(Float16.leastNormalMagnitude))) = $0 { return true }
            return false
        }, "Expected Float16.leastNormalMagnitude to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.double(Double(Float16.leastNonzeroMagnitude))) = $0 { return true }
            return false
        }, "Expected Float16.leastNonzeroMagnitude to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.double(Double(Float16.infinity))) = $0 { return true }
            return false
        }, "Expected Float16.infinity to fold into a loadConst")

        XCTAssertTrue(program.ops.contains {
            if case .loadConst(.double(Double(Float16.pi))) = $0 { return true }
            return false
        }, "Expected Float16.pi to fold into a loadConst")

        XCTAssertFalse(program.ops.contains {
            if case .getProp = $0 { return true }
            return false
        }, "Metatype statics should not be lowered to property lookups when folding succeeds")

        XCTAssertFalse(program.propertyTable.contains("greatestFiniteMagnitude"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("leastNormalMagnitude"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("leastNonzeroMagnitude"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("infinity"), "Folding should avoid interning property names for known statics")
        XCTAssertFalse(program.propertyTable.contains("pi"), "Folding should avoid interning property names for known statics")
    }
}
