import XCTest
import Swiftly
import InterpreterCompiler
import InterpreterVM

final class FoundationInteropWasmTests: XCTestCase {

    private func run(_ src: String, file: String = "wasm.swift") throws -> String {
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: file)
        var vm = VM()
        let _ = foundationHostBridgeRegistration
        let _ = swiftStdlibHostBridgeRegistration
        try vm.prepare(p)
        try vm.run(p)
        return vm.output
    }

//    func testDataInitAndCount() throws {
//        let src = """
//        let d = Data()
//        print(d.count)
//        """
//        let out = try run(src)
//        XCTAssertEqual(out, "0\n")
//    }
    func testDateInitHasPositiveEpoch() throws {
        let src = """
        let t = Date()
        print(t.timeIntervalSince1970 > 0)
        """
        let out = try run(src)
        XCTAssertEqual(out.trimmingCharacters(in: .whitespacesAndNewlines), "true")
    }

    func testTimeIntervalRoundTripAcrossDateAndTimeZone() throws {
        let src = """
        let epoch = Date(timeIntervalSince1970: 1.5)
        print(epoch.timeIntervalSince1970 > 1)
        let delta = epoch.timeIntervalSince(Date(timeIntervalSince1970: 0.5))
        print(delta == 1.0)
        let gmt = TimeZone.gmt
        let offset = gmt.daylightSavingTimeOffset(for: Date(timeIntervalSince1970: 0))
        print(offset == 0.0)
        print(gmt.nextDaylightSavingTimeTransition(after: Date(timeIntervalSince1970: 0)) == nil)
        """
        let out = try run(src)
        XCTAssertEqual(out, "true\ntrue\ntrue\ntrue\n")
    }

//    func testDateDescriptionPropertyGetter() throws {
//        let src = """
//        let d = Date()
//        // Ensure property getter bridges as a property, not a zero-arg method
//        print(d.description.isEmpty == false)
//        """
//        let out = try run(src)
//        XCTAssertEqual(out.trimmingCharacters(in: .whitespacesAndNewlines), "true")
//    }

//    // TODO: Add back later
//    func testDataBase64InitOverloads() throws {
//        let src = """
//        let encodedString = "SGVsbG8h"
//        if let fromString = Data(base64Encoded: encodedString, options: []) {
//            print(fromString.count)
//            let base64Data = fromString.base64EncodedData(options: [])
//            if let fromData = Data(base64Encoded: base64Data, options: []) {
//                print(fromData.count)
//            }
//        }
//        """
//        let out = try run(src)
//        XCTAssertEqual(out, "6\n6\n")
//    }

    // TODO: Add back later when $0 is supported
//    func testArrayMapProducesMappedArray() throws {
//        let src = """
//        let xs = [1, 2, 3]
//        let ys = xs.map({ $0 + 1 })
//        print(ys.count)
//        """
//        let out = try run(src)
//        XCTAssertEqual(out, "3\n")
//    }
}
