import XCTest
@testable import HostSurfaceKit

final class TypeNameParserPackExpansionTests: XCTestCase {

    func testParsesRepeatEachPackElementAsUnderlyingType() throws {
        let t = try TypeNameParser.parse("repeat each Foundation.Data")
        XCTAssertEqual(t.canonicalDescription(), "Foundation.Data")
    }

    func testParsesRepeatEachWithModuleQualified() throws {
        let t = try TypeNameParser.parse("repeat each Swift.String")
        XCTAssertEqual(t.canonicalDescription(), "Swift.String")
    }

    func testParsesBasicFunctionTypes() throws {
        let zero = try TypeNameParser.parse("(() -> Swift.Int)")
        XCTAssertTrue(zero.isFunctionType)
        XCTAssertEqual(zero.canonicalDescription(), "() -> Swift.Int")

        let one = try TypeNameParser.parse("(Swift.Int) -> Swift.String")
        XCTAssertTrue(one.isFunctionType)
        XCTAssertEqual(one.canonicalDescription(), "(Swift.Int) -> Swift.String")

        let throwing = try TypeNameParser.parse("(@Sendable (Element) async throws -> Swift.Bool)")
        XCTAssertTrue(throwing.isFunctionType)
        XCTAssertEqual(throwing.canonicalDescription(), "(Element) async throws -> Swift.Bool")
    }

    func testParsesTypedThrowsFunctions() throws {
        let typed = try TypeNameParser.parse("(Element) throws(E) -> Swift.Array<Element>")
        XCTAssertTrue(typed.isFunctionType)
        XCTAssertEqual(typed.functionSignature?.throwsKind, .throws)
        XCTAssertEqual(typed.canonicalDescription(), "(Element) throws -> Swift.Array<Element>")

        let nested = try TypeNameParser.parse("(@Sendable (Swift.Int) throws(MyModule.CustomError) -> Swift.Bool)")
        XCTAssertTrue(nested.isFunctionType)
        XCTAssertEqual(nested.functionSignature?.throwsKind, .throws)
        XCTAssertEqual(nested.canonicalDescription(), "(Swift.Int) throws -> Swift.Bool")
    }
}
