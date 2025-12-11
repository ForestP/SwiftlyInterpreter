//
//  CanonicalTypeNameHelperTests.swift
//  HostSurfaceKitTests
//
//  Created by Forest Plasencia on 12/03/25.
//

import XCTest
@testable import HostSurfaceKit

final class CanonicalTypeNameHelperTests: XCTestCase {

    func testCanonicalizesStdlibOptional() {
        let result = CanonicalTypeNameHelper.canonicalizedAllowlistEntry("Optional<Wrapped>")
        XCTAssertEqual(result, "Swift.Optional<Wrapped>")
    }

    func testCanonicalizesProjectTypeWithModule() {
        let result = CanonicalTypeNameHelper.canonicalizedAllowlistEntry("Box<Element>",
                                                                         defaultModule: "Demo")
        XCTAssertEqual(result, "Demo.Box<Element>")
    }

    func testCanonicalizesNestedGenerics() {
        let source = "Dictionary<String, Optional<Int>>"
        let result = CanonicalTypeNameHelper.canonicalizedAllowlistEntry(source)
        XCTAssertEqual(result, "Swift.Dictionary<Swift.String,Swift.Optional<Swift.Int>>")
    }
}
