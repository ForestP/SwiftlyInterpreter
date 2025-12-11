//
//  HostOperatorGeneratorTests.swift
//  HostCodegenCore
//
//  Ensures static operators with inout params (e.g., Date.-=) emit
//  mutation via vm.withHost on the inout argument rather than mutating
//  a local let binding.
//

import XCTest
@testable import HostSurfaceKit
@testable import HostCodegenCore

final class HostOperatorGeneratorTests: XCTestCase {

    private func type(_ text: String) -> TypeName { try! TypeNameParser.parse(text) }

    private func exportedTypeName(_ canonical: String) -> String {
        if let angleIndex = canonical.firstIndex(of: "<") {
            let base = String(canonical[..<angleIndex])
            let suffix = String(canonical[angleIndex...])
            return exportedTypeName(base) + suffix
        }
        if let dotIndex = canonical.lastIndex(of: ".") {
            let next = canonical.index(after: dotIndex)
            return String(canonical[next...])
        }
        return canonical
    }

    func testStaticOperatorInoutGeneratesWithHostMutation() throws {
        let descriptor = HostMethodDescriptor(
            receiver: type("Foundation.Date"),
            kind: .static,
            baseName: "-=",
            selector: "-=(lhs:rhs:)",
            displayName: "Date.-=(lhs:rhs:)",
            returnType: type("Swift.Void"),
            parameters: [
                HostMethodParameter(label: "lhs", name: "lhs", type: type("Foundation.Date"), isInout: true, isVariadic: false),
                HostMethodParameter(label: "rhs", name: "rhs", type: type("Foundation.TimeInterval"), isInout: false, isVariadic: false),
            ],
            genericParameters: [],
            requirements: [],
            conformanceTable: [:],
            throwsKind: .none,
            availability: []
        )
        let gen = HostMethodGenerator(method: descriptor,
                                      rewriteMap: [:],
                                      exportedTypeNameProvider: exportedTypeName,
                                      primitiveTypeAliases: [:],
                                      genericPlaceholderMap: [:])
        let render = try gen.render(allowedSpecializations: [:])
        XCTAssertEqual(render.thunks.count, 1)
        let src = render.thunks[0].source
        // Expect vm.withHost on args[1] (lhs) and an infix "lhs -= rhs" inside.
        XCTAssertTrue(src.contains("return try vm.withHost(args[1], typeName: \"Foundation.Date\", as: Foundation.Date.self) { lhs in"), src)
        XCTAssertTrue(src.contains("lhs -= rhs"), src)
        // Should not declare lhs as a let binding.
        XCTAssertFalse(src.contains("let lhs:"), src)
    }
}
