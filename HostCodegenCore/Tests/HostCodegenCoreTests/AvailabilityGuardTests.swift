//
//  AvailabilityGuardTests.swift
//  HostCodegenCore
//

import XCTest
@testable import HostSurfaceKit
@testable import HostCodegenCore

final class AvailabilityGuardTests: XCTestCase {
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

    func testMethodDispatcherNestsAvailabilityGuard() throws {
        let descriptor = HostMethodDescriptor(
            receiver: type("Foundation.DateComponents"),
            kind: .instance(isMutating: false),
            baseName: "isRepeatedDay",
            selector: "isRepeatedDay",
            displayName: "Foundation.DateComponents.isRepeatedDay",
            returnType: type("Swift.Optional<Swift.Bool>"),
            parameters: [],
            genericParameters: [],
            requirements: [],
            conformanceTable: [:],
            throwsKind: .none,
            availability: ["@available(macOS 26.0, iOS 26.0, tvOS 26.0, watchOS 26.0, visionOS 26.0, *)"]
        )
        let gen = HostMethodGenerator(method: descriptor,
                                      rewriteMap: [:],
                                      exportedTypeNameProvider: exportedTypeName,
                                      primitiveTypeAliases: [:],
                                      genericPlaceholderMap: [:])
        let render = try gen.render(allowedSpecializations: [:])
        let disp = render.dispatcherSource
        XCTAssertTrue(disp.contains("#available("), disp)
        XCTAssertTrue(disp.contains("return try Host_"), disp)
        XCTAssertTrue(disp.contains("is not available on this platform"), disp)
    }

    func testPropertyDispatcherNestsAvailabilityGuard() throws {
        let descriptor = HostPropertyDescriptor(
            receiver: type("Foundation.Locale"),
            kind: .static,
            name: "preferredLocales",
            type: type("Swift.Array<Foundation.Locale>"),
            isSettable: false,
            genericParameters: [],
            requirements: [],
            conformanceTable: [:],
            availability: ["@available(macOS 26.0, iOS 26.0, tvOS 26.0, watchOS 26.0, visionOS 26.0, *)"]
        )
        let gen = HostPropertyGenerator(property: descriptor,
                                        rewriteMap: [:],
                                        exportedTypeNameProvider: exportedTypeName,
                                        primitiveTypeAliases: [:],
                                        genericPlaceholderMap: [:])
        let render = try gen.render()
        let disp = render.getter.dispatcherSource
        XCTAssertTrue(disp.contains("#available("), disp)
        XCTAssertTrue(disp.contains("Getter for"), disp)
        XCTAssertTrue(disp.contains("is not available on this platform"), disp)
    }
}
