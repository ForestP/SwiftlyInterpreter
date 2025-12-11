//
//  HostSurfacePipelineTests.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/19/25.
//

import XCTest
@testable import HostSurfaceKit

final class HostSurfacePipelineTests: XCTestCase {

    func testDumpWritesPrettyJSON() throws {
        let source = """
        public struct Foo {
            public init()
            public func latest() -> Foo
        }
        """

        let tempURL = FileManager.default.temporaryDirectory
            .appendingPathComponent(UUID().uuidString)
            .appendingPathExtension("json")

        defer { try? FileManager.default.removeItem(at: tempURL) }

        var pipeline = HostSurfacePipeline(
            moduleName: "Demo",
            interfaceContents: source,
            options: .init(
                dumpModelURL: tempURL,
                dumpPrettyPrint: true
            )
        )

        let result = try pipeline.run()
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertTrue(FileManager.default.fileExists(atPath: tempURL.path))

        let data = try Data(contentsOf: tempURL)
        let contents = try JSONSerialization.jsonObject(with: data) as? AnyHashable
        let expected: [String: AnyHashable] = [
            "types" : [
                [
                    "availability" : [] as [AnyHashable],
                    "members" : [
                        [
                            "attributes" : [] as [AnyHashable],
                            "availability" : [] as [AnyHashable],
                            "baseName" : "init",
                            "genericParameters" : [] as [AnyHashable],
                            "isAsync" : false,
                            "isFailable" : false,
                            "isThrowing" : false,
                            "kind" : "initializer",
                            "parameters" : [] as [AnyHashable],
                            "returnType" : "Demo.Foo",
                            "selector" : "init()"
                        ] as [String: AnyHashable],
                        [
                            "attributes" : [] as [AnyHashable],
                            "availability" : [] as [AnyHashable],
                            "baseName" : "latest",
                            "genericParameters" : [] as [AnyHashable],
                            "isAsync" : false,
                            "isMutating" : false,
                            "isThrowing" : false,
                            "kind" : "instanceMethod",
                            "parameters" : [] as [AnyHashable],
                            "returnType" : "Demo.Foo",
                            "selector" : "latest()"
                        ] as [String: AnyHashable]
                    ] as [AnyHashable],
                    "name": "Demo.Foo"
                ] as [String: AnyHashable]
            ]
        ]

        XCTAssertEqual(contents, expected)
    }

    func testPipelineCollectsMembersDeclaredInsideNominal() throws {
        let source = """
        public struct Foo {
            public func value() -> Swift.Int
            public var count: Swift.Int { get }
        }
        """

        var pipeline = HostSurfacePipeline(
            moduleName: "Demo",
            interfaceContents: source
        )

        let result = try pipeline.run()
        XCTAssertTrue(result.diagnostics.isEmpty)

        let type = try XCTUnwrap(result.apiSurface.types.first { $0.canonicalName == "Demo.Foo" })

        let methods = type.members.compactMap { member -> ResolvedInstanceMethod? in
            guard case .instanceMethod(let method) = member else { return nil }
            return method
        }
        XCTAssertEqual(methods.count, 1)
        XCTAssertEqual(methods[0].signature.selector, "value()")
        XCTAssertEqual(methods[0].signature.returnType?.canonicalDescription(), "Swift.Int")

        let properties = type.members.compactMap { member -> ResolvedProperty? in
            switch member {
            case .instanceProperty(let property):
                return property
            default:
                return nil
            }
        }
        XCTAssertEqual(properties.count, 1)
        XCTAssertEqual(properties[0].name, "count")
        XCTAssertEqual(properties[0].type.canonicalDescription(), "Swift.Int")
    }
}
