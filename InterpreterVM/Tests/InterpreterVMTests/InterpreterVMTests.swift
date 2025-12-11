import XCTest
@testable import InterpreterVM
@testable import InterpreterModels

final class InterpreterVMTests: XCTestCase {
    func testModuleQualifiedHostTypeResolvesAlias() throws {
        var vm = VM()
        var called = false

        vm.registerHostTypeAlias(canonical: "Foundation.Date", exported: "Date")
        vm.registerHostMethods(type: "Foundation.Date", methods: [
            "init()": { vm, args in
                XCTAssertEqual(args.count, 1, "Date.init bridge arity")
                called = true
                return .bool(true)
            }
        ])

        let program = Program(
            ops: [
                .loadConst(.metatype(TypeID(raw: 0))),
                .callMethod(selector: SelectorID(raw: 0), argc: 1),
                .pop
            ],
            locs: [nil, nil, nil],
            source: "Date()",
            file: "alias.swift",
            debug: nil,
            typeTable: ["Date"],
            selectorTable: ["init()"],
            propertyTable: [],
            id: 0xDEADBEEF,
            userMethods: [:]
        )

        XCTAssertNoThrow(try vm.prepare(program))
        XCTAssertNoThrow(try vm.run(program))
        XCTAssertTrue(called, "Module-qualified host registration should be reachable via short type name")

        let shortID = vm.hostTypeID(named: "Date")
        let qualifiedID = vm.hostTypeID(named: "Foundation.Date")
        XCTAssertEqual(shortID, qualifiedID)
    }
}
