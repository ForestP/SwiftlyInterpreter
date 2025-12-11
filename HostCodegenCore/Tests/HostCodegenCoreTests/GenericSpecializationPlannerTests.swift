//
//  GenericSpecializationPlannerTests.swift
//  HostCodegenCore
//
//  Created by Forest Plasencia on 9/19/25.
//

import XCTest
@testable import HostSurfaceKit
@testable import HostCodegenCore

final class GenericSpecializationPlannerTests: XCTestCase {

    private func type(_ text: String) -> TypeName {
        // Force-unwrap in tests; the parser is validated elsewhere.
        return try! TypeNameParser.parse(text)
    }

    private func param(_ name: String, origin: GenericParameter.Origin = .method) -> GenericParameter {
        GenericParameter(name: name, origin: origin)
    }

    func testCartesianProductAcrossParameters() throws {
        let planner = GenericSpecializationPlanner(parameters: [param("Key"), param("Value")])
        let allowed: [String: [TypeName]] = [
            "Key": [type("Swift.String"), type("Swift.Int")],
            "Value": [type("Foundation.Data"), type("Swift.String")],
        ]

        let specializations = try planner.planSpecializations(from: allowed)
        let keys = specializations.map(\.canonicalKey)
        XCTAssertEqual(keys, [
            "Key=Swift.Int|Value=Foundation.Data",
            "Key=Swift.Int|Value=Swift.String",
            "Key=Swift.String|Value=Foundation.Data",
            "Key=Swift.String|Value=Swift.String",
        ])
    }

    func testSameTypeRequirementFiltersCandidates() throws {
        let planner = GenericSpecializationPlanner(
            parameters: [param("Element")],
            requirements: [.sameType(param: "Element", type("Swift.Int"))]
        )
        let allowed: [String: [TypeName]] = [
            "Element": [type("Swift.Int"), type("Swift.String")]
        ]

        let specializations = try planner.planSpecializations(from: allowed)
        XCTAssertEqual(specializations.map(\.canonicalKey), ["Element=Swift.Int"])
    }

    func testSameTypeParametersRequirement() throws {
        let planner = GenericSpecializationPlanner(
            parameters: [param("Success"), param("Failure")],
            requirements: [.sameTypeParameters(lhs: "Failure", rhs: "Success")]
        )
        let allowed: [String: [TypeName]] = [
            "Success": [type("Swift.String"), type("Swift.Int")],
            "Failure": [type("Swift.String"), type("Swift.Error")],
        ]

        let specializations = try planner.planSpecializations(from: allowed)
        XCTAssertEqual(specializations.map(\.canonicalKey), [
            "Failure=Swift.String|Success=Swift.String",
        ])
    }

    func testConformanceRequirementUsesLookupTable() throws {
        let planner = GenericSpecializationPlanner(
            parameters: [param("Value")],
            requirements: [.conformsTo(param: "Value", protocolName: "Swift.StringProtocol")],
            conformanceTable: [
                "Swift.String": ["Swift.StringProtocol", "Swift.Equatable"],
                "Foundation.Data": ["Swift.Sequence"],
            ]
        )
        let allowed: [String: [TypeName]] = [
            "Value": [type("Swift.String"), type("Foundation.Data")]
        ]

        let specializations = try planner.planSpecializations(from: allowed)
        XCTAssertEqual(specializations.map(\.canonicalKey), ["Value=Swift.String"])
    }

    func testSameTypeResolvedSelfElementRequirement() throws {
        let planner = GenericSpecializationPlanner(
            parameters: [param("Element")],
            requirements: [.sameTypeResolved(lhs: type("Self.Element"), rhs: type("Swift.String"))],
            placeholderMap: ["Swift.Array": ["Element"]],
            receiverTemplate: type("Swift.Array<Element>")
        )
        let allowed: [String: [TypeName]] = [
            "Element": [type("Swift.String"), type("Foundation.Data")]
        ]

        let specializations = try planner.planSpecializations(from: allowed)
        XCTAssertEqual(specializations.map(\.canonicalKey), ["Element=Swift.String"])
    }

    func testConformsResolvedSelfElementRequirement() throws {
        let planner = GenericSpecializationPlanner(
            parameters: [param("Element")],
            requirements: [.conformsResolved(type: type("Self.Element"), protocolName: "Swift.Sequence")],
            conformanceTable: [
                "Swift.String": ["Swift.Sequence"],
            ],
            placeholderMap: ["Swift.Array": ["Element"]],
            receiverTemplate: type("Swift.Array<Element>")
        )
        let allowed: [String: [TypeName]] = [
            "Element": [type("Swift.String"), type("Swift.Bool")]
        ]

        let specializations = try planner.planSpecializations(from: allowed)
        XCTAssertEqual(specializations.map(\.canonicalKey), ["Element=Swift.String"])
    }

    func testOptionalMapParameterOrdering() throws {
        let planner = GenericSpecializationPlanner(parameters: [
            param("U", origin: .method),
            param("Wrapped", origin: .receiver)
        ])
        let allowed: [String: [TypeName]] = [
            "U": [type("Swift.Int")],
            "Wrapped": [type("Swift.String")]
        ]

        let specializations = try planner.planSpecializations(from: allowed)
        XCTAssertEqual(specializations.map(\.canonicalKey), ["U=Swift.Int|Wrapped=Swift.String"])
    }

    func testMissingSpecializationsThrows() {
        let planner = GenericSpecializationPlanner(parameters: [param("T")])
        XCTAssertThrowsError(try planner.planSpecializations(from: [:])) { error in
            XCTAssertEqual(error as? GenericSpecializationPlannerError, .missingSpecializations(parameter: "T"))
        }
    }

    func testUnknownRequirementParameterThrows() {
        let planner = GenericSpecializationPlanner(
            parameters: [param("T")],
            requirements: [.sameType(param: "U", type("Swift.Int"))]
        )
        XCTAssertThrowsError(try planner.planSpecializations(from: ["T": [type("Swift.Int")]])) { error in
            XCTAssertEqual(error as? GenericSpecializationPlannerError, .requirementReferencesUnknownParameter(parameter: "U"))
        }
    }

    func testNoValidSpecializationsThrows() {
        let planner = GenericSpecializationPlanner(
            parameters: [param("Element")],
            requirements: [.sameType(param: "Element", type("Swift.Bool"))]
        )
        XCTAssertThrowsError(try planner.planSpecializations(from: ["Element": [type("Swift.Int")]])) { error in
            XCTAssertEqual(error as? GenericSpecializationPlannerError, .noValidSpecializations)
        }
    }
}
