//
//  ApiSurfaceJSONEncoder.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/19/25.
//

import Foundation

struct ApiSurfaceJSONEncoder {
    var prettyPrinted: Bool

    init(prettyPrinted: Bool = false) {
        self.prettyPrinted = prettyPrinted
    }

    func encode(_ surface: ApiSurface) throws -> Data {
        let dto = SurfaceDTO(surface: surface)
        let encoder = JSONEncoder()
        var formatting: JSONEncoder.OutputFormatting = [.sortedKeys]
        if prettyPrinted {
            formatting.insert(.prettyPrinted)
        }
        encoder.outputFormatting = formatting
        return try encoder.encode(dto)
    }
}

private struct SurfaceDTO: Codable {
    let types: [TypeDTO]

    init(surface: ApiSurface) {
        self.types = surface.types.map(TypeDTO.init)
    }
}

private struct TypeDTO: Codable {
    let availability: [String]
    let members: [MemberDTO]
    let name: String

    init(type: HostTypeSurface) {
        self.availability = type.availability
        self.members = type.members.map(MemberDTO.init)
        self.name = type.canonicalName
    }
}

private struct MemberDTO: Codable {
    let attributes: [String]
    let availability: [String]
    let baseName: String
    let genericParameters: [GenericParameterDTO]
    let isAsync: Bool
    let isFailable: Bool?
    let isMutating: Bool?
    let isThrowing: Bool
    let isSettable: Bool?
    let kind: String
    let parameters: [ParameterDTO]
    let propertyType: String?
    let returnType: String?
    let selector: String
    let whereClause: String?

    init(member: HostMember) {
        switch member {
        case .initializer(let info):
            attributes = info.signature.attributes
            availability = info.signature.availability
            baseName = info.signature.baseName
            genericParameters = info.signature.genericParameters.map(GenericParameterDTO.init)
            isAsync = info.signature.isAsync
            isFailable = info.isFailable
            isMutating = nil
            isThrowing = info.signature.isThrowing
            isSettable = nil
            kind = "initializer"
            parameters = info.signature.parameters.map(ParameterDTO.init)
            propertyType = nil
            returnType = info.signature.returnType?.canonicalDescription()
            selector = info.signature.selector
            whereClause = info.signature.whereClause
        case .instanceMethod(let info):
            attributes = info.signature.attributes
            availability = info.signature.availability
            baseName = info.signature.baseName
            genericParameters = info.signature.genericParameters.map(GenericParameterDTO.init)
            isAsync = info.signature.isAsync
            isFailable = nil
            isMutating = info.isMutating
            isThrowing = info.signature.isThrowing
            isSettable = nil
            kind = "instanceMethod"
            parameters = info.signature.parameters.map(ParameterDTO.init)
            propertyType = nil
            returnType = info.signature.returnType?.canonicalDescription()
            selector = info.signature.selector
            whereClause = info.signature.whereClause
        case .staticMethod(let info):
            attributes = info.signature.attributes
            availability = info.signature.availability
            baseName = info.signature.baseName
            genericParameters = info.signature.genericParameters.map(GenericParameterDTO.init)
            isAsync = info.signature.isAsync
            isFailable = nil
            isMutating = nil
            isThrowing = info.signature.isThrowing
            isSettable = nil
            kind = "staticMethod"
            parameters = info.signature.parameters.map(ParameterDTO.init)
            propertyType = nil
            returnType = info.signature.returnType?.canonicalDescription()
            selector = info.signature.selector
            whereClause = info.signature.whereClause
        case .instanceProperty(let info):
            attributes = info.attributes
            availability = info.availability
            baseName = info.name
            genericParameters = []
            isAsync = false
            isFailable = nil
            isMutating = nil
            isThrowing = false
            isSettable = info.isSettable
            kind = "instanceProperty"
            parameters = []
            propertyType = info.type.canonicalDescription()
            returnType = nil
            selector = info.name
            whereClause = nil
        case .staticProperty(let info):
            attributes = info.attributes
            availability = info.availability
            baseName = info.name
            genericParameters = []
            isAsync = false
            isFailable = nil
            isMutating = nil
            isThrowing = false
            isSettable = info.isSettable
            kind = "staticProperty"
            parameters = []
            propertyType = info.type.canonicalDescription()
            returnType = nil
            selector = info.name
            whereClause = nil
        }
    }
}

private struct ParameterDTO: Codable {
    let isInout: Bool
    let isVariadic: Bool
    let label: String?
    let name: String
    let type: String

    init(parameter: ResolvedParameter) {
        self.isInout = parameter.isInout
        self.isVariadic = parameter.isVariadic
        self.label = parameter.label
        self.name = parameter.name
        self.type = parameter.type.canonicalDescription()
    }
}

private struct GenericParameterDTO: Codable {
    let name: String
    let origin: String

    init(_ parameter: GenericParameter) {
        self.name = parameter.name
        self.origin = parameter.origin.rawValue
    }
}
