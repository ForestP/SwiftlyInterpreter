//
//  ApiSurfaceJSONDecoder.swift
//  HostSurfaceKit
//
//  Created for closure type inference testing
//

import Foundation

public struct ApiSurfaceJSONDecoder {
    public init() {}
    
    public func decode(_ data: Data) throws -> ApiSurface {
        let decoder = JSONDecoder()
        let dto = try decoder.decode(SurfaceDTO.self, from: data)
        return dto.toApiSurface()
    }
}

// Shared type name parsing function
private func parseTypeName(_ canonical: String) -> TypeName {
    guard !canonical.isEmpty else {
        return TypeName(path: [])
    }
    if let parsed = try? TypeNameParser.parse(canonical) {
        return parsed
    }
    let path = canonical.split(separator: ".").map(String.init)
    return TypeName(path: path)
}

private struct SurfaceDTO: Codable {
    let types: [TypeDTO]
    
    func toApiSurface() -> ApiSurface {
        return ApiSurface(types: types.map { $0.toHostTypeSurface() })
    }
}

private struct TypeDTO: Codable {
    let availability: [String]
    let members: [MemberDTO]
    let name: String
    
    func toHostTypeSurface() -> HostTypeSurface {
        return HostTypeSurface(
            canonicalName: name,
            availability: availability,
            members: members.map { $0.toHostMember() }
        )
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
    
    func toHostMember() -> HostMember {
        switch kind {
        case "initializer":
            let signature = ResolvedCallable(
                selector: selector,
                baseName: baseName,
                returnType: returnType.map { parseTypeName($0) },
                parameters: parameters.map { $0.toResolvedParameter() },
                genericParameters: genericParameters.map { $0.toGenericParameter() },
                whereClause: whereClause,
                availability: availability,
                attributes: attributes,
                throwsKind: isThrowing ? .throws : .none,
                isAsync: isAsync,
                sourceLine: 0
            )
            return .initializer(ResolvedInitializer(signature: signature, isFailable: isFailable ?? false))
            
        case "instanceMethod":
            let signature = ResolvedCallable(
                selector: selector,
                baseName: baseName,
                returnType: returnType.map { parseTypeName($0) },
                parameters: parameters.map { $0.toResolvedParameter() },
                genericParameters: genericParameters.map { $0.toGenericParameter() },
                whereClause: whereClause,
                availability: availability,
                attributes: attributes,
                throwsKind: isThrowing ? .throws : .none,
                isAsync: isAsync,
                sourceLine: 0
            )
            return .instanceMethod(ResolvedInstanceMethod(signature: signature, isMutating: isMutating ?? false))
            
        case "staticMethod":
            let signature = ResolvedCallable(
                selector: selector,
                baseName: baseName,
                returnType: returnType.map { parseTypeName($0) },
                parameters: parameters.map { $0.toResolvedParameter() },
                genericParameters: genericParameters.map { $0.toGenericParameter() },
                whereClause: whereClause,
                availability: availability,
                attributes: attributes,
                throwsKind: isThrowing ? .throws : .none,
                isAsync: isAsync,
                sourceLine: 0
            )
            return .staticMethod(ResolvedStaticMethod(signature: signature))
            
        case "instanceProperty":
            let property = ResolvedProperty(
                name: baseName,
                type: parseTypeName(propertyType ?? ""),
                isSettable: isSettable ?? false,
                availability: availability,
                attributes: attributes,
                sourceLine: 0
            )
            return .instanceProperty(property)
            
        case "staticProperty":
            let property = ResolvedProperty(
                name: baseName,
                type: parseTypeName(propertyType ?? ""),
                isSettable: isSettable ?? false,
                availability: availability,
                attributes: attributes,
                sourceLine: 0
            )
            return .staticProperty(property)
            
        default:
            fatalError("Unknown member kind: \(kind)")
        }
    }
}

private struct ParameterDTO: Codable {
    let isInout: Bool
    let isVariadic: Bool
    let label: String?
    let name: String
    let type: String
    
    func toResolvedParameter() -> ResolvedParameter {
        return ResolvedParameter(
            label: label,
            name: name,
            type: parseTypeName(type),
            isInout: isInout,
            isVariadic: isVariadic
        )
    }
}

private struct GenericParameterDTO: Codable {
    let name: String
    let origin: String
    
    func toGenericParameter() -> GenericParameter {
        let paramOrigin: GenericParameter.Origin
        switch origin {
        case "method": paramOrigin = .method
        case "receiver": paramOrigin = .receiver
        default: paramOrigin = .method
        }
        
        return GenericParameter(name: name, origin: paramOrigin)
    }
}
