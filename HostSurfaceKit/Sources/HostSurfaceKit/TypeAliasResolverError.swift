//
//  TypeAliasResolverError.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/18/25.
//


enum TypeAliasResolverError: Error, CustomStringConvertible, Equatable {
    case cycleDetected([String])
    case parseError(String)

    var description: String {
        switch self {
        case .cycleDetected(let chain):
            return "Typealias cycle detected: \(chain.joined(separator: " -> "))"
        case .parseError(let message):
            return message
        }
    }
}
