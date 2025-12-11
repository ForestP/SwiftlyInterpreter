//
//  TypeAliasDiagnostic.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/18/25.
//

struct TypeAliasDiagnostic: Equatable, CustomStringConvertible {
    enum Kind: Equatable {
        case parseError(String)
        case unsupported(String)
    }
    let kind: Kind
    let line: Int?

    var description: String {
        switch kind {
        case .parseError(let message):
            return "Parse error: \(message)"
        case .unsupported(let message):
            return "Unsupported: \(message)"
        }
    }
}
