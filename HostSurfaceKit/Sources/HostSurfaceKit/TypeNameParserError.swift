//
//  TypeNameParserError.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/18/25.
//


enum TypeNameParserError: Error, CustomStringConvertible {
    case unexpectedEnd
    case unexpectedToken(String)
    case expectedIdentifier

    var description: String {
        switch self {
        case .unexpectedEnd:
            return "unexpected end of type expression"
        case .unexpectedToken(let token):
            return "unexpected token '\(token)' in type expression"
        case .expectedIdentifier:
            return "expected identifier in type expression"
        }
    }
}
