//
//  TypeTokenizer.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/18/25.
//

import Foundation

struct TypeTokenizer {
    private let scalars: [UnicodeScalar]
    private var index: Int = 0

    init(_ text: String) {
        self.scalars = Array(text.unicodeScalars)
    }

    mutating func nextToken() -> TypeToken {
        skipWhitespace()
        guard index < scalars.count else { return .eof }
        let scalar = scalars[index]
        switch scalar {
        case ".": index += 1; return .dot
        case ",": index += 1; return .comma
        case "<": index += 1; return .lessThan
        case ">": index += 1; return .greaterThan
        case "[": index += 1; return .leftBracket
        case "]": index += 1; return .rightBracket
        case "(": index += 1; return .leftParen
        case ")": index += 1; return .rightParen
        case ":": index += 1; return .colon
        case "?": index += 1; return .question
        case "&": index += 1; return .ampersand
        case "-":
            if index + 1 < scalars.count, scalars[index + 1] == ">" {
                index += 2
                return .arrow
            }
            index += 1
            return .identifier("-")
        default:
            if isIdentifierStart(scalar) {
                let start = index
                index += 1
                while index < scalars.count, isIdentifierContinuation(scalars[index]) {
                    index += 1
                }
                let slice = scalars[start..<index]
                let ident = String(String.UnicodeScalarView(slice))
                return .identifier(ident)
            }
            index += 1
            return .identifier(String(scalar))
        }
    }

    func saveState() -> Int {
        index
    }

    mutating func restoreState(_ state: Int) {
        index = state
    }

    mutating func peekToken() -> TypeToken {
        let saved = saveState()
        let token = nextToken()
        restoreState(saved)
        return token
    }

    private mutating func skipWhitespace() {
        while index < scalars.count,
              CharacterSet.whitespacesAndNewlines.contains(scalars[index]) {
            index += 1
        }
    }

    private func isIdentifierStart(_ scalar: UnicodeScalar) -> Bool {
        CharacterSet.letters.contains(scalar) || scalar == "_"
    }

    private func isIdentifierContinuation(_ scalar: UnicodeScalar) -> Bool {
        CharacterSet.alphanumerics.contains(scalar) || scalar == "_"
    }
}
