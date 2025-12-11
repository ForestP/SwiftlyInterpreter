//
//  TypeNameParser.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/18/25.
//

import Foundation

public struct TypeNameParser {
    public static func parse(_ text: String) throws -> TypeName {
        let tokenizer = TypeTokenizer(text)
        var parser = TypeNameParser(tokenizer: tokenizer)
        return try parser.parseType()
    }

    private var tokenizer: TypeTokenizer
    private var lookahead: TypeToken

    private init(tokenizer: TypeTokenizer) {
        self.tokenizer = tokenizer
        self.lookahead = self.tokenizer.nextToken()
    }

    private mutating func advance() {
        lookahead = tokenizer.nextToken()
    }

    private mutating func consume(_ token: TypeToken) throws {
        guard lookahead == token else {
            throw TypeNameParserError.unexpectedToken(describe(lookahead))
        }
        advance()
    }

    private func describe(_ token: TypeToken) -> String {
        switch token {
        case .identifier(let name): return name
        case .dot: return "."
        case .comma: return ","
        case .lessThan: return "<"
        case .greaterThan: return ">"
        case .leftBracket: return "["
        case .rightBracket: return "]"
        case .leftParen: return "("
        case .rightParen: return ")"
        case .colon: return ":"
        case .question: return "?"
        case .ampersand: return "&"
        case .eof: return "EOF"
        case .arrow: return "->"
        }
    }

    private mutating func parseType() throws -> TypeName {
        var type = try parsePrimary()
        while lookahead == .question {
            advance()
            type = .optional(type)
        }
        return type
    }

    private mutating func parsePrimary() throws -> TypeName {
        // Skip leading attributes (e.g., @escaping, @Sendable) and ownership qualifiers (__owned, __shared,
        // borrowing, consuming) that can prefix a type in swiftinterface files.
        skipAttributesAndQualifiers()

        switch lookahead {
        case .leftBracket:
            advance()
            let first = try parseType()
            if lookahead == .colon {
                advance()
                let second = try parseType()
                try consume(.rightBracket)
                return .dictionary(key: first, value: second)
            } else {
                try consume(.rightBracket)
                return .array(first)
            }
        case .leftParen:
            advance()
            let parameters = try parseFunctionParameterList()
            try consume(.rightParen)

            let effects = try parseFunctionEffects()
            if lookahead == .arrow {
                advance()
                let returnType = try parseType()
                return .function(parameters: parameters,
                                  returnType: returnType,
                                  isAsync: effects.isAsync,
                                  throwsKind: effects.throwsKind)
            } else if effects.consumedEffectToken {
                throw TypeNameParserError.unexpectedToken(describe(lookahead))
            }
            if parameters.isEmpty { return TypeName(path: ["Swift", "Void"]) }
            if parameters.count == 1 { return parameters[0] }
            return TypeName(tupleElements: parameters)
        case .identifier("-"):
            // Bail out on closure/function arrows when seen as leading token (e.g., from malformed input)
            throw TypeNameParserError.unexpectedToken("-")
        case .identifier(let ident):
            var identifiers: [String] = []
            var currentIdent = ident
            advance()

            // Skip existential introducers (any/some)
            if isExistentialKeyword(currentIdent) {
                guard case .identifier(let next) = lookahead else {
                    throw TypeNameParserError.expectedIdentifier
                }
                currentIdent = next
                advance()
            }

            // Skip parameter pack introducers: `repeat each T` → parse T as the type
            if currentIdent == "repeat" {
                if case .identifier("each") = lookahead { advance() }
                // Parse the next primary as the actual type of the pack element
                return try parsePrimary()
            }

            // Bail out early on function/closure type spellings we don't support yet
            if currentIdent == "(" { throw TypeNameParserError.unexpectedToken("(") }
            identifiers.append(currentIdent)

            var genericOwnerIndex: Int? = nil
            var genericArguments: [TypeName] = []

            parsingLoop: while true {
                if lookahead == .lessThan {
                    advance()
                    var args: [TypeName] = []
                    if lookahead != .greaterThan {
                        args.append(try parseType())
                        while lookahead == .comma {
                            advance()
                            args.append(try parseType())
                        }
                    }
                    try consume(.greaterThan)
                    genericArguments = args
                    genericOwnerIndex = identifiers.count - 1
                    continue parsingLoop
                }
                if lookahead == .dot {
                    advance()
                    guard case .identifier(let part) = lookahead else {
                        throw TypeNameParserError.expectedIdentifier
                    }
                    advance()
                    if isExistentialKeyword(part) { continue parsingLoop }
                    identifiers.append(part)
                    continue parsingLoop
                }
                break parsingLoop
            }

            return TypeName(path: identifiers,
                            genericArguments: genericArguments,
                            genericOwnerIndex: genericOwnerIndex)
        case .eof:
            throw TypeNameParserError.unexpectedEnd
        default:
            throw TypeNameParserError.unexpectedToken(describe(lookahead))
        }
    }

    private func isExistentialKeyword(_ value: String) -> Bool {
        value == "any" || value == "some"
    }

    // MARK: - Attribute/Qualifier Skipping

    private mutating func skipAttributesAndQualifiers() {
        var progressed = true
        while progressed {
            progressed = false
            switch lookahead {
            case .identifier("@"):
                // Consume '@' and the attribute name (e.g., 'escaping', 'Sendable'),
                // and skip any parenthesized attribute arguments.
                advance()
                var attributeName: String?
                if case .identifier(let name) = lookahead {
                    attributeName = name
                    advance()
                }
                // Skip optional attribute argument list for known argument-bearing attributes.
                if let attr = attributeName,
                   attributeAcceptsArgumentList(attr),
                   lookahead == .leftParen {
                    skipParenthesizedBlock()
                }
                progressed = true
            case .identifier(let name) where isOwnershipQualifier(name):
                // Skip ownership/borrowing qualifiers that can precede a type
                advance()
                progressed = true
            default:
                break
            }
        }
    }

    private func attributeAcceptsArgumentList(_ name: String) -> Bool {
        switch name {
        case "available", "deprecated", "objc", "objcMembers", "convention", "selector", "private", "frozen":
            return true
        default:
            return false
        }
    }

    private mutating func skipParenthesizedBlock() {
        // Assumes current lookahead == .leftParen; skips to the matching ')'
        var depth = 0
        while true {
            switch lookahead {
            case .leftParen:
                depth += 1
            case .rightParen:
                depth -= 1
                if depth == 0 { advance(); return }
            case .eof:
                return
            default:
                break
            }
            advance()
        }
    }

    private func isOwnershipQualifier(_ value: String) -> Bool {
        switch value {
        case "__owned", "__shared", "borrowing", "consuming", "__consuming", "inout":
            return true
        default:
            return false
        }
    }

    // MARK: - Function parsing helpers

    private mutating func parseFunctionParameterList() throws -> [TypeName] {
        var parameters: [TypeName] = []
        var first = true
        while lookahead != .rightParen && lookahead != .eof {
            if !first {
                guard lookahead == .comma else { break }
                advance()
            }
            parameters.append(try parseFunctionParameterType())
            first = false
        }
        return parameters
    }

    private mutating func parseFunctionParameterType() throws -> TypeName {
        skipAttributesAndQualifiers()

        // Skip optional argument labels (e.g., `_`, `someLabel`) followed by ':'
        var consumedLabel = false
        repeat {
            consumedLabel = false
            if case .identifier = lookahead {
                let savedIndex = tokenizer.saveState()
                let savedLookahead = lookahead
                advance()
                if lookahead == .colon {
                    advance()
                    consumedLabel = true
                } else {
                    tokenizer.restoreState(savedIndex)
                    lookahead = savedLookahead
                }
            }
        } while consumedLabel

        skipAttributesAndQualifiers()
        return try parseType()
    }

    private mutating func parseFunctionEffects() throws -> (isAsync: Bool,
                                                            throwsKind: TypeName.FunctionSignature.ThrowsKind,
                                                            consumedEffectToken: Bool) {
        var isAsync = false
        var throwsKind: TypeName.FunctionSignature.ThrowsKind = .none
        var consumedEffect = false

        effectLoop: while true {
            switch lookahead {
            case .identifier(let ident):
                switch ident {
                case "async":
                    if isAsync { break effectLoop }
                    isAsync = true
                    consumedEffect = true
                    advance()
                case "throws":
                    if throwsKind != .none { break effectLoop }
                    throwsKind = .throws
                    consumedEffect = true
                    advance()
                    try consumeTypedThrowsClauseIfPresent()
                case "rethrows":
                    if throwsKind != .none { break effectLoop }
                    throwsKind = .rethrows
                    consumedEffect = true
                    advance()
                default:
                    break effectLoop
                }
            default:
                break effectLoop
            }
        }
        return (isAsync, throwsKind, consumedEffect)
    }

    private mutating func consumeTypedThrowsClauseIfPresent() throws {
        guard lookahead == .leftParen else { return }
        advance()
        var expectComma = false
        while lookahead != .rightParen && lookahead != .eof {
            if expectComma {
                try consume(.comma)
            }
            _ = try parseType()
            expectComma = true
        }
        try consume(.rightParen)
    }
}
