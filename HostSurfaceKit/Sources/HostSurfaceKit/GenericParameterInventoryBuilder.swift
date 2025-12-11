//
//  GenericParameterInventoryBuilder.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 11/24/25.
//

import Foundation
import SwiftSyntax
#if canImport(SwiftParser)
import SwiftParser
#elseif canImport(SwiftSyntaxParser)
import SwiftSyntaxParser
#endif

struct GenericParameterInventoryBuilder {
    static func build(moduleName: String, contents: String) -> [String: [String]] {
        let tree: SourceFileSyntax
#if canImport(SwiftParser)
        tree = Parser.parse(source: contents)
#elseif canImport(SwiftSyntaxParser)
        guard let parsed = try? SyntaxParser.parse(source: contents) else {
            return [:]
        }
        tree = parsed
#else
        return [:]
#endif
        let visitor = GenericInventoryVisitor(moduleName: moduleName)
        visitor.walk(tree)
        return visitor.inventory
    }
}

private final class GenericInventoryVisitor: SyntaxVisitor {
    private struct TypeContext {
        let path: [String]
    }

    private let moduleName: String
    private var contextStack: [TypeContext] = []
    private(set) var inventory: [String: [String]] = [:]

    init(moduleName: String) {
        self.moduleName = moduleName
        super.init(viewMode: .sourceAccurate)
    }

    override func visit(_ node: StructDeclSyntax) -> SyntaxVisitorContinueKind {
        guard accessInfo(for: node.modifiers).isPublicOrOpen else { return .skipChildren }
        let parameters = node.genericParameterClause?.parameters.map { $0.name.text } ?? []
        pushType(named: node.identifier.text, parameters: parameters)
        return .visitChildren
    }

    override func visitPost(_ node: StructDeclSyntax) {
        popType()
    }

    override func visit(_ node: ClassDeclSyntax) -> SyntaxVisitorContinueKind {
        guard accessInfo(for: node.modifiers).isPublicOrOpen else { return .skipChildren }
        let parameters = node.genericParameterClause?.parameters.map { $0.name.text } ?? []
        pushType(named: node.identifier.text, parameters: parameters)
        return .visitChildren
    }

    override func visitPost(_ node: ClassDeclSyntax) {
        popType()
    }

    override func visit(_ node: EnumDeclSyntax) -> SyntaxVisitorContinueKind {
        guard accessInfo(for: node.modifiers).isPublicOrOpen else { return .skipChildren }
        let parameters = node.genericParameterClause?.parameters.map { $0.name.text } ?? []
        pushType(named: node.identifier.text, parameters: parameters)
        return .visitChildren
    }

    override func visitPost(_ node: EnumDeclSyntax) {
        popType()
    }

    override func visit(_ node: ActorDeclSyntax) -> SyntaxVisitorContinueKind {
        guard accessInfo(for: node.modifiers).isPublicOrOpen else { return .skipChildren }
        let parameters = node.genericParameterClause?.parameters.map { $0.name.text } ?? []
        pushType(named: node.identifier.text, parameters: parameters)
        return .visitChildren
    }

    override func visitPost(_ node: ActorDeclSyntax) {
        popType()
    }

    private func pushType(named name: String, parameters: [String]) {
        let parentPath = contextStack.last?.path ?? [moduleName]
        let fullPath = parentPath + [name]
        if !parameters.isEmpty {
            inventory[fullPath.joined(separator: ".")] = parameters
        }
        contextStack.append(TypeContext(path: fullPath))
    }

    private func popType() {
        if !contextStack.isEmpty {
            contextStack.removeLast()
        }
    }
}

private struct AccessInfo {
    var isPublicOrOpen: Bool = false
}

private func accessInfo(for modifiers: DeclModifierListSyntax?) -> AccessInfo {
    guard let modifiers else { return AccessInfo() }
    var info = AccessInfo()
    for modifier in modifiers {
        switch modifier.name.text {
        case "public", "open":
            info.isPublicOrOpen = true
        default:
            break
        }
    }
    return info
}
