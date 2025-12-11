//
//  BytecodeBuilder.swift
//  InterpreterCompiler
//
//  Created by Forest Plasencia on 9/18/25.
//

import SwiftSyntax
#if canImport(SwiftParser)
import SwiftParser
#elseif canImport(SwiftSyntaxParser)
import SwiftSyntaxParser
#endif

import InterpreterModels

final class BytecodeBuilder {
    var ops: [OpCode] = []
    var locs: [SourceLoc?] = []
    var scopeNotes: [DebugSidecar.ScopeNote] = []
    var localsMaps: [DebugSidecar.LocalsMap] = []
    // F1: user method table built during codegen
    var userMethods: [Program.MethodKey: Program.MethodEntry] = [:]

    // Intern tables for types, selectors, and properties (id -> string)
    private(set) var typeTable: [String] = []
    private(set) var selectorTable: [String] = []
    private(set) var propertyTable: [String] = []
    // Pools for fast lookup (string -> id)
    private var typePool: [String: TypeID] = [:]
    private var selectorPool: [String: SelectorID] = [:]
    private var propertyPool: [String: PropertyID] = [:]

    private let fileName: String
    private let converter: SourceLocationConverter

    // Loop context stack to support break/continue patching
    struct LoopContext {
        let startIndex: Int            // index at loop head (condition evaluation)
        var breakSites: [Int] = []     // indices of pending break jump(0)
        var continueSites: [Int] = []  // indices of pending continue jump(0)
    }
    var loopStack: [LoopContext] = []

    init(fileName: String, converter: SourceLocationConverter) {
        self.fileName = fileName
        self.converter = converter
    }

    private func loc(for node: SyntaxProtocol?) -> SourceLoc? {
        guard let node else { return nil }
        let start = converter.location(for: node.positionAfterSkippingLeadingTrivia)
        let end   = converter.location(for: node.endPositionBeforeTrailingTrivia)
        return .init(file: start.file ?? fileName,
                     line: start.line ?? 0, column: start.column ?? 0,
                     endLine: end.line ?? (start.line ?? 0), endColumn: end.column ?? (start.column ?? 0))
    }

    @discardableResult
    func emit(_ op: OpCode, at node: SyntaxProtocol? = nil) -> Int {
        ops.append(op)
        locs.append(loc(for: node))
        return ops.count - 1
    }

    var count: Int { ops.count }

    func patch(_ index: Int, to op: OpCode) {
        ops[index] = op
    }

    // MARK: - Interning
    func internType(_ name: String) -> TypeID {
        if let id = typePool[name] { return id }
        let id = TypeID(raw: typeTable.count)
        typePool[name] = id
        typeTable.append(name)
        return id
    }
    func internSelector(_ sel: String) -> SelectorID {
        if let id = selectorPool[sel] { return id }
        let id = SelectorID(raw: selectorTable.count)
        selectorPool[sel] = id
        selectorTable.append(sel)
        return id
    }
    func internProperty(_ name: String) -> PropertyID {
        if let id = propertyPool[name] { return id }
        let id = PropertyID(raw: propertyTable.count)
        propertyPool[name] = id
        propertyTable.append(name)
        return id
    }

    // Record a user method entry (type and selector IDs are interned via this builder)
    func addUserMethod(typeName: String, selector: String, entry: Int, params: [String], localCount: Int, mutating: Bool) {
        let tid = internType(typeName)
        let sid = internSelector(selector)
        userMethods[Program.MethodKey(type: tid, selector: sid)] = Program.MethodEntry(entry: entry, params: params, localCount: localCount, mutating: mutating)
    }

    // MARK: - Loop helpers
    func pushLoop(startIndex: Int) {
        loopStack.append(LoopContext(startIndex: startIndex))
    }
    func popLoop() -> LoopContext? {
        return loopStack.popLast()
    }
    func recordBreakSite(_ index: Int) {
        guard !loopStack.isEmpty else { return }
        loopStack[loopStack.count - 1].breakSites.append(index)
    }
    func recordContinueSite(_ index: Int) {
        guard !loopStack.isEmpty else { return }
        loopStack[loopStack.count - 1].continueSites.append(index)
    }

    // MARK: - Sidecar helpers
    func addScopeNote(start: Int, end: Int, kind: String, name: String? = nil) {
        scopeNotes.append(.init(startIP: start, endIP: end, kind: kind, name: name))
    }
    func addLocalsMap(start: Int, end: Int, depth: Int, namesBySlot: [Int: String]) {
        localsMaps.append(.init(startIP: start, endIP: end, depth: depth, namesBySlot: namesBySlot))
    }
}
