//
//  TypeAliasParser.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/18/25.
//


struct TypeAliasParser {
    static func parse(moduleName: String, contents: String) -> TypeAliasParserResult {
        var decls: [TypeAliasDecl] = []
        var diagnostics: [TypeAliasDiagnostic] = []

        let lines = contents.split(omittingEmptySubsequences: false, whereSeparator: \.isNewline)
        for (idx, rawLine) in lines.enumerated() {
            let lineNumber = idx + 1
            let line = rawLine.trimmingCharacters(in: .whitespaces)
            guard line.hasPrefix("public typealias ") else { continue }

            let rest = line.dropFirst("public typealias ".count)
            guard let equalsIndex = rest.firstIndex(of: "=") else {
                diagnostics.append(.init(kind: .parseError("missing '=' in typealias"), line: lineNumber))
                continue
            }
            let namePart = rest[..<equalsIndex].trimmingCharacters(in: .whitespaces)
            var valuePart = rest[rest.index(after: equalsIndex)...].trimmingCharacters(in: .whitespaces)
            if let commentRange = valuePart.range(of: "//") {
                valuePart = valuePart[..<commentRange.lowerBound].trimmingCharacters(in: .whitespaces)
            }
            if valuePart.isEmpty {
                diagnostics.append(.init(kind: .parseError("missing target for typealias '\(namePart)'"), line: lineNumber))
                continue
            }

            if namePart.contains("<") || namePart.contains("(") {
                diagnostics.append(.init(kind: .unsupported("generic or complex typealias '\(namePart)'"), line: lineNumber))
                continue
            }
            if valuePart.contains(" where ") {
                diagnostics.append(.init(kind: .unsupported("typealias with constraints '\(namePart)'"), line: lineNumber))
                continue
            }

            do {
                let target = try TypeNameParser.parse(String(valuePart))
                let simpleName = String(namePart)
                let qualified = "\(moduleName).\(simpleName)"
                decls.append(.init(moduleName: moduleName,
                                   name: simpleName,
                                   qualifiedName: qualified,
                                   target: target,
                                   line: lineNumber))
            } catch {
                diagnostics.append(.init(kind: .parseError("\(error)"), line: lineNumber))
            }
        }

        return TypeAliasParserResult(aliases: decls, diagnostics: diagnostics)
    }
}
