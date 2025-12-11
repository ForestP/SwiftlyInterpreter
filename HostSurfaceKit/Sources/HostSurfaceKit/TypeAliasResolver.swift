//
//  TypeAliasResolver.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/18/25.
//

/// Resolves typealias declarations (including config overrides) and produces
/// canonical, fully qualified type names.
struct TypeAliasResolver {
    struct ResolvedAliasRecord {
        let name: String
        let qualifiedName: String
        let target: TypeName
        let canonicalTarget: String
        let line: Int
    }
    private struct Entry {
        let decl: TypeAliasDecl
    }

    private var aliasMap: [String: Entry] = [:]
    private var simpleOwner: [String: String] = [:]
    private var overrides: [String: TypeName] = [:]
    private var cache: [String: TypeName] = [:]
    private var canonicalGenericParameters: [String: [String]]

    private(set) var diagnostics: [TypeAliasDiagnostic] = []

    init(aliases: [TypeAliasDecl],
         overrides: [String: String] = [:],
         diagnostics: [TypeAliasDiagnostic] = [],
         genericParameterInventory: [String: [String]] = [:]) throws {
        self.canonicalGenericParameters = genericParameterInventory
        self.diagnostics = diagnostics
        for decl in aliases {
            register(decl)
        }
        for (name, targetString) in overrides {
            let parsed = try TypeNameParser.parse(targetString)
            self.overrides[name] = parsed
            if let simple = name.split(separator: ".").last.map(String.init), self.overrides[simple] == nil {
                self.overrides[simple] = parsed
            }
        }
        registerGenericParametersFromAliases(aliases)
    }

    private mutating func register(_ decl: TypeAliasDecl) {
        aliasMap[decl.qualifiedName] = Entry(decl: decl)
        if simpleOwner[decl.name] == nil || simpleOwner[decl.name] == decl.moduleName {
            aliasMap[decl.name] = Entry(decl: decl)
            simpleOwner[decl.name] = decl.moduleName
        } else {
            diagnostics.append(.init(kind: .unsupported("ambiguous simple name '\(decl.name)'"), line: decl.line))
        }
    }

    mutating func canonicalName(for typeString: String,
                                protectedSimpleNames: Set<String> = []) throws -> String {
        let parsed = try TypeNameParser.parse(typeString)
        return try canonicalName(for: parsed, protectedSimpleNames: protectedSimpleNames)
    }

    mutating func canonicalName(for type: TypeName,
                                protectedSimpleNames: Set<String> = []) throws -> String {
        let resolved = try resolve(type,
                                   visited: [],
                                   protectedSimpleNames: protectedSimpleNames)
        return resolved.canonicalDescription()
    }

    mutating func resolvedAliasRecords() throws -> [ResolvedAliasRecord] {
        var seenQualified: Set<String> = []
        var records: [ResolvedAliasRecord] = []
        for entry in aliasMap.values {
            let decl = entry.decl
            guard seenQualified.insert(decl.qualifiedName).inserted else { continue }
            let canonical = try canonicalName(for: decl.target)
            let parsed = try TypeNameParser.parse(canonical)
            records.append(.init(name: decl.name,
                                 qualifiedName: decl.qualifiedName,
                                 target: parsed,
                                 canonicalTarget: canonical,
                                 line: decl.line))
        }
        records.sort { $0.qualifiedName < $1.qualifiedName }
        return records
    }

    func genericParameters(for canonicalName: String) -> [String]? {
        return canonicalGenericParameters[canonicalName]
    }

    var genericParameterInventorySnapshot: [String: [String]] {
        canonicalGenericParameters
    }

    private mutating func resolve(_ type: TypeName,
                                  visited: [String],
                                  protectedSimpleNames: Set<String> = []) throws -> TypeName {
        if let signature = type.functionSignature {
            let resolvedParams = try signature.parameters.map {
                try resolve($0, visited: visited, protectedSimpleNames: protectedSimpleNames)
            }
            let resolvedReturn = try resolve(signature.returnType,
                                             visited: visited,
                                             protectedSimpleNames: protectedSimpleNames)
            return TypeName.function(parameters: resolvedParams,
                                     returnType: resolvedReturn,
                                     isAsync: signature.isAsync,
                                     throwsKind: signature.throwsKind)
        }

        var combinedProtected = protectedSimpleNames
        if let placeholders = genericPlaceholders(for: type) {
            combinedProtected.formUnion(placeholders)
        }

        let resolvedGenerics = try type.genericArguments.map {
            try resolve($0, visited: visited, protectedSimpleNames: combinedProtected)
        }
        var path = type.path

        if path.count == 1,
           let name = path.first,
           combinedProtected.contains(name),
           PrimitiveTypeMetadata.canonicalPaths[name] == nil {
            return TypeName(path: path,
                            genericArguments: resolvedGenerics,
                            genericOwnerIndex: type.genericOwnerIndex)
        }

        let baseName = path.joined(separator: ".")
        if let override = overrides[baseName] ?? overrides[path.last ?? ""] {
            return try resolve(override, visited: visited, protectedSimpleNames: combinedProtected)
        }

        if let cached = cache[baseName] {
            return try resolve(cached, visited: visited, protectedSimpleNames: combinedProtected)
        }

        if let entry = aliasMap[baseName] {
            let qualifiedName = entry.decl.qualifiedName
            if visited.contains(qualifiedName) {
                throw TypeAliasResolverError.cycleDetected(visited + [qualifiedName])
            }
            var newVisited = visited
            newVisited.append(qualifiedName)
            let expanded = try resolve(entry.decl.target,
                                       visited: newVisited,
                                       protectedSimpleNames: combinedProtected)
            cache[baseName] = expanded
            return expanded
        }

        if path.count == 1, let canonical = PrimitiveTypeMetadata.canonicalPaths[path[0]] {
            path = canonical
        }

        let ownerIndex: Int? = {
            guard !resolvedGenerics.isEmpty else { return nil }
            if let originalOwner = type.genericOwnerIndex {
                if path.count == type.path.count {
                    return originalOwner
                }
                return max(path.count - 1, 0)
            }
            return max(path.count - 1, 0)
        }()

        return TypeName(path: path,
                        genericArguments: resolvedGenerics,
                        genericOwnerIndex: ownerIndex)
    }

    private mutating func registerGenericParametersFromAliases(_ aliases: [TypeAliasDecl]) {
        for decl in aliases {
            guard !decl.target.genericArguments.isEmpty else { continue }
            guard let placeholders = placeholderNames(from: decl.target.genericArguments) else { continue }
            do {
                let canonicalBase = try canonicalBaseName(for: decl.target)
                if canonicalGenericParameters[canonicalBase] == nil {
                    canonicalGenericParameters[canonicalBase] = placeholders
                }
            } catch {
                continue
            }
        }
    }

    private func placeholderNames(from arguments: [TypeName]) -> [String]? {
        var result: [String] = []
        for argument in arguments {
            guard argument.genericArguments.isEmpty,
                  argument.functionSignature == nil,
                  argument.path.count == 1 else { return nil }
            guard let name = argument.path.first else { return nil }
            if PrimitiveTypeMetadata.canonicalPaths[name] != nil {
                return nil
            }
            result.append(name)
        }
        return result
    }

    private mutating func canonicalBaseName(for type: TypeName) throws -> String {
        var baseOnly = type
        baseOnly.genericArguments = []
        baseOnly.genericOwnerIndex = nil
        let canonical = try resolve(baseOnly, visited: [])
        return canonical.canonicalDescription()
    }

    private func genericPlaceholders(for type: TypeName) -> [String]? {
        guard !type.genericArguments.isEmpty else { return nil }

        if let ownerKey = genericOwnerLookupKey(for: type),
           let placeholders = canonicalGenericParameters[ownerKey] {
            return placeholders
        }

        var lookupPath = type.path
        if lookupPath.count == 1, let canonical = PrimitiveTypeMetadata.canonicalPaths[lookupPath[0]] {
            lookupPath = canonical
        }

        let key = lookupPath.joined(separator: ".")
        return canonicalGenericParameters[key]
    }

    private func genericOwnerLookupKey(for type: TypeName) -> String? {
        guard let ownerIndex = type.genericOwnerIndex ?? defaultOwnerIndex(for: type) else {
            return nil
        }
        var ownerPath = Array(type.path.prefix(ownerIndex + 1))
        if ownerPath.count == 1, let canonical = PrimitiveTypeMetadata.canonicalPaths[ownerPath[0]] {
            ownerPath = canonical
        }
        guard !ownerPath.isEmpty else { return nil }
        return ownerPath.joined(separator: ".")
    }

    private func defaultOwnerIndex(for type: TypeName) -> Int? {
        guard !type.genericArguments.isEmpty else { return nil }
        return max(type.path.count - 1, 0)
    }
}

public enum CanonicalTypeNameHelper {
    /// Returns a canonicalized string representation for an allowlist entry.
    /// - Parameters:
    ///   - raw: The raw allowlist entry (e.g. "Optional<Wrapped>" or "Array<Element>").
    ///   - defaultModule: Optional module prefix to apply when the type is referenced
    ///     without one (e.g. "Demo" for project types).
    /// - Returns: A canonical string (e.g. "Swift.Optional<Wrapped>" or "Demo.Box<Element>").
    public static func canonicalizedAllowlistEntry(_ raw: String,
                                                   defaultModule: String? = nil) -> String? {
        guard let parsed = try? TypeNameParser.parse(raw) else {
            return nil
        }
        let canonicalized = canonicalize(type: parsed, defaultModule: defaultModule)
        return canonicalized.canonicalDescription()
    }

    /// Canonicalizes a parsed `TypeName`, optionally applying a default module prefix.
    public static func canonicalize(type: TypeName,
                                    defaultModule: String? = nil) -> TypeName {
        if let signature = type.functionSignature {
            let canonicalParams = signature.parameters.map {
                canonicalize(type: $0, defaultModule: defaultModule)
            }
            let canonicalReturn = canonicalize(type: signature.returnType,
                                               defaultModule: defaultModule)
            return TypeName.function(parameters: canonicalParams,
                                     returnType: canonicalReturn,
                                     isAsync: signature.isAsync,
                                     throwsKind: signature.throwsKind)
        }

        let originalPath = type.path
        let normalizedPath = canonicalPath(for: originalPath, defaultModule: defaultModule)
        let normalizedArguments = type.genericArguments.map {
            canonicalize(type: $0, defaultModule: defaultModule)
        }

        let ownerIndex: Int? = {
            guard let originalOwner = type.genericOwnerIndex ?? (!type.genericArguments.isEmpty ? max(originalPath.count - 1, 0) : nil) else {
                return normalizedArguments.isEmpty ? nil : max(normalizedPath.count - 1, 0)
            }
            if normalizedPath.count == originalPath.count {
                return originalOwner
            }
            // Path length changed (e.g. module prefix inserted). Anchor generics to the last component.
            return max(normalizedPath.count - 1, 0)
        }()

        return TypeName(path: normalizedPath,
                        genericArguments: normalizedArguments,
                        genericOwnerIndex: ownerIndex)
    }

    private static func canonicalPath(for path: [String],
                                      defaultModule: String?) -> [String] {
        guard !path.isEmpty else { return path }
        if path.count == 1 {
            let key = path[0]
            if let primitive = PrimitiveTypeMetadata.canonicalPaths[key] {
                return primitive
            }
            if let module = defaultModule {
                return [module, key]
            }
        }
        return path
    }
}

enum PrimitiveTypeMetadata {
    static let canonicalPaths: [String: [String]] = [
        "Int": ["Swift", "Int"],
        "Int8": ["Swift", "Int8"],
        "Int16": ["Swift", "Int16"],
        "Int32": ["Swift", "Int32"],
        "Int64": ["Swift", "Int64"],
        "UInt": ["Swift", "UInt"],
        "UInt8": ["Swift", "UInt8"],
        "UInt16": ["Swift", "UInt16"],
        "UInt32": ["Swift", "UInt32"],
        "UInt64": ["Swift", "UInt64"],
        "Float": ["Swift", "Float"],
        "Double": ["Swift", "Double"],
        "Bool": ["Swift", "Bool"],
        "String": ["Swift", "String"],
        "Character": ["Swift", "Character"],
        "Any": ["Swift", "Any"],
        "AnyObject": ["Swift", "AnyObject"],
        "Never": ["Swift", "Never"],
        "Error": ["Swift", "Error"],
        "Optional": ["Swift", "Optional"],
        "Array": ["Swift", "Array"],
        "Dictionary": ["Swift", "Dictionary"],
        "Void": ["Swift", "Void"]
    ]
}
