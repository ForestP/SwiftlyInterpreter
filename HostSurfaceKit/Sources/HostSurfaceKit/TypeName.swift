//
//  TypeName.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/18/25.
//

import Foundation

/// Represents a Swift type in a lightweight, canonical-friendly form.
/// The type is a nominal reference plus optional generic arguments that are
/// themselves `TypeName` values. Collections and optionals are normalized to
/// their stdlib spellings (`Swift.Array`, `Swift.Dictionary`, `Swift.Optional`).
public struct TypeName: Equatable {
    public var path: [String]
    public var genericArguments: [TypeName]
    public var genericOwnerIndex: Int?
    
    public var functionSignature: FunctionSignature?

    public init(path: [String],
                genericArguments: [TypeName] = [],
                genericOwnerIndex: Int? = nil,
                functionSignature: FunctionSignature? = nil) {
        self.path = path
        self.genericArguments = genericArguments
        if let explicit = genericOwnerIndex {
            self.genericOwnerIndex = explicit
        } else if !genericArguments.isEmpty, !path.isEmpty {
            self.genericOwnerIndex = max(path.count - 1, 0)
        } else {
            self.genericOwnerIndex = nil
        }
        self.functionSignature = functionSignature
    }

    /// Returns the canonical string form, joining module/type components with
    /// dots and printing generic arguments recursively with no whitespace.
    public func canonicalDescription() -> String {
        if let fn = functionSignature {
            let paramList = fn.parameters
                .map { $0.canonicalDescription() }
                .joined(separator: ", ")
            var pieces: [String] = []
            pieces.append("(\(paramList))")
            if fn.isAsync { pieces.append("async") }
            switch fn.throwsKind {
            case .none:
                break
            case .throws:
                pieces.append("throws")
            case .rethrows:
                pieces.append("rethrows")
            }
            pieces.append("->")
            pieces.append(fn.returnType.canonicalDescription())
            return pieces.joined(separator: " ")
        }
        if path == ["Swift", "Tuple"] {
            let body = genericArguments.map { $0.canonicalDescription() }.joined(separator: ", ")
            return "(\(body))"
        }
        let ownerIndex = genericOwnerIndex ?? (genericArguments.isEmpty ? nil : max(path.count - 1, 0))
        var components: [String] = []
        for (idx, component) in path.enumerated() {
            if let ownerIndex, ownerIndex == idx, !genericArguments.isEmpty {
                let args = genericArguments.map { $0.canonicalDescription() }.joined(separator: ",")
                components.append("\(component)<\(args)>")
            } else {
                components.append(component)
            }
        }
        return components.joined(separator: ".")
    }
}

public extension TypeName {
    /// Convenience helpers for stdlib sugar types.
    static func optional(_ wrapped: TypeName) -> TypeName {
        TypeName(path: ["Swift", "Optional"], genericArguments: [wrapped])
    }

    static func array(_ element: TypeName) -> TypeName {
        TypeName(path: ["Swift", "Array"], genericArguments: [element])
    }

    static func dictionary(key: TypeName, value: TypeName) -> TypeName {
        TypeName(path: ["Swift", "Dictionary"], genericArguments: [key, value])
    }

    static func tuple(_ elements: [TypeName]) -> TypeName {
        TypeName(path: ["Swift", "Tuple"], genericArguments: elements)
    }

    /// Applies generic substitutions by replacing single-segment paths whose
    /// identifier matches a substitution key. Substitutions are propagated
    /// recursively into generic arguments.
    func applyingSubstitutions(_ substitutions: [String: TypeName]) -> TypeName {
        if path.count == 1, let replacement = substitutions[path[0]] {
            return replacement.applyingSubstitutions(substitutions)
        }
        if genericArguments.isEmpty { return self }
        let substitutedGenerics = genericArguments.map { $0.applyingSubstitutions(substitutions) }
        var signatureCopy = functionSignature
        if let fn = functionSignature {
            let substitutedParams = fn.parameters.map { $0.applyingSubstitutions(substitutions) }
            let substitutedReturn = fn.returnType.applyingSubstitutions(substitutions)
            signatureCopy = FunctionSignature(parameters: substitutedParams,
                                              returnType: substitutedReturn,
                                              isAsync: fn.isAsync,
                                              throwsKind: fn.throwsKind)
        }
        return TypeName(path: path,
                        genericArguments: substitutedGenerics,
                        genericOwnerIndex: genericOwnerIndex,
                        functionSignature: signatureCopy)
    }

    static func function(parameters: [TypeName],
                          returnType: TypeName,
                          isAsync: Bool,
                          throwsKind: FunctionSignature.ThrowsKind) -> TypeName {
        TypeName(path: [],
                 genericArguments: [],
                 genericOwnerIndex: nil,
                 functionSignature: .init(parameters: parameters,
                                          returnType: returnType,
                                          isAsync: isAsync,
                                          throwsKind: throwsKind))
    }

    init(tupleElements: [TypeName]) {
        self.init(path: ["Swift", "Tuple"], genericArguments: tupleElements)
    }
}

public extension TypeName {
    final class FunctionSignature: Equatable {
        public enum ThrowsKind: Equatable {
            case none
            case `throws`
            case `rethrows`
        }

        public var parameters: [TypeName]
        public var returnType: TypeName
        public var isAsync: Bool
        public var throwsKind: ThrowsKind

        public init(parameters: [TypeName],
                    returnType: TypeName,
                    isAsync: Bool,
                    throwsKind: ThrowsKind) {
            self.parameters = parameters
            self.returnType = returnType
            self.isAsync = isAsync
            self.throwsKind = throwsKind
        }

        public static func == (lhs: FunctionSignature, rhs: FunctionSignature) -> Bool {
            lhs.parameters == rhs.parameters &&
            lhs.returnType == rhs.returnType &&
            lhs.isAsync == rhs.isAsync &&
            lhs.throwsKind == rhs.throwsKind
        }
    }

    var isFunctionType: Bool { functionSignature != nil }
}

public extension TypeName {
    func resolvingPlaceholders(using inventory: [String: [String]],
                               substitutions: [String: TypeName]) -> TypeName {
        if let signature = functionSignature {
            let resolvedParams = signature.parameters.map {
                $0.resolvingPlaceholders(using: inventory, substitutions: substitutions)
            }
            let resolvedReturn = signature.returnType.resolvingPlaceholders(using: inventory,
                                                                            substitutions: substitutions)
            return TypeName.function(parameters: resolvedParams,
                                     returnType: resolvedReturn,
                                     isAsync: signature.isAsync,
                                     throwsKind: signature.throwsKind)
        }

        if path.count == 1,
           genericArguments.isEmpty,
           let name = path.first,
           let replacement = substitutions[name] {
            return replacement.resolvingPlaceholders(using: inventory,
                                                     substitutions: substitutions)
        }

        let resolvedArguments = genericArguments.map {
            $0.resolvingPlaceholders(using: inventory, substitutions: substitutions)
        }
        let resolved = TypeName(path: path,
                                genericArguments: resolvedArguments,
                                genericOwnerIndex: genericOwnerIndex)

        if let ownerIndex = genericOwnerIndex,
           ownerIndex < path.count {
            let ownerPath = Array(path.prefix(ownerIndex + 1))
            let ownerKey = ownerPath.joined(separator: ".")
            if let placeholders = inventory[ownerKey],
               let lastComponent = path.last,
               let placeholderIndex = placeholders.firstIndex(of: lastComponent),
               placeholderIndex < resolvedArguments.count {
                return resolvedArguments[placeholderIndex]
            }
        }

        return resolved
    }
}
