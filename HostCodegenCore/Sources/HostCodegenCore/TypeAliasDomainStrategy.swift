//
//  TypeAliasDomainStrategy.swift
//  HostCodegenCore
//
//  Created by Codex on 5/1/24.
//

import HostSurfaceKit

/// Auto-specialization strategy that derives concrete domains from `public typealias`
/// declarations emitted in the .swiftinterface.
public struct TypeAliasDomainStrategy: AutoSpecializationStrategy {
    public let identifier = "alias"

    private struct AliasEntry {
        let arguments: [TypeName]
    }

    private let aliasesByBaseType: [String: [AliasEntry]]

    public init(typeAliases: [ResolvedTypeAlias]) {
        var grouped: [String: [AliasEntry]] = [:]
        for alias in typeAliases {
            guard let parsed = try? TypeNameParser.parse(alias.canonicalTarget) else {
                continue
            }
            guard !parsed.genericArguments.isEmpty else { continue }
            var keys: Set<String> = []
            let base = parsed.path.joined(separator: ".")
            keys.insert(base)

            if let module = alias.qualifiedName.split(separator: ".").first {
                let moduleString = String(module)
                if parsed.path.first.map({ $0 != moduleString }) ?? true {
                    let moduleBase = "\(moduleString).\(base)"
                    keys.insert(moduleBase)
                }
            }

            for key in keys {
                grouped[key, default: []].append(AliasEntry(arguments: parsed.genericArguments))
            }
        }
        aliasesByBaseType = grouped.mapValues { entries in
            // Deduplicate identical argument payloads while preserving source order.
            var seen: Set<String> = []
            var ordered: [AliasEntry] = []
            for entry in entries {
                let key = entry.arguments.map { $0.canonicalDescription() }.joined(separator: "|")
                if seen.insert(key).inserted {
                    ordered.append(entry)
                }
            }
            return ordered
        }
    }

    public func methodDomains(for descriptor: HostMethodDescriptor,
                              surface: ApiSurface) -> [String: [TypeName]] {
        inferDomains(receiver: descriptor.receiver,
                     parameters: descriptor.genericParameters)
    }

    public func propertyDomains(for descriptor: HostPropertyDescriptor,
                                surface: ApiSurface) -> [String: [TypeName]] {
        inferDomains(receiver: descriptor.receiver,
                     parameters: descriptor.genericParameters)
    }

    private func inferDomains(receiver: TypeName,
                              parameters: [GenericParameter]) -> [String: [TypeName]] {
        guard !parameters.isEmpty else { return [:] }
        let parameterNames = Set(parameters.map(\.name))
        let base = receiver.path.joined(separator: ".")
        guard let aliasEntries = aliasesByBaseType[base],
              !aliasEntries.isEmpty,
              !receiver.genericArguments.isEmpty else {
            return [:]
        }

        var results: [String: [TypeName]] = [:]
        var seen: [String: Set<String>] = [:]

        for entry in aliasEntries {
            guard entry.arguments.count == receiver.genericArguments.count else { continue }
            for (placeholder, concrete) in zip(receiver.genericArguments, entry.arguments) {
                guard placeholder.genericArguments.isEmpty,
                      placeholder.path.count == 1 else { continue }
                let paramName = placeholder.path[0]
                guard parameterNames.contains(paramName) else { continue }
                let canonical = concrete.canonicalDescription()
                var seenSet = seen[paramName] ?? Set<String>()
                if seenSet.insert(canonical).inserted {
                    results[paramName, default: []].append(concrete)
                    seen[paramName] = seenSet
                }
            }
        }

        return results
    }
}
