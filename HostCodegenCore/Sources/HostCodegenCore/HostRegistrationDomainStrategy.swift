//
//  HostRegistrationDomainStrategy.swift
//  HostCodegenCore
//
//  Created by Codex on 5/1/24.
//

import HostSurfaceKit

/// Auto-specialization strategy that derives domains from a manifest of concrete
/// host types the app intends to register.
///
/// The manifest supplies canonical type names (e.g. "Demo.Box<Swift.String>").
/// We group by base receiver and harvest generic arguments, mapping them back
/// to the receiver's generic parameters for methods/properties.
public struct HostRegistrationDomainStrategy: AutoSpecializationStrategy {
    public let identifier: String = "registrations"

    private struct Entry { let arguments: [TypeName] }
    private let byBase: [String: [Entry]]

    public init(registeredTypeNames: [String]) {
        var grouped: [String: [Entry]] = [:]
        for name in registeredTypeNames {
            guard let parsed = try? TypeNameParser.parse(name), !parsed.genericArguments.isEmpty else { continue }
            let base = parsed.path.joined(separator: ".")
            var keys: Set<String> = [base]
            // Also index under module-qualified variant if missing a module on the parsed path
            // to tolerate inconsistent module qualification upstream; see roadmap E (cleanup).
            if let module = parsed.path.first, !module.contains(".") {
                // If the base already includes a module prefix (path.count>1), skip.
                // Otherwise there's no additional key to add.
            }
            for key in keys {
                grouped[key, default: []].append(Entry(arguments: parsed.genericArguments))
            }
        }
        self.byBase = grouped
    }

    public func methodDomains(for descriptor: HostMethodDescriptor,
                              surface: ApiSurface) -> [String: [TypeName]] {
        inferDomains(receiver: descriptor.receiver, params: descriptor.genericParameters)
    }

    public func propertyDomains(for descriptor: HostPropertyDescriptor,
                                surface: ApiSurface) -> [String: [TypeName]] {
        inferDomains(receiver: descriptor.receiver, params: descriptor.genericParameters)
    }

    private func inferDomains(receiver: TypeName, params: [GenericParameter]) -> [String: [TypeName]] {
        guard !params.isEmpty, !receiver.genericArguments.isEmpty else { return [:] }
        let parameterNames = Set(params.map(\.name))
        let base = receiver.path.joined(separator: ".")
        guard let entries = byBase[base], !entries.isEmpty else { return [:] }

        var results: [String: [TypeName]] = [:]
        var seen: [String: Set<String>] = [:]
        for entry in entries {
            guard entry.arguments.count == receiver.genericArguments.count else { continue }
            for (placeholder, concrete) in zip(receiver.genericArguments, entry.arguments) {
                guard placeholder.genericArguments.isEmpty, placeholder.path.count == 1 else { continue }
                let pname = placeholder.path[0]
                guard parameterNames.contains(pname) else { continue }
                let canon = concrete.canonicalDescription()
                var set = seen[pname] ?? Set<String>()
                if set.insert(canon).inserted {
                    results[pname, default: []].append(concrete)
                    seen[pname] = set
                }
            }
        }
        return results
    }
}
