//
//  HostBridgeEmitter.swift
//  HostCodegenCore
//
//  Created by Forest Plasencia on 9/19/25.
//

import Foundation

public struct HostBridgeFile: Equatable {
    public let relativePath: String
    public let contents: String
}

struct HostBridgeEmitter {
    var filePrefix: String = "HostBridges"
    var exportedTypeNames: [String: String] = [:]
    var installerBasename: String = "GeneratedHostBridges"
    var autoRegisterInstaller: Bool = true

    struct TypeRegistration {
        let exportedTypeName: String
        var methodDispatch: [(selector: String, dispatcher: String)]
        var propertyDispatch: [PropertyEntry]

        struct PropertyEntry {
            let name: String
            let getter: String
            let setter: String?
        }
    }

    private struct SelectorDispatchEntry {
        let artifact: HostBridgeMethodArtifact
        let dispatcherName: String
    }

    func emitFiles(
        methodArtifacts: [HostBridgeMethodArtifact],
        propertyArtifacts: [HostBridgePropertyArtifact]
    ) -> [HostBridgeFile] {
        if methodArtifacts.isEmpty && propertyArtifacts.isEmpty {
            let installer = makeInstallerFile(typeMappings: [:])
            return [installer]
        }

        var grouped: [String: (methods: [HostBridgeMethodArtifact], properties: [HostBridgePropertyArtifact])] = [:]

        for artifact in methodArtifacts {
            // Prefer the normalized receiver type name used in emitted thunks
            let typeName = artifact.render.thunks.first?.receiverTypeName
                ?? artifact.descriptor.receiver.canonicalDescription()
            var entry = grouped[typeName] ?? (methods: [], properties: [])
            entry.methods.append(artifact)
            grouped[typeName] = entry
        }

        for artifact in propertyArtifacts {
            let typeName = artifact.render.getter.thunks.first?.receiverTypeName
                ?? artifact.descriptor.receiver.canonicalDescription()
            var entry = grouped[typeName] ?? (methods: [], properties: [])
            entry.properties.append(artifact)
            grouped[typeName] = entry
        }

        let sortedTypeNames = grouped.keys.sorted()
        var files: [HostBridgeFile] = []
        var installerMappings: [String: TypeRegistration] = [:]
        for typeName in sortedTypeNames {
            guard let entry = grouped[typeName] else { continue }
            let methodItems = entry.methods.sorted(by: { $0.descriptor.selector < $1.descriptor.selector })
            let propertyItems = entry.properties.sorted(by: { $0.descriptor.name < $1.descriptor.name })
            let context = makeTypeFile(typeName: typeName,
                                      methodArtifacts: methodItems,
                                      propertyArtifacts: propertyItems)
            files.append(context.file)
            for (canonical, registration) in context.registrations {
                installerMappings[canonical] = registration
            }
        }
        files.append(makeInstallerFile(typeMappings: installerMappings))
        return files
    }

    // MARK: - Type file

private func makeTypeFile(typeName: String,
                          methodArtifacts: [HostBridgeMethodArtifact],
                          propertyArtifacts: [HostBridgePropertyArtifact]) -> (file: HostBridgeFile,
                                                                                registrations: [String: TypeRegistration]) {
        let sanitizedTypeName = sanitizeTypeName(typeName)
        let relativePath = "\(filePrefix)/\(sanitizedTypeName)+Host.generated.swift"
        var lines = headerLines()
        lines.append(contentsOf: ["// MARK: - \(typeName)", ""])
        var methodMappings: [(selector: String, dispatcher: String)] = []
        var propertyMappings: [TypeRegistration.PropertyEntry] = []
        var selectorDispatchEntries: [String: [SelectorDispatchEntry]] = [:]
        var receiverVariants: [String: String] = [:]
        for artifact in methodArtifacts {
            for thunk in artifact.render.thunks {
                lines.append(thunk.source)
                lines.append("")
                receiverVariants[thunk.receiverTypeName] = thunk.runtimeTypeName
            }
            if let fallback = artifact.render.fallback {
                lines.append(fallback.source)
                lines.append("")
            }
            lines.append(artifact.render.dispatcherSource)
            lines.append("")
            let dispatcherToRegister: String
            if !artifact.descriptor.availability.isEmpty {
                let wrapperName = sanitizeTypeName("HostAvailWrap_\(artifact.render.dispatcherName)")
                let conditions = availabilityConditions(from: artifact.descriptor.availability)
                var wrapper: [String] = []
                wrapper.append("public func \(wrapperName)(vm: inout VM, _ args: [Value]) throws -> Value {")
                wrapper.append("    if \(conditions) { return try \(artifact.render.dispatcherName)(vm: &vm, args) }")
                wrapper.append("    throw VMError.unsupported(\"\(artifact.descriptor.displayName) is not available on this platform\")")
                wrapper.append("}")
                lines.append(wrapper.joined(separator: "\n"))
                lines.append("")
                dispatcherToRegister = wrapperName
            } else {
                dispatcherToRegister = artifact.render.dispatcherName
            }
            let entry = SelectorDispatchEntry(artifact: artifact, dispatcherName: dispatcherToRegister)
            selectorDispatchEntries[artifact.descriptor.selector, default: []].append(entry)
        }

        // Group properties by name to unify static + instance getters under a single dispatcher.
        var propsByName: [String: [HostBridgePropertyArtifact]] = [:]
        for artifact in propertyArtifacts { propsByName[artifact.descriptor.name, default: []].append(artifact) }

        for (propName, artifacts) in propsByName.sorted(by: { $0.key < $1.key }) {
            // Emit all getter thunks (now disambiguated by _instance/_static suffixes)
            if let first = artifacts.first,
               let receiverName = first.render.getter.thunks.first?.receiverTypeName,
               let runtimeReceiverName = first.render.getter.thunks.first?.runtimeTypeName {
                for art in artifacts {
                    for thunk in art.render.getter.thunks {
                        lines.append(thunk.source)
                        lines.append("")
                        receiverVariants[thunk.receiverTypeName] = thunk.runtimeTypeName
                    }
                    if let fallback = art.render.getter.fallback {
                        lines.append(fallback.source)
                        lines.append("")
                    }
                }
                // Unified getter dispatcher that accepts host or metatype receiver
                let dispatcherName = sanitizeTypeName("HostDispatcher_\(receiverName)_\(propName)")
                var disp: [String] = []
                // Propagate availability onto the dispatcher itself
                let propAvailAttrs = artifacts.first?.descriptor.availability ?? []
                for attr in propAvailAttrs { disp.append(attr) }
                disp.append("public func \(dispatcherName)(vm: inout VM, _ args: [Value]) throws -> Value {")
                var body: [String] = []
                body.append("guard !args.isEmpty else { throw VMError.typeError(\"missing receiver\") }")
                body.append("let tid: TypeID")
                body.append("switch args[0] {")
                body.append("case .host(_, let receiverTid): tid = receiverTid")
                body.append("case .metatype(let receiverTid): tid = receiverTid")
                body.append("default: throw VMError.typeError(\"receiver must be host or metatype\")")
                body.append("}")
                body.append("if tid == vm.hostTypeID(named: \"\(runtimeReceiverName)\") {")
                body.append("    switch args[0] {")
                // If both variants exist, call the right thunk based on case
                let hasInstance = artifacts.contains { $0.descriptor.kind == .instance }
                let hasStatic   = artifacts.contains { $0.descriptor.kind == .static }
                let cond = availabilityConditions(from: propAvailAttrs)
                if hasInstance {
                    let thunkName = sanitizeTypeName("Host_\(receiverName)_\(propName)_instance")
                    if propAvailAttrs.isEmpty {
                        body.append("    case .host: return try \(thunkName)(vm: &vm, args)")
                    } else {
                        body.append("    case .host:")
                        body.append("        if \(cond) { return try \(thunkName)(vm: &vm, args) }")
                        body.append("        else { throw VMError.unsupported(\"Getter for \(typeName).\(propName) is not available on this platform\") }")
                    }
                }
                if hasStatic {
                    let thunkName = sanitizeTypeName("Host_\(receiverName)_\(propName)_static")
                    if propAvailAttrs.isEmpty {
                        body.append("    case .metatype: return try \(thunkName)(vm: &vm, args)")
                    } else {
                        body.append("    case .metatype:")
                        body.append("        if \(cond) { return try \(thunkName)(vm: &vm, args) }")
                        body.append("        else { throw VMError.unsupported(\"Getter for \(typeName).\(propName) is not available on this platform\") }")
                    }
                }
                body.append("    default: break")
                body.append("    }")
                body.append("}")
                body.append("let receiverName = vm.debugTypeName(tid)")
                body.append("let supported = [\n    \"  - \(receiverName) ()\",\n] .joined(separator: \"\\n\")")
                body.append("throw VMError.typeError(\"Getter for \(receiverName).\(propName) has no specialization for receiver \\(receiverName). Supported specializations:\\(supported)\")")
                disp.append(contentsOf: indentLines(body, indent: 1))
                disp.append("}")
                lines.append(disp.joined(separator: "\n"))
                lines.append("")

                // Emit setters (if any) as-is; prefer instance setter if both present
                var setterName: String? = nil
                for art in artifacts {
                    if let setterRender = art.render.setter {
                        for thunk in setterRender.thunks {
                            lines.append(thunk.source)
                            lines.append("")
                            receiverVariants[thunk.receiverTypeName] = thunk.runtimeTypeName
                        }
                        if let fallback = setterRender.fallback {
                            lines.append(fallback.source)
                            lines.append("")
                        }
                        lines.append(setterRender.dispatcherSource)
                        lines.append("")
                        setterName = setterRender.dispatcherName
                    }
                }
                // If any availability is present on this property surface, wrap getter/setter
                let anyAvailability = artifacts.contains { !$0.descriptor.availability.isEmpty }
                let getterToRegister: String
                var setterToRegister: String? = nil
                if anyAvailability {
                    let cond = availabilityConditions(from: artifacts.first?.descriptor.availability ?? [])
                    let getterWrap = sanitizeTypeName("HostAvailWrap_\(dispatcherName)")
                    var wrapG: [String] = []
                    wrapG.append("public func \(getterWrap)(vm: inout VM, _ args: [Value]) throws -> Value {")
                    wrapG.append("    if \(cond) { return try \(dispatcherName)(vm: &vm, args) }")
                    wrapG.append("    throw VMError.unsupported(\"Getter for \(typeName).\(propName) is not available on this platform\")")
                    wrapG.append("}")
                    lines.append(wrapG.joined(separator: "\n"))
                    lines.append("")
                    getterToRegister = getterWrap
                    if let s = setterName {
                        let setterWrap = sanitizeTypeName("HostAvailWrap_\(s)")
                        var wrapS: [String] = []
                        wrapS.append("public func \(setterWrap)(vm: inout VM, _ args: [Value]) throws -> Value {")
                        wrapS.append("    if \(cond) { return try \(s)(vm: &vm, args) }")
                        wrapS.append("    throw VMError.unsupported(\"Setter for \(typeName).\(propName) is not available on this platform\")")
                        wrapS.append("}")
                        lines.append(wrapS.joined(separator: "\n"))
                        lines.append("")
                        setterToRegister = setterWrap
                    }
                } else {
                    getterToRegister = dispatcherName
                    setterToRegister = setterName
                }
                propertyMappings.append(.init(name: propName,
                                               getter: getterToRegister,
                                               setter: setterToRegister))
            }
        }
        for selector in selectorDispatchEntries.keys.sorted() {
            guard let entries = selectorDispatchEntries[selector] else { continue }
            if entries.count == 1 {
                methodMappings.append((selector, entries[0].dispatcherName))
            } else {
                let overloadDispatcher = makeOverloadDispatcher(typeName: typeName,
                                                                selector: selector,
                                                                entries: entries)
                lines.append(overloadDispatcher.source)
                lines.append("")
                methodMappings.append((selector, overloadDispatcher.name))
            }
        }
        let contents = lines.joined(separator: "\n") + "\n"
        if receiverVariants[typeName] == nil {
            receiverVariants[typeName] = exportedTypeName(for: typeName)
        }

        var registrations: [String: TypeRegistration] = [:]
        for (canonical, exported) in receiverVariants {
            registrations[canonical] = TypeRegistration(exportedTypeName: exported,
                                                        methodDispatch: methodMappings,
                                                        propertyDispatch: propertyMappings)
        }
        return (HostBridgeFile(relativePath: relativePath, contents: contents),
                registrations)
    }

    // MARK: - Installer file

    private func makeInstallerFile(typeMappings: [String: TypeRegistration]) -> HostBridgeFile {
        let rawBase = sanitizeTypeName(installerBasename.isEmpty ? "GeneratedHostBridges" : installerBasename)
        let sanitizedBase: String
        if let first = rawBase.first, first.isNumber {
            sanitizedBase = "_" + rawBase
        } else {
            sanitizedBase = rawBase
        }
        let relativePath = "\(filePrefix)/\(sanitizedBase).generated.swift"

        let registrationBase: String
        if sanitizedBase.count > 1, sanitizedBase.hasSuffix("s") {
            registrationBase = String(sanitizedBase.dropLast())
        } else {
            registrationBase = sanitizedBase
        }
        let installerFunctionName = "install\(sanitizedBase)"
        let registrationSymbolBase = lowerCamelized(registrationBase)
        let privateRegistrationName = "_\(registrationSymbolBase)Registration"
        let publicRegistrationName = "\(registrationSymbolBase)Registration"

        var lines = headerLines()
        lines.append("public func \(installerFunctionName)(into vm: inout VM) {")
        if typeMappings.isEmpty {
            lines.append("    // No generated host bridges")
        } else {
            for typeName in typeMappings.keys.sorted() {
                guard let entry = typeMappings[typeName] else { continue }
                let exportedName = entry.exportedTypeName
                if exportedName != typeName {
                    lines.append("    vm.registerHostTypeAlias(canonical: \"\(typeName)\", exported: \"\(exportedName)\")")
                }
                if !entry.methodDispatch.isEmpty {
                    lines.append("    vm.registerHostMethods(type: \"\(typeName)\", methods: [")
                    for (selector, dispatcher) in entry.methodDispatch {
                        lines.append("        \"\(selector)\": \(dispatcher),")
                    }
                    lines.append("    ])")
                }
                for property in entry.propertyDispatch {
                    if let setter = property.setter {
                        lines.append("    vm.registerHostProperty(type: \"\(typeName)\", name: \"\(property.name)\", get: \(property.getter), set: \(setter))")
                    } else {
                        lines.append("    vm.registerHostProperty(type: \"\(typeName)\", name: \"\(property.name)\", get: \(property.getter))")
                    }
                }
            }
        }
        lines.append("}")
        lines.append("")
        if autoRegisterInstaller {
            lines.append("private let \(privateRegistrationName): Void = {")
            lines.append("    VM.registerHostBridgeInstaller(\(installerFunctionName)(into:))")
            lines.append("    return ()")
            lines.append("}()")
            lines.append("")
            lines.append("public let \(publicRegistrationName): Void = {")
            lines.append("    \(privateRegistrationName)")
            lines.append("    return ()")
            lines.append("}()")
        } else {
            lines.append("public let \(publicRegistrationName): Void = {")
            lines.append("    VM.registerHostBridgeInstaller(\(installerFunctionName)(into:))")
            lines.append("    return ()")
            lines.append("}()")
        }
        let contents = lines.joined(separator: "\n") + "\n"
        return HostBridgeFile(relativePath: relativePath, contents: contents)
    }

    // MARK: - Helpers

    private func headerLines() -> [String] {
        [
            "// Generated by HostBridgeEmitter. Do not edit.",
            "import Foundation",
            "import InterpreterModels",
            "import InterpreterVM",
            ""
        ]
    }

    private func sanitizeTypeName(_ typeName: String) -> String {
        guard !typeName.isEmpty else { return "" }
        let allowed = CharacterSet.alphanumerics.union(CharacterSet(charactersIn: "_"))
        var result = ""
        for scalar in typeName.unicodeScalars {
            if allowed.contains(scalar) {
                result.append(Character(scalar))
            } else {
                let code = String(format: "%04X", scalar.value)
                result.append("_u\(code)_")
            }
        }
        if let first = result.first, first.isNumber {
            result = "_" + result
        }
        return result
    }

    private func lowerCamelized(_ identifier: String) -> String {
        guard !identifier.isEmpty else { return identifier }
        let parts = identifier.split(separator: "_").map(String.init)
        guard parts.count > 1 else {
            return identifier.prefix(1).lowercased() + identifier.dropFirst()
        }
        let first = parts.first!.lowercased()
        let rest = parts.dropFirst().map { $0.prefix(1).uppercased() + $0.dropFirst() }
        return ([first] + rest).joined()
    }

    // Keep helper here to mirror HostInteropGenerator’s indentation utility
    private func indentLines(_ lines: [String], indent: Int) -> [String] {
        guard indent > 0 else { return lines }
        let prefix = String(repeating: "    ", count: indent)
        return lines.map { prefix + $0 }
    }

    private func escapeForSwiftLiteral(_ text: String) -> String {
        text
            .replacingOccurrences(of: "\\", with: "\\\\")
            .replacingOccurrences(of: "\"", with: "\\\"")
    }

    // Convert collected @available attributes to a strict #available(...) condition usable in an if statement.
    // We parse all attributes and take the maximum version per platform to avoid under‑guarding newer APIs.
    private func availabilityConditions(from attributes: [String]) -> String {
        let cond = strictAvailabilityCondition(from: attributes)
        return cond ?? "true"
    }

    // MARK: - Availability helpers
    private func strictAvailabilityCondition(from attributes: [String]) -> String? {
        if attributes.isEmpty { return nil }
        var maxVersions: [String: String] = [:]
        let platforms = ["macOS", "iOS", "tvOS", "watchOS", "visionOS"]
        for attr in attributes {
            guard let open = attr.firstIndex(of: "("), let close = attr.lastIndex(of: ")"), open < close else { continue }
            let inside = String(attr[attr.index(after: open)..<close])
            let parts = inside.split(separator: ",")
            for raw in parts {
                let token = raw.trimmingCharacters(in: .whitespaces)
                for plat in platforms {
                    if token.hasPrefix(plat + " ") {
                        let rest = token.dropFirst((plat + " ").count)
                        let ver = rest.split(whereSeparator: { $0 == " " || $0 == ":" }).first.map(String.init)
                        if let ver, Double(ver.filter({ $0 == "." || ($0 >= "0" && $0 <= "9") })) != nil {
                            if let existing = maxVersions[plat] {
                                if compareVersion(ver, existing) == .orderedDescending { maxVersions[plat] = ver }
                            } else {
                                maxVersions[plat] = ver
                            }
                        }
                    }
                }
            }
        }
        if maxVersions.isEmpty { return nil }
        var clauses: [String] = []
        for plat in ["macOS","iOS","tvOS","watchOS","visionOS"] {
            if let ver = maxVersions[plat] { clauses.append("\(plat) \(ver)") }
        }
        clauses.append("*")
        return "#available(\(clauses.joined(separator: ", ")))"
    }

    private func compareVersion(_ a: String, _ b: String) -> ComparisonResult {
        func split(_ s: String) -> [Int] { s.split(separator: ".").compactMap { Int($0) } }
        let va = split(a), vb = split(b)
        let n = max(va.count, vb.count)
        for i in 0..<n {
            let ai = i < va.count ? va[i] : 0
            let bi = i < vb.count ? vb[i] : 0
            if ai != bi { return ai < bi ? .orderedAscending : .orderedDescending }
        }
        return .orderedSame
    }

    private func exportedTypeName(for canonical: String) -> String {
        if let override = exportedTypeNames[canonical] { return override }
        if let angle = canonical.firstIndex(of: "<") {
            let base = String(canonical[..<angle])
            let suffix = canonical[angle...]
            let exportedBase = exportedTypeName(for: base)
            return exportedBase + suffix
        }
        if let dot = canonical.lastIndex(of: ".") {
            let next = canonical.index(after: dot)
            return String(canonical[next...])
        }
        return canonical
    }

    private func makeOverloadDispatcher(typeName: String,
                                        selector: String,
                                        entries: [SelectorDispatchEntry]) -> (name: String, source: String) {
        precondition(!entries.isEmpty, "overload dispatcher requires entries")
        let baseName = sanitizeTypeName("HostOverloadDispatcher_\(typeName)_\(selector)")
        var lines: [String] = []
        lines.append("public func \(baseName)(vm: inout VM, _ args: [Value]) throws -> Value {")
        lines.append("    var lastTypeError: VMError? = nil")
        for entry in entries {
            lines.append("    do {")
            lines.append("        return try \(entry.dispatcherName)(vm: &vm, args)")
            lines.append("    } catch let error as VMError {")
            lines.append("        if case .typeError = error {")
            lines.append("            lastTypeError = error")
            lines.append("        } else {")
            lines.append("            throw error")
            lines.append("        }")
            lines.append("    } catch {")
            lines.append("        throw error")
            lines.append("    }")
        }
        let displayName = entries.first?.artifact.descriptor.displayName
            ?? "\(typeName).\(selector)"
        let summaries = overloadSummaries(typeName: typeName,
                                          entries: entries)
        lines.append("    let supported = [")
        for summary in summaries {
            lines.append("        \"\(escapeForSwiftLiteral(summary))\",")
        }
        lines.append("    ].joined(separator: \"\\n\")")
        lines.append("    var message = \"\(escapeForSwiftLiteral(displayName)) has no overload compatible with runtime arguments.\"")
        lines.append("    if let lastError = lastTypeError {")
        lines.append("        message += \" Last error: \" + lastError.description")
        lines.append("    }")
        lines.append("    message += \" Supported overloads:\\n\" + supported")
        lines.append("    throw VMError.typeError(message)")
        lines.append("}")
        return (baseName, lines.joined(separator: "\n"))
    }

    private func overloadSummaries(typeName: String,
                                   entries: [SelectorDispatchEntry]) -> [String] {
        var result: [String] = []
        for entry in entries {
            let descriptor = entry.artifact.descriptor
            let parameters = descriptor.parameters
            for thunk in entry.artifact.render.thunks {
                let summary: String
                if parameters.isEmpty {
                    summary = "  - \(thunk.receiverTypeName) ()"
                } else {
                    var parts: [String] = []
                    for (index, parameter) in parameters.enumerated() {
                        let typeDescription = index < thunk.parameterTypeDescriptions.count
                            ? thunk.parameterTypeDescriptions[index]
                            : "<unknown>"
                        if let label = parameter.label, label != "_" {
                            parts.append("\(label): \(typeDescription)")
                        } else {
                            parts.append(typeDescription)
                        }
                    }
                    summary = "  - \(thunk.receiverTypeName) (\(parts.joined(separator: ", ")))"
                }
                result.append(summary)
            }
        }
        if result.isEmpty {
            result.append("  - \(typeName) ()")
        }
        return result
    }
}
