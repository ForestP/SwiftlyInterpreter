//
//  HostSurfacePipeline.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/19/25.
//

import Foundation

public struct HostSurfacePipelineOptions {
    public var dumpModelURL: URL?
    public var dumpInventoryURL: URL?
    public var dumpPrettyPrint: Bool

    public init(dumpModelURL: URL? = nil,
                dumpInventoryURL: URL? = nil,
                dumpPrettyPrint: Bool = true) {
        self.dumpModelURL = dumpModelURL
        self.dumpInventoryURL = dumpInventoryURL
        self.dumpPrettyPrint = dumpPrettyPrint
    }
}

enum HostSurfacePipelineError: Error, CustomStringConvertible {
    case invalidDumpURL

    var description: String {
        switch self {
        case .invalidDumpURL:
            return "dumpModelURL must be a file URL"
        }
    }
}

public struct HostSurfacePipeline {
    let moduleName: String
    let interfaceContents: String
    var options: HostSurfacePipelineOptions

    public init(
        moduleName: String,
        interfaceContents: String,
        options: HostSurfacePipelineOptions = .init()
    ) {
        self.moduleName = moduleName
        self.interfaceContents = interfaceContents
        self.options = options
    }

    public mutating func run() throws -> HostSurfaceResolveResult {
        let aliasParse = TypeAliasParser.parse(
            moduleName: moduleName,
            contents: interfaceContents
        )
        let genericInventory = GenericParameterInventoryBuilder.build(
            moduleName: moduleName,
            contents: interfaceContents
        )
        var aliasResolver = try TypeAliasResolver(
            aliases: aliasParse.aliases,
            diagnostics: aliasParse.diagnostics,
            genericParameterInventory: genericInventory
        )

        var surface = HostSurfaceCollector(genericParameterInventory: genericInventory).collect(
            moduleName: moduleName,
            contents: interfaceContents
        )

        print("[COLLECT] Collected \(surface.methods.count) methods, \(surface.properties.count) properties")
        let arrayMethods = surface.methods.filter { $0.type.canonicalDescription().contains("Array") }
        let arrayTypes = Set(arrayMethods.map { $0.type.canonicalDescription() })
        print("[COLLECT] Array-related types from methods: \(arrayTypes.sorted())")

        let aliasDiagnostics = aliasResolver.diagnostics.map { diag -> HostSurfaceDiagnostic in
            switch diag.kind {
            case .parseError(let message):
                return HostSurfaceDiagnostic(kind: .parseError(message), line: diag.line)
            case .unsupported(let message):
                return HostSurfaceDiagnostic(kind: .unsupported(message), line: diag.line)
            }
        }
        surface.diagnostics.append(contentsOf: aliasDiagnostics)

        var resolver = HostSurfaceResolver(aliasResolver: aliasResolver)
        let result = resolver.resolve(surface: surface)

        if let dumpURL = options.dumpModelURL {
            print("[DUMP] Dumping surface with \(result.apiSurface.types.count) types")
            let arrayTypes = result.apiSurface.types.filter { $0.canonicalName.contains("Array") }
            print("[DUMP] Array-related types: \(arrayTypes.map { $0.canonicalName }.sorted())")
            if let swiftArray = result.apiSurface.types.first(where: { $0.canonicalName == "Swift.Array" || $0.canonicalName.starts(with: "Swift.Array<") }) {
                print("[DUMP] Found Swift.Array type: \(swiftArray.canonicalName)")
                print("[DUMP] Members count: \(swiftArray.members.count)")
                print("[DUMP] Has map: \(swiftArray.members.contains(where: { $0.selector == "map(_:)" }))")
            }
            try dump(apiSurface: result.apiSurface, to: dumpURL)
        }

        if let inventoryURL = options.dumpInventoryURL {
            try dumpInventory(result.genericParameterInventory, to: inventoryURL)
        }

        return result
    }

    private func dump(apiSurface: ApiSurface, to url: URL) throws {
        guard url.isFileURL else { throw HostSurfacePipelineError.invalidDumpURL }
        let encoder = ApiSurfaceJSONEncoder(prettyPrinted: options.dumpPrettyPrint)
        let data = try encoder.encode(apiSurface)
        let directory = url.deletingLastPathComponent()
        if !directory.path.isEmpty && !FileManager.default.fileExists(atPath: directory.path) {
            try FileManager.default.createDirectory(at: directory,
                                                    withIntermediateDirectories: true)
        }
        try write(data: data, to: url)
    }

    private func dumpInventory(_ inventory: [String: [String]], to url: URL) throws {
        guard url.isFileURL else { throw HostSurfacePipelineError.invalidDumpURL }
        let encoder = JSONEncoder()
        var formatting: JSONEncoder.OutputFormatting = [.sortedKeys, .withoutEscapingSlashes]
        if options.dumpPrettyPrint {
            formatting.insert(.prettyPrinted)
        }
        encoder.outputFormatting = formatting
        let data = try encoder.encode(inventory)
        let directory = url.deletingLastPathComponent()
        if !directory.path.isEmpty && !FileManager.default.fileExists(atPath: directory.path) {
            try FileManager.default.createDirectory(at: directory,
                                                    withIntermediateDirectories: true)
        }
        try write(data: data, to: url)
    }

    private func write(data: Data, to url: URL) throws {
#if os(WASI)
        try data.write(to: url)
#else
        try data.write(to: url, options: .atomic)
#endif
    }
}
