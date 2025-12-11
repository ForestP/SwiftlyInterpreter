import Foundation
import BridgeGenCLI
import HostCodegenCore
import HostSurfaceKit
#if canImport(Darwin)
import Darwin
#else
import Glibc
#endif

struct BridgeGenCommand {
    static func main() {
        let arguments = CommandLine.arguments
        if arguments.contains("--help") || arguments.contains("-h") {
            printUsage()
            return
        }

        do {
            let summary = try BridgeGenCLI().run(arguments: arguments)
            printSummary(summary)
            let hasErrors = containsErrors(summary: summary)
            if hasErrors {
                exit(EXIT_FAILURE)
            }
        } catch let error as BridgeGenCLI.Error {
            fputs("error: \(error)\n\n", stderr)
            printUsage()
            exit(EXIT_FAILURE)
        } catch {
            fputs("error: \(error)\n", stderr)
            exit(EXIT_FAILURE)
        }
    }

    private static func printUsage() {
        let usage = """
        Usage: bridgegen --module <NAME> --interface <FILE> --out <DIRECTORY> [options]

        Required:
          --module <NAME>           Module name associated with the .swiftinterface contents
          --interface <FILE>        Path to the source .swiftinterface file
          --out <DIRECTORY>         Directory where generated Swift files will be written

        Options:
          --config <FILE>           JSON config describing generics/property specializations
          --file-prefix <PREFIX>    Folder prefix for generated files (default: HostBridges)
          --installer-basename <NAME>
                                    Basename used for the generated installer function/file (default: GeneratedHostBridges)
          --skip-installer-registration
                                    Skip auto-registering the installer; caller must register manually
          --report <FILE>           Writes a JSON generation report to the given path
          --dump-surface <FILE>     Writes collected API surface (pre-generation) to JSON for debugging
          --format                  Run swift-format on generated files if available on PATH
          --swift-format <PATH>     Explicit path to a swift-format executable
          --auto-specialize <LIST>  Aggregated strategies: aliases, palette, registrations, all
                                    Combine with comma or plus (e.g. aliases,palette or all)
                                    Note: registrations requires --auto-specialize-registrations <FILE>
                                    Precedence: config > aliases > registrations > palette
          --auto-specialize-aliases Enable alias-driven auto specialization
          --auto-specialize-palette <NAME>
                                    Enable palette-driven auto specialization ("standard" or "none")
          --auto-specialize-registrations <FILE>
                                    JSON with {"registered_types": ["Module.Type<...>", ...]}
          -h, --help                Show this help message
        """
        print(usage)
    }

    private static func printSummary(_ summary: HostBridgeGenerationSummary) {
        let written = summary.writtenFiles.sorted { $0.path < $1.path }
        if written.isEmpty {
            print("No host bridge files generated.")
        } else {
            let directory = written.first?.deletingLastPathComponent().path ?? "(unknown)"
            print("Generated \(written.count) file(s) under \(directory):")
            for url in written {
                print("  \(url.path)")
            }
        }

        if !summary.report.typeAliases.isEmpty {
            print("\nResolved typealiases:")
            for alias in summary.report.typeAliases {
                let location = alias.sourceLine > 0 ? "#\(alias.sourceLine)" : ""
                print("  \(alias.qualifiedName) -> \(alias.canonicalTarget) \(location)")
            }
        }

        let specializationLines = BridgeGenCLI.specializationSummaryLines(from: summary.report.types)
        if !specializationLines.isEmpty {
            print("\nSpecialization sources:")
            specializationLines.forEach { print($0) }
        }

        let fallbackHits = summary.report.metrics.collectorFallbackPlaceholderHitCount
        print("\nHostSurfaceCollector fallback placeholder hits: \(fallbackHits)")
        if fallbackHits > 0 {
            fputs("error: HostSurfaceCollector fallback placeholder heuristic fired \(fallbackHits) time(s).\n", stderr)
        }
        let resolverDrops = summary.report.metrics.resolverPlaceholderDropHitCount
        print("HostSurfaceResolver placeholder drop hits: \(resolverDrops)")
        if resolverDrops > 0 {
            fputs("error: HostSurfaceResolver lost generic placeholders \(resolverDrops) time(s).\n", stderr)
        }

        let diagnostics = summary.surfaceResult.diagnostics +
            summary.generationResult.diagnostics +
            summary.formatDiagnostics
        let errorCount = diagnostics.filter { $0.kind.isError }.count
        let warningCount = diagnostics.count - errorCount

        if warningCount > 0 {
            print("\nWarnings emitted: \(warningCount) (see stderr for details)")
        }
        if errorCount > 0 {
            fputs("Encountered \(errorCount) error(s). See diagnostics above.\n", stderr)
        }
    }

    private static func containsErrors(summary: HostBridgeGenerationSummary) -> Bool {
        let diagnostics = summary.surfaceResult.diagnostics +
            summary.generationResult.diagnostics +
            summary.formatDiagnostics
        let hasError = diagnostics.contains(where: { $0.kind.isError })
        if summary.report.metrics.collectorFallbackPlaceholderHitCount > 0 {
            return true
        }
        if summary.report.metrics.resolverPlaceholderDropHitCount > 0 {
            return true
        }
        if summary.surfaceResult.diagnostics.contains(where: { $0.kind.isMissingPlaceholderWarning }) {
            return true
        }
        return hasError
    }
    // Summary line formatting now lives in BridgeGenCLI.specializationSummaryLines
}

BridgeGenCommand.main()

private extension HostSurfaceDiagnostic.Kind {
    var isError: Bool {
        switch self {
        case .parseError:
            return true
        case .unsupported:
            return false
        case .missingPlaceholders:
            return true
        }
    }

    var isMissingPlaceholderWarning: Bool {
        switch self {
        case .missingPlaceholders:
            return true
        default:
            return false
        }
    }
}
