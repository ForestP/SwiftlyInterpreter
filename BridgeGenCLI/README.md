# BridgeGen CLI Usage

These commands assume you are inside the repository root (`Interpreter/`). They build the `bridgegen` executable and run it against a Swift interface file to generate host bridge sources.

## Build the CLI

```sh
cd BridgeGenCLI
swift build
cd ..
```

> Tip: If SwiftPM warns about module caches, set `MODULE_CACHE_DIR` and `SWIFT_BUILD_MODULECACHE_PATH` to writable locations before running `swift build`.

## Example Assets

The repo ships with sample inputs under `BridgeGenCLI/Examples/`:

- `Demo.swiftinterface` — a tiny surface exposing `Foo` methods and a generic `Box<Value>`.
- `DemoConfig.json` — specialization config that instantiates `Foo.wrap<T>` for `String`/`Int` and `Box.item` for `String`.

## Run BridgeGen

This example collects surface metadata from the provided demo files and writes the generated Swift to `BridgeGenCLI/Examples/Generated` (created automatically if needed).

```sh
BridgeGenCLI/.build/debug/bridgegen \
  --module Demo \
  --interface BridgeGenCLI/Examples/Demo.swiftinterface \
  --out BridgeGenCLI/Examples/Generated \
  --config BridgeGenCLI/Examples/DemoConfig.json \
  --file-prefix GeneratedHost \
  --report BridgeGenCLI/Examples/Generated/bridgegen-report.json \
  --format
```

> If `swift-format` is not available on your PATH, the command prints `Unsupported: swift-format not found; skipped formatting` but still generates files successfully.

## Auto-Specialization Examples

Infer typed specialization domains without a JSON config. All examples write to `BridgeGenCLI/Examples/Generated`.

1) Aliases (auto.alias)

```sh
BridgeGenCLI/.build/debug/bridgegen \
  --module Demo \
  --interface BridgeGenCLI/Examples/DemoAliases.swiftinterface \
  --out BridgeGenCLI/Examples/Generated \
  --auto-specialize-aliases \
  --report BridgeGenCLI/Examples/Generated/bridgegen-aliases-report.json
```

2) Palette (auto.palette.standard)

```sh
BridgeGenCLI/.build/debug/bridgegen \
  --module Demo \
  --interface BridgeGenCLI/Examples/Demo.swiftinterface \
  --out BridgeGenCLI/Examples/Generated \
  --auto-specialize-palette standard \
  --report BridgeGenCLI/Examples/Generated/bridgegen-palette-report.json
```

3) Registrations (auto.registrations)

```sh
BridgeGenCLI/.build/debug/bridgegen \
  --module Demo \
  --interface BridgeGenCLI/Examples/Demo.swiftinterface \
  --out BridgeGenCLI/Examples/Generated \
  --auto-specialize-registrations BridgeGenCLI/Examples/Registrations.json \
  --report BridgeGenCLI/Examples/Generated/bridgegen-registrations-report.json
```

The CLI prints a specialization summary and writes a JSON report including `specializationSource` (e.g., `auto.alias`). Guardrails still apply; if caps trim domains, a warning is printed and a typed fallback thunk may be emitted.

4) Aggregated flag (`--auto-specialize`)

Enable multiple strategies at once. The list accepts comma or plus separators.

Aliases + Palette:

```sh
BridgeGenCLI/.build/debug/bridgegen \
  --module Demo \
  --interface BridgeGenCLI/Examples/Demo.swiftinterface \
  --out BridgeGenCLI/Examples/Generated \
  --auto-specialize aliases,palette \
  --report BridgeGenCLI/Examples/Generated/bridgegen-agg-aliases-palette.json
```

All (aliases + palette) plus registrations (requires manifest):

```sh
BridgeGenCLI/.build/debug/bridgegen \
  --module Demo \
  --interface BridgeGenCLI/Examples/Demo.swiftinterface \
  --out BridgeGenCLI/Examples/Generated \
  --auto-specialize all \
  --auto-specialize-registrations BridgeGenCLI/Examples/Registrations.json \
  --report BridgeGenCLI/Examples/Generated/bridgegen-agg-all.json
```

Notes:
- `palette` under the aggregator defaults to the `standard` palette. Use `--auto-specialize-palette` to override.
- Including `registrations` in the aggregator requires `--auto-specialize-registrations <FILE>`; otherwise it is skipped with a warning.

### Arguments

- `--module` — Module name declared by the `.swiftinterface`.
- `--interface` — Path to the `.swiftinterface` to parse.
- `--out` — Destination directory for generated Swift source files.
- `--config` (optional) — JSON describing specialization domains and conformances.
- `--file-prefix` (optional) — Folder/prefix used for generated files (`HostBridges` by default).
- `--report` (optional) — Emit a JSON summary of the pipeline and diagnostics.
- `--format` (optional) — Run `swift-format` on generated files (use `--swift-format <path>` to point at a specific binary).

### Auto-Specialization

In addition to explicit JSON config, `bridgegen` can infer typed specialization domains:

- Precedence: config > aliases > registrations > palette (first non-empty domains win).
- Aggregator can enable multiple; precedence still applies across enabled strategies.
- `--auto-specialize <LIST>` — Aggregates strategies (`aliases`, `palette`, `registrations`, `all`). Combine with comma or plus (e.g., `aliases,palette` or `all`). `palette` defaults to `standard`. `registrations` requires `--auto-specialize-registrations <FILE>`.
- `--auto-specialize-aliases` — Derive domains from `public typealias` targets found in the interface. Telemetry reports `auto.alias`.
- `--auto-specialize-palette standard` — Use a curated palette: `Swift.Int`, `Swift.String`, `Swift.Bool`, `Foundation.Data`. Telemetry reports `auto.palette.standard`.
- `--auto-specialize-registrations <FILE>` — Provide a JSON manifest of concrete host types the app will register. Format:

  ```json
  { "registered_types": ["Demo.Box<Swift.String>", "Demo.Box<Swift.Int>"] }
  ```

  Telemetry reports `auto.registrations`.

Auto-specialization respects guardrails: `maxDomainPerGenericParam` caps per-parameter domains; `maxSpecializationsPerSelector` caps total thunk count with a typed multi-branch fallback. Explicit `--config` domains take precedence over auto strategies.

The JSON report includes per-parameter domain sizes for auditing:

```json
{
  "types": [
    {
      "canonicalName": "Demo.Box<Value>",
      "properties": [
        {
          "name": "stored",
          "specializationSource": "auto.registrations",
          "genericParameterDomainSizes": { "Value": 2 }
        }
      ]
    }
  ]
}
```

The command exits with a non-zero status if any diagnostics classified as errors occur during collection or generation.
