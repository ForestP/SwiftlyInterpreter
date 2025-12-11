# Swift Bytecode Interpreter (Swiftly)

Embeddable Swift interpreter that parses Swift source with SwiftSyntax, compiles it into a compact bytecode, and executes it on a stack VM. It is designed to run inside iOS/macOS apps and on the web (WASM) without a JIT or external processes, with host interop to call real Swift/Foundation APIs.

## What’s here
- **Interpreter core:** `InterpreterCompiler` lowers a Swift subset to bytecode; `InterpreterVM` executes it; `InterpreterModels` defines values/opcodes/IDs.
- **Host interop:** Runtime registry for host methods/properties with selector/property interning, allowlists, collision guards, and VM state gating (`prepare`, `invoke`). Foundation bindings are generated; Swift stdlib bindings are a WIP.
- **Bridge generation:** `HostSurfaceKit` harvests/normalizes API surfaces from `.swiftinterface` files; `BridgeGenCLI` turns them into Swift host bridges. Scripts live under `Swiftly/scripts`.
- **Swiftly package:** Library + `SwiftlyExecutable` target that bundles generated bridges and exports a WASM-friendly C ABI (`run_program`, `compile_program_*`).
- **Docs/Roadmaps:** Extensive design and roadmap notes under the repo root and `docs/` for language features, host interop, wasm builds, and recovery plans.

## Language coverage today
- Numbers with widening (`Int`, `Double`, `UInt`), relational ops, `%`, unary +/-.
- Control flow: `if/else`, `while`, `repeat…while`, simple `for-in` over ranges, ternary `?:`, `break`/`continue`.
- Functions & closures with captures, implicit returns for single-expr closures, slot- or name-based locals, source locations on every opcode.
- Collections: array/dictionary literals, subscript get/set, `count` property, pretty-printing, append via host dispatch.
- User-defined `struct` with labeled init synthesis, stored properties, methods (implicit `self`, mutating tag), method dispatch keyed by `(TypeID, SelectorID)`.
- Host calls: receiver-first `callMethod`, `Value.metatype` for initializers/static calls, optional chaining and `??` using VM opcodes rather than special host hooks.

## Host interop and BridgeGen
- Runtime interop matches Swift semantics: property vs zero-arg method collision rule, allowlists for types/selectors/properties, and fast property dispatch by `PropertyID`. Host calls fail with `missingMethod`/`missingProperty` diagnostics.
- Bridge generation flow:
  - Foundation: `Swiftly/scripts/gen_foundation_bridges.sh` uses `Swiftly/bridgegen-foundation.json` to specialize generics (e.g., `FloatingPointFormatStyle<T>`) and writes bridges under `Swiftly/Sources/Swiftly/HostBridges/HostBridges`.
  - Swift stdlib: `Swiftly/scripts/gen_swiftstdlib_bridges.sh` + `Swiftly/bridgegen-swiftstdlib-allowlist.json` / `bridgegen-swiftstdlib-specializations.json`.
  - `BridgeGenCLI/README.md` documents CLI flags, auto-specialization, and reporting; `FOUNDATION_HOST_INTEROP_GUIDE.md` covers config schema and troubleshooting.

## Current status (Dec 2025)
- The interpreter and host-runtime plumbing are stable for the feature set above (tests cover precedence, scopes, collections, host property/method dispatch, and user methods).
- Foundation bridges and the host surface tooling are in place; regeneration is scriptable and keeps installers up to date.
- **Swift stdlib BridgeGen is currently broken/regressing.** Recent runs leave gaps for mutating/closure-heavy APIs (e.g., `Array.append/remove`, `Optional.map`), and protocol-witness flattening across constrained extensions is incomplete. See `Swiftly/SWIFT_STDLIB_HOST_BRIDGE_RECOVERY_PLAN.md` and `Swiftly/SWIFT_STDLIB_CLOSURE_BRIDGE_FIX_ROADMAP.md` for the open tasks and missing selectors. The latest commit (`9174040 – "bridgen still broken"`) reflects this state.
- WASM builds work for the core interpreter; host interop on the web depends on the generated surfaces being present. Build guidance lives in `docs/wasm-build.md`.

## Building & testing
- Swift packages target Swift 6.2. Common entry points:
  - Library/executable: `swift build --package-path Swiftly --product SwiftlyExecutable`
  - Compiler CLI: `swift build --package-path InterpreterCompiler --product Compiler`
  - Tests: `swift test --package-path Swiftly` (and per-package `swift test --package-path InterpreterCompiler`, `InterpreterVM`, `HostSurfaceKit`).
- Regenerate bridges when configs change:
  - Foundation: `bash Swiftly/scripts/gen_foundation_bridges.sh`
  - Swift stdlib (currently failing): `bash Swiftly/scripts/gen_swiftstdlib_bridges.sh`
- For WASM targets, follow `docs/wasm-build.md` (use the Swift 6.2 open-source toolchain + `swift-6.2-RELEASE_wasm` SDK).

## Useful docs to read next
- `doc.md` — interpreter architecture, instruction set, host-interop design notes.
- `FOUNDATION_HOST_INTEROP_GUIDE.md` — config/schema for Foundation bridges.
- `Swiftly/SWIFT_STDLIB_HOST_INTEROP_ROADMAP.md` — desired stdlib surface & generation plan.
- `Swiftly/SWIFT_STDLIB_HOST_BRIDGE_RECOVERY_PLAN.md` — known breakages and validation steps.
- `docs/wasm-build.md` — building the WASM targets.
