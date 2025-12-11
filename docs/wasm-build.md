# Building for WebAssembly (Swift 6.2 + Swiftly)

This documents the working setup to build the WASM targets with the Swift 6.2 open‑source toolchain and the Swift 6.2 WASM Swift SDK.

Prerequisites

- Install the Swift 6.2 WASM Swift SDK:
  - `swift sdk install https://download.swift.org/swift-6.2-release/wasm/swift-6.2-RELEASE/swift-6.2-RELEASE_wasm.artifactbundle.tar.gz --checksum fe4e8648309fce86ea522e9e0d1dc48e82df6ba6e5743dbf0c53db8429fb5224`
  - Confirm it’s listed: `swift sdk list` should include `swift-6.2-RELEASE_wasm`.
- Select the open‑source Swift toolchain (not the Xcode default):
  - Xcode → Toolchains → “Swift (swift.org)” 6.2, or
  - Use `xcrun --toolchain swift ...` (shown below), or
  - `TOOLCHAINS=swift` in your shell.

Key idea

- Ensure builds run with the open‑source Swift toolchain so Clang and the linker support `wasm32-unknown-wasip1`. If you see `/Applications/Xcode.app/.../clang` in logs, you’re on the wrong toolchain.

Build examples

- SwiftlyExecutable (package in `Swiftly/`):
  - `xcrun --toolchain swift swift build -c release --package-path Swiftly --product SwiftlyExecutable --swift-sdk swift-6.2-RELEASE_wasm -v`

- Integration (package in `IntegrationTest/`):
  - `xcrun --toolchain swift swift build -c release --package-path IntegrationTest --product Integration --swift-sdk swift-6.2-RELEASE_wasm -v`

- Threads-enabled variant (if you need atomics/pthreads):
  - Replace the SDK ID with `6.2-SNAPSHOT-2025-09-10-a-wasm32-unknown-wasip1-threads`.

Verification

- Check the active Swift toolchain in use:
  - `xcrun --toolchain swift swift --version` (should show the open‑source toolchain, not `swiftlang-...`).
  - `xcrun --toolchain swift clang -v` and `xcrun --toolchain swift swiftc -print-target-info -target wasm32-unknown-wasip1`.
- Confirm the SDK ID is used (not a file path): `--swift-sdk swift-6.2-RELEASE_wasm`.

Troubleshooting

- Error: `No available targets are compatible with triple "wasm32-unknown-wasip1"`.
  - Cause: Apple Clang (from Xcode) is being used; it can’t target wasm.
  - Fix: Run builds via `xcrun --toolchain swift ...` (or `TOOLCHAINS=swift`) so the open‑source toolchain’s Clang/LLD are used.

- Error: `No Swift SDK found matching query ...`.
  - Fix: Use the installed SDK ID (e.g. `swift-6.2-RELEASE_wasm`), not a bundle path. Check with `swift sdk list`.

Notes

- If you prefer not to pass `--package-path`, run commands from the respective package directory (`Swiftly/` or `IntegrationTest/`).
- If you later encounter pthread/atomics/shared‑memory linker flags issues, switch to the threads SDK variant as shown above.

