#!/usr/bin/env bash
set -euo pipefail

# Swiftly host-interop generator for Swift stdlib (native + WASM)
# Mirrors the Foundation generator but targets Swift.swiftinterface.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd -P)"
if REPO_ROOT_GIT=$(git -C "$SCRIPT_DIR/.." rev-parse --show-toplevel 2>/dev/null); then
  REPO_ROOT="$REPO_ROOT_GIT"
else
  REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd -P)"
fi
SWIFTLY_DIR="$REPO_ROOT/Swiftly"

CLI_DIR="$REPO_ROOT/BridgeGenCLI"
BRIDGEGEN_SCRATCH_DIR="${BRIDGEGEN_SCRATCH_DIR:-$REPO_ROOT/.build/bridgegencli}"
CLI_BIN_DEBUG="$BRIDGEGEN_SCRATCH_DIR/debug/bridgegen"
CLI_BIN_RELEASE_GLOB="$BRIDGEGEN_SCRATCH_DIR"/*/release/bridgegen

SWIFT_INTERFACE_DEFAULT="$REPO_ROOT/Swift.swiftinterface"
OUT_DIR_DEFAULT="$SWIFTLY_DIR/Sources/Swiftly/HostBridges"
REPORT_PATH_DEFAULT="$OUT_DIR_DEFAULT/bridgegen-swiftstdlib-report.json"
CONFIG_DEFAULT="$SWIFTLY_DIR/bridgegen-swiftstdlib-specializations.json"
ALLOWLIST_DEFAULT="$SWIFTLY_DIR/bridgegen-swiftstdlib-allowlist.json"

MODULE_NAME="Swift"
INTERFACE_PATH="${1:-$SWIFT_INTERFACE_DEFAULT}"
OUT_DIR="${2:-$OUT_DIR_DEFAULT}"
REPORT_PATH="${3:-$REPORT_PATH_DEFAULT}"

embed_wasi_resources() {
  local surface_path="$1"
  local inventory_path="$2"
  local enum_name="$3"
  local output_path="$4"
  local tag="$5"
  echo "[${tag}] Embedding WASI resource blobs into $(basename "$output_path") …"
  swift <<'SWIFT' "$surface_path" "$inventory_path" "$enum_name" "$output_path"
import Foundation

let args = CommandLine.arguments
guard args.count == 5 else {
    fputs("[embed] ERROR: expected surface, inventory, enum name, output path\n", stderr)
    exit(1)
}

let surfaceURL = URL(fileURLWithPath: args[1])
let inventoryURL = URL(fileURLWithPath: args[2])
let enumName = args[3]
let outputURL = URL(fileURLWithPath: args[4])

let fileManager = FileManager.default
guard fileManager.fileExists(atPath: surfaceURL.path),
      fileManager.fileExists(atPath: inventoryURL.path) else {
    fputs("[embed] WARNING: Surface or inventory JSON missing; skipping WASI embed.\n", stderr)
    exit(0)
}

do {
    let surfaceData = try Data(contentsOf: surfaceURL)
    let inventoryData = try Data(contentsOf: inventoryURL)
    let surfaceB64 = surfaceData.base64EncodedString()
    let inventoryB64 = inventoryData.base64EncodedString()

    let block = """
#if os(WASI)
import Foundation

#if compiler(>=5.10)
@_documentation(visibility: internal)
#endif
public enum \(enumName) {
    public static let surfaceJSONData: Data = Data(base64Encoded: "\(surfaceB64)", options: .ignoreUnknownCharacters)!
    public static let inventoryJSONData: Data = Data(base64Encoded: "\(inventoryB64)", options: .ignoreUnknownCharacters)!
}
#endif
"""

    try block.appending("\n").write(to: outputURL, atomically: true, encoding: .utf8)
} catch {
    fputs("[embed] ERROR: \(error)\n", stderr)
    exit(1)
}
SWIFT
}

echo "[gen-stdlib] Repo root:          $REPO_ROOT"
echo "[gen-stdlib] Swiftly dir:        $SWIFTLY_DIR"
echo "[gen-stdlib] Interface path:     $INTERFACE_PATH"
echo "[gen-stdlib] Output dir:         $OUT_DIR"
echo "[gen-stdlib] Report path:        $REPORT_PATH"
echo "[gen-stdlib] Inventory path:     ${OUT_DIR}/bridgegen-swiftstdlib-inventory.json"
if [[ -f "$CONFIG_DEFAULT" ]]; then
  echo "[gen-stdlib] Config path:        $CONFIG_DEFAULT"
else
  echo "[gen-stdlib] Config path:        (none found; relying on auto specialization)"
fi
if [[ -f "$ALLOWLIST_DEFAULT" ]]; then
  echo "[gen-stdlib] Allowlist path:     $ALLOWLIST_DEFAULT"
else
  echo "[gen-stdlib] Allowlist path:     (none)"
fi

if [[ ! -f "$INTERFACE_PATH" ]]; then
  echo "[gen-stdlib] ERROR: Swift.swiftinterface not found at: $INTERFACE_PATH" >&2
  exit 1
fi

mkdir -p "$OUT_DIR"
INSTALLER_PATH="$OUT_DIR/HostBridges/SwiftStdlibHostBridges.generated.swift"
SURFACE_PATH="$OUT_DIR/bridgegen-swiftstdlib-surface.json"
INVENTORY_PATH="$OUT_DIR/bridgegen-swiftstdlib-inventory.json"

export MODULE_CACHE_DIR="${MODULE_CACHE_DIR:-$REPO_ROOT/.build/modulecache}"
export SWIFT_BUILD_MODULECACHE_PATH="${SWIFT_BUILD_MODULECACHE_PATH:-$REPO_ROOT/.build/swift-modulecache}"
export SWIFT_MODULECACHE_PATH="${SWIFT_MODULECACHE_PATH:-$REPO_ROOT/.build/swift-modulecache}"
export CLANG_MODULE_CACHE_PATH="${CLANG_MODULE_CACHE_PATH:-$REPO_ROOT/.build/clang-modulecache}"
mkdir -p "$MODULE_CACHE_DIR" "$SWIFT_BUILD_MODULECACHE_PATH" "$SWIFT_MODULECACHE_PATH" "$CLANG_MODULE_CACHE_PATH"

echo "[gen-stdlib] Building BridgeGenCLI (ensuring latest) …"
if [[ -d "$BRIDGEGEN_SCRATCH_DIR" ]]; then
  echo "[gen-stdlib] Cleaning previous BridgeGenCLI scratch build …"
  rm -rf "$BRIDGEGEN_SCRATCH_DIR"
fi
(cd "$CLI_DIR" && swift build -c release --scratch-path "$BRIDGEGEN_SCRATCH_DIR")

BRIDGEGEN_BIN=""
for cand in $CLI_BIN_RELEASE_GLOB; do
  if [[ -x "$cand" ]]; then BRIDGEGEN_BIN="$cand"; break; fi
done
if [[ -z "$BRIDGEGEN_BIN" || ! -x "$BRIDGEGEN_BIN" ]]; then
  BRIDGEGEN_BIN="$CLI_BIN_DEBUG"
fi
if [[ ! -x "$BRIDGEGEN_BIN" ]]; then
  echo "[gen-stdlib] ERROR: bridgegen binary not found after build." >&2
  exit 1
fi

SWIFT_FORMAT_BIN="$(command -v swift-format || true)"
FORMAT_FLAGS=("--format")
if [[ -n "$SWIFT_FORMAT_BIN" ]]; then
  FORMAT_FLAGS+=("--swift-format" "$SWIFT_FORMAT_BIN")
fi

echo "[gen-stdlib] Cleaning previous Swift stdlib generated files …"
if [[ -d "$OUT_DIR/HostBridges" ]]; then
  find "$OUT_DIR/HostBridges" -maxdepth 1 -type f -name 'SwiftStdlib_*+Host.generated.swift' -delete
  rm -f "$OUT_DIR/HostBridges/SwiftStdlibHostBridges.generated.swift"
fi

echo "[gen-stdlib] Running bridgegen …"
LOG_PATH="$OUT_DIR/bridgegen-swiftstdlib.log"
set -o pipefail
set -x
CFG_ARGS=()
if [[ -f "$CONFIG_DEFAULT" ]]; then
  CFG_ARGS+=("--config" "$CONFIG_DEFAULT")
fi
if [[ -f "$ALLOWLIST_DEFAULT" ]]; then
  CFG_ARGS+=("--allow-types" "$ALLOWLIST_DEFAULT")
fi
"$BRIDGEGEN_BIN" \
  --module "$MODULE_NAME" \
  --interface "$INTERFACE_PATH" \
  --out "$OUT_DIR" \
  --file-prefix HostBridges \
  --installer-basename SwiftStdlibHostBridges \
  --skip-installer-registration \
  --auto-specialize palette \
  --report "$REPORT_PATH" \
  --dump-surface "$SURFACE_PATH" \
  --dump-inventory "$INVENTORY_PATH" \
  "${FORMAT_FLAGS[@]}" \
  "${CFG_ARGS[@]}" 2>&1 | tee "$LOG_PATH"
set +x

BRIDGEGEN_STATUS=$?
echo "[gen-stdlib] bridgegen exit status: $BRIDGEGEN_STATUS"
if [[ $BRIDGEGEN_STATUS -ne 0 ]]; then
  echo "[gen-stdlib] ERROR: bridgegen failed with status $BRIDGEGEN_STATUS" >&2
  exit $BRIDGEGEN_STATUS
fi

if [[ ! -f "$REPORT_PATH" ]]; then
  echo "[gen-stdlib] ERROR: bridgegen did not write report: $REPORT_PATH" >&2
  echo "[gen-stdlib] Listing output directory for debugging:" >&2
  ls -la "$OUT_DIR" >&2 || true
  echo "[gen-stdlib] --- bridgegen stdout/stderr ---" >&2
  tail -n 200 "$LOG_PATH" >&2 || true
  exit 2
fi
if [[ ! -f "$INSTALLER_PATH" ]]; then
  echo "[gen-stdlib] ERROR: No installer emitted. Expected at: $INSTALLER_PATH" >&2
  echo "[gen-stdlib] --- bridgegen stdout/stderr ---" >&2
  tail -n 200 "$LOG_PATH" >&2 || true
  exit 2
fi

EMBED_OUT_PATH="$OUT_DIR/HostBridges/SwiftStdlibHostBridgeEmbeddedResources+WASI.swift"
embed_wasi_resources "$SURFACE_PATH" "$INVENTORY_PATH" "SwiftStdlibHostBridgeEmbeddedResources" "$EMBED_OUT_PATH" "gen-stdlib"

echo "[gen-stdlib] Done. Generated Swift stdlib sources in: $OUT_DIR"
