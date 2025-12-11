// swift-tools-version: 6.2
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "InterpreterCompiler",
    platforms: [
        .iOS(.v17),
        .macOS(.v13)
    ],
    products: [
        // Products define the executables and libraries a package produces, making them visible to other packages.
        .executable(
            name: "Compiler",
            targets: ["Executable"]
        ),
        .library(
            name: "InterpreterCompiler",
            targets: ["InterpreterCompiler"]
        ),
    ],
    dependencies: [
        .package(path: "../InterpreterModels"),
        .package(path: "../HostSurfaceKit"),
        .package(url: "https://github.com/apple/swift-syntax.git", from: "602.0.0")
    ],
    targets: [
        // Targets are the basic building blocks of a package, defining a module or a test suite.
        // Targets can depend on other targets in this package and products from dependencies.
        .executableTarget(
            name: "Executable",
            dependencies: [
                "InterpreterCompiler"
            ],
//            path: "Sources/InterpreterCompiler",
            // In the target that builds the WASM
            linkerSettings: [
              .unsafeFlags([
//                "-Xlinker", "--export=memory",
//                "-Xlinker", "--export=__heap_base",
//                "-Xlinker", "--export=compile_program_json",
//                "-Xlinker", "--export=compile_program_text"
              ], .when(platforms: [.wasi]))
            ]
        ),
        .target(
            name: "InterpreterCompiler",
            dependencies: [
                "InterpreterModels",
                "HostSurfaceKit",
                .product(name: "SwiftSyntax", package: "swift-syntax"),
                .product(name: "SwiftParser", package: "swift-syntax")
            ]
        ),
        .testTarget(
            name: "InterpreterCompilerTests",
            dependencies: [
                "InterpreterCompiler",
                "InterpreterModels",
                "HostSurfaceKit",
            ]
        ),
    ]
)
