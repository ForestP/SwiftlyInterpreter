// swift-tools-version: 6.2
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "Swiftly",
    platforms: [
        .iOS(.v17),
        .macOS(.v15)
    ],
    products: [
        // Products define the executables and libraries a package produces, making them visible to other packages.
        .library(
            name: "Swiftly",
            targets: ["Swiftly"]
        ),
        .executable(
            name: "SwiftlyExecutable",
            targets: ["SwiftlyExecutable"]
        ),
    ],
    dependencies: [
        .package(path: "../InterpreterModels"),
        .package(path: "../InterpreterCompiler"),
        .package(path: "../InterpreterVM"),
        .package(path: "../HostSurfaceKit")
    ],
    targets: [
        // Targets are the basic building blocks of a package, defining a module or a test suite.
        // Targets can depend on other targets in this package and products from dependencies.
        .target(
            name: "Swiftly",
            dependencies: [
                "InterpreterModels",
                "InterpreterCompiler",
                "InterpreterVM"
            ],
            exclude: [
                // Generated metadata artifacts; not source files
                "./HostBridges/bridgegen.log",
//                "./HostBridges/bridgegen-surface.json"
            ],
            resources: [
                .copy("../../Sources/Swiftly/HostBridges/bridgegen-foundation-surface.json"),
                .copy("../../Sources/Swiftly/HostBridges/bridgegen-foundation-inventory.json"),
                .copy("../../Sources/Swiftly/HostBridges/bridgegen-swiftstdlib-surface.json"),
                .copy("../../Sources/Swiftly/HostBridges/bridgegen-swiftstdlib-inventory.json")
            ]
        ),
        .executableTarget(
            name: "SwiftlyExecutable",
            dependencies: [
                "Swiftly",
                "InterpreterModels",
                "InterpreterCompiler",
                "InterpreterVM"
            ],
            linkerSettings: [
                .unsafeFlags([
//                    "-Xlinker", "--export=memory",
                    //                "-Xlinker", "--export=__heap_base",
//                    "-Xlinker", "--export=compile_program_json",
//                    "-Xlinker", "--export=compile_program_text",
                    "-Xlinker", "--export=run_program"
//                ], .when(platforms: [.wasi]))
                ], .when(platforms: [.wasi]))
            ]
        ),
        .testTarget(
            name: "SwiftlyTests",
            dependencies: [
                "Swiftly",
                "InterpreterModels",
                "InterpreterCompiler",
                "InterpreterVM",
                "HostSurfaceKit"
            ],
            resources: [
                .copy("../../Sources/Swiftly/HostBridges/bridgegen-foundation-surface.json"),
                .copy("../../Sources/Swiftly/HostBridges/bridgegen-foundation-inventory.json"),
                .copy("../../Sources/Swiftly/HostBridges/bridgegen-swiftstdlib-surface.json"),
                .copy("../../Sources/Swiftly/HostBridges/bridgegen-swiftstdlib-inventory.json")
            ]
        ),
    ]
)
