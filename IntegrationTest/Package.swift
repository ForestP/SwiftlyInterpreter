// swift-tools-version: 6.2
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "IntegrationTest",
    platforms: [
        .iOS(.v17),
        .macOS(.v13)
    ],
    products: [
        // Products define the executables and libraries a package produces, making them visible to other packages.
        .library(
            name: "IntegrationTest",
            targets: ["IntegrationTest"]
        ),
        .executable(
            name: "Integration",
            targets: ["Integration"]
        ),
    ],
    dependencies: [
        .package(path: "../InterpreterModels"),
        .package(path: "../InterpreterCompiler"),
        .package(path: "../InterpreterVM")
    ],
    targets: [
        // Targets are the basic building blocks of a package, defining a module or a test suite.
        // Targets can depend on other targets in this package and products from dependencies.
        .target(
            name: "IntegrationTest",
            dependencies: [
                "InterpreterModels",
                "InterpreterCompiler",
                "InterpreterVM"
            ]
        ),
        .executableTarget(
            name: "Integration",
            dependencies: [
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
                ])
            ]
        ),
        .testTarget(
            name: "IntegrationTestTests",
            dependencies: [
//                "IntegrationTest",
                "InterpreterModels",
                "InterpreterCompiler",
                "InterpreterVM"
            ]
        ),
    ]
)
