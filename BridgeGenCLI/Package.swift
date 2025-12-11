// swift-tools-version: 6.2
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "BridgeGenCLI",
    platforms: [
        .iOS(.v17),
        .macOS(.v13)
    ],
    products: [
        // Products define the executables and libraries a package produces, making them visible to other packages.
        .library(
            name: "BridgeGenCLI",
            targets: ["BridgeGenCLI"]
        ),
        .executable(
            name: "bridgegen",
            targets: ["BridgeGenCLIRunner"]
        ),
    ],
    dependencies: [
        .package(path: "../HostSurfaceKit"),
        .package(path: "../HostCodegenCore")
    ],
    targets: [
        // Targets are the basic building blocks of a package, defining a module or a test suite.
        // Targets can depend on other targets in this package and products from dependencies.
        .target(
            name: "BridgeGenCLI",
            dependencies: [
                "HostSurfaceKit",
                "HostCodegenCore"
            ]
        ),
        .executableTarget(
            name: "BridgeGenCLIRunner",
            dependencies: [
                "BridgeGenCLI",
                "HostCodegenCore",
                "HostSurfaceKit"
            ]
        ),
        .testTarget(
            name: "BridgeGenCLITests",
            dependencies: [
                "HostSurfaceKit",
                "HostCodegenCore",
                "BridgeGenCLI"
            ]
        ),
    ]
)
