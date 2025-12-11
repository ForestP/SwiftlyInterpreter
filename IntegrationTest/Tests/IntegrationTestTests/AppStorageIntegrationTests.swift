import InterpreterModels
import InterpreterCompiler
import InterpreterVM

import XCTest

final class AppStorageIntegrationTests: XCTestCase {

    private final class AppStorageBox { let key: String; var `default`: Value; init(key: String, default def: Value) { self.key = key; self.default = def } }

    func runWithAppStorage(_ src: String, file: String = "appstorage.swift") throws -> String {
        let c = Compiler()
        let p = try c.compileProgram(source: src, fileName: file)
        var vm = VM()
        var store: [String: Value] = [:]
        // Register AppStorage init(_:wrappedValue:)
        vm.registerHostMethod(type: "AppStorage", selector: "init(_:wrappedValue:)") { vm, args in
            // args: [metatype, key, default]
            guard args.count == 3 else { throw VMError.typeError("AppStorage.init arity") }
            guard case let .metatype(tid) = args[0] else { throw VMError.typeError("missing metatype receiver") }
            guard case let .string(k) = args[1] else { throw VMError.typeError("AppStorage key must be String") }
            return .host(HostRef(box: makeOpaqueBox(AppStorageBox(key: k, default: args[2]))), tid)
        }
        // Register AppStorage init(key:wrappedValue:)
        vm.registerHostMethod(type: "AppStorage", selector: "init(key:wrappedValue:)") { vm, args in
            guard args.count == 3 else { throw VMError.typeError("AppStorage.init key:wrappedValue: arity") }
            guard case let .metatype(tid) = args[0] else { throw VMError.typeError("missing metatype receiver") }
            guard case let .string(k) = args[1] else { throw VMError.typeError("AppStorage key must be String") }
            return .host(HostRef(box: makeOpaqueBox(AppStorageBox(key: k, default: args[2]))), tid)
        }
        // wrappedValue get/set
        vm.registerHostProperty(
            type: "AppStorage",
            name: "wrappedValue",
            get: { vm, args in
                try vm.withHost(args[0], typeName: "AppStorage", as: AppStorageBox.self) { box in
                    if let v = store[box.key] { return v }
                    return box.default
                }
            },
            set: { vm, args in
                guard args.count == 2 else { throw VMError.typeError("wrappedValue set arity") }
                
                return try vm.withHost(args[0], typeName: "AppStorage", as: AppStorageBox.self) { box in
                    store[box.key] = args[1]
                    return args[0]
                }
            }
        )

        try vm.run(p)
        return vm.output
    }

    func testUnlabeledKeyAndDefault() throws {
        let src = """
        struct Settings { @AppStorage("theme") var theme: String = "light" }
        var s = Settings()
        print(s.theme)
        s.theme = "dark"
        print(s.theme)
        """
        let out = try runWithAppStorage(src)
        XCTAssertEqual(out, "light\ndark\n")
    }

    func testLabeledKeyAndDefault() throws {
        let src = """
        struct Settings { @AppStorage(key: "volume") var volume: Int = 1 }
        let s = Settings()
        print(s.volume)
        """
        let out = try runWithAppStorage(src)
        XCTAssertEqual(out, "1\n")
    }
}

