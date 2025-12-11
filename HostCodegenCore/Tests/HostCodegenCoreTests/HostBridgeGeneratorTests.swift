//
//  HostBridgeGeneratorTests.swift
//  HostCodegenCore
//
//  Created by Forest Plasencia on 9/19/25.
//

import XCTest
@testable import HostSurfaceKit
@testable import HostCodegenCore

final class HostBridgeGeneratorTests: XCTestCase {

    func testGeneratesRendersFromApiSurface() throws {
        let source = """
        public struct Foo {
            public init()
            public func latest() -> Foo
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        XCTAssertTrue(resolveResult.diagnostics.isEmpty)

        let generator = HostBridgeGenerator()
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.methodDescriptors.count, 2)
        XCTAssertEqual(result.methodArtifacts.count, 2)
        let selectors = result.methodDescriptors.map { $0.selector }
        XCTAssertTrue(selectors.contains("init()"))
        XCTAssertTrue(selectors.contains("latest()"))
    }

    func testGenericMethodRequiresSpecializations() throws {
        let source = """
        public struct Foo {
            public func wrap<T>(_ value: T) -> T
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        XCTAssertTrue(resolveResult.diagnostics.isEmpty)

        let generator = HostBridgeGenerator()
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertEqual(result.methodDescriptors.count, 1)
        XCTAssertTrue(result.methodArtifacts.isEmpty)
        XCTAssertEqual(result.diagnostics.count, 1)
//        XCTAssertTrue(result.diagnostics[0].description.contains("Missing specialization"))
    }

    func testGenericMethodUsesConfigSpecializations() throws {
        let source = """
        public struct Foo {
            public func wrap<T>(_ value: T) -> T
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        XCTAssertTrue(resolveResult.diagnostics.isEmpty)

        let config = HostBridgeGenerationConfig(specializationsByType: [
            "Demo.Foo": [
                "T": [try TypeNameParser.parse("Swift.Int"), try TypeNameParser.parse("Swift.String")]
            ]
        ])
        let generator = HostBridgeGenerator(config: config)
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.methodDescriptors.count, 1)
        XCTAssertEqual(result.methodArtifacts.count, 1)
        XCTAssertEqual(result.methodArtifacts.first?.render.thunks.count, 2)
    }

    func testAutoStrategyProvidesDomainsWhenConfigMissing() throws {
        let source = """
        public struct Foo {
            public func wrap<T>(_ value: T) -> T
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        XCTAssertTrue(resolveResult.diagnostics.isEmpty)

        let typeName = try TypeNameParser.parse("Swift.Int")
        let strategy = AutoSpecializationStrategyStub(identifier: "alias",
                                                      methodDomainsByKey: [
                                                        "Demo.Foo::wrap(_:)": ["T": [typeName]]
                                                      ])

        let generator = HostBridgeGenerator(autoSpecializationStrategies: [strategy])
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.methodArtifacts.count, 1)
        XCTAssertEqual(result.methodArtifacts.first?.render.thunks.count, 1)
        XCTAssertEqual(result.methodArtifacts.first?.origin, .auto(strategyIdentifier: "alias"))
    }

    func testTypeAliasStrategyInfersDomains() throws {
        let source = """
        public struct Box<Value> {
            public init(value: Value)
            public func current() -> Value
            public var stored: Value { get }
        }
        public typealias StringBox = Box<Swift.String>
        public typealias IntBox = Box<Swift.Int>
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        XCTAssertEqual(resolveResult.typeAliases.count, 2)

        let strategy = TypeAliasDomainStrategy(typeAliases: resolveResult.typeAliases)
        let generator = HostBridgeGenerator(autoSpecializationStrategies: [strategy])
        let result = generator.generate(from: resolveResult.apiSurface)

        XCTAssertTrue(result.diagnostics.isEmpty)

        guard let propertyArtifact = result.propertyArtifacts.first(where: { $0.descriptor.name == "stored" }) else {
            return XCTFail("Missing stored property artifact")
        }
        XCTAssertEqual(propertyArtifact.origin, .auto(strategyIdentifier: "alias"))
        XCTAssertEqual(propertyArtifact.render.getter.thunks.count, 2)
    }

    func testPaletteStrategyProvidesDomains() throws {
        let source = """
        public struct Foo {
            public func wrap<T>(_ value: T) -> T
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        XCTAssertTrue(resolveResult.diagnostics.isEmpty)

        let strategy = PaletteSpecializationStrategy(kind: .standard)
        let generator = HostBridgeGenerator(autoSpecializationStrategies: [strategy])
        let result = generator.generate(from: resolveResult.apiSurface)

        XCTAssertTrue(result.diagnostics.isEmpty)
        guard let artifact = result.methodArtifacts.first(where: { $0.descriptor.selector == "wrap(_:)" }) else {
            return XCTFail("Missing wrap(_:) artifact")
        }
        XCTAssertEqual(artifact.origin, .auto(strategyIdentifier: "palette.standard"))
        XCTAssertEqual(artifact.render.thunks.count, 6)
    }

    func testPaletteStrategyUsesNeverForErrorTypedThrows() throws {
        let source = """
        public struct Foo {
            public func typed<T, E>(_ closure: (Swift.Int) throws(E) -> T) throws(E) -> T where E: Swift.Error
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        XCTAssertTrue(resolveResult.diagnostics.isEmpty)

        let strategy = PaletteSpecializationStrategy(kind: .standard)
        let generator = HostBridgeGenerator(autoSpecializationStrategies: [strategy])
        let result = generator.generate(from: resolveResult.apiSurface)

        XCTAssertTrue(result.diagnostics.isEmpty)
        guard let artifact = result.methodArtifacts.first(where: { $0.descriptor.selector == "typed(_:)" }) else {
            return XCTFail("Missing typed(_:) artifact")
        }
        XCTAssertEqual(artifact.origin, .auto(strategyIdentifier: "palette.standard"))
        XCTAssertEqual(artifact.domainSizes["E"], 1)
        XCTAssertEqual(artifact.domainSizes["T"], 6)
        XCTAssertEqual(artifact.render.thunks.count, 6)
    }

    func testPaletteStrategyProvidesDomainsForSet() throws {
        let strategy = PaletteSpecializationStrategy(kind: .standard)
        let receiver = try TypeNameParser.parse("Swift.Set<Element>")
        let elementType = try TypeNameParser.parse("Swift.Set<Element>.Element")
        let param = HostMethodParameter(label: nil,
                                        name: "member",
                                        type: elementType,
                                        isInout: false,
                                        isVariadic: false)
        let descriptor = HostMethodDescriptor(receiver: receiver,
                                              kind: .instance(isMutating: false),
                                              baseName: "contains",
                                              selector: "contains(_:)",
                                              displayName: "Set.contains(_:)",
                                              returnType: try TypeNameParser.parse("Swift.Bool"),
                                              parameters: [param],
                                              genericParameters: [GenericParameter(name: "Element", origin: .receiver)],
                                              requirements: [.conformsTo(param: "Element", protocolName: "Swift.Hashable")],
                                              conformanceTable: [:],
                                              throwsKind: .none,
                                              availability: [])

        let domains = strategy.methodDomains(for: descriptor, surface: ApiSurface(types: []))
        guard let elementDomain = domains["Element"] else {
            return XCTFail("Missing domain for Element")
        }
        let descriptions = Set(elementDomain.map { $0.canonicalDescription() })
        XCTAssertEqual(descriptions, Set([
            "Foundation.Data",
            "Swift.Bool",
            "Swift.Double",
            "Swift.Int",
            "Swift.String",
            "Swift.UInt8"
        ]))
    }

    func testHostRegistrationStrategyInfersDomains() throws {
        let source = """
        public struct Box<Value> {
            public var stored: Value { get }
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()

        let strategy = HostRegistrationDomainStrategy(registeredTypeNames: [
            "Demo.Box<Swift.String>",
            "Demo.Box<Swift.Int>"
        ])
        let generator = HostBridgeGenerator(autoSpecializationStrategies: [strategy])
        let result = generator.generate(from: resolveResult.apiSurface)

        XCTAssertTrue(result.diagnostics.isEmpty)
        guard let prop = result.propertyArtifacts.first(where: { $0.descriptor.name == "stored" }) else {
            return XCTFail("Missing stored property artifact")
        }
        XCTAssertEqual(prop.origin, .auto(strategyIdentifier: "registrations"))
        XCTAssertEqual(prop.render.getter.thunks.count, 2)
    }

    func testWhereClauseRespectsConformanceTable() throws {
        let source = """
        public struct Foo {
            public func hashable<T>(_ value: T) -> Bool where T: Swift.Hashable
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        XCTAssertTrue(resolveResult.diagnostics.isEmpty)

        let config = HostBridgeGenerationConfig(
            specializationsByType: [
                "Demo.Foo": [
                    "T": [try TypeNameParser.parse("Swift.Int"), try TypeNameParser.parse("Foundation.Data")]
                ]
            ],
            conformanceTable: [
                "Swift.Int": Set(["Swift.Hashable"]),
                "Foundation.Data": Set<String>()
            ]
        )

        let generator = HostBridgeGenerator(config: config)
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.methodDescriptors.count, 1)
        XCTAssertEqual(result.methodDescriptors.first?.requirements, [.conformsTo(param: "T", protocolName: "Swift.Hashable")])
        XCTAssertEqual(result.methodArtifacts.count, 1)
        XCTAssertEqual(result.methodArtifacts.first?.render.thunks.count, 1)
    }

    func testDomainCapTrimsSpecializations() throws {
        let source = """
        public struct Foo {
            public func wrap<T>(_ value: T) -> T
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        XCTAssertTrue(resolveResult.diagnostics.isEmpty)

        let config = HostBridgeGenerationConfig(
            specializationsByType: [
                "Demo.Foo": [
                    "T": [
                        try TypeNameParser.parse("Swift.Int"),
                        try TypeNameParser.parse("Swift.String"),
                        try TypeNameParser.parse("Foundation.Date")
                    ]
                ]
            ],
            maxDomainPerGenericParam: 2
        )

        let generator = HostBridgeGenerator(config: config)
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertEqual(result.methodArtifacts.count, 1)
        XCTAssertEqual(result.methodArtifacts.first?.render.thunks.count, 3)
        XCTAssertTrue(result.diagnostics.contains { $0.description.contains("Auto-expanded specialization domain") })
    }

    func testSpecializationCapTrimsCombinationCount() throws {
        let source = """
        public struct Foo {
            public func pair<A, B>(_ lhs: A, _ rhs: B)
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()

        let config = HostBridgeGenerationConfig(
            specializationsByType: [
                "Demo.Foo": [
                    "A": [try TypeNameParser.parse("Swift.Int"), try TypeNameParser.parse("Swift.String")],
                    "B": [try TypeNameParser.parse("Swift.Int"), try TypeNameParser.parse("Swift.String")]
                ]
            ],
            maxSpecializationsPerSelector: 2
        )

        let generator = HostBridgeGenerator(config: config)
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertEqual(result.methodArtifacts.count, 1)
        XCTAssertEqual(result.methodArtifacts.first?.render.thunks.count, 4)
        XCTAssertNil(result.methodArtifacts.first?.render.fallback)
        XCTAssertTrue(result.diagnostics.contains { $0.description.contains("Auto-expanded specialization combinations") })
    }

    func testForceSpecializationsSkipsCaps() throws {
        let source = """
        public struct Foo {
            public func wrap<T>(_ value: T) -> T
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()

        let config = HostBridgeGenerationConfig(
            specializationsByType: [
                "Demo.Foo": [
                    "T": [
                        try TypeNameParser.parse("Swift.Int"),
                        try TypeNameParser.parse("Swift.String"),
                        try TypeNameParser.parse("Foundation.Date")
                    ]
                ]
            ],
            maxSpecializationsPerSelector: 1,
            maxDomainPerGenericParam: 2,
            forceSpecializations: ["Demo.Foo": ["wrap(_:)"]]
        )

        let generator = HostBridgeGenerator(config: config)
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertEqual(result.methodArtifacts.count, 1)
        XCTAssertEqual(result.methodArtifacts.first?.render.thunks.count, 3)
        XCTAssertFalse(result.diagnostics.contains { $0.description.contains("Auto-expanded specialization domain") })
        XCTAssertFalse(result.diagnostics.contains { $0.description.contains("Auto-expanded specialization combinations") })
    }

    func testOptionalParametersAndReturnsRenderNilHandling() throws {
        let source = """
        public struct Foo {
            public init?(id: Swift.Int)
            public func maybeName() -> Swift.String?
            public func takeOptional(_ value: Swift.Int?)
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        XCTAssertTrue(resolveResult.diagnostics.isEmpty)

        let generator = HostBridgeGenerator()
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertTrue(result.diagnostics.isEmpty)

        guard let initArtifact = result.methodArtifacts.first(where: { $0.descriptor.selector == "init(id:)" }) else {
            return XCTFail("Missing init(id:) artifact")
        }
        guard let initThunk = initArtifact.render.thunks.first else {
            return XCTFail("Missing init(id:) thunk")
        }
        XCTAssertTrue(initThunk.source.contains("switch result"))
        XCTAssertTrue(initThunk.source.contains("return .nilValue"))

        guard let maybeNameArtifact = result.methodArtifacts.first(where: { $0.descriptor.selector == "maybeName()" }) else {
            return XCTFail("Missing maybeName() artifact")
        }
        guard let maybeNameThunk = maybeNameArtifact.render.thunks.first else {
            return XCTFail("Missing maybeName() thunk")
        }
        XCTAssertTrue(maybeNameThunk.source.contains("switch result"))
        XCTAssertTrue(maybeNameThunk.source.contains("return .nilValue"))

        guard let takeOptionalArtifact = result.methodArtifacts.first(where: { $0.descriptor.selector == "takeOptional(_:)" }) else {
            return XCTFail("Missing takeOptional(_:) artifact")
        }
        guard let takeOptionalThunk = takeOptionalArtifact.render.thunks.first else {
            return XCTFail("Missing takeOptional(_:) thunk")
        }
        XCTAssertTrue(takeOptionalThunk.source.contains("var value: Swift.Int? = nil"))
        XCTAssertTrue(takeOptionalThunk.source.contains("if case .nilValue = args[1]"))
    }

    func testGeneratesPropertyArtifacts() throws {
        let source = """
        public struct Foo {
            public var value: Swift.Int { get }
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        XCTAssertTrue(resolveResult.diagnostics.isEmpty)

        let generator = HostBridgeGenerator()
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.propertyDescriptors.count, 1)
        XCTAssertEqual(result.propertyArtifacts.count, 1)
        XCTAssertEqual(result.propertyArtifacts.first?.render.getter.thunks.count, 1)
        XCTAssertNil(result.propertyArtifacts.first?.render.setter)
    }

    func testAutoStrategyProvidesPropertyDomainsWhenConfigMissing() throws {
        let source = """
        public struct Box<Value> {
            public var item: Value { get }
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()

        let typeName = try TypeNameParser.parse("Swift.String")
        let strategy = AutoSpecializationStrategyStub(identifier: "alias",
                                                      propertyDomainsByKey: [
                                                        "Demo.Box<Value>::instance::item": ["Value": [typeName]]
                                                      ])

        let generator = HostBridgeGenerator(autoSpecializationStrategies: [strategy])
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.propertyArtifacts.count, 1)
        XCTAssertEqual(result.propertyArtifacts.first?.render.getter.thunks.count, 1)
        XCTAssertEqual(result.propertyArtifacts.first?.origin, .auto(strategyIdentifier: "alias"))
    }

    func testGeneratesSettablePropertyArtifacts() throws {
        let source = """
        public struct Foo {
            public var value: Swift.Int { get set }
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()
        XCTAssertTrue(resolveResult.diagnostics.isEmpty)

        let generator = HostBridgeGenerator()
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.propertyDescriptors.count, 1)
        XCTAssertEqual(result.propertyArtifacts.count, 1)
        guard let artifact = result.propertyArtifacts.first else {
            return XCTFail("Missing property artifact")
        }
        XCTAssertEqual(artifact.render.getter.thunks.count, 1)
        guard let setterRender = artifact.render.setter else {
            return XCTFail("Missing setter render")
        }
        XCTAssertEqual(setterRender.thunks.count, 1)
        XCTAssertTrue(setterRender.thunks.first?.source.contains("return .nilValue") == true)
    }

    func testGenericPropertyRequiresSpecializations() throws {
        let source = """
        public struct Box<Value> {
            public var item: Value { get }
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()

        let generator = HostBridgeGenerator()
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertEqual(result.propertyDescriptors.count, 1)
        XCTAssertTrue(result.propertyArtifacts.isEmpty)
        XCTAssertTrue(result.diagnostics.contains { $0.description.contains("Missing property specialization config for Demo.Box<Value>.item") })
    }

    func testGenericPropertyUsesConfig() throws {
        let source = """
        public struct Box<Value> {
            public var item: Value { get set }
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()

        let config = HostBridgeGenerationConfig(
            propertySpecializationsByType: [
                "Demo.Box<Value>": [
                    "instance::item": [
                        "Value": [
                            try TypeNameParser.parse("Swift.Int"),
                            try TypeNameParser.parse("Swift.String")
                        ]
                    ]
                ]
            ]
        )
        let generator = HostBridgeGenerator(config: config)
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.propertyArtifacts.count, 1)
        guard let artifact = result.propertyArtifacts.first else {
            return XCTFail("Missing property artifact")
        }
        XCTAssertEqual(artifact.render.getter.thunks.count, 2)
        guard let setterRender = artifact.render.setter else {
            return XCTFail("Missing setter render")
        }
        XCTAssertEqual(setterRender.thunks.count, 2)
    }

    func testStaticPropertyGeneratesMetatypeThunks() throws {
        let source = """
        public struct Clock {
            public static var now: Swift.Int { get set }
        }
        """

        var pipeline = HostSurfacePipeline(moduleName: "Demo", interfaceContents: source)
        let resolveResult = try pipeline.run()

        let generator = HostBridgeGenerator()
        let result = generator.generate(from: resolveResult.apiSurface)
        XCTAssertTrue(result.diagnostics.isEmpty)
        XCTAssertEqual(result.propertyArtifacts.count, 1)
        guard let artifact = result.propertyArtifacts.first else {
            return XCTFail("Missing property artifact")
        }
        let getterSource = artifact.render.getter.thunks.first?.source ?? ""
        XCTAssertTrue(getterSource.contains("receiver must be Clock metatype"), getterSource)
        guard let setterSource = artifact.render.setter?.thunks.first?.source else {
            return XCTFail("Missing setter source")
        }
        XCTAssertTrue(setterSource.contains("receiver must be Clock metatype"), setterSource)
    }
}
