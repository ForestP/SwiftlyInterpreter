import XCTest
import HostSurfaceKit
@testable import HostCodegenCore

final class FloatingPointFormatStyleFilterTests: XCTestCase {
    func testGeneratesFormatThunkAcceptingDouble() throws {
        // Build a minimal ApiSurface that includes a FloatingPointFormatStyle<T> receiver
        let receiverName = "Foundation.FloatingPointFormatStyle<T>"
        let method = ResolvedInstanceMethod(
            signature: ResolvedCallable(
                selector: "format(_:)",
                baseName: "format",
                returnType: try TypeNameParser.parse("Swift.String"),
                parameters: [ResolvedParameter(label: nil, name: "value", type: TypeName(path: ["T"]), isInout: false, isVariadic: false)],
                genericParameters: [],
                whereClause: nil,
                availability: [],
                attributes: [],
                throwsKind: .none,
                isAsync: false,
                sourceLine: 1
            ),
            isMutating: false
        )
        let surface = ApiSurface(types: [
            HostTypeSurface(canonicalName: receiverName, availability: [], members: [ .instanceMethod(method) ])
        ])

        // Provide a valid specialization domain for T so generation would normally proceed
        let config = HostBridgeGenerationConfig(
            specializationsByType: [ receiverName: [ "T": [ try TypeNameParser.parse("Swift.Double") ] ] ],
            propertySpecializationsByType: [:],
            conformanceTable: [:]
        )
        let generator = HostBridgeGenerator(config: config, autoSpecializationStrategies: [])
        let result = generator.generate(from: surface)
        XCTAssertFalse(result.methodArtifacts.isEmpty)
        let sources = result.methodArtifacts.flatMap { $0.render.thunks.map { $0.source } } +
                      result.propertyArtifacts.flatMap { $0.render.getter.thunks.map { $0.source } }
        let joined = sources.joined(separator: "\n")
        // Ensure we do not emit invalid type arguments with Swift.Any in style generics
        XCTAssertFalse(joined.contains("FloatingPointFormatStyle<Swift.Any>"), joined)
    }
}
