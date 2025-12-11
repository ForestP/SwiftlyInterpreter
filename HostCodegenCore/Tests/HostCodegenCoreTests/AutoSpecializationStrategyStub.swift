import HostSurfaceKit
@testable import HostCodegenCore

final class AutoSpecializationStrategyStub: AutoSpecializationStrategy {
    let identifier: String
    var methodDomainsByKey: [String: [String: [TypeName]]]
    var propertyDomainsByKey: [String: [String: [TypeName]]]

    init(identifier: String,
         methodDomainsByKey: [String: [String: [TypeName]]] = [:],
         propertyDomainsByKey: [String: [String: [TypeName]]] = [:]) {
        self.identifier = identifier
        self.methodDomainsByKey = methodDomainsByKey
        self.propertyDomainsByKey = propertyDomainsByKey
    }

    func methodDomains(for descriptor: HostMethodDescriptor,
                       surface: ApiSurface) -> [String: [TypeName]] {
        let key = "\(descriptor.receiver.canonicalDescription())::\(descriptor.selector)"
        return methodDomainsByKey[key] ?? [:]
    }

    func propertyDomains(for descriptor: HostPropertyDescriptor,
                         surface: ApiSurface) -> [String: [TypeName]] {
        let kind = descriptor.kind == .static ? "static" : "instance"
        let key = "\(descriptor.receiver.canonicalDescription())::\(kind)::\(descriptor.name)"
        return propertyDomainsByKey[key] ?? [:]
    }
}
