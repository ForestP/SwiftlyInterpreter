//
//  AutoSpecializationStrategy.swift
//  HostCodegenCore
//
//  Created by Codex on 5/1/24.
//

import HostSurfaceKit

/// Provides automatic specialization domains when explicit config is missing.
public protocol AutoSpecializationStrategy {
    /// Identifier used for telemetry (e.g. "alias", "palette").
    var identifier: String { get }

    /// Returns allowed type domains for the given method descriptor.
    /// - Parameters:
    ///   - descriptor: Method descriptor requiring specialization.
    ///   - surface: Canonical API surface for contextual lookups.
    /// - Returns: Mapping of generic parameter name to allowed type names.
    func methodDomains(for descriptor: HostMethodDescriptor,
                       surface: ApiSurface) -> [String: [TypeName]]

    /// Returns allowed type domains for the given property descriptor.
    /// Default implementation returns an empty map indicating no support.
    func propertyDomains(for descriptor: HostPropertyDescriptor,
                         surface: ApiSurface) -> [String: [TypeName]]
}

public extension AutoSpecializationStrategy {
    func propertyDomains(for descriptor: HostPropertyDescriptor,
                         surface: ApiSurface) -> [String: [TypeName]] {
        [:]
    }
}
