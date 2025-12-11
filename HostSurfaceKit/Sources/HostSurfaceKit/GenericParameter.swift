//
//  GenericParameter.swift
//  HostSurfaceKit
//
//  Created by Codex on 6/3/24.
//

import Foundation

/// Describes a generic parameter discovered in the host surface.
public struct GenericParameter: Equatable, Codable {
    /// Indicates where the parameter originates from.
    public enum Origin: String, Codable {
        /// Declared directly on the member (function/initializer).
        case method
        /// Declared on the receiver type and inherited by the member.
        case receiver
    }

    public let name: String
    public let origin: Origin

    public init(name: String, origin: Origin) {
        self.name = name
        self.origin = origin
    }
}
