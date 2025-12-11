//
//  HostBridgeSpecializationOrigin.swift
//  HostCodegenCore
//
//  Created by Codex on 5/1/24.
//

enum HostBridgeSpecializationOrigin: Equatable {
    case notApplicable
    case config
    case auto(strategyIdentifier: String)
    case none

    var reportLabel: String {
        switch self {
        case .notApplicable:
            return "not_applicable"
        case .config:
            return "config"
        case .auto(let identifier):
            return "auto.\(identifier)"
        case .none:
            return "none"
        }
    }
}
