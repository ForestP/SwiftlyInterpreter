//
//  HostAllowlist.swift
//  InterpreterVM
//
//  Created by Forest Plasencia on 9/18/25.
//


public struct HostAllowlist {
    public var allowedTypes: Set<String>?
    public var allowedSelectorsByType: [String: Set<String>]?
    public var allowedPropertiesByType: [String: Set<String>]?
    public init(
        types: Set<String>? = nil,
        methodsByType: [String: Set<String>]? = nil,
        propertiesByType: [String: Set<String>]? = nil
    ) {
        self.allowedTypes = types
        self.allowedSelectorsByType = methodsByType
        self.allowedPropertiesByType = propertiesByType
    }
    public func isTypeAllowed(_ type: String) -> Bool {
        guard let t = allowedTypes else { return true }
        return t.contains(type)
    }
    public func isSelectorAllowed(type: String, selector: String) -> Bool {
        guard let m = allowedSelectorsByType?[type] else { return true }
        return m.contains(selector)
    }
    public func isPropertyAllowed(type: String, property: String) -> Bool {
        guard let p = allowedPropertiesByType?[type] else { return true }
        return p.contains(property)
    }
}
