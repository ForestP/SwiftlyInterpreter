//
//  SwiftlyIntegrationTests.swift
//  Swiftly
//
//  Created by Forest Plasencia on 10/9/25.
//

import XCTest
import Foundation
import Swiftly
import InterpreterCompiler
import InterpreterVM
import HostSurfaceKit

@MainActor
final class SwiftlyIntegrationTests: XCTestCase {
    
    static let generatedAPISurface = loadGeneratedApiSurface()

    // Load actual generated API surface from bridgegen output
    private static func loadGeneratedApiSurface() -> (ApiSurface, [String: [String]])? {
#if os(WASI)
        let surfaceData = SwiftStdlibHostBridgeEmbeddedResources.surfaceJSONData
#else
        guard let surfaceURL = Bundle.module.url(forResource: "bridgegen-swiftstdlib-surface", withExtension: "json") else {
            print("⚠️  Generated surface file not found in test bundle")
            return nil
        }

        guard let surfaceData = try? Data(contentsOf: surfaceURL) else {
            print("⚠️  Failed to read data from: \(surfaceURL.path)")
            return nil
        }
#endif

        let apiSurface: ApiSurface
        do {
            apiSurface = try ApiSurfaceJSONDecoder().decode(surfaceData)
        } catch {
            print("⚠️  Failed to decode API surface: \(error)")
            return nil
        }

#if os(WASI)
        let inventoryData = SwiftStdlibHostBridgeEmbeddedResources.inventoryJSONData
#else
        guard let inventoryURL = Bundle.module.url(forResource: "bridgegen-swiftstdlib-inventory", withExtension: "json") else {
            print("⚠️  Generated inventory file not found in test bundle")
            return nil
        }

        let inventoryData: Data
        do {
            inventoryData = try Data(contentsOf: inventoryURL)
        } catch {
            print("⚠️  Failed to read inventory data: \(error)")
            return nil
        }
#endif

        let inventory: [String: [String]]
        do {
            inventory = try JSONDecoder().decode([String: [String]].self, from: inventoryData)
        } catch {
            print("⚠️  Failed to decode inventory JSON: \(error)")
            return nil
        }

        return (apiSurface, inventory)
    }

    // MARK: - Surface Assertions

    @MainActor
    private func assertSurface(_ surface: ApiSurface, contains selector: String, on typeName: String, file: StaticString = #file, line: UInt = #line) {
        guard let typeSurface = surface.types.first(where: { $0.canonicalName == typeName }) else {
            XCTFail("Missing type \(typeName) in generated API surface", file: file, line: line)
            return
        }
        let hasSelector = typeSurface.members.contains { member in
            switch member {
            case .instanceMethod(let method):
                return method.signature.selector == selector
            case .staticMethod(let method):
                return method.signature.selector == selector
            case .initializer(let initializer):
                return initializer.signature.selector == selector
            case .instanceProperty(let property):
                return property.name == selector
            case .staticProperty(let property):
                return property.name == selector
            }
        }
        XCTAssertTrue(hasSelector, "Missing selector \(selector) for \(typeName)", file: file, line: line)
    }

    @MainActor
    func testGeneratedSurfaceContainsExpectedSelectors() throws {
        guard let (surface, _) = Self.generatedAPISurface else {
            throw XCTSkip("Generated API surface not available - run gen_swiftstdlib_bridges.sh first")
        }

        // Array
        assertSurface(surface, contains: "append(_:)", on: "Swift.Array<Element>")
        assertSurface(surface, contains: "remove(at:)", on: "Swift.Array<Element>")
        assertSurface(surface, contains: "removeLast()", on: "Swift.Array<Element>")
        assertSurface(surface, contains: "popLast()", on: "Swift.Array<Element>")

        // Dictionary
        assertSurface(surface, contains: "updateValue(_:forKey:)", on: "Swift.Dictionary<Key,Value>")
        assertSurface(surface, contains: "removeValue(forKey:)", on: "Swift.Dictionary<Key,Value>")
        assertSurface(surface, contains: "merge(_:uniquingKeysWith:)", on: "Swift.Dictionary<Key,Value>")
        assertSurface(surface, contains: "merging(_:uniquingKeysWith:)", on: "Swift.Dictionary<Key,Value>")

        // Set
        assertSurface(surface, contains: "insert(_:)", on: "Swift.Set<Element>")
        assertSurface(surface, contains: "remove(_:)", on: "Swift.Set<Element>")
        assertSurface(surface, contains: "union(_:)", on: "Swift.Set<Element>")
        assertSurface(surface, contains: "intersection(_:)", on: "Swift.Set<Element>")

        // Optional
        assertSurface(surface, contains: "map(_:)", on: "Swift.Optional<Wrapped>")
        assertSurface(surface, contains: "flatMap(_:)", on: "Swift.Optional<Wrapped>")
        assertSurface(surface, contains: "filter(_:)", on: "Swift.Optional<Wrapped>")

        // String
        assertSurface(surface, contains: "uppercased()", on: "Swift.String")
        assertSurface(surface, contains: "lowercased()", on: "Swift.String")
        assertSurface(surface, contains: "hasPrefix(_:)", on: "Swift.String")
        assertSurface(surface, contains: "hasSuffix(_:)", on: "Swift.String")
        assertSurface(surface, contains: "contains(_:)", on: "Swift.String")
    }

    private func run(_ src: String, file: String = "test.swift") throws -> String {
        guard let (apiSurface, inventory) = Self.generatedAPISurface else {
            throw XCTSkip("Generated API surface not available - run gen_swiftstdlib_bridges.sh first")
        }
        let c = Compiler(hostSurface: apiSurface, genericParameterInventory: inventory)
        let p = try c.compileProgram(source: src, fileName: file)
        var vm = VM()
        let _ = foundationHostBridgeRegistration
        let _ = swiftStdlibHostBridgeRegistration
        try vm.run(p)
        return vm.output
    }

    private func runSlot(_ src: String, file: String = "test.swift") throws -> String {
        guard let (apiSurface, inventory) = Self.generatedAPISurface else {
            throw XCTSkip("Generated API surface not available - run gen_swiftstdlib_bridges.sh first")
        }
        let c = Compiler(options: .init(mode: .slot), hostSurface: apiSurface, genericParameterInventory: inventory)
        let p = try c.compileProgram(source: src, fileName: file)
        var vm = VM()
        let _ = foundationHostBridgeRegistration
        let _ = swiftStdlibHostBridgeRegistration
        try vm.run(p)
        return vm.output
    }
    
    func testComplexExample() throws {
        let src = """
        func nextGreaterPermutation(_ n: Int) -> Int? {
            // Reject negatives per prompt
            guard n >= 0 else { return nil }
        
            // Convert to digits (most significant first)
            func toDigits(_ x: Int) -> [Int] {
                if x == 0 { return [0] }
                var x = x
                var rev: [Int] = []
                while x > 0 {
                    rev.append(x % 10)
                    x /= 10
                }
                return rev.reversed()
            }
        
            // Safely rebuild Int from digits with overflow check
            func fromDigits(_ digits: [Int]) -> Int? {
                var result = 0
                for d in digits {
                    guard (0...9).contains(d) else { return nil }
                    // Overflow guard: result*10 + d <= Int.max
                    if result > (Int.max - d) / 10 { return nil }
                    result = result * 10 + d
                }
                return result
            }
        
            var digits = toDigits(n)
        
            // Single digit -> no greater permutation
            if digits.count <= 1 { return nil }
        
            // 1) Find pivot i where digits[i] < digits[i+1], scanning from right
            var i = digits.count - 2
            while i >= 0 && digits[i] >= digits[i + 1] {
                i -= 1
            }
            if i < 0 { return nil } // already max permutation
        
            // 2) Find rightmost j > i with digits[j] > digits[i] (smallest such)
            var j = digits.count - 1
            while j > i && digits[j] <= digits[i] {
                j -= 1
            }
        
            // 3) Swap
            digits.swapAt(i, j)
        
            // 4) Reverse suffix i+1...end to make it minimal
            var left = i + 1, right = digits.count - 1
            while left < right {
                digits.swapAt(left, right)
                left += 1
                right -= 1
            }
        
            // 5) Rebuild and overflow-check
            return fromDigits(digits)
        }
        
        print(nextGreaterPermutation(3456))
        """
        
        let out = try run(src)
        XCTAssertEqual(out, "3465\n")
    }
    
    func testCommonCases() throws {
        let integer: Int = 1000
        let intString: String = String(integer)
        
        let src = """
        let integer: Int = 1000
        let intString: String = String(integer)
        
        print(intString)
        """
        
        XCTAssertEqual(try run(src), "\(intString)\n")
        XCTAssertEqual(try runSlot(src), "\(intString)\n")
    }
    
    func testStringInitializerLosslessConversions() throws {
        let src = """
        print(String(-42))
        print(String(0))
        print(String(42))
        print(String(3.5))
        print(String(true))
        """
        
        let expected = "-42\n0\n42\n3.5\ntrue\n"
        XCTAssertEqual(try run(src), expected)
        XCTAssertEqual(try runSlot(src), expected)
    }
    
    
    func testParameterShadowingAllowsRedeclaration() throws {
        let src = """
        func increment(_ value: Int) -> Int {
            var value = value
            value += 1
            return value
        }
        
        print(increment(7))
        """
        
        XCTAssertEqual(try run(src), "8\n")
        XCTAssertEqual(try runSlot(src), "8\n")
    }
    
    func testArrayDoubleHostInterop() throws {
        let src = """
        let values: [Double] = [1.5, 2.0, 3.5]
        let doubled = values.map { $0 * 2.0 }
        let total = doubled.reduce(0.0, +)
        let filtered = doubled.filter { $0 > 5.0 }.map { String($0) }.joined(separator: ",")
        print(filtered)
        print(total == 14.0)
        """
        
        let expected = "7.0\ntrue\n"
        XCTAssertEqual(try run(src), expected)
        XCTAssertEqual(try runSlot(src), expected)
    }
    
    func testDictionaryBoolAndDoubleHostInterop() throws {
        let src = """
        var metrics: [String: Double] = [:]
        metrics["pi"] = 3.14159
        metrics["tau"] = 6.28318
        let total = metrics.reduce(0.0) { $0 + $1.value }
        print(total > 9.42 && total < 9.43)
        let sortedKeys = metrics.keys.sorted().joined(separator: ",")
        print(sortedKeys)
        var toggles: [String: Bool] = ["feature": false]
        toggles["feature"] = true
        print(toggles["feature"] ?? false)
        """
        
        let expected = "true\npi,tau\ntrue\n"
        XCTAssertEqual(try run(src), expected)
        XCTAssertEqual(try runSlot(src), expected)
    }

    func testSetStringHostInterop() throws {
        let src = """
        var tags = Set<String>()
        tags.insert("swift")
        tags.insert("wasm")
        let sorted = tags.sorted().joined(separator: ",")
        print(sorted)
        print(tags.contains("swift"))
        let unioned = tags.union(["bridge"])
        print(unioned.contains("bridge"))
        """

        let expected = "swift,wasm\ntrue\ntrue\n"
        XCTAssertEqual(try run(src), expected)
        XCTAssertEqual(try runSlot(src), expected)
    }

    func testArrayMutatingHostInterop() throws {
        let src = """
        var numbers: [Int] = [1, 2, 3]
        numbers.append(4)
        let removed = numbers.remove(at: 1)
        let popped = numbers.popLast() ?? -1
        print(numbers.map(String.init).joined(separator: ","))
        print(removed)
        print(popped)
        """

        let expected = "1,3\n2\n4\n"
        XCTAssertEqual(try run(src), expected)
        XCTAssertEqual(try runSlot(src), expected)
    }

    func testDictionaryUpdateAndRemoveHostInterop() throws {
        let src = """
        var counts: [String: Int] = ["apples": 1]
        let previous = counts.updateValue(2, forKey: "apples") ?? -1
        counts.updateValue(3, forKey: "bananas")
        let removed = counts.removeValue(forKey: "bananas") ?? -1
        print(previous)
        print(removed)
        print(counts.count)
        """

        let expected = "1\n3\n1\n"
        XCTAssertEqual(try run(src), expected)
        XCTAssertEqual(try runSlot(src), expected)
    }

    func testDictionaryMergeHostInterop() throws {
        let src = """
        var inventory: [String: Int] = ["apples": 2]
        inventory.merge([("bananas", 3), ("apples", 1)]) { current, new in current + new }
        let merged = inventory.merging([("grapes", 4)]) { current, new in max(current, new) }
        let constructed = Dictionary(uniqueKeysWithValues: [("limes", 5), ("lemons", 6)])
        print(inventory.sorted { $0.key < $1.key }.map { "\\($0.key)=\\($0.value)" }.joined(separator: ","))
        print(merged.sorted { $0.key < $1.key }.map { "\\($0.key)=\\($0.value)" }.joined(separator: ","))
        print(constructed.sorted { $0.key < $1.key }.map { "\\($0.key)=\\($0.value)" }.joined(separator: ","))
        """

        let expected = "apples=3,bananas=3\napples=3,bananas=3,grapes=4\nlemons=6,limes=5\n"
        XCTAssertEqual(try run(src), expected)
        XCTAssertEqual(try runSlot(src), expected)
    }

    func testSetOperationsHostInterop() throws {
        let src = """
        var tags: Set<String> = ["swift", "bridge"]
        let removed = tags.remove("bridge") ?? "missing"
        let unioned = tags.union(["wasm", "interop"])
        let intersected = unioned.intersection(["swift", "objc"])
        print(removed)
        print(unioned.sorted().joined(separator: ","))
        print(intersected.sorted().joined(separator: ","))
        """

        let expected = "bridge\ninterop,swift,wasm\nswift\n"
        XCTAssertEqual(try run(src), expected)
        XCTAssertEqual(try runSlot(src), expected)
    }

    func testStringQueriesHostInterop() throws {
        let src = """
        let phrase = "Swiftly"
        print(phrase.uppercased())
        print(phrase.lowercased())
        print(phrase.hasPrefix("Swi"))
        print(phrase.hasSuffix("ly"))
        print(phrase.contains("ft"))
        """

        let expected = "SWIFTLY\nswiftly\ntrue\ntrue\ntrue\n"
        XCTAssertEqual(try run(src), expected)
        XCTAssertEqual(try runSlot(src), expected)
    }

}
