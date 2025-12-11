public final class Obj: CustomStringConvertible, Equatable {
    public let typeName: String
    public var storage: [String: Value]
    
    public init(typeName: String, storage: [String: Value]) {
        self.typeName = typeName
        self.storage = storage
    }
    
    public var description: String {
        let fields = storage.map { "\($0): \($1)" }.sorted().joined(separator: ", ")
        return "\(typeName) { \(fields) }"
    }
    
    public static func == (lhs: Obj, rhs: Obj) -> Bool {
        ObjectIdentifier(lhs) == ObjectIdentifier(rhs)
    }
}

public struct TypeID: Hashable, Equatable, CustomStringConvertible {
    public let raw: Int
    public var description: String { "TypeID(\(raw))" }
    
    public init(raw: Int) {
        self.raw = raw
    }
}
public struct SelectorID: Hashable, Equatable, CustomStringConvertible {
    public let raw: Int
    public var description: String { "SelectorID(\(raw))" }
    
    public init(raw: Int) {
        self.raw = raw
    }
}
public struct PropertyID: Hashable, Equatable, CustomStringConvertible {
    public let raw: Int
    public var description: String { "PropertyID(\(raw))" }
    
    public init(raw: Int) {
        self.raw = raw
    }
}

public final class Box<T> {
    public var value: T
    public init(_ v: T) {
        value = v
    }
}

public struct OpaqueBox {
    fileprivate let raw: UnsafeMutableRawPointer
    fileprivate let release: (UnsafeMutableRawPointer) -> Void
    fileprivate let anyProject: (UnsafeMutableRawPointer) -> Any

    fileprivate func releaseBox() {
        release(raw)
    }

    fileprivate func project<T, R>(as _: T.Type, _ body: (inout T) throws -> R) rethrows -> R {
        try body(&Unmanaged<Box<T>>.fromOpaque(raw).takeUnretainedValue().value)
    }

    fileprivate func projectAny() -> Any {
        anyProject(raw)
    }
}

@inline(__always)
public func makeOpaqueBox<T>(_ value: T) -> OpaqueBox {
    let retained = Unmanaged.passRetained(Box(value))
    let raw = UnsafeMutableRawPointer(retained.toOpaque())
    return OpaqueBox(
        raw: raw,
        release: { rawPtr in
            Unmanaged<Box<T>>.fromOpaque(rawPtr).release()
        },
        anyProject: { rawPtr in
            Unmanaged<Box<T>>.fromOpaque(rawPtr).takeUnretainedValue().value as Any
        }
    )
}

public final class HostRef {
    fileprivate let box: OpaqueBox

    public init(box: OpaqueBox) {
        self.box = box
    }

    deinit {
        box.releaseBox()
    }
}

extension HostRef {
    public func withProjectedValue<T, R>(as type: T.Type, _ body: (inout T) throws -> R) rethrows -> R {
        try box.project(as: type, body)
    }

    public func asAny() -> Any {
        box.projectAny()
    }
}

public indirect enum Value: Equatable, CustomStringConvertible {
    case nilValue
    case int(Int)
    case uint(UInt64)
    case bool(Bool)
    case string(String)
    case double(Double)
    case object(Obj)
    case closure(Closure)
    case array(ArrBox)
    case dict(DictBox)
    case range(RangeBox)
    case host(HostRef, TypeID)
    case metatype(TypeID)
    
    public var description: String {
        switch self {
        case .nilValue: return "nil"
        case .int(let n): return "\(n)"
        case .uint(let u): return "\(u)"
        case .bool(let b): return "\(b)"
        case .string(let s): return s
        case .double(let d): return String(d)
        case .object(let o): return o.description
        case .closure(let c): return c.description
        case .array(let a): return a.description
        case .dict(let d): return d.description
        case .range(let r): return r.description
        case .host(_, let tid): return "<host \(tid.raw)>"
        case .metatype(let tid): return "<metatype #\(tid.raw)>"
        }
    }
    
    public static func == (lhs: Value, rhs: Value) -> Bool {
        switch (lhs, rhs) {
        case (.nilValue, .nilValue): return true
        case let (.int(a), .int(b)): return a == b
        case let (.uint(a), .uint(b)): return a == b
        case let (.bool(a), .bool(b)): return a == b
        case let (.string(a), .string(b)): return a == b
        case let (.double(a), .double(b)): return a == b
        case let (.object(a), .object(b)): return a == b
        case let (.closure(a), .closure(b)): return a == b
        case let (.array(a), .array(b)): return a == b
        case let (.dict(a), .dict(b)): return a == b
        case let (.range(a), .range(b)): return a == b
        case let (.host(ra, ta), .host(rb, tb)): return ta == tb && ObjectIdentifier(ra) == ObjectIdentifier(rb)
        case let (.metatype(a), .metatype(b)): return a == b
        case let (.int(a), .uint(b)):
            if a < 0 { return false }
            return UInt64(a) == b
        case let (.uint(a), .int(b)):
            if b < 0 { return false }
            return a == UInt64(b)
        case let (.double(a), .int(b)): return a == Double(b)
        case let (.int(a), .double(b)): return Double(a) == b
        case let (.double(a), .uint(b)): return a == Double(b)
        case let (.uint(a), .double(b)): return Double(a) == b
        default: return false
        }
    }
}

extension Value {
    public func expectHost() throws -> (HostRef, TypeID) {
        guard case let .host(ref, tid) = self else {
            throw VMError.typeError("value is not a host reference")
        }
        return (ref, tid)
    }

    public func expectInt(_ label: String) throws -> Int {
        guard case let .int(value) = self else {
            throw VMError.typeError("\(label) expects Int")
        }
        return value
    }

    public func expectDouble(_ label: String) throws -> Double {
        switch self {
        case .double(let value): return value
        case .int(let value): return Double(value)
        case .uint(let value): return Double(value)
        default:
            throw VMError.typeError("\(label) expects Double")
        }
    }

    public func expectBool(_ label: String) throws -> Bool {
        guard case let .bool(value) = self else {
            throw VMError.typeError("\(label) expects Bool")
        }
        return value
    }

    public func expectString(_ label: String) throws -> String {
        guard case let .string(value) = self else {
            throw VMError.typeError("\(label) expects String")
        }
        return value
    }
}

public final class ArrBox: CustomStringConvertible, Equatable {
    public var elements: [Value]
    public init(_ elements: [Value]) { self.elements = elements }
    public var description: String {
        let body = elements.map { $0.description }.joined(separator: ", ")
        return "[\(body)]"
    }
    public static func == (lhs: ArrBox, rhs: ArrBox) -> Bool { ObjectIdentifier(lhs) == ObjectIdentifier(rhs) }
}

public final class DictBox: CustomStringConvertible, Equatable {
    public var storage: [String: Value]
    public init(_ storage: [String: Value]) { self.storage = storage }
    public var description: String {
        let pairs = storage.map { "\($0): \($1)" }.sorted().joined(separator: ", ")
        return "[\(pairs)]"
    }
    public static func == (lhs: DictBox, rhs: DictBox) -> Bool { ObjectIdentifier(lhs) == ObjectIdentifier(rhs) }
}

public struct RangeBox: CustomStringConvertible, Equatable {
    public enum Kind: Equatable {
        case closed
        case halfOpen
    }

    public var lower: Value
    public var upper: Value
    public var kind: Kind

    public init(lower: Value, upper: Value, kind: Kind) {
        self.lower = lower
        self.upper = upper
        self.kind = kind
    }

    public var description: String {
        let symbol = (kind == .closed) ? "..." : "..<"
        return "\(lower)\(symbol)\(upper)"
    }

    public static func == (lhs: RangeBox, rhs: RangeBox) -> Bool {
        lhs.kind == rhs.kind && lhs.lower == rhs.lower && lhs.upper == rhs.upper
    }
}

public final class Closure: CustomStringConvertible, Equatable {
    public let params: [String]
    public let paramTypes: [String]  // NEW: e.g., ["Swift.String", "Swift.Int"]
    public let returnType: String?   // NEW: e.g., "Swift.String" or nil if unknown
    public let entry: Int
    public let captures: [String: Value]

    public init(
        params: [String],
        paramTypes: [String] = [],  // Default to empty for backward compatibility
        returnType: String? = nil,  // Default to nil for backward compatibility
        entry: Int,
        captures: [String: Value]
    ) {
        self.params = params
        self.paramTypes = paramTypes
        self.returnType = returnType
        self.entry = entry
        self.captures = captures
    }

    public var description: String {
        let ps = params.joined(separator: ", ")
        if !paramTypes.isEmpty || returnType != nil {
            let types = paramTypes.isEmpty ? params.map { _ in "?" } : paramTypes
            let ret = returnType ?? "?"
            return "<closure(\(ps): (\(types.joined(separator: ", "))) -> \(ret))>"
        }
        return "<closure(\(ps))>"
    }

    public static func == (lhs: Closure, rhs: Closure) -> Bool {
        return ObjectIdentifier(lhs) == ObjectIdentifier(rhs)
    }
}

public enum OpCode: Equatable, CustomStringConvertible {
    // values/vars/arithmetic/print (existing)
    case loadConst(Value)
    case loadVar(String)

    // Lexical scoping
    case pushScope
    case popScope
    case defVar(String)
    case defLet(String)
    case setVar(String)

    // Slot-mode (I3) — scaffolded
    case enterScope(nLocals: Int)
    case leaveScope
    case defLocal(slot: Int, isConst: Bool)
    case loadLocal(depth: Int, slot: Int)
    case setLocal(depth: Int, slot: Int)
    
    // Comparisons
    case add, sub, mul, div, mod
    case lt, le, gt, ge
    case printTop
    
    // logical
    case logicalNot
    case neg
    // Small primitives
    case dup                       // duplicate top of stack
    case pop                       // pop and discard top of stack
    
    // User-defined type support
    case defineType(name: String, fields: [String])
    
    /// Pops N values (for the given labels order) and creates an instance.
    case newObject(typeName: String, argLabels: [String])
    /// Expects object on stack; pushes the named property.
    case getProp(PropertyID)
    /// Expects (object, value) on stack; sets property (not yet used by compiler).
    case setProp(PropertyID)
    
    // comparisons / control flow
    case eq                       // pop rhs, lhs → push Bool
    case jump(Int)                // relative: skip `offset` instrs
    case jumpIfFalse(Int)         // pop Bool; if false, skip `offset`
    case jumpIfNil(Int)           // pop value; if nil, skip `offset`
    
    case defineFunction(name: String, params: [String], entry: Int, localCount: Int)
    case callFunc(name: String, argc: Int)
    case ret
    // Closures
    case makeClosure(params: [String], paramTypes: [String], returnType: String?, entry: Int, captures: [String])
    // Slot-mode friendly: expects capture values already on stack in capture order
    case makeClosureVals(params: [String], paramTypes: [String], returnType: String?, entry: Int, captures: [String])
    case callValue(argc: Int)
    // Host bridging
    case callMethod(selector: SelectorID, argc: Int)
    // Collections
    case makeArray(count: Int)
    case makeDict(count: Int)
    case getIndex
    case setIndex
    case makeRange(isClosed: Bool)
    case trap(String)

    public var description: String {
        switch self {
        case .loadConst(let v): return "loadConst(\(v))"
        case .loadVar(let n): return "loadVar(\(n))"
        case .pushScope: return "pushScope"
        case .popScope: return "popScope"
        case .defVar(let n): return "defVar(\(n))"
        case .defLet(let n): return "defLet(\(n))"
        case .setVar(let n): return "setVar(\(n))"
        case .enterScope(let n): return "enterScope(\(n))"
        case .leaveScope: return "leaveScope"
        case .defLocal(let s, let c): return "defLocal(slot: \(s), const: \(c))"
        case .loadLocal(let d, let s): return "loadLocal(depth: \(d), slot: \(s))"
        case .setLocal(let d, let s): return "setLocal(depth: \(d), slot: \(s))"
        case .add: return "add"
        case .sub: return "sub"
        case .mul: return "mul"
        case .div: return "div"
        case .mod: return "mod"
        case .printTop: return "printTop"
            
        case .defineType(let n, let f): return "defineType(\(n), fields: \(f))"
        case .newObject(let t, let labels): return "newObject(\(t), labels: \(labels))"
        case .getProp(let p): return "getProp(#\(p.raw))"
        case .setProp(let p): return "setProp(#\(p.raw))"
            
            // if expressions
        case .eq: return "eq"
        case .jump(let o): return "jump(\(o))"
        case .jumpIfFalse(let o): return "jumpIfFalse(\(o))"
        case .jumpIfNil(let o): return "jumpIfNil(\(o))"
            
            // logical operations
        case .lt: return "lt"
        case .le: return "le"
        case .gt: return "gt"
        case .ge: return "ge"
        case .logicalNot: return "not"
        case .neg: return "neg"
        case .dup: return "dup"
        case .pop: return "pop"
            
            // functions
        case .defineFunction(let n, let ps, let e, let lc): return "defineFunction(\(n), params: \(ps), entry: \(e), locals: \(lc))"
        case .callFunc(let n, let argc):            return "callFunc(\(n), argc: \(argc))"
        case .ret:                                  return "ret"
        case .makeClosure(let ps, let pts, let ret, let e, let caps):
            return "makeClosure(params: \(ps), paramTypes: \(pts), returnType: \(ret ?? "nil"), entry: \(e), captures: \(caps))"
        case .makeClosureVals(let ps, let pts, let ret, let e, let caps):
            return "makeClosureVals(params: \(ps), paramTypes: \(pts), returnType: \(ret ?? "nil"), entry: \(e), captures: \(caps))"
        case .callValue(let argc):                  return "callValue(argc: \(argc))"
        case .makeArray(let n):                     return "makeArray(count: \(n))"
        case .makeDict(let n):                      return "makeDict(count: \(n))"
        case .getIndex:                             return "getIndex"
        case .setIndex:                             return "setIndex"
        case .makeRange(let closed):                return "makeRange(closed: \(closed))"
        case .callMethod(let sel, let argc):        return "callMethod(selector: \(sel.raw), argc: \(argc))"
        case .trap(let message):                    return "trap(\(message))"
        }
    }
}

public struct SourceLoc: Equatable, CustomStringConvertible, Sendable {
    public let file: String
    public let line: Int
    public let column: Int
    public let endLine: Int
    public let endColumn: Int
    
    public var description: String { "\(file):\(line):\(column)" }
    
    public init(file: String, line: Int, column: Int, endLine: Int, endColumn: Int) {
        self.file = file
        self.line = line
        self.column = column
        self.endLine = endLine
        self.endColumn = endColumn
    }
}

public struct DebugSidecar {
    public struct ScopeNote {
        public let startIP: Int
        public let endIP: Int
        public let kind: String
        public let name: String?
        
        public init(startIP: Int, endIP: Int, kind: String, name: String?) {
            self.startIP = startIP
            self.endIP = endIP
            self.kind = kind
            self.name = name
        }
    }
    public struct LocalsMap {
        public let startIP: Int
        public let endIP: Int
        public let depth: Int
        public let namesBySlot: [Int: String]
        
        public init(startIP: Int, endIP: Int, depth: Int, namesBySlot: [Int : String]) {
            self.startIP = startIP
            self.endIP = endIP
            self.depth = depth
            self.namesBySlot = namesBySlot
        }
    }
    
    public let scopeNotes: [ScopeNote]
    public let localsMaps: [LocalsMap]
    
    public init(scopeNotes: [ScopeNote] = [], localsMaps: [LocalsMap] = []) {
        self.scopeNotes = scopeNotes
        self.localsMaps = localsMaps
    }
}

public struct Program {
    public let ops: [OpCode]
    public let locs: [SourceLoc?]   // parallel to ops; nil for synthetic ops
    public let source: String
    public let file: String
    public let debug: DebugSidecar?
    public let typeTable: [String]
    public let selectorTable: [String]
    public let propertyTable: [String]
    public let id: UInt64
    
    public init(ops: [OpCode], locs: [SourceLoc?], source: String, file: String, debug: DebugSidecar?, typeTable: [String], selectorTable: [String], propertyTable: [String], id: UInt64, userMethods: [MethodKey : MethodEntry]) {
        self.ops = ops
        self.locs = locs
        self.source = source
        self.file = file
        self.debug = debug
        self.typeTable = typeTable
        self.selectorTable = selectorTable
        self.propertyTable = propertyTable
        self.id = id
        self.userMethods = userMethods
    }
    
    // User methods: keyed by (TypeID, SelectorID)
    public struct MethodKey: Hashable {
        public let type: TypeID
        public let selector: SelectorID
        
        public init(type: TypeID, selector: SelectorID) {
            self.type = type
            self.selector = selector
        }
    }
    public struct PropertyKey: Hashable {
        public let type: TypeID
        public let property: PropertyID
        
        public init(type: TypeID, property: PropertyID) {
            self.type = type
            self.property = property
        }
    }
    public struct MethodEntry {
        public let entry: Int
        public let params: [String]
        public let localCount: Int
        public let mutating: Bool
        
        public init(entry: Int, params: [String], localCount: Int, mutating: Bool) {
            self.entry = entry
            self.params = params
            self.localCount = localCount
            self.mutating = mutating
        }
    }
    public let userMethods: [MethodKey: MethodEntry]
}


public struct VMExecutionError: Error, CustomStringConvertible {
    public let underlying: VMError
    public let loc: SourceLoc?
    
    public var description: String {
        if let l = loc { return "\(underlying.description) at \(l.file):\(l.line):\(l.column)" }
        return underlying.description
    }
    
    public init(underlying: VMError, loc: SourceLoc?) {
        self.underlying = underlying
        self.loc = loc
    }
}

public enum VMError: Error, CustomStringConvertible {
    case stackUnderflow
    case varNotFound(String)
    case varNotFoundWithSuggestions(name: String, suggestions: [String])
    case typeError(String)
    case divideByZero
    case unknownType(String)
    case badInitializer(String)
    case missingProperty(String, String) // (type, prop)
    case missingMethod(String, String)   // (type, selector)
    case indexOutOfRange(Int, Int)       // index, count
    case keyNotFound(String)
    case unsupported(String)
    case programMismatch(expected: UInt64, got: UInt64)
    case constAssignment(name: String, declaredAt: SourceLoc?)
    case redeclaration(name: String, originalAt: SourceLoc?)
    
    public var description: String {
        switch self {
        case .stackUnderflow: return "Stack underflow"
        case .varNotFound(let n): return "Variable not found: \(n)"
        case .varNotFoundWithSuggestions(let name, let sugg):
            if sugg.isEmpty { return "Variable not found: \(name)" }
            if sugg.count == 1 { return "Variable not found: '\(name)'. Did you mean '\(sugg[0])'?" }
            let list = sugg.prefix(3).joined(separator: ", ")
            return "Variable not found: '\(name)'. Did you mean: \(list)?"
        case .typeError(let msg): return "Type error: \(msg)"
        case .divideByZero: return "Divide by zero"
        case .unknownType(let t): return "Unknown type: \(t)"
        case .badInitializer(let msg): return "Bad initializer: \(msg)"
        case .missingProperty(let t, let p): return "Missing property \(t).\(p)"
        case .missingMethod(let t, let s):   return "Missing method \(t).\(s)"
        case .indexOutOfRange(let i, let n): return "Index out of range: \(i) (count \(n))"
        case .keyNotFound(let k): return "Key not found: \(k)"
        case .unsupported(let msg): return "Unsupported: \(msg)"
        case .constAssignment(let name, let loc):
            if let l = loc { return "cannot assign to '\(name)' (let constant declared at \(l.file):\(l.line):\(l.column))" }
            return "cannot assign to '\(name)' (let constant)"
        case .redeclaration(let name, let loc):
            if let l = loc { return "redeclaration of '\(name)' (originally declared at \(l.file):\(l.line):\(l.column))" }
            return "redeclaration of '\(name)' in the same scope"
        case .programMismatch(let exp, let got):
            return "Program mismatch: expected id \(exp), got \(got)"
        }
    }
}
