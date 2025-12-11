//
//  main.swift
//  IntegrationTest
//
//  Created by Forest Plasencia on 9/23/25.
//

import InterpreterCompiler
import InterpreterVM
import Foundation

let compiler = Compiler()

// MARK: - Simple heap for returning buffers to JS
@_cdecl("wasm_alloc")
public func wasm_alloc(_ size: Int32) -> UnsafeMutablePointer<UInt8> {
    let p = UnsafeMutablePointer<UInt8>.allocate(capacity: Int(size))
    return p
}
@_cdecl("wasm_free")
public func wasm_free(_ ptr: UnsafeMutablePointer<UInt8>?, _ len: Int32) {
    ptr?.deallocate()
}

// MARK: - Public C-ABI: compile -> JSON (success=0, failure!=0)
@_cdecl("compile_program_json")
public func compile_program_json(
    _ srcPtr: UnsafePointer<UInt8>, _ srcLen: Int32,
    _ filePtr: UnsafePointer<UInt8>?, _ fileLen: Int32,
    _ outPtr: UnsafeMutablePointer<UnsafeMutablePointer<UInt8>?>,
    _ outLen: UnsafeMutablePointer<Int32>,
    _ errPtr: UnsafeMutablePointer<UnsafeMutablePointer<UInt8>?>,
    _ errLen: UnsafeMutablePointer<Int32>
) -> Int32 {
    func makeString(_ p: UnsafePointer<UInt8>?, _ n: Int32, default def: String) -> String {
        guard let p = p, n > 0 else { return def }
        return String(decoding: UnsafeBufferPointer(start: p, count: Int(n)), as: UTF8.self)
    }
    
    let source   = makeString(srcPtr,  srcLen,  default: "")
    let fileName = makeString(filePtr, fileLen, default: "input.swift")
    
    do {
        let program = try Compiler().compileProgram(source: source, fileName: fileName)
        // Serialize to JSON
        //        let data = try encodeProgramJSON(program)
        let data = program.ops.map(\.description).joined(separator: "\n").data(using: .utf8) ?? .init()
        let buf  = wasm_alloc(Int32(data.count))
        data.withUnsafeBytes { raw in
            guard let base = raw.baseAddress else { return }
            buf.assign(from: base.assumingMemoryBound(to: UInt8.self), count: data.count)
        }
        outPtr.pointee = buf
        outLen.pointee = Int32(data.count)
        errPtr.pointee = nil
        errLen.pointee = 0
        return 0
    } catch {
        let msg = "[compile error] \(error)"
        let bytes = Array(msg.utf8)
        let buf = wasm_alloc(Int32(bytes.count))
        buf.assign(from: bytes, count: bytes.count)
        errPtr.pointee = buf
        errLen.pointee = Int32(bytes.count)
        outPtr.pointee = nil
        outLen.pointee = 0
        return 1
    }
}

// (Optional) Public C-ABI: compile -> human-readable bytecode text
@_cdecl("compile_program_text")
public func compile_program_text(
    _ srcPtr: UnsafePointer<UInt8>, _ srcLen: Int32,
    _ filePtr: UnsafePointer<UInt8>?, _ fileLen: Int32,
    _ outPtr: UnsafeMutablePointer<UnsafeMutablePointer<UInt8>?>,
    _ outLen: UnsafeMutablePointer<Int32>
) -> Int32 {
    let src = String(decoding: UnsafeBufferPointer(start: srcPtr, count: Int(srcLen)), as: UTF8.self)
    let file = (filePtr != nil && fileLen > 0)
    ? String(decoding: UnsafeBufferPointer(start: filePtr!, count: Int(fileLen)), as: UTF8.self)
    : "input.swift"
    do {
        let program = try Compiler().compileProgram(source: src, fileName: file)
        var s = "=== Bytecode ===\n"
        for (i, op) in program.ops.enumerated() { s += String(format:"%3d", i) + " " + op.description + "\n" }
        let bytes = Array(s.utf8)
        let buf = UnsafeMutablePointer<UInt8>.allocate(capacity: bytes.count)
        buf.assign(from: bytes, count: bytes.count)
        outPtr.pointee = buf
        outLen.pointee = Int32(bytes.count)
        return 0
    } catch {
        let bytes = Array(("[compile error] \(error)").utf8)
        let buf = UnsafeMutablePointer<UInt8>.allocate(capacity: bytes.count)
        buf.assign(from: bytes, count: bytes.count)
        outPtr.pointee = buf
        outLen.pointee = Int32(bytes.count)
        return 1
    }
}

@_cdecl("run_program")
public func run_program(
    _ srcPtr: UnsafePointer<UInt8>, _ srcLen: Int32,
    _ filePtr: UnsafePointer<UInt8>?, _ fileLen: Int32,
    _ outPtr: UnsafeMutablePointer<UnsafeMutablePointer<UInt8>?>,
    _ outLen: UnsafeMutablePointer<Int32>
) -> Int32 {
    let src = String(decoding: UnsafeBufferPointer(start: srcPtr, count: Int(srcLen)), as: UTF8.self)
    let file = (filePtr != nil && fileLen > 0)
    ? String(decoding: UnsafeBufferPointer(start: filePtr!, count: Int(fileLen)), as: UTF8.self)
    : "input.swift"
    do {
        var vm = VM()
        let program = try Compiler().compileProgram(source: src, fileName: file)
        try vm.prepare(program)
        
        
        try vm.run(program)
        
        var s = vm.output
//        for (i, op) in program.ops.enumerated() { s += String(format:"%3d", i) + " " + op.description + "\n" }
        let bytes = Array(s.utf8)
        let buf = UnsafeMutablePointer<UInt8>.allocate(capacity: bytes.count)
        buf.assign(from: bytes, count: bytes.count)
        outPtr.pointee = buf
        outLen.pointee = Int32(bytes.count)
        return 0
    } catch {
        let bytes = Array(("[compile error] \(error)").utf8)
        let buf = UnsafeMutablePointer<UInt8>.allocate(capacity: bytes.count)
        buf.assign(from: bytes, count: bytes.count)
        outPtr.pointee = buf
        outLen.pointee = Int32(bytes.count)
        return 1
    }
}

