//
//  CompileError.swift
//  InterpreterCompiler
//
//  Created by Forest Plasencia on 9/18/25.
//

public enum CompileError: Error, CustomStringConvertible {
    case unsupported(String)
    case message(String)
    
    public var description: String {
        switch self {
        case .unsupported(let s): return "Unsupported: \(s)"
        case .message(let s): return s
        }
    }
}
