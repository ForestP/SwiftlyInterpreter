//
//  TypeToken.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/18/25.
//

import Foundation

enum TypeToken: Equatable {
    case identifier(String)
    case dot
    case comma
    case lessThan
    case greaterThan
    case leftBracket
    case rightBracket
    case leftParen
    case rightParen
    case colon
    case question
    case ampersand
    case arrow
    case eof
}
