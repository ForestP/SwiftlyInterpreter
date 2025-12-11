//
//  TypeAliasDecl.swift
//  HostSurfaceKit
//
//  Created by Forest Plasencia on 9/18/25.
//


struct TypeAliasDecl: Equatable {
    let moduleName: String
    let name: String
    let qualifiedName: String
    let target: TypeName
    let line: Int
}
