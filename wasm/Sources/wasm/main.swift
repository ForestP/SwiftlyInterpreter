import Foundation
import SwiftParser
import SwiftSyntax

// If you pass a file path, we'll try to read it. Otherwise use a default snippet.
let source: String = {
    if let path = CommandLine.arguments.dropFirst().first,
       let text = try? String(contentsOfFile: path, encoding: .utf8) {
        return text
    } else {
        return """
        struct Greeter {
            func hello(name: String) {
                print("Hello, \\(name)!")
            }
        }

        class Welcome {
            func wave() {}
        }
        """
    }
}()

let tree = Parser.parse(source: source)

// Walk the tree and print some declarations.
final class DeclPrinter: SyntaxAnyVisitor {
    override func visit(_ node: StructDeclSyntax) -> SyntaxVisitorContinueKind {
        print("struct \(node.name.text)")
        return .visitChildren
    }
    override func visit(_ node: ClassDeclSyntax) -> SyntaxVisitorContinueKind {
        print("class \(node.name.text)")
        return .visitChildren
    }
    override func visit(_ node: FunctionDeclSyntax) -> SyntaxVisitorContinueKind {
        // Show the function name and its parameter list
        print("func \(node.name.text)\(node.signature.parameterClause.description.trimmingCharacters(in: .whitespacesAndNewlines))")
        return .visitChildren
    }
}

let printer = DeclPrinter(viewMode: .all)
printer.walk(tree)

print("\n--- Reprinted source (formatted) ---\n")
print(tree.description)
