using System;
using IronJS;
using IronJS.Compiler.Ast.Nodes;
using IronJS.Compiler.Ast;

namespace IronJS.REPL {
	class Program {
		static void Main(string[] args) {
            var parser = new Compiler.Parsers.Ecma3();
            var analyzer = new Compiler.Analyzers.VariableScopeAnalyzer();
            var analyzer2 = new Compiler.Analyzers.StaticTypeAnalyzer();

            var nodes = parser.ParseFile("testing.js");
            var function = Function.Create(SourcePosition.Unknown, new INode[0], Block.Create(SourcePosition.Unknown, nodes));
            var analyzed = analyzer2.Analyze(analyzer.Analyze(function));
		}
	}
}
