using System;
using IronJS;

namespace IronJS.REPL {
	class Program {
		static void Main(string[] args) {
            var parser = new Compiler.Parsers.Ecma3();
            var analyzer = new Compiler.Analyzers.VariableScopeAnalyzer();
            var analyzer2 = new Compiler.Analyzers.StaticTypeAnalyzer();

            var nodes = parser.ParseFile("testing.js");
            var global = new Compiler.Ast.Context.Scope();
            var analyzed = analyzer2.Analyze(global, analyzer.Analyze(global, nodes));
		}
	}
}
