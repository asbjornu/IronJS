using System;
using IronJS;

namespace IronJS.REPL {
	class Program {
		static void Main(string[] args) {
            var parser = new Compiler.Parser.Ecma3();
            var analyzer = new Compiler.Analyzers.Default();

            var nodes = parser.ParseFile("testing.js");
            var global = new Compiler.Ast.Context.Scope();
            var analyzed = analyzer.Analyze(global, nodes);
		}
	}
}
