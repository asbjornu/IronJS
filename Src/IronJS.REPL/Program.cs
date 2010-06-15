using System;
using IronJS;

namespace IronJS.REPL {
	class Program {
		static void Main(string[] args) {
            var parser = new Compiler.Parser.AntlrEcma3();
            var analyzer = new Compiler.Analyzer.Default();

            var nodes = parser.ParseFile("testing.js");

            var scopeChain = new Compiler.Ast.Context.ScopeChain();
            scopeChain.Enter(new Compiler.Ast.Context.Scope());

            var analyzed = analyzer.Analyze(scopeChain, nodes);
		}
	}
}
