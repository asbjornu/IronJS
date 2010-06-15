using System;
using IronJS;

namespace IronJS.REPL {
	class Program {
		static void Main(string[] args) {
            var parser = new Compiler.Parser.AntlrEcma3();
            var analyzer = new Compiler.Analyzer.Default();

            var nodes = parser.ParseFile("testing.js");
            var analyzed = analyzer.Analyze(new Compiler.Ast.Context.Scope(), nodes);
		}
	}
}
