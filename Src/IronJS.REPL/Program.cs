using System;
using IronJS;
using IronJS.Compiler.Ast.Nodes;
using IronJS.Compiler.Ast;
using IronJS.Compiler.Analyzers;
using System.Collections.Generic;

namespace IronJS.REPL {
	class Program {
		static void Main(string[] args) {
            var parser = new Compiler.Parsers.Ecma3();
            var ast = Function.Create(SourcePosition.Unknown, parser.ParseFile("testing.js"));

            var analyzers = new List<IAnalyzer>();
            analyzers.Add(new Compiler.Analyzers.DeclarationAnalyzer());
            analyzers.Add(new Compiler.Analyzers.StaticTypeAnalyzer());
            analyzers.Add(new Compiler.Analyzers.ScopeAnalyzer());

            foreach(var analyzer in analyzers) {
                ast = analyzer.Analyze(ast);
            }
		}
	}
}
