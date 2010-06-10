using System;
using IronJS;

namespace IronJS.REPL {
	class Program {
		static void Main(string[] args) {
            var parser = new Compiler.Parser.AntlrEcma3();
            var nodes = parser.ParseFile("testing.js");
		}
	}
}
