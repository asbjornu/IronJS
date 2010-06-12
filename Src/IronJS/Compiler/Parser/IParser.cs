using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using IronJS.Compiler.Ast;

namespace IronJS.Compiler.Parser {
    interface IParser {
        INode[] ParseFile(string fileName);
        INode[] ParseSource(string source);
    }
}
