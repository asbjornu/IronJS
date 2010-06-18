using IronJS.Compiler.Ast;
using IronJS.Compiler.Ast.Context;

namespace IronJS.Compiler.Analyzer {
    public interface IAnalyzer {
        INode[] Analyze(Scope scope, INode[] nodes);
    }
}
