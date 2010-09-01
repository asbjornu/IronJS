using IronJS.Compiler.Ast;
using IronJS.Compiler.Ast.Context;

namespace IronJS.Compiler.Analyzers {
    public interface IAnalyzer {
        INode[] Analyze(Scope scope, INode[] nodes);
    }
}
