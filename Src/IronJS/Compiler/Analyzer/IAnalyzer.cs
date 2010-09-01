using IronJS.Compiler.Ast;

namespace IronJS.Compiler.Analyzer {
    public interface IAnalyzer {
        INode[] Analyze(Ast.Context.ScopeChain context, INode[] nodes);
        IAnalyzer Next { get; }
    }
}
