using IronJS.Compiler.Ast;

namespace IronJS.Compiler.Analyzer {
    public interface IAnalyzer {
        INode[] Analyze(Ast.Context.Scope context, INode[] nodes);
    }
}
