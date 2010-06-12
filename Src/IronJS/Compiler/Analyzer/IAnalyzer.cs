using IronJS.Compiler.Ast;

namespace IronJS.Compiler.Analyzer {
    public interface IAnalyzer {
        INode[] Analyze(INode[] nodes);
    }
}
