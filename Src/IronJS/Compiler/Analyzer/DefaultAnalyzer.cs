using System;
using IronJS.Compiler.Ast;

namespace IronJS.Compiler.Analyzer {
    public class DefaultAnalyzer : IAnalyzer {
        public INode[] Analyze(INode[] nodes) {
            throw new NotImplementedException();
        }

        INode Analyze(INode node) {
            return node;
        }
    }
}
