using System;
using IronJS.Compiler;
using IronJS.Compiler.Ast;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Analyzer {
    public class Default : IAnalyzer {
        public INode[] Analyze(Ast.Context.Scope function, INode[] nodes) {
            var analyzedNodes = new INode[nodes.Length];

            for(var i = 0; i < nodes.Length; ++i) {
                analyzedNodes[i] = Analyze(nodes[i]);
            }

            return analyzedNodes;
        }

        INode Analyze(INode node) {
            if (node is Var) {
                return Analyze(node as Var);
            } else {
                return node;
            }
        }

        INode Analyze(Var node) {
            throw new NotImplementedException();
        }
    }
}
