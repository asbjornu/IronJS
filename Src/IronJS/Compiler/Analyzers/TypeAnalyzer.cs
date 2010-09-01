using IronJS.Compiler.Ast;
using IronJS.Compiler.Ast.Context;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Analyzers {
    public class TypeAnalyzer : Base {
        protected override INode Analyze(INode node) {
            if (node == null) {
                return Pass.Instance;

            } else if (node is Binary) {
                return Analyze(node as Binary);

            } else {
                return AnalyzeChildrenAndClone(node);
            }
        }

        INode Analyze(Binary node) {
            if (node.Op == Binary.OpType.Assign && node.Left is Identifier) {
                var left = Analyze(node.Left);
                var right = Analyze(node.Right);

                Scope.Variables.Get(left).AddAssignedFrom(right);

                return Binary.CreateAssign(node.Source, left, right);
            } else {
                return AnalyzeChildrenAndClone(node);
            }
        }
    }
}
