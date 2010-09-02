using IronJS.Compiler.Ast;
using IronJS.Compiler.Ast.Context;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Analyzers {
    public class StaticTypeAnalyzer : Base {
        protected override INode AnalyzeNode(INode node) {
            if (node == null) {
                return Pass.Instance;

            } else if (node is Binary) {
                return AnalyzeBinary(node as Binary);

            } else if (node is Function) {
                return AnalyzeFunction(node as Function);

            } else {
                return AnalyzeChildren(node);
            }
        }

        INode AnalyzeBinary(Binary node) {
            if (node.Op == Binary.OpType.Assign && node.Left is Identifier) {
                var left = AnalyzeNode(node.Left);
                var right = AnalyzeNode(node.Right);

                Scope.Variables.Get(left).AddAssignedFrom(right);

                return Binary.CreateAssign(node.Source, left, right);
            } else {
                return AnalyzeChildren(node);
            }
        }
    }
}
