using IronJS.Compiler.Ast;
using IronJS.Compiler.Ast.Context;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Analyzers {
    public class VariableScopeAnalyzer : Base {
        protected override INode AnalyzeNode(INode node) {
            if (node == null) {
                return Pass.Instance;

            } else if (node is Var) {
                return AnalyzeVar(node as Var);

            } else if (node is Function) {
                return AnalyzeFunction(node as Function);

            } else {
                return AnalyzeChildren(node);
            }
        }

        INode AnalyzeVar(Var node) {
            Binary binary;
            Identifier identifier;

            if (node.Node.As<Binary>(out binary) && binary.IsAssign) {
                if (binary.Left.As<Identifier>(out identifier)) {
                    Scope.Variables.Add(Variable.CreateLocal(identifier.Name));
                    return AnalyzeNode(binary);
                }

                throw new CompilerError();

            } else if (node.Node.As<Identifier>(out identifier)) {
                Scope.Variables.Add(Variable.CreateLocal(identifier.Name));
                return Pass.Instance;

            }

            throw new CompilerError();
        }
    }
}
