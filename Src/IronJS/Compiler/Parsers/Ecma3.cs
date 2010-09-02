using Antlr.Runtime;
using Antlr.Runtime.Tree;
using IronJS.Compiler.Ast;
using IronJS.Compiler.Ast.Nodes;
using IronJS.Compiler.Parsers.Extensions;

namespace IronJS.Compiler.Parsers {
    public class Ecma3 : IParser {
        public INode[] ParseFile(string fileName) {
            return Parse(new ANTLRFileStream(fileName), fileName);
        }

        public INode[] ParseSource(string source) {
            return Parse(new ANTLRStringStream(source), "<source>");
        }

        INode[] Parse(ICharStream source, string fileName) {
            var lexer = new Xebic.ES3Lexer(source);
            var parser = new Xebic.ES3Parser(new CommonTokenStream(lexer));
            var tree = (CommonTree)parser.program().Tree;

            if (tree.IsNil) {
                return tree.MapChildren((_, x) => GetNode(x));
            }

            return new[] { GetNode(tree) };
        }

        INode[] GetNodes(CommonTree tree) {
            if (tree.ChildCount > 0) {
                return tree.MapChildren((_, x) => GetNode(x));
            } else {
                return new INode[0];
            }
        }

        INode GetNodeNull(CommonTree tree, int i) {
            return GetNode(tree.GetChildNull(i));
        }

        INode GetNodeChild(CommonTree tree, int i) {
            return GetNode(tree.GetChildSafe(i));
        }

        INode GetNode(CommonTree tree) {
            if (tree == null)
                return Pass.Instance;

            var pos = tree.GetSourcePosition();

            switch (tree.Type) {
                case Xebic.ES3Parser.VAR:
                    return Var.Create(pos, GetNodeChild(tree, 0));

                case Xebic.ES3Parser.Identifier:
                    return Identifier.Create(pos, tree.Text);

                case Xebic.ES3Parser.DecimalLiteral:
                    return Literal.Create(pos, tree.Text.ToDouble());

                case Xebic.ES3Parser.StringLiteral:
                    return Literal.Create(pos, tree.Text.ToJsString());

                case Xebic.ES3Parser.TRUE:
                    return Literal.True;

                case Xebic.ES3Parser.FALSE:
                    return Literal.False;

                case Xebic.ES3Parser.IF:
                    return If.Create(pos, GetNodeChild(tree, 0), GetNodeChild(tree, 1), GetNodeNull(tree, 2));

                case Xebic.ES3Parser.BLOCK:
                    return Block.Create(pos, GetNodes(tree));

                case Xebic.ES3Parser.FUNCTION:
                    return Function.Create(pos, GetNodes(tree.GetChildSafe(0)), GetNodeChild(tree, 1));

                case Xebic.ES3Parser.CALL:
                    return Invoke.Create(pos, GetNodeChild(tree, 0), GetNodes(tree.GetChildSafe(0)));

                case Xebic.ES3Parser.OBJECT:
                    return New.Object(pos, new INode[0]);

                case Xebic.ES3Parser.ARRAY:
                    return New.Array(pos, tree.MapChildren((_, x) => GetNodeChild(x, 0)));

                case Xebic.ES3Parser.EXPR:
                    return GetNode(tree.GetChildSafe(0));

                case Xebic.ES3Parser.ASSIGN:
                    return ParseBinary(tree, Binary.OpType.Assign);

                case Xebic.ES3Parser.EQ:
                    return ParseBinary(tree, Binary.OpType.Eq);

                case Xebic.ES3Parser.LT:
                    return ParseBinary(tree, Binary.OpType.Lt);

                case Xebic.ES3Parser.LTE:
                    return ParseBinary(tree, Binary.OpType.LtEq);

                case Xebic.ES3Parser.GT:
                    return ParseBinary(tree, Binary.OpType.Gt);

                case Xebic.ES3Parser.GTE:
                    return ParseBinary(tree, Binary.OpType.GtEq);

                case Xebic.ES3Parser.BYFIELD:
                    return ParseProperty(tree, Property.AccessMode.Field);

                case Xebic.ES3Parser.BYINDEX:
                    return ParseProperty(tree, Property.AccessMode.Index);

                case Xebic.ES3Parser.FOR:
                    return ParseFor(tree.GetChildSafe(0), tree.GetChildSafe(1));

                case Xebic.ES3Parser.INC:
                    return ParseUnary(tree, Unary.OpType.Inc);

                case Xebic.ES3Parser.DEC:
                    return ParseUnary(tree, Unary.OpType.Dec);

                case Xebic.ES3Parser.PINC:
                    return ParseUnary(tree, Unary.OpType.PostInc);

                case Xebic.ES3Parser.PDEC:
                    return ParseUnary(tree, Unary.OpType.PostDec);

                case Xebic.ES3Parser.RETURN:
                    return ParseUnary(tree, Unary.OpType.Return);

                default:
                    throw new CompilerError("Can't convert token '{0}' to INode", Xebic.ES3Parser.tokenNames[tree.Type]);
            }
        }

        INode ParseProperty(CommonTree tree, Property.AccessMode type) {
            switch (type) {
                case Property.AccessMode.Field:
                    return Property.CreateField(tree.GetSourcePosition(), GetNodeChild(tree, 1), GetNodeChild(tree, 0));

                default: //Property.AccessMode.Field
                    return Property.CreateIndex(tree.GetSourcePosition(), GetNodeChild(tree, 1), GetNodeChild(tree, 0));
            }
        }

        INode ParseBinary(CommonTree tree, Binary.OpType op) {
            return Binary.Create(tree.GetSourcePosition(), op, GetNodeChild(tree, 0), GetNodeChild(tree, 1));
        }

        INode ParseUnary(CommonTree tree, Unary.OpType op) {
            return Unary.Create(tree.GetSourcePosition(), GetNodeChild(tree, 0), op);
        }

        INode ParseFor(CommonTree headTree, CommonTree bodyTree) {
            if (headTree.Type == Xebic.ES3Parser.FORSTEP) {
                return  For.Create(
                            headTree.GetSourcePosition(),
                            GetNodeNull(headTree, 0),
                            GetNodeNull(headTree, 1),
                            GetNodeNull(headTree, 2),
                            GetNode(bodyTree)
                        );
            }

            return Pass.Instance;
        }
    }
}
