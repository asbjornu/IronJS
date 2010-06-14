using System;
using Antlr.Runtime;
using Antlr.Runtime.Tree;
using IronJS.Compiler.Ast;
using IronJS.Compiler.Ast.Nodes;
using IronJS.Compiler.Parser.Extensions;

namespace IronJS.Compiler.Parser {
    public class AntlrEcma3 : IParser {
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
                return tree.MapChildren(x => GetNode(x));
            }

            return new[] { GetNode(tree) };
        }

        INode[] GetNodes(CommonTree tree) {
            if (tree.ChildCount > 0) {
                return tree.MapChildren(x => GetNode(x));
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
                return new Pass();

            var pos = tree.ToSourcePosition();

            switch (tree.Type) {
                case Xebic.ES3Parser.VAR:
                    return new Var(pos, GetNodeChild(tree, 0));

                case Xebic.ES3Parser.Identifier:
                    return new Identifier(pos, tree.Text);

                case Xebic.ES3Parser.DecimalLiteral:
                    return new Literal<double>(pos, tree.Text.ToDouble());

                case Xebic.ES3Parser.StringLiteral:
                    return new Literal<string>(pos, tree.Text.ToJsString());

                case Xebic.ES3Parser.IF:
                    return new If(pos, GetNodeChild(tree, 0), GetNodeChild(tree, 1), GetNodeNull(tree, 2));

                case Xebic.ES3Parser.BLOCK:
                    return new Block(pos, GetNodes(tree));

                case Xebic.ES3Parser.FUNCTION:
                    return new Function(pos, GetNodes(tree.GetChildSafe(0)), GetNodeChild(tree, 1));

                case Xebic.ES3Parser.CALL:
                    return new Invoke(pos, GetNodeChild(tree, 0), GetNodes(tree.GetChildSafe(0)));

                case Xebic.ES3Parser.OBJECT:
                    return new NewObject(pos, tree.MapChildren(x => Tuple.Create(GetNodeChild(x, 0), GetNodeChild(x, 1))));

                case Xebic.ES3Parser.ARRAY:
                    return new NewArray(pos, tree.MapChildren(x => GetNodeChild(x, 0)));

                case Xebic.ES3Parser.EXPR:
                    return GetNode(tree.GetChildSafe(0));

                case Xebic.ES3Parser.ASSIGN:
                    return ParseBinary(tree, BinaryOp.Assign);

                case Xebic.ES3Parser.LT:
                    return ParseBinary(tree, BinaryOp.LessThan);

                case Xebic.ES3Parser.BYFIELD:
                    return ParseMemberAccess(tree, MemberAccessType.ByField);

                case Xebic.ES3Parser.BYINDEX:
                    return ParseMemberAccess(tree, MemberAccessType.ByIndex);

                case Xebic.ES3Parser.FOR:
                    return ParseFor(tree.GetChildSafe(0), tree.GetChildSafe(1));

                case Xebic.ES3Parser.INC:
                    return ParseUnaryAddSub(tree.GetChildSafe(0), BinaryOp.Add);

                case Xebic.ES3Parser.DEC:
                    return ParseUnaryAddSub(tree.GetChildSafe(0), BinaryOp.Sub);

                default:
                    throw new Exception(String.Format("Can't convert token '{0}' to INode", Xebic.ES3Parser.tokenNames[tree.Type]));
            }
        }

        INode ParseUnaryAddSub(CommonTree tree, BinaryOp op) {
            if(op != BinaryOp.Add && op != BinaryOp.Sub)
                throw new Exception("Can only parse unary ++x and --x");

            var pos = tree.ToSourcePosition();

            var target = GetNode(tree);
            var opNode = new Binary(pos, op, target, new Literal<double>(pos, 1.0));

            return new Binary(pos, BinaryOp.Assign, target, opNode);
        }

        INode ParseMemberAccess(CommonTree tree, MemberAccessType type) {
            return new MemberAccess(tree.ToSourcePosition(), GetNodeChild(tree, 1), GetNodeChild(tree, 0), type);
        }

        INode ParseBinary(CommonTree tree, BinaryOp op) {
            return new Binary(tree.ToSourcePosition(), op, GetNodeChild(tree, 0), GetNodeChild(tree, 1));
        }

        INode ParseFor(CommonTree headTree, CommonTree bodyTree) {
            if (headTree.Type == Xebic.ES3Parser.FORSTEP) {
                return  new ForStep(
                            headTree.ToSourcePosition(),
                            GetNodeNull(headTree, 0),
                            GetNodeNull(headTree, 1),
                            GetNodeNull(headTree, 2),
                            GetNode(bodyTree)
                        );
            }

            return new Pass();
        }
    }
}
