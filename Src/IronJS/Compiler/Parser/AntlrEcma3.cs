using System;
using Antlr.Runtime;
using Antlr.Runtime.Tree;
using IronJS.Compiler.Ast;
using IronJS.Compiler.Ast.Nodes;
using IronJS.Extensions;

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

        INode GetNode(CommonTree tree, int i) {
            return GetNode(tree.GetChildSafe(i));
        }

        INode GetNode(CommonTree tree) {
            var pos = tree.ToSourcePosition();

            switch (tree.Type) {
                case Xebic.ES3Parser.VAR:
                    return new Var(pos, GetNode(tree, 0));

                case Xebic.ES3Parser.ASSIGN:
                    return new Binary(pos, BinaryOp.Assign, GetNode(tree, 0), GetNode(tree, 1));

                case Xebic.ES3Parser.Identifier:
                    return new Identifier(pos, tree.Text);

                case Xebic.ES3Parser.DecimalLiteral:
                    return new Literal<double>(pos, StringConverter.ToDouble(tree.Text));

                case Xebic.ES3Parser.StringLiteral:
                    return new Literal<string>(pos, StringConverter.ToJsString(tree.Text));

                default:
                    throw new Exception(String.Format("Can't convert token '{0}' to INode", Xebic.ES3Parser.tokenNames[tree.Type]));
            }
        }
    }
}
