using System;
using Antlr.Runtime.Tree;
using IronJS.Compiler.Ast;

namespace IronJS.Compiler.Parser.Extensions {
    public static class Antlr {
        public static CommonTree GetChildNull(this CommonTree tree, int i) {
            if (tree.Children == null || tree.Children.Count <= i) {
                return null;
            }

            return (CommonTree)tree.Children[i];
        }

        public static CommonTree GetChildSafe(this CommonTree tree, int i) {
            if (tree.Children == null || tree.Children.Count <= i) {
                throw new Exception("");
            }

            return (CommonTree)tree.Children[i];
        }

        public static T[] MapChildren<T>(this CommonTree tree, Func<int, CommonTree, T> func) {
            var children = new T[tree.ChildCount];

            for (var i = 0; i < tree.ChildCount; ++i) {
                children[i] = func(i, tree.GetChildSafe(i));
            }

            return children;
        }

        public static SourcePosition GetSourcePosition(this CommonTree tree) {
            SourcePosition pos;

            pos.Line = tree.Line;
            pos.Column = tree.CharPositionInLine;

            return pos;
        }
    }
}
