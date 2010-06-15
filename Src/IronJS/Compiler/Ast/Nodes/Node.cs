﻿using System;

namespace IronJS.Compiler.Ast.Nodes {
    public abstract class Node : INode {
        public SourcePosition SourcePosition { get; private set; }

        public Node(SourcePosition position) {
            SourcePosition = position;
        }

        public Node() {
            SourcePosition pos;

            pos.Column = -1;
            pos.Line = -1;

            SourcePosition = pos;
        }
    }
}
