using System;
using System.Linq;
using System.Collections.Generic;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Ast.Context {
    public class Variable {
        public string Name { get; private set; }
        public int Index { get; private set; }

        public Runtime.Type Type { get; set; }
        public HashSet<INode> AssignedFrom { get; private set; }

        public bool NeedsProxy { get; set; }
        public bool IsClosedOver { get; set; }
        public bool TypeResolved { get; set; }
        public bool InitAsUndefind { get; set; }

        public Variable(string name, int index) {
            Name = name;
            Index = index;

            Type = Runtime.Type.Nothing;
            AssignedFrom = new HashSet<INode>();

            NeedsProxy = false;
            IsClosedOver = false;
            TypeResolved = false;
            InitAsUndefind = false;
        }

        public Variable(string name)
            : this(name, -1) {
        }
    }
}
