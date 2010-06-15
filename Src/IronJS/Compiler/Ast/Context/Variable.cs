using System;
using System.Collections.Generic;

namespace IronJS.Compiler.Ast.Context {
    public class Variable {
        public string Name { get; private set; }
        public HashSet<INode> AssignedFrom { get; private set; }
        public bool IsClosedOver { get; set; }
        public Runtime.Type Type { get; set; }
        public int Index { get; private set; }

        public Variable(string name, int index) {
            Name = name;
            Index = index;
            Type = Runtime.Type.Nothing;
            IsClosedOver = false;
            AssignedFrom = new HashSet<INode>();
        }

        public Variable(string name)
            : this(name, -1) {
        }
    }
}
