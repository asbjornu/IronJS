using System;
using System.Collections.Generic;

namespace IronJS.Compiler.Ast.Context {
    public class Variable {
        public string Name { get; private set; }
        public int Index { get; private set; }

        public Runtime.Type Type { get; private set; }
        public HashSet<INode> AssignedFrom { get; private set; }

        public bool NeedsProxy { get; set; }
        public bool TypeResolved { get; set; }
        public bool IsClosedOver { get; set; }
        public bool InitAsUndefind { get; set; }

        public Variable(string name, int index) {
            Name = name;
            Index = index;

            Type = Runtime.Type.None;
            AssignedFrom = new HashSet<INode>();

            IsClosedOver = false;
            InitAsUndefind = false;
            NeedsProxy = false;
            TypeResolved = false;
        }

        public Variable(string name)
            : this(name, -1) {
        }

        public void AddType(Runtime.Type type) {
            Type |= type;
        }

        public void AddAssignedFrom(INode value) {
            if (value.TypeResolved) {
                AddType(value.Type);
            } else {
                AssignedFrom.Add(value);
            }
        }
    }
}
