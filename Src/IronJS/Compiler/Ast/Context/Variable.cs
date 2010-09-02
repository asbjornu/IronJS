using System;
using System.Linq;
using System.Collections.Generic;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Ast.Context {
    public class Variable : INamed {
        public string Name { get; private set; }
        public int Index { get; set; }

        public Runtime.Type Type { get; set; }
        public HashSet<INode> AssignedFrom { get; private set; }

        public bool NeedsProxy { get; set; }
        public bool IsClosedOver { get; set; }
        public bool TypeResolved { get; set; }
        public bool InitAsUndefind { get; set; }
        public bool IsParameter { get; set; }

        Variable(string name, int index) {
            Name = name;
            Index = index;

            Type = Runtime.Type.None;
            AssignedFrom = new HashSet<INode>();

            NeedsProxy = false;
            IsClosedOver = false;
            TypeResolved = false;
            InitAsUndefind = false;
            IsParameter = false;
        }

        Variable(string name)
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

        public Variable Clone() {
            var clone = new Variable(Name, Index);

            clone.Type = Type;
            clone.NeedsProxy = NeedsProxy;
            clone.IsClosedOver = IsClosedOver;
            clone.TypeResolved = TypeResolved;
            clone.InitAsUndefind = InitAsUndefind;

            return clone;
        }

        public static Variable CreateLocal(string name) {
            return new Variable(name, -1);
        }

        public static Variable CreateParameter(string name, int index) {
            var variable = new Variable(name, index);
            variable.IsParameter = true;
            return variable;
        }
    }
}
