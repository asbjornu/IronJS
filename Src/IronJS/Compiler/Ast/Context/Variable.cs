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
        public bool TypeResolved { get; set; }
        public bool IsClosedOver { get; set; }
        public bool InitAsUndefind { get; set; }

        public Variable(string name, int index) {
            Name = name;
            Index = index;

            Type = Runtime.Type.Nothing;
            AssignedFrom = new HashSet<INode>();

            IsClosedOver = false;
            InitAsUndefind = false;
            NeedsProxy = false;
            TypeResolved = false;
        }

        public Variable(string name)
            : this(name, -1) {
        }
    }

    public class VariableList {
        HashSet<Variable> _variables;

        class VariableComparer : IEqualityComparer<Variable> {
            public bool Equals(Variable x, Variable y) { return x.Name == y.Name; }
            public int GetHashCode(Variable obj) { return obj.Name.GetHashCode(); }
        }

        public VariableList() {
            _variables = new HashSet<Variable>(new VariableComparer());
        }

        public void Add(Variable var) {
            _variables.Add(var);
        }

        public Variable Get(string name) {
            return _variables.First(x => x.Name == name);
        }

        public Variable Get(INode node) {
            return _variables.First(x => x.Name == (node as Identifier).Name);
        }

        public void AddType(INode node, Runtime.Type type) {
            Get(node).Type |= type;
        }

        public void AddAssignedFrom(INode node, INode value) {
            if (value.TypeIsStatic) {
                AddType(node, value.Type);
            } else {
                Get(node).AssignedFrom.Add(value);
            }
        }
    }
}
