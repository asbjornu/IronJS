using System;

namespace IronJS.Compiler.Ast.Context {
    public class Variable {
        public string Name { get; private set; }
        public bool IsClosedOver { get; set; }
        public Runtime.Type Type { get; set; }

        public Variable(string name) {
            Name = name;
            Type = Runtime.Type.Nothing;
            IsClosedOver = false;
        }
    }
}
