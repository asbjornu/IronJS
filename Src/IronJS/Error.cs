using System;
using IronJS.Compiler.Ast;

namespace IronJS {
    public class Error : Exception {
        public Error(string message, params object[] args)
            : base(String.Format(message, args)) {
        }

        public Error()
            : base() {
        }
    }

    public class CompilerError : Error {
        public CompilerError(string message, params object[] args)
            : base(message, args) {
        }

        public CompilerError(string message, INode node, params object[] args)
            : base(message.Trim() + String.Format(" (line: {0}, column: {1})", node.Source.Line, node.Source.Column), args) {
        }

        public CompilerError(INode node)
            : base(String.Format("Compiler Error (line: {0}, column: {1})", node.Source.Line, node.Source.Column)) {
        }

        public CompilerError()
            : base() {
        }
    }

    public class RuntimeError : Error {
        public RuntimeError(string message, params object[] args)
            : base(message, args) {
        }

        public RuntimeError()
            : base() {
        }
    }
}
