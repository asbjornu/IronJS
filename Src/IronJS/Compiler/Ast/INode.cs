using System;

namespace IronJS.Compiler.Ast {
    public interface INode {
        SourcePosition SourcePosition { get; }
        Runtime.Type Type { get; }
        bool TypeResolved { get; }
        bool As<T>(out T result) where T : class, INode;
    }
}
