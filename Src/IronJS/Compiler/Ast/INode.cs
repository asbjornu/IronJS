﻿using System;

namespace IronJS.Compiler.Ast {
    public interface INode {
        INode[] Children { get; set; }
        SourcePosition Source { get; }
        Runtime.Type Type { get; }
        bool TypeIsStatic { get; }
        bool HasChildren { get; }
        bool As<T>(out T result) where T : class;
    }
}
