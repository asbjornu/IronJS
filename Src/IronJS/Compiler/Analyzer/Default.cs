﻿using System;
using IronJS.Compiler;
using IronJS.Compiler.Ast;
using IronJS.Compiler.Ast.Context;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Analyzer {
    public class Default : IAnalyzer {
        public INode[] Analyze(ScopeChain scopes, INode[] nodes) {
            var analyzedNodes = new INode[nodes.Length];

            for(var i = 0; i < nodes.Length; ++i) {
                analyzedNodes[i] = Analyze(scopes, nodes[i]);
            }

            return analyzedNodes;
        }

        INode Analyze(ScopeChain scopes, INode node) {
            if (node is Var) {
                return Analyze(scopes, node as Var);

            } else if (node is Binary) {
                return Analyze(scopes, node as Binary);

            } else if (node is Function) {
                return Analyze(scopes, node as Function);

            } else if (node is Unary) {
                return Analyze(scopes, node as Unary);

            } else {
                return node;
            }
        }

        INode Analyze(ScopeChain scopes, Unary node) {
            var target = Analyze(scopes, node.Target);

            switch (node.Op) {
                case UnaryOp.Inc:
                case UnaryOp.Dec:
                case UnaryOp.PostDec:
                case UnaryOp.PostInc:
                    if (target is Identifier) {
                        scopes.Current.Variables.AddType(target, Runtime.Type.Double);
                    }
                    break;
            }

            return new Unary(node.SourcePosition, target, node.Op);
        }

        INode Analyze(ScopeChain scopes, Function node) {
            scopes.Enter(node.Scope);
            return new Function(node.SourcePosition, Analyze(scopes, node.Body), scopes.Exit());
        }

        INode Analyze(ScopeChain scopes, Binary node) {
            var left = Analyze(scopes, node.Left);
            var right = Analyze(scopes, node.Right);

            switch (node.Op) {
                case BinaryOp.Assign:
                    if (left is Identifier) {
                        scopes.Current.Variables.AddAssignedFrom(left, right);
                    }
                    break;
            }

            return new Binary(node.SourcePosition, node.Op, left, right);
        }

        INode Analyze(ScopeChain scopes, Var node) {
            Identifier identifier;

            if (node.Node is Binary) {
                identifier = (node.Node as Binary).Left as Identifier;
                scopes.Current.Variables.Add(new Variable(identifier.Name));
                return Analyze(scopes, node.Node);
            }

            identifier = node.Node as Identifier;
            scopes.Current.Variables.Add(new Variable(identifier.Name));
            return new Pass();
        }
    }
}
