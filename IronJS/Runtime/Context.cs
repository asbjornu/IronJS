﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using IronJS.Runtime.Js;
using IronJS.Runtime.Binders;
using System.Linq.Expressions;
using System.Dynamic;
using System.Reflection;

namespace IronJS.Runtime
{
    public class Context
    {
        public IFrame SuperGlobals { get; protected set; }

        public IFunction Object { get; protected set; }
        public IObj ObjectPrototype { get; protected set; }

        public IFunction Function { get; protected set; }
        public IFunction FunctionPrototype { get; protected set; }

        protected Context()
        {

        }

        internal IFrame Run(Action<IFrame> delegat)
        {
            var globals = new Frame(SuperGlobals, true);

            delegat(globals);

            return globals;
        }

        public IObj CreateObject()
        {
            return CreateObject(null);
        }

        public IObj CreateObject(IObj ctor)
        {
            var obj = new Obj();

            obj.Context = this;
            obj.Class = ObjClass.Object;

            if (ctor == null)
            {
                obj.Prototype = ObjectPrototype;
            }
            else
            {
                var ptype = ctor.GetOwnProperty("prototype");

                obj.Prototype = (ptype is IObj)
                                ? ptype as IObj
                                : ObjectPrototype;
            }

            return obj;
        }

        public IFunction CreateFunction(IFrame frame, Lambda lambda)
        {
            var obj = new Function(frame, lambda);

            var protoObj = CreateObject();
            protoObj.SetOwnProperty("constructor", obj);

            obj.Context = this;
            obj.Class = ObjClass.Function;
            obj.Prototype = FunctionPrototype;
            obj.SetOwnProperty("prototype", protoObj);

            return obj;
        }

        #region Binders

        internal JsBinaryOpBinder CreateBinaryOpBinder(ExpressionType op)
        {
            return new JsBinaryOpBinder(op, this);
        }

        internal JsUnaryOpBinder CreateUnaryOpBinder(ExpressionType op)
        {
            return new JsUnaryOpBinder(op, this);
        }

        internal JsConvertBinder CreateConvertBinder(Type type)
        {
            return new JsConvertBinder(type, this);
        }

        internal JsGetIndexBinder CreateGetIndexBinder(CallInfo callInfo)
        {
            return new JsGetIndexBinder(callInfo, this);
        }

        internal JsSetIndexBinder CreateSetIndexBinder(CallInfo callInfo)
        {
            return new JsSetIndexBinder(callInfo, this);
        }

        internal JsGetMemberBinder CreateGetMemberBinder(object name)
        {
            return new JsGetMemberBinder(name, this);
        }

        internal JsSetMemberBinder CreateSetMemberBinder(object name)
        {
            return new JsSetMemberBinder(name, this);
        }

        internal JsInvokeBinder CreateInvokeBinder(CallInfo callInfo)
        {
            return new JsInvokeBinder(callInfo, this);
        }

        internal JsInvokeMemberBinder CreateInvokeMemberBinder(object name, CallInfo callInfo)
        {
            return new JsInvokeMemberBinder(name, callInfo, this);
        }

        internal JsCreateInstanceBinder CreateInstanceBinder(CallInfo callInfo)
        {
            return new JsCreateInstanceBinder(callInfo, this);
        }

        #endregion

        #region Static

        static public Context Setup()
        {
            var ctx = new Context();

            ctx.SuperGlobals = new Frame();

            ctx.ObjectPrototype = ctx.CreateObject();

            ctx.FunctionPrototype = ctx.CreateFunction(
                ctx.SuperGlobals,
                new Lambda(
                    new Func<IFrame, object>(FunctionPrototypeLambda),
                    new string[] { }.ToList()
                )
            );

            ctx.Object = ctx.CreateFunction(
                ctx.SuperGlobals,
                new Lambda(
                    new Func<IFrame, object>(ObjectConstructorLambda),
                    new[] { "value" }.ToList()
                )
            );

            ctx.Function = ctx.CreateFunction(
                ctx.SuperGlobals,
                new Lambda(
                    new Func<IFrame, object>(FunctionConstructorLambda),
                    new string[] { }.ToList()
                )
            );

            // Object
            (ctx.Object as Function).Prototype = ctx.FunctionPrototype;
            ctx.Object.SetOwnProperty("prototype", ctx.ObjectPrototype);

            // Function
            (ctx.Function as Function).Prototype = ctx.FunctionPrototype;
            ctx.Function.SetOwnProperty("prototype", ctx.FunctionPrototype);

            // Function.prototype
            (ctx.FunctionPrototype as Function).Prototype = ctx.ObjectPrototype;
            ctx.FunctionPrototype.SetOwnProperty("constructor", ctx.Function);

            // Push on global frame
            ctx.SuperGlobals.Push("Object", ctx.Object, VarType.Global);
            ctx.SuperGlobals.Push("Function", ctx.Function, VarType.Global);
            ctx.SuperGlobals.Push("undefined", Js.Undefined.Instance, VarType.Global);
            ctx.SuperGlobals.Push("Infinity", double.PositiveInfinity, VarType.Global);
            ctx.SuperGlobals.Push("NaN", double.NaN, VarType.Global);

            return ctx;
        }

        static public object FunctionPrototypeLambda(IFrame frame)
        {
            return Js.Undefined.Instance;
        }

        static public object FunctionConstructorLambda(IFrame frame)
        {
            return null;
        }

        static public object ObjectConstructorLambda(IFrame frame)
        {
            var value = (frame as Frame).Arg("value");

            if (value != null || value == Js.Undefined.Instance)
            {
                throw new NotImplementedException("ToObject() not implemented");
            }

            return null;
        }

        #endregion

        #region Methods

        static public class Methods
        {
            static public MethodInfo CreateFunction = typeof(Context).GetMethod("CreateFunction");
            static public MethodInfo CreateObject = typeof(Context).GetMethod("CreateObject", Type.EmptyTypes);
            static public MethodInfo CreateObjectCtor = typeof(Context).GetMethod("CreateObject", new[]{ typeof(IObj) });
        }

        #endregion

    }
}
