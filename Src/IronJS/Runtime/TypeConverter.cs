using System;

namespace IronJS.Runtime {
    public static class TypeConverter {
        public static Type ClrToJs<T>() {
            return ClrToJs(typeof(T));
        }

        public static Type ClrToJs(System.Type type) {
            if (type == typeof(string)) {
                return Type.String;

            } else if (type == typeof(double)) {
                return Type.Double;

            } else if (type == typeof(bool)) {
                return Type.Boolean;
            }

            return Type.Dynamic;
        }

        public static System.Type JsToClr(Type type) {
            switch (NormalizeJsType(type)) {
                case Type.String:
                    return typeof(string);

                case Type.Double:
                    return typeof(double);

                case Type.Boolean:
                    return typeof(bool);

                default:
                    return typeof(object);
            }
        }

        public static Type NormalizeJsType(Type type) {
            switch (type) {
                case Type.Number:
                case Type.Integer:
                case Type.Double:
                    return Type.Double;

                case Type.String:
                case Type.StringNull:
                    return Type.StringNull;

                case Type.Boolean:
                    return Type.Boolean;

                case Type.Undefined:
                case Type.UndefinedNull:
                    return Type.UndefinedNull;

                case Type.Null:
                case Type.Clr:
                case Type.ClrNull:
                case Type.Dynamic:
                    return Type.Clr;

                case Type.Array:
                case Type.ArrayNull:
                    return Type.Array;

                case Type.Function:
                case Type.FunctionNull:
                    return Type.Function;

                case Type.Object:
                case Type.ObjectNull:
                case Type.ArrFunc:
                case Type.ArrFuncNull:
                case Type.ObjArr:
                case Type.ObjArrNull:
                case Type.ObjFunc:
                case Type.ObjFuncArr:
                case Type.ObjFuncArrNull:
                case Type.ObjFuncNull:
                    return Type.Object;

                case Type.None:
                    throw new Error("'None' is not a valid IronJS.Runtime.Type value");

                default:
                    throw new Error("Invalid IronJS.Runtime.Type value ({0})", type);
            }
        }
    }
}
