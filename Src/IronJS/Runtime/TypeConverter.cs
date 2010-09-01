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
            if (type == Type.String) {
                return typeof(string);

            } else if (type == Type.Double) {
                return typeof(double);

            } else if (type == Type.Boolean) {
                return typeof(bool);

            }

            return typeof(object);
        }
    }
}
