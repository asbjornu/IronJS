using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

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
    }
}
