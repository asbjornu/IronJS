using System.Globalization;
using System;

namespace IronJS.Compiler.Parser {
    public static class StringConverter {
        public static bool TryToDouble(this string s, out double result) {
            return double.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture, out result);
        }

        public static double ToDouble(this string s) {
            double result;

            if (double.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture, out result))
                return result;

            throw new Exception("Could not convert to number");
        }

        public static string ToJsString(this string s) {
            if (s.Length < 2)
                throw new Exception("String to short");

            return s.Substring(1, s.Length - 2);
        }
    }
}
