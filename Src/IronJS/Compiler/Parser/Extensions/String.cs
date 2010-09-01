using System;
using System.Globalization;

namespace IronJS.Compiler.Parser.Extensions {
    public static class StringExtensions {
        public static bool TryToDouble(this string s, out double result) {
            return double.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture, out result);
        }

        public static double ToDouble(this string s) {
            double result;

            if (s.TryToDouble(out result))
                return result;

            throw new Exception("Could not convert to number");
        }

        public static string ToJsString(this string s) {
            if (s.Length < 2)
                throw new Exception("String to short");

            if(s[0] == s[s.Length-1] && (s[0] == '"' || s[0] == '\'')) {
                return s.Substring(1, s.Length - 2);
            }

            return s;
        }
    }
}
