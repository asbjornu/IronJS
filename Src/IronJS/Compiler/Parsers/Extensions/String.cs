using System;
using System.Globalization;

namespace IronJS.Compiler.Parsers.Extensions {
    public static class StringExtensions {
        public static bool TryToDouble(this string s, out double result) {
            return double.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture, out result);
        }

        public static double ToDouble(this string s) {
            double result;

            if (s.TryToDouble(out result))
                return result;

            throw new CompilerError("Could not convert '{0}' to number", s);
        }

        public static string ToJsString(this string s) {
            //If string is >= 2 characters, the start and end 
            //character are equal and one of " or ', strip them
            if (s.Length >= 2 && s[0] == s[s.Length - 1] && (s[0] == '"' || s[0] == '\'')) {
                return s.Substring(1, s.Length - 2);
            }

            return s;
        }
    }
}
