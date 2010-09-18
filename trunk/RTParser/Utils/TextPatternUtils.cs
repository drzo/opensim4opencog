using System;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;

namespace RTParser.Utils
{
    public class TextPatternUtils : StaticXMLUtils
    {

        public static bool SplitOff(string args, string split, out string left, out string right)
        {
            int lastIndex = args.IndexOf(split);
            if (lastIndex == -1)
            {
                left = args;
                right = "";
                return false;
            }
            left = args.Substring(0, lastIndex).Trim();
            right = args.Substring(lastIndex + split.Length).Trim();
            return true;
        }

        internal static bool DifferentBesidesCase(string sentenceIn, string sentence)
        {
            return sentence.ToLower() != sentenceIn.ToLower();
        }

        internal static string ReTrimAndspace(string substitute)
        {
            if (substitute == null) return null;
            //var csubstitute = substitute.ToCharArray();
            return substitute.Replace("  ", " ").Replace("  ", " ").Replace("  ", " ").Trim();
        }

        /// Checks that the provided sentence ends with a sentence splitter
        /// </summary>
        /// <param name="sentence">the sentence to check</param>
        /// <returns>True if ends with an appropriate sentence splitter</returns>
        static public bool checkEndsAsSentence(string sentence)
        {
            sentence = sentence.Trim();

            if ("!?.".Contains(sentence.Substring(sentence.Length - 1))) return true;
            foreach (Unifiable splitter in RTPBot.Splitters)
            {
                if (sentence.EndsWith(splitter))
                {
                    return true;
                }
            }
            return false;
        }

        public static OutputDelegate DEVNULL = TextFilter.DEVNULL;

        static TextPatternUtils()
        {
            XmlDocumentLineInfo.TextFormatter = CleanWildcards;
        }

        public static bool IsTrue(Unifiable v)
        {
            if (!IsFalse(v))
            {
                if (!IsNullOrEmpty(v)) return true;
                return false;
            }
            return true;
        }

        public static bool IsLogicTF(Unifiable v, SubQuery subquery)
        {
            if (IsFalse(v)) return false;
            String value = v.ToValue(subquery).ToLower();
            if (value.Length == 0) return false;
            char c = value[0];
            if (c == 'n' || c == 'f') return false;
            return true;
        }

        //public static Unifiable Format(string s, params object[] args)
        //{
        //    return string.Format(s, args);
        //}

        //static public bool operator ==(Unifiable t, string s)
        //{
        //    return t.AsString().ToLower() == s.ToLower();
        //}


        public static bool IsFalse(Unifiable tf)
        {
            if (ReferenceEquals(tf, null)) return true;
            if (ReferenceEquals(tf.Raw, null)) return true;
            return tf.IsFalse();
        }

        public static bool IsNullOrEmpty(Object name)
        {
            if (name is String)
            {
                return ((String)name).Length == 0;
            }
            if (IsNull(name)) return true;
            return (name is Unifiable && ((Unifiable)name).IsEmpty);
        }

        public static bool IsNull(Object name)
        {
            if (ReferenceEquals(name, null)) return true;
            return (name is Unifiable && ((Unifiable)name).Raw == null);
        }

        public static bool IsEMPTY(Object name)
        {
            if (name is String)
            {
                return ((String)name).Trim().Length == 0;
            }
            return (name is Unifiable && ((Unifiable)name).Raw == null);
        }

        public static bool IsUnknown(object unifiable)
        {
            if (IsNullOrEmpty(unifiable)) return true;
            string ss = MakeMatchable(unifiable.ToString());
            string s = " " + ss.Replace("_", " ").Replace("-", " ") + " ";
            bool b = s.Contains("unknown") || s.Contains("unrec") || s.Contains("unnam")
                     || s.Contains("unseen") || s.Contains("default")
                     || s.Contains(" some") || s.Contains("*") || s.Contains(" _ ")
                     || s.Contains(" nothing ") || s.Contains("undefine")
                     || s.Contains("$");
            if (b) return true;
            if (unifiable is Unifiable)
            {
                if (!((Unifiable)unifiable).IsWildCard()) return false;
                return true;
            }
            return false;
            //switch (s)
            //{
            //    case "":
            //        return true;
            //    case "unknown":
            //        return true;
            //    case "nothing":
            //        return true;
            //    case "*":
            //        return true;
            //    case "_":
            //        return true;
            //    case "undefined":
            //        return true;
            //    default:
            //        return false;
            //}
        }

        public static string MakeMatchable(string xml2)
        {
            if (xml2 == null) return xml2;
            return CleanWhitepaces(xml2.ToLower().Replace(".", " ").Replace("?", " ").Replace("!", " "));
        }

        public static string MatchKeyClean(Unifiable unifiable)
        {
            return MatchKeyClean(unifiable.AsString());
        }

        public static string MatchKeyClean(string s)
        {
            s = CleanWhitepaces(s);
            if (s == "")
            {
                return "*";
            }
            return s;
        }

        protected static bool ContansNoInfo(Unifiable cond)
        {
            return cond == null || cond == Unifiable.STAR || cond == Unifiable.Empty;
        }


        internal static string GetNamedType(string name, string value)
        {
            if (value != null)
            {
                value = value.ToLower();
                if (value == "true" || value == "t" || value == "yes" || value == "y")
                {
                    return name;
                }
                if (value == "false" || value == "f" || value == "no" || value == "n")
                {
                    return null;
                }
                else
                {
                    return value;
                }
            }
            return value;
        }

        public static string CleanPunct(string normalizedPattern)
        {
            if (normalizedPattern.EndsWith("?") || normalizedPattern.EndsWith(".") || normalizedPattern.EndsWith("!"))
            {
                normalizedPattern = normalizedPattern.Substring(0, normalizedPattern.Length - 1).Trim();
            }
            return normalizedPattern;
        }

        protected static string NoWilds(string pattern)
        {
            pattern = pattern.Trim();
            int pl = pattern.Length;
            if (pl < 4) return pattern;
            while (pattern.Contains("*"))
            {
                pattern = pattern.Replace("*", " ").Trim();
            }
            return pattern;
        }

        public static string CleanWildcards(string text)
        {
            if (text == null) return text;
            if (text.Contains("\""))
            {
                return CleanWhitepaces(text);
            }
            string clean = text;
            clean = CleanWhitepaces(text, "*",
                                    (c0, c1) =>
                                    {
                                        if (char.IsLetterOrDigit(c1) || char.IsControl(c1)) return true;
                                        if ("\"'".Contains("" + c1)) return false;
                                        return false;
                                    },
                                    (c0, c1) =>
                                    {
                                        if (char.IsLetterOrDigit(c0) || char.IsControl(c0)) return true;
                                        if ("\"'".Contains("" + c0)) return false;
                                        return false;
                                    });
            return clean;
        }
    }
}