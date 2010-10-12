using System;
using System.Xml;
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
            left = Trim(args.Substring(0, lastIndex));
            right = Trim(args.Substring(lastIndex + split.Length));
            return true;
        }

        internal static bool DifferentBesidesCase(string sentenceIn, string sentence)
        {
            return sentence.ToLower() != sentenceIn.ToLower();
        }


        public static string Replace(string strTrim, string[][] pairs)
        {
            foreach (string[] pair in pairs)
            {
                strTrim = Replace(strTrim, pair[0], pair[1]);
            }
            return strTrim;
        }

        protected static string Replace(string source, string b, string a)
        {
            return OlderReference(source, source.Replace(b, a));
        }

        public static string ReTrimAndspace(string substitute)
        {
            if (substitute == null) return null;
            var s = OlderReference(substitute, substitute.Replace("  ", " "));
            if (s.Length == 1)
            {
                if (s != " ") return s;
                return s;
            }
            return Trim(substitute);
        }

        /// Checks that the provided sentence ends with a sentence splitter
        /// </summary>
        /// <param name="sentence">the sentence to check</param>
        /// <returns>True if ends with an appropriate sentence splitter</returns>
        static public bool checkEndsAsSentence(string sentence)
        {
            sentence = Trim(sentence);

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
            if (name is Unifiable) return IsNullOrEmpty(((Unifiable) name).Raw);
            if (name == null) return true;
            if (IsNull(name)) return true;
            name = name.ToString();
            if (IsMissing(name))
            {
                //writeDebugLine("Special IsMissing " + Unifiable.DescribeUnifiable(name));
                return true;
            }
            return ((String) name).Length == 0;
        }

        public static bool IsNull(Object name)
        {
            if (ReferenceEquals(name, null)) return true;
            if (ReferenceEquals(name, Unifiable.NULL)) return true;
            object s = name;
            if (name is Unifiable)
            {
                s = ((Unifiable) name).SpecialName;
            }
            if (s is string)
            {
                if ((string) s == "$NULL")
                {
                    return true;
                }
                return false;
            }
            if (s == null)
            {
                return true;
            }
            return false;
        }

        public static bool IsIncomplete(Object name)
        {
            if (ReferenceEquals(name, null))
            {
                return true;
            }
            if (name is Unifiable)
            {
                if (ReferenceEquals(name, Unifiable.INCOMPLETE))
                {
                    return true;
                } if (ReferenceEquals(name, Unifiable.MISSING))
                {
                    return true;
                }
                if (ReferenceEquals(name, Unifiable.Empty))
                {
                    return false;
                }
                if (ReferenceEquals(name, Unifiable.NULL) || IsNull(name))
                {
                    return true;
                }
                var name2 = ((Unifiable)name).SpecialName;
                if (IsNull(name2))
                {
                    writeDebugLine("WARN: this case isNull '" + name2 + "' was never supposed to happen");
                    return false;
                }
                if (IsEMPTY(name2)) return false;
                name = name2;
            }
            if (name is string)
            {
                string sname = ToUpper(((string)name));
                if (sname == "$INCOMPLETE" || sname == "$NULL") return true;
                if (sname == "OM" || sname == "$MISSING")
                {
                    return true;
                }
            }            
            return false;
        }


        public static bool IsValue(Unifiable name)
        {
            return (!IsNull(name) && !IsIncomplete(name)) || IsMissing(name);
        }

        public static bool IsMissing(Object name)
        {
            if (ReferenceEquals(name, Unifiable.MISSING) || name == null)
            {
                return true;
            }
            if (ReferenceEquals(name, Unifiable.NULL) || IsNull(name))
            {
                return false;
            }
            if ((name is string))
            {
                string sname = ToUpper(((string) name));
                return sname == "OM" || sname == "$MISSING";
            }
            if (!(name is Unifiable))
            {
                return false;
            }
            var name2 = ((Unifiable)name).SpecialName;
            if (IsNull(name2)) return false;
            return IsIncomplete(name2);
        }

        public static bool IsEMPTY(Object name)
        {
            if (ReferenceEquals(name, Unifiable.Empty))
            {
                return true;
            }
            if (name is String)
            {
                string stringValue = ((String)name);
                if (stringValue.Length == 0) return true;
                string stringValueTrim = Trim(stringValue);
                if (stringValueTrim.Length == 0)
                {

                    writeDebugLine("WARN: this case isNull '" + Unifiable.DescribeUnifiable(name) + "' was never supposed to happen");
                    return true;
                }
                if (stringValueTrim == "$EMPTY")
                {
                    return true;
                }
                return false;
            }
            if (ReferenceEquals(name, null) || ReferenceEquals(name, Unifiable.NULL))
            {
                return false;
            }
            if (name is Unifiable)
            {
                if (IsEMPTY(((Unifiable)name).SpecialName))
                {
                    return true;
                }
                return false;
            }
            return false;
        }

        public static bool IsUnknown(object unifiable)
        {
            if (IsNullOrEmpty(unifiable)) return true;
            if (IsIncomplete(unifiable))
            {
                return true;
            }
            string ss = Unifiable.ToStringLValue(unifiable);
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
            if (!ContainsXml(xml2))
            {
                xml2 = OlderReference(xml2, xml2.Replace(".", " ").Replace("?", " ").Replace("!", " "));
            }
            Unifiable xml22 = CleanWhitepaces(xml2);
            return OlderReference(xml2, xml22.ToUpper());
        }

        public static Unifiable MatchKeyClean(Unifiable unifiable)
        {
            throw new NotImplementedException();
            return ToUpper(MatchKeyClean(unifiable.AsString()));
        }

        public static Unifiable MatchKeyClean(string s)
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
            pattern = Trim(pattern);
            int pl = pattern.Length;
            if (pl < 4) return pattern;
            while (pattern.Contains("*"))
            {
                pattern = pattern.Replace("*", " ");
                pattern = Trim(pattern);
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
                                        if (Char.IsLetterOrDigit(c1) || Char.IsControl(c1)) return true;
                                        if ("\"'".Contains("" + c1)) return false;
                                        return false;
                                    },
                                    (c0, c1) =>
                                    {
                                        if (Char.IsLetterOrDigit(c0) || Char.IsControl(c0)) return true;
                                        if ("\"'".Contains("" + c0)) return false;
                                        return false;
                                    });
            return clean;
        }

        public static bool IsSomething(string s, out string something)
        {
            something = s;
            if (String.IsNullOrEmpty(s) || IsIncomplete(s))
            {
                return false;
            }
            return (s.ToLower() != "nothing");
        }

        public static string ToUpper(string param1)
        {
            var outp = param1.ToUpper();
            return OlderReference(param1, outp);
        }

        public static Unifiable ToUpper(Unifiable param1)
        {
            return param1.ToUpper();
        }

        public static string ToLower(string param1)
        {
            var outp = param1.ToLower();
            return OlderReference(param1, outp);
        }
        public static string Trim(string param1)
        {
            var outp = param1.Trim();
            return OlderReference(param1, outp);
        }
        public static string ConsolidSpaces(string param1)
        {
            var outp = param1.Replace("  ", " ");
            return OlderReference(param1, outp);
        }
        public static string Trim(Unifiable param1)
        {
            var outp = param1.Trim();
            return outp;
        }

        public static string OlderReference(string param1, string outp)
        {
            if (outp == param1)
            {
                return param1;
            }
            return outp;
        }

        public static string SafeFormat(string fmt, params object[] args)
        {
            return DLRConsole.SafeFormat(fmt, args);
        }
    }
}