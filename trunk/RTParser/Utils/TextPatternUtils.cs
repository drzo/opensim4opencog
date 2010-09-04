using System;
using System.Text;
using System.Xml;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;

namespace RTParser.Utils
{
    public class TextPatternUtils
    {
        public static bool TryParseBool(string parse, out bool tf)
        {
            if (IsNullOrEmpty(parse))
            {
                tf = default(Boolean);
                return false;
            }
            parse = parse.ToUpper();
            if (IsFalseOrNo(parse))
            {
                tf = false;
                return true;
            }
            if (IsTrueOrYes(parse))
            {
                tf = true;
                return true;
            }
            tf = default(Boolean);
            return false;
        }

        public static bool IsFalseOrNo(string tst)
        {
            return (tst == "NO" || tst == "N" || tst == "FALSE" || tst == "F" || tst == "NIL");
        }
        public static bool IsTrueOrYes(string tst)
        {
            return (tst == "YES" || tst == "Y" || tst == "TRUE" || tst == "T");
        }


        static public bool IsTrue(Unifiable v)
        {
            if (!IsFalse(v))
            {
                if (!IsNullOrEmpty(v)) return true;
                return false;
            }
            return true;
        }

        static public bool IsLogicTF(Unifiable v, SubQuery subquery)
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
            string ss = CleanWhitepacesLower(unifiable.ToString());
            string s = " " + ss.Replace("_", " ").Replace("-", " ") + " ";
            bool b = s.Contains("unknown") || s.Contains("unrec") || s.Contains("unnam")
                     || s.Contains("unseen") || s.Contains("default")
                     || s.Contains(" some") || s.Contains("*") || s.Contains(" _ ")
                     || s.Contains(" nothing ") || s.Contains("undefined");
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

        public static OutputDelegate DEVNULL = TextFilter.DEVNULL;
        public static string CleanWhitepaces(object info)
        {
            if (info is XmlNode)
            {
                XmlNode n = (XmlNode) info;
                if (n.Name == "template") info = n.ParentNode;
            }
            if (info is TemplateInfo)
            {
                info = ((TemplateInfo) info).CategoryInfo;
            }
            return CleanWhitepaces("" + info);
        }


        public static string CleanWhitepacesLower(string xml2)
        {
            if (xml2 == null) return xml2;
            return CleanWhitepaces(xml2).ToLower().Replace(".", "").Replace("?", "").Replace("!", "");
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

        protected static string[] NamesStrings(string name)
        {
            return name.Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
        }

        protected static bool NameMatches(XmlNode node, string s)
        {
            return node.Name.ToLower() == s || node.LocalName.ToLower() == s;
        }


        public static string CleanWhitepaces(string xml2)
        {
            return CleanWhitepaces(xml2, null, Unused, Unused);
        }

        protected static bool Unused(char arg1, char arg2)
        {
            throw new NotImplementedException();
            return false;
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

        public static string CleanWhitepaces(string xml2, string padchars,
                                             Func<char, char, bool> ifBefore, Func<char, char, bool> ifAfter)
        {
            if (xml2 == null) return xml2;
            const long maxCleanSize = 2 << 14;
            int inlen = xml2.Length;
            if (inlen > maxCleanSize)
            {
                return xml2;
            }

            bool padWildCards = true;

            padWildCards = xml2.IndexOfAny("\\:/".ToCharArray(), 0) == -1;

            if (!padWildCards) padchars = null;

            StringBuilder s = new StringBuilder(inlen);

            bool chgd = false;
            bool xmlFound = false;
            bool inwhite = true;
            bool pendingWhitespace = false;
            char lastChar = '\0';
            foreach (char c0 in xml2)
            {
                if (c0 <= 32)
                {
                    if (inwhite)
                    {
                        chgd = true;
                        continue;
                    }
                    inwhite = true;
                    pendingWhitespace = true;
                    continue;
                }
                switch (c0)
                {
                    case '/':
                        if (lastChar == 'r')
                        {
                            xmlFound = true;
                        }
                        inwhite = true;
                        if (pendingWhitespace)
                        {
                            chgd = true;
                            pendingWhitespace = false;
                        }
                        break;
                    case '>':
                    case '<':
                    case '\\':
                        inwhite = true;
                        if (pendingWhitespace)
                        {
                            chgd = true;
                            pendingWhitespace = false;
                        }
                        break;
                    default:
                        if (padchars != null)
                        {
                            bool before = padchars.Contains("" + lastChar);
                            if (before && ifBefore(lastChar, c0))
                            {
                                if (!inwhite) pendingWhitespace = true;
                            }

                            bool after = padchars.Contains("" + c0);
                            if (after && ifAfter(lastChar, c0))
                            {
                                if (!inwhite) pendingWhitespace = true;
                            }
                        }
                        inwhite = false;
                        break;
                }
                if (pendingWhitespace)
                {
                    s.Append(' ');
                    pendingWhitespace = false;
                }
                s.Append(c0);
                lastChar = c0;
            }
            if (pendingWhitespace) chgd = true;
            int len = s.Length;
            if (xmlFound)
            {
                s = s.Replace("<sr/>", "<srai><star index=\"1\"/></srai>");
                s = s.Replace("star/>", "star index=\"1\"/>");
                if (len != s.Length) chgd = true;
            }
            if (!chgd)
            {
                if (len != inlen)
                {
                    return s.ToString();
                }
                return xml2;
            }
            //s = s.Replace("<star index=\"1\"", "<star");

            return s.ToString();
        }
    }
}