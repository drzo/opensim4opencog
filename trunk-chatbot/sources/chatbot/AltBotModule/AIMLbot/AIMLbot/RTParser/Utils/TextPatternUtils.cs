using System;
using System.Collections;
using System.IO;
using System.Xml;
using AltAIMLbot.Utils;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;

namespace RTParser.Utils
{
    [Serializable]
    public class TextPatternUtils : StaticXMLUtils
    {
        public static string StrTrimWSpace = "@#$%^&*()_+,/{}[]\\\";'~. ";
        public static char[] SymTrimWSpace = StrTrimWSpace.ToCharArray();
        public static char[] SymTrimWSpaceWQuest = (StrTrimWSpace + "?").ToCharArray();

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

        /// Checks that the provided sentence ends with a sentence splitter
        /// </summary>
        /// <param name="sentence">the sentence to check</param>
        /// <returns>True if ends with an appropriate sentence splitter</returns>
        static public bool checkEndsAsSentence(string sentence)
        {
            sentence = Trim(sentence);

            if ("!?.".Contains(sentence.Substring(sentence.Length - 1))) return true;
            foreach (Unifiable splitter in AltBot.Splitters)
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
            //XmlDocumentLineInfo.TextFormatter = CleanWildcards;
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
            if (name is Unifiable) return IsNullOrEmpty(((Unifiable)name).Raw);
            if (name == null) return true;
            if (IsNull(name)) return true;
            name = name.ToString();
            if (IsMissing(name))
            {
                //writeDebugLine("Special IsMissing " + Unifiable.DescribeUnifiable(name));
                return true;
            }
            return ((String)name).Length == 0;
        }

        public static bool IsNull(Object name)
        {
            if (ReferenceEquals(name, null)) return true;
            if (ReferenceEquals(name, Unifiable.NULL)) return true;
            object s = name;
            if (name is Unifiable)
            {
                s = ((Unifiable)name).SpecialName;
            }
            if (s is string)
            {
                if ((string)s == "$NULL")
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
            if (ReferenceEquals(name, Unifiable.MISSING) || ReferenceEquals(name, null))
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
            var name2 = ((Unifiable) name).SpecialName;
            if (IsNull(name2)) return false;
            return IsIncomplete(name2);
        }

        public static bool IsEMPTY(Object name)
        {
            if (name is String)
            {
                string stringValue = ((String)name);
                if (stringValue.Length == 0) return true;
                if (stringValue == " ")
                {
                    return true;
                }
                string stringValueTrim = Trim(stringValue);
                if (stringValueTrim.Length == 0)
                {
                    writeDebugLine("WARN: Trim(self).Length == 0  '" + name + "' was never supposed to happen");
                    return true;
                }
                if (stringValueTrim == "$EMPTY")
                {
                    return true;
                }
                if (stringValueTrim == "$SPACE")
                {
                    return true;
                }
                return false;
            }
            if (ReferenceEquals(name, null))
            {
                return false;
            }
            if (name is Unifiable)
            {
                if (ReferenceEquals(name, Unifiable.NULL))
                {
                    return false;
                }
                if (ReferenceEquals(name, Unifiable.Empty))
                {
                    return true;
                }
                if (ReferenceEquals(name, Unifiable.SPACE))
                {
                    return true;
                }
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
            if (b) return true && !s.Contains(" in ") && !s.Contains(" at ") && !s.Contains(" on ");
            if (unifiable is Unifiable)
            {
                if (!((Unifiable)unifiable).IsWildCard) return false;
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

        private static string[][] wasThese = null;
        public static string SPLITMATCHABLE(string local)
        {
            var these = new string[][]
                            {
                                new[] {"_", "*",},


                                new[] {"<BR>", "<BR />"},

                                new[] {"RN'T ", "RE NOT "},
                                new[] {"N'T ", " NOT "},

                                new[] {"<P>", "<BR />",},
                                new[] {"</P>", "<BR />",},
                                new[] {"<P />", "<BR />",},
                                
                                new[] {"<EM>", " ",},
                                new[] {"</EM>", " ",},
                                new[] {"<EM />", " ",},

                                new[] {"<B>", " ",},
                                new[] {"</B>", " ",},
                                new[] {"<B />", " ",},
                                



                                new[] {" VAR=", " NAME="},
                                new[] {" IDX=", " INDEX="},
                                new[] {" VAL=", " VALUE="},

                                new[] {"1", "2"},
                                new[] {"3", "2"},

                                new[] {"2,2", "2"},
                                new[] {"2,*", "2"},
                                new[] {"*,2", "2"},

                                new[] {" INDEX=\"2\"", " "},

                                new[] {" NAME=\"*\"", " "},

                                new[] {"2>", ">"},
                                new[] {"2 />", " />"},


                                new[] {" NAME=\"*\"", " "},

                                new[] {"<SR />", "<SRAI><STAR /></SRAI>",},

                                new[] {"<SRAI>", " ",},
                                new[] {"</SRAI>", " ",},

                                new[] {"PERSON2", "PERSON",},
                                new[] {"PATTERN", "STAR",},
                                new[] {"INPUT", "STAR",},                                                                
                                new[] {"THATSTAR", "STAR",},


                                new[] {"<PERSON>", " ",},
                                new[] {"</PERSON>", " ",},

                                new[] {"<RANDOM>", " ",},
                                new[] {"</RANDOM>", " ",},

                                new[] {"?", " <BR />",},
                                new[] {",", " ",},
                                new[] {".", "<BR />",},
                                new[] {"!", "<BR />",},
                                new[] {"</LI>", "<BR />",},
                                new[] {"<LI>", "<BR />",},

                                new[] {"<SR />", " * ",},
                                new[] {"<STAR />", " * "},
                                new[] {"<PERSON />", " * "},
                                
                                new[] {"  ", " ",},
                                new[] {" <", "<",},
                                new[] {"> ", ">",},
                                new[] {"<BR /><BR />", "<BR />",},

                            };
            if (wasThese != these)
            {
                wasThese = these;
            }
            var strUn = ReplaceMap(local, wasThese);
            bool bred = false;
            if (strUn.Contains("<BR /><BR />"))
            {
                strUn = strUn.Replace("<BR /><BR />", "<BR />");
                bred = true;
            }
            while (strUn.StartsWith("<BR />"))
            {
                strUn = strUn.Substring(6);
            }
            while (strUn.EndsWith("<BR />"))
            {
                strUn = strUn.Substring(0, strUn.Length - 6);
            }
            if (ReferenceEquals(local, strUn))
            {
                return Unifiable.Intern(local);
            }

            local = ReTrimAndspace(strUn);
            if (local.Contains("<BR />"))
            {
                strUn = local.Replace("<BR />", " ");
                if (strUn.Contains("<"))
                {
                    return Unifiable.Intern(local);
                }
            }
            return Unifiable.Intern(local);
        }

        public static string MakeMatchable(string xml2)
        {
            if (xml2 == null) return xml2;
            if (!ContainsXml(xml2))
            {
                xml2 = ReplaceMap(xml2, new[]
                                     {
                                         new string[] {".", " "},
                                         new string[] {",", " "},
                                         new string[] {"?", " "},
                                         new string[] {"!", " "},
                                     });
                return ReTrimAndspace(xml2);
            }
            string xml22 = CleanWhitepaces(xml2);
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

        public static bool ContansNoInfo(Unifiable cond)
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
            if (normalizedPattern == null) return null;
            if (normalizedPattern.EndsWith("?") || normalizedPattern.EndsWith(".") || normalizedPattern.EndsWith("!"))
            {
                normalizedPattern = normalizedPattern.Substring(0, normalizedPattern.Length - 1).Trim();
            }
            return normalizedPattern;
        }

        public static string NoWilds(string pattern)
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

        public static bool IsSomething(Unifiable s, out Unifiable something)
        {
            something = s;
            if (IsNullOrEmpty(s) || IsIncomplete(s))
            {
                return false;
            }
            string ss = s.ToUpper().Trim();
            if (ss.Contains("TAG-"))
            {
                return false;
            }
            if (ss.Length < 2) return false;
            if (ss == ".")
            {
                return false;
            }
            return (ss != "NOTHING");
        }
        /*  public static string ToUpper(Unifiable param1)
          {
              return param1.ToUpper();
          }*/

        public static bool MessagePrefixName(string sep, string message, out string toWhom, out string fromWhom, out string newMessage)
        {
            if (!SplitOff(message, sep, out fromWhom, out newMessage))
            {
                toWhom = null;
                return false;
            }
            string newNewMessage;
            if (SplitOff(newMessage, ",", out toWhom, out newNewMessage))
            {
                newMessage = newNewMessage;
                return true;
            }
            else
            {
                toWhom = null;
            }
            return true;
        }

        public static int CountOf(string source, string findStr)
        {
            int found = 0;
            int from = 0;
            int at = source.IndexOf(findStr, from);
            while (at > -1)
            {
                found++;
                from = at + 1;
                at = source.IndexOf(findStr, from);
            }
            return found;
        }

        internal static string CollectionString(ICollection list)
        {
            if (list == null) return "=NULLCOL=";
            bool needComma = false;
            var writer = new StringWriter().GetStringBuilder();
            foreach (object o in list)
            {
                if (needComma) writer.Append(",");
                writer.Append("\"");
                writer.Append(o);
                writer.Append("\"");
                needComma = true;
            }
            return writer.ToString();
        }

        public static string ToEmptyOrNot(string message)
        {
            if (message == null) return null;
            string trim = Trim(message);
            int trimLength = trim.Length;
            if (trimLength == 0) return "";
            if (trimLength == 1)
            {
                char ch = trim[0];
                if (ch == ',' || ch == '.') return "";
            }
            return trim;
        }


        public static string SymTrim(string sentence, params char[] moreChars)
        {
            if (sentence == null) return sentence;
            string s2;
            if (moreChars == null || moreChars.Length == 0)
            {
                s2 = sentence.Trim(TextPatternUtils.SymTrimWSpace);
            }
            else if (moreChars[0] == '?')
            {
                s2 = sentence.Trim(TextPatternUtils.SymTrimWSpaceWQuest);
            }
            else
            {
                return SymTrim(sentence, StrTrimWSpace + new string(moreChars));
            }
            return OlderReference(sentence, s2);
        }

        public static string SymTrim(string sentence, string chars)
        {
            string s2 = sentence.Trim(chars.ToCharArray());
            return OlderReference(sentence, s2);
        }
    }
}