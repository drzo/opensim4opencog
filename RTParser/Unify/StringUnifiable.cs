using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using System.Xml;
using RTParser.AIMLTagHandlers;
using RTParser.Database;
using RTParser.Utils;
using Console=System.Console;

namespace RTParser
{
    public class StringUnifiable : Unifiable
    {

        protected void SpoilCache()
        {
            splittedCache = null;
            valueCache = null;
            restCache = null;
            upperCache = null;
        }

        protected string str;

        protected Unifiable[] splittedCache = null;
        protected Unifiable restCache = null;
        protected string upperCache;
        private object valueCache;

        private string _str
        {
            set
            {
                if (value!=null)
                {
                    if (value.Contains("  "))
                    {
                        value = value.Replace("  ", " ");
                    }
                }
                Flags = FlagsForString(value);
                str = value;
            }
           // get { return str; }
        }
        public override string ToUpper()
        {
            if (upperCache == null)
            {
                upperCache = AsString().ToUpper().Trim();
            }
            return upperCache;
        }

        protected StringUnifiable()
        {
            str = "";
        }

        public StringUnifiable(string value)
        {
            _str = value;
        }
        public StringUnifiable(string v, bool tf)
        {
            str = v;
            if (tf || true)
            {
                int vLength = v.Length;
                ClassifyFlagTypes(v, vLength);
            }
            else
            {
                Flags = FlagsForString(str);
            }
        }

        private UFlags ClassifyFlagTypes(string v, int vLength)
        {

            if (vLength == 0)
            {
                Flags = UFlags.IS_EMPTY | UFlags.IS_EXACT;
                valueCache = "";
                upperCache = "";
                return Flags;
            }

            UFlags flag = GuessLastVType(vLength, v);

            while (vLength > 0)
            {
                if (v.StartsWith("()"))
                {
                    Flags |= UFlags.IS_FALSE;
                    v = v.Substring(2);
                    vLength -= 2;
                    continue;
                }
                if (v.StartsWith("~"))
                {
                    Flags |= UFlags.NO_BINDS_STARS;
                    v = v.Substring(1);
                    vLength -= 1;
                    continue;
                }
                if (v == "T")
                {
                    Flags |= UFlags.IS_TRUE;
                    v = v.Substring(1);
                    vLength -= 1;
                    continue;
                }
                if (v == "NIL")
                {
                    Flags |= UFlags.IS_FALSE;
                    v = v.Substring(3);
                    vLength -= 3;
                    continue;
                }
                if (v.StartsWith("TAG-"))
                {
                    Flags |= UFlags.IS_TAG;
                    v = v.Substring(4);
                    vLength -= 4;
                    continue;
                }
                else
                {
                    break;
                }

            }
            UFlags Flags0 = FlagsForString(str);
            return Flags | Flags0;
        }

        private UFlags GuessLastVType(int vLength, string v)
        {
            int vLengthM1 = vLength - 1;
            switch (v[vLengthM1])
            {
                case '*':
                    {
                        Flags |= UFlags.LONG_WILDCARD;
                        vLength = vLengthM1;
                        v = v.Substring(0, vLengthM1);
                    }
                    break;
                case '_':
                    {
                        Flags |= UFlags.SHORT_WILDCARD;
                        vLength = vLengthM1;
                        v = v.Substring(0, vLengthM1);
                    }
                    break;
                case '>':
                    {
                        Flags |= UFlags.LAZY_XML;
                        int find = v.IndexOf("h=\"");
                        if (find == 4)
                        {
                            switch (v[find])
                            {
                                case '*':
                                    {
                                        Flags |= UFlags.LONG_WILDCARD;
                                    }
                                    break;
                                case '_':
                                    {
                                        Flags |= UFlags.SHORT_WILDCARD;
                                    }
                                    break;
                                case '.':
                                    {
                                        Flags |= UFlags.ZERO_OR_MORE;
                                    }
                                    break;
                            }
                        }
                        //vLength = vLengthM1;
                    }
                    break;
                default:
                    Flags |= UFlags.IS_EXACT;
                    break;
            }
            return Flags;
        }

        static UFlags FlagsForString(string str)
        {
            if (str == null) return UFlags.IS_NULL;
            else
            {
                int len = str.Length;
                if (len == 0)
                    return UFlags.IS_EMPTY | UFlags.IS_EXACT;
                else
                {
                    UFlags Flags = UFlags.NO_FLAGS;
                    char c;
                    str = str.Trim();
                    if (str == "") return UFlags.IS_EMPTY;
                    c = str[0];
                    if (len == 1)
                    {
                        switch (c)
                        {
                            case '*':
                                return UFlags.LONG_WILDCARD | UFlags.BINDS_STARS;
                            case '_':
                                return UFlags.SHORT_WILDCARD | UFlags.BINDS_STARS;
                            case 'T':
                                return UFlags.IS_TRUE | UFlags.SHORT_WILDCARD;
                            default:
                                if (char.IsLetter(c))
                                {
                                    return UFlags.IS_EXACT;
                                }
                                else if (char.IsNumber(c) || c == '%')
                                {
                                    return UFlags.IS_EXACT;
                                }
                                else if (char.IsPunctuation(c) || c == '%')
                                {
                                    return UFlags.IS_PUNCT;
                                }
                                return UFlags.IS_EXACT;
                        }
                    }
                    string found = str.ToUpper();
                    int c1 = str[str.Length - 1];
                    switch (c)
                    {

                        case '(':
                            if (c1 == ')' && len == 2)
                                return UFlags.IS_FALSE;
                            break;                        
                        case 'O':
                            if (c1 == 'N')
                                return UFlags.IS_TRUE;
                            else if (c1 == 'F')
                                return UFlags.IS_TRUE;
                            break;
                        case 'N':
                            if (c1 == 'O' && len == 2)
                                return UFlags.IS_FALSE;
                            if (c1 == 'L' && len == 3)
                                return UFlags.IS_FALSE;
                            break;
                        case 'F':
                            if (c1 == 'E' && len == 5)
                                return UFlags.IS_FALSE;
                            break;
                        case 'T':
                            if (len > 4)
                            {
                                if (str.StartsWith("TAG-")) Flags |= UFlags.IS_TAG;
                            }
                            break;
                        case '*':
                            Flags |= UFlags.LONG_WILDCARD;
                            break;
                        case '~':
                            Flags |= UFlags.REG_CLASS | UFlags.NO_BINDS_STARS;
                            break;
                        case '^':
                            Flags |= UFlags.REG_CLASS;
                            break;
                        case '#':
                            Flags |= UFlags.REG_CLASS | UFlags.IS_TRUE | UFlags.IS_EXACT;
                            break;
                        default:
                            if (len > 3)
                            {
                                if (c == '<')
                                {
                                    Flags |= UFlags.LAZY_XML;

                                    if (UFlags.LAZY_XML == Flags)
                                    {
                                        if (str.Contains("_")) Flags |= UFlags.SHORT_WILDCARD;
                                        else if (str.Contains("*")) Flags |= UFlags.LONG_WILDCARD;
                                        else
                                        {
                                            Flags |= UFlags.NO_BINDS_STARS;
                                        }
                                    }
                                }
                                else
                                {
                                    int wh = str.IndexOf(' ');
                                    if (wh > 1) Flags |= UFlags.MORE_THAN_ONE;
                                    else Flags |= UFlags.ONLY_ONE;
                                    Flags |= UFlags.IS_EXACT;
                                }
                            }
                            break;

                    }


                    if (c == '_')
                        Flags |= UFlags.SHORT_WILDCARD;
                    else if (c == '*')
                        Flags |= UFlags.LONG_WILDCARD;
                    else if (Flags == UFlags.NO_FLAGS)
                    {
                        if (char.IsLetter(c))
                        {
                            return UFlags.IS_EXACT;
                        }
                        else if (char.IsNumber(c) || c == '%')
                        {
                            return UFlags.IS_EXACT;
                        }
                        else if (char.IsPunctuation(c) || c == '%')
                        {
                            return UFlags.IS_PUNCT;
                        }
                        return UFlags.IS_EXACT;
                    }
                    return Flags;
                }

            }
        }

        //public int Length
        //{
        //    get
        //    {
        //        if (str == null)
        //        {
        //            return 0;
        //        }
        //        return str.Length;
        //    }
        //}




        //public Unifiable Replace(object marker, object param1)
        //{
        //    return str.Replace(astr(marker), astr(param1));
        //}

        public static string astr(object param1)
        {
            return "" + param1;
        }

        public override Unifiable Trim()
        {
            if (str==null)
            {
                writeToLog("ERROR NULL.Tirm()!!");
                return NULL;
            }
            string str2 = AIMLLoader.CleanWhitepaces(str);
            if (str2 == str) return this;
            return str.Trim();
        }

        public override string AsString()
        {
            return str;
        }

        public override Unifiable ToCaseInsenitive()
        {
            return Create(str.ToUpper());
        }

        public virtual char[] ToCharArray()
        {
            return str.ToCharArray();
        }

        public override bool Equals(object obj)
        {
            if (obj is Unifiable) return ((Unifiable)obj) == this;
            var os = astr(obj);
            if (str == os) return true;
            if (str.ToLower() == os.ToLower())
            {
                return true;
            }
            return false;

        }

        public override object AsNodeXML()
        {
            return str.Replace(" /", "/").Replace("/ ", "/").
                Replace(" >", ">").Replace("> ", ">").
                Replace(" <", "<").Replace("< ", "<").
                ToLower();
        }

        public override string ToString()
        {
            if (str == null)
            {
                writeToLog("ToSTring=NULL");
                return null;
            }
            return str;
        }

        public override int GetHashCode()
        {
            if (IsWildCard()) return -1;
            return str.GetHashCode();
        }

        //public override Unifiable[] Split(Unifiable[] tokens, StringSplitOptions stringSplitOptions)
        //{
        //    return arrayOf(str.Split(FromArrayOf(tokens), stringSplitOptions));
        //}

        public override object Raw
        {
            get { return str; }
        }

        protected override bool IsFalse()
        {
            if (IsFlag(UFlags.IS_FALSE)) return true;
            if (IsFlag(UFlags.IS_TRUE)) return false;
            return false;
        }

        public override bool IsWildCard()
        {
            if (string.IsNullOrEmpty(str)) return false;
            char c = str[str.Length - 1];
            return c == '_' || c == '*';


            return false;

            if (str == "*" || str == "_")
            {
                return true;
            }
            if (str.Contains("*") || str.Contains("_")) return true;
            //if (IsMarkerTag()) return false;
            if (str.StartsWith("<"))
            {
                return IsLazyStar();
            }
            return false;
        }

        public override Unifiable[] ToArray()
        {
            if (splittedCache != null)
            {
                return splittedCache;
            }
            if (splittedCache == null) splittedCache = Splitter(str);
            return splittedCache;
        }

        public static Unifiable[] Splitter(string str)
        {
            string strTrim = str.Trim().Replace("  ", " ").Replace("  ", " ");
            if (!strTrim.Contains("<"))
                return arrayOf(strTrim.Split(BRKCHARS, StringSplitOptions.RemoveEmptyEntries));

            XmlDocumentLineInfo doc = new XmlDocumentLineInfo("split str: " + str, false);
            StringReader sr = new StringReader("<node>" + strTrim + "</node>");
            List<Unifiable> u = new List<Unifiable>();

            try
            {
                doc.Load(sr);

                if (!doc.HasChildNodes)
                {
                    writeToLog("ERROR No Children");
                }
                foreach (XmlNode node in doc.FirstChild.ChildNodes)
                {
                    if (node.NodeType == XmlNodeType.Comment) continue;
                    if (node.NodeType == XmlNodeType.Whitespace) continue;
                    if (node.NodeType == XmlNodeType.Text)
                    {
                        string splitMe = node.Value.Trim();
                        u.AddRange(Splitter(splitMe));
                    }
                    else if (node.NodeType == XmlNodeType.Element)
                    {
                        string splitMe = node.OuterXml.Trim();
                        u.Add(splitMe);
                    }
                    else
                    {
                        string splitMe = node.OuterXml.Trim();
                        u.Add(splitMe);
                    }
                }
                return u.ToArray();
            }
            catch (Exception e)
            {
                RTPBot.writeDebugLine("" + e.Message + ": " + strTrim);
            }
            return arrayOf(strTrim.Split(BRKCHARS, StringSplitOptions.RemoveEmptyEntries));
        }

        public override bool IsTag(string that)
        {
            return str == "TAG-" + that || str.StartsWith("<" + that.ToLower());
        }

        public override void Append(Unifiable p)
        {
            throw new Exception("this " + AsString() + " cannot be appended with " + p);
        }

        public override void Append(string part)
        {
            throw new NotImplementedException();
        }

        public override Unifiable Frozen(SubQuery subquery)
        {
            return Create(str);
        }

        public override Unifiable ToPropper()
        {
            int len = str.Length;

            if (len == 0) return this;
            string newWord = str.Substring(0, 1).ToUpper();
            if (len == 1)
            {
                if (newWord == str) return this;
            }
            newWord += str.Substring(1).ToLower();
            return newWord;
        }

        public override Unifiable Rest()
        {

            splittedCache = ToArray();
            if (restCache == null) 
             return Join(" ", splittedCache, 1, splittedCache.Length - 1);
            return restCache;

            if (String.IsNullOrEmpty(this.str)) return Unifiable.Empty;
            int i = str.IndexOfAny(BRKCHARS);
            if (i == -1) return Empty;
            restCache = str.Substring(i + 1);
            return Create(restCache.Trim());
        }

        readonly static char[] BRKCHARS = " \r\n\t".ToCharArray();

        public override Unifiable First()
        {
            if (String.IsNullOrEmpty(str)) return Unifiable.Empty;
            //int i = str.IndexOfAny(BRKCHARS);
            //if (i == -1) return Create(str);
            var s = ToArray();
            if (s == null) return null;
            if (s.Length < 1) return Empty;
            return s[0];
            //string rest = str.Substring(0, i - 1);
            //return Create(rest.Trim());
        }

        public override bool IsShort()
        {
            if (str == "_") return true;
            if (str.EndsWith("_")) return true;
            // if (this.IsMarkerTag()) return false; // tested by the next line
            if (IsLazyStar()) return false;
            if (IsLazy()) return true;
            return false;
        }

        public override bool IsFiniteWildCard()
        {
            if (str == "_") return true;
            // if (this.IsMarkerTag()) return false; // tested by the next line
            if (IsLazyStar()) return false;
            if (IsLazy()) return true;
            return false;
        }

        public override bool IsLongWildCard()
        {
            if (str == null) return false;
            if (str.Length == 0) return false;
            if (str.EndsWith("*")) return true;
            if (char.IsLetterOrDigit(str[0])) return false;
            if (str.StartsWith("<regex")) return false;
            return IsFlag(UFlags.LONG_WILDCARD);
            if (str == ("*")) return true;
            if (str == ("^")) return true;
            if (this.IsMarkerTag()) return false;
            if (IsLazyStar()) return true;
            return false;
        }

        public override bool IsLazy()
        {
            if (this.IsMarkerTag()) return false;
            if (str == null) return false;
            if (str == "") return false;
            if (str[0] == '~')
            {
                return true;
            }
            return str.StartsWith("<");
        }

        public override bool IsLitteral()
        {
            if (this.IsLazy()) return false;
            if (this.IsMarkerTag()) return true;
            if (this.IsWildCard()) return false;
            return true;
        }

        public virtual bool IsMarkerTag()
        {
            return IsFlag(UFlags.IS_TAG);
        }

        override public bool StoreWildCard()
        {
            return !str.StartsWith("~");// && !str.StartsWith("TAG-");           
        }

        public override bool IsAnySingleUnit()
        {
            return str == "_";
        }

        public override bool ConsumePath(int at, string[] tokens, out string fw, out Unifiable after, out int newAt, SubQuery query)
        {
            int tokenLen = tokens.Length;
            if (tokenLen == 0)
            {
                bool WasEmpty = IsEmpty;
                fw = "";
                after = Unifiable.Empty;
                newAt = at;
                return WasEmpty;
            }
            fw = tokens[at];
            newAt = at + 1;
            string fws = fw.ToUpper();
            string su = ToUpper();
            if (su == fws)
            {
                int atPlusOne = at + 1;
                after = string.Join(" ", tokens, atPlusOne, tokenLen - atPlusOne);
                return true;
            }
            int minLen = LengthMin;
            if (minLen > tokens.Length)
            {
                after = null;
                return false;
            }
            if (fws == "NOTHING")
            {
                after = null;
                return false;
            }
            Unifiable ovs = fws;
            if (IsFlag(UFlags.REG_CLASS))
            {

            }
            else if (IsFlag(UFlags.IS_EXACT))
            {
                if (ovs.Flags == Flags)
                {
                    after = null;
                    return false;
                }
            }
            if (UniifyPath0(at, tokens, tokenLen, ovs, newAt, minLen, query, out after))
            {
                return true;
            }
            if (MustBeFast) return false;

            int len = tokens.Length;
            Unifiable[] myA = ToArray();
            int upTo = myA.Length;
            // if (upTo == 0) return false;
            int min = 1;
            Unifiable matchMe = this;
            if (!IsLazy())
            {
                upTo = matchMe.ToUpper().Split(new char[] {' '}).Length;
                min = upTo;
            }
            else
            {
                matchMe = ToValue(query);
                upTo = matchMe.ToUpper().Split(new char[] {' '}).Length;
                min = upTo;
            }
            if (upTo > len)
            {
                upTo = len;
            }
            //if (upTo > 1) writeToLog("ConsumeFirst Try: " + fullpath);

            for (int j = min; j <= upTo; j++)
            {
                fw = Join(" ", tokens, 0, j);
                if (matchMe.WillUnify(fw, query))
                {
                    if (j > 1) writeToLog("ConsumeFirst Success!: " + at);
                    restCache = Join(" ", tokens, j, len - j);
                    return true;
                }
            }
            return false;
        }

        private bool UniifyPath0(int at, string[] tokens, int tokenLen, Unifiable ovs, int usedAt, int minLen, SubQuery query, out Unifiable after)
        {
            if (ovs.IsFlag(UFlags.IS_TAG | UFlags.IS_EMPTY))
            {
                after = null;
                return false;
            }

            if (ovs.IsFlag(UFlags.BINDS_STARS))
            {
                after = null;
                return false;
            }
            if (str.StartsWith("<"))
            {
                int usedReally = usedAt - at;
                if (UnifyTagHandler(ovs, query))
                {
                    after = string.Join(" ", tokens, usedReally, tokenLen - usedReally);
                    return true;
                }
                after = null;
                return false;
            }
            if (minLen > 1)
            {
                writeToLog("MinLen=" + minLen);
            }
            after = null;
            return false;
        }


        protected int LengthMin
        {
            get { return ToArray().Length; }
        }

        public override float Unify(Unifiable other, SubQuery query)
        {
            if (Object.ReferenceEquals(this, other)) return UNIFY_TRUE;
            if (Object.ReferenceEquals(null, other)) return UNIFY_FALSE;

            string su = ToUpper();
            string ou = other.ToUpper();
            if (su == ou) return UNIFY_TRUE;
            bool otherIsLitteral = other.IsLitteral();
            if (IsLitteral())
            {
                if (!otherIsLitteral)
                {
                    return other.Unify(this, query);
                }
                string sv = ToValue(query);
                string ov = other.ToValue(query);
                if (IsStringMatch(sv, ov))
                {
                    writeToLog("IsStringMatch({0}, {1})", sv, ov);
                    return UNIFY_TRUE;
                }
                return UNIFY_FALSE;
            }
            if (su == "*")
            {
                writeToLog("CALL CALL/WILL UNIFY");
                return !other.IsEmpty ? UNIFY_TRUE : UNIFY_FALSE;
            }
            else if (su == "_")
            {
                writeToLog("CALL CALL/WILL UNIFY");
                return other.IsShort() ? UNIFY_TRUE : UNIFY_FALSE;
            }
            else
            {
                string sv = ToValue(query);
                string ov = other.ToValue(query);
                if (IsStringMatch(sv, ov))
                {
                    writeToLog("IsStringMatch({0}, {1})", sv, ov);
                    return UNIFY_TRUE;
                }
                if (IsLazy())
                {
                    return UnifyTagHandler(other, query) ? UNIFY_TRUE : UNIFY_FALSE;
                }
                if (IsWildCard())
                {
                    writeToLog("UnifyLazy SUCCESS: " + other + " in " + query);
                    return UNIFY_TRUE;
                }
                writeToLog("UnifyLazy FALSE: " + other + " in " + query);
                return UNIFY_FALSE;
            }
        }

        private bool UnifyTagHandler(Unifiable ov, SubQuery query)
        {
            try
            {
                var flags = ov.Flags;
                ///bot.GetTagHandler(templateNode, subquery, request, result, request.user);
                if (IsCachedMatch(ov,query))
                {
                    writeToLog("UnifyLazy: CACHED" + ov + " in " + query);
                    return true;
                }
                if (ReferenceEquals(ov, valueCache))
                {
                    writeToLog("UnifyLazy: SUCCEED" + ov + " in " + query);
                    return true;
                }
                if (valueCache is String)
                {
                    String sv = (String)valueCache;
                    if (IsStringMatch(sv, ov.ToValue(query)))
                    {
                        writeToLog("UnifyLazy: SUCCEED" + ov + " in " + query);
                        return true;
                    }
                }
                valueCache = ToValue(query);
                var tagHandler = GetTagHandler(query);
                if (tagHandler.CanUnify(ov) == UNIFY_TRUE)
                {
                    writeToLog("UnifyLazy: SUCCEED" + ov + " in " + query);
                    return true;
                }
                Unifiable outputSentence = tagHandler.CompleteAimlProcess();
                string value = outputSentence.AsString();
                if (ov.ToUpper() == value.ToUpper())
                {
                    writeToLog("UnifyLazy: SUCCEED" + ov + " in " + query);
                    return true;
                }
                return false;
            }
            catch (Exception e)
            {
                writeToLog("UnifyLazy ERROR! " + e);
                return false;
            }
        }

        private bool IsCachedMatch(Unifiable unifiable, SubQuery query)
        {
            return false;
        }

        //private SubQuery savedSQ;
        //AIMLTagHandler savedTagHandler;
        //public XmlNode node;
        public AIMLTagHandler GetTagHandler(SubQuery subquery)
        {
            if (valueCache is AIMLTagHandler) return (AIMLTagHandler)valueCache;
            return subquery.GetTagHandler(GetNode());
        }
        public virtual XmlNode GetNode()
        {
            if (valueCache is XmlNode) return (XmlNode)valueCache;
            try
            {
                return AIMLTagHandler.getNode(str);
            }
            catch (Exception e)
            {
                return AIMLTagHandler.getNode("<template>" + str + "</template>");
            }
        }

        public override void Clear()
        {
            throw new IndexOutOfRangeException();
            _str = "";
        }

        sealed public override string ToValue(SubQuery query)
        {
            if (valueCache is string) return (String)valueCache;
            if (valueCache == null) valueCache = ToValue0(query);
            return "" + valueCache;
        }
       protected string ToValue0(SubQuery query)
        {
            if (IsLitteral()) return str;
            if (str.Length < 2)
            {
                return str;
            }
            if (IsLazy())
            {
                //todo 
                if (query == null) return AsString();
                AIMLTagHandler tagHandler = GetTagHandler(query);
                Unifiable outputSentence = tagHandler.CompleteAimlProcess();
                if (!outputSentence.IsEmpty) return outputSentence.AsString();
                writeToLog("Failed Eval " + str);
                ///bot.GetTagHandler(templateNode, subquery, request, result, request.user);
            }
            if (IsWildCard())
            {
                return str;
            }
            return AsString();
        }


        public override bool IsLazyStar()
        {
            if (!IsLazy()) return false;
            if (!str.StartsWith("<")) return false;
            if (str.Contains("star") || str.Contains("match="))
            {
                return true;
            }
            if (str.Contains("name="))
            {
                return true;
            }
            return false;
        }
    }
}