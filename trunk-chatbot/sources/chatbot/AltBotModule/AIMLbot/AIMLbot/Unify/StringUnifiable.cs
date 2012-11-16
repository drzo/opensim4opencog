using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading;
using System.Xml;
using AltAIMLbot.Utils;
using RTParser.Database;
using RTParser.Utils;
using LineInfoElement = MushDLR223.Utilities.LineInfoElementImpl;
using IndexTargetList = System.Collections.Generic.ICollection<RTParser.IndexTarget>;
using IndexTargetListImpl = System.Collections.Generic.HashSet<RTParser.IndexTarget>;

namespace RTParser
{
    [Serializable]
    public class StringUnifiable : Unifiable
    {
        public virtual int SpoilCache()
        {
            int total = 0;
            if (splittedCache != null) total++;
            splittedCache = null;
            if (valueCache != null) total++;
            valueCache = null;
            if (valueCache is IDisposable)
            {
                IDisposable idispose = (IDisposable) valueCache;
                try
                {
                    idispose.Dispose();
                }
                catch (Exception e)
                {
                    writeToLog("ERROR: DISPOSE caused " + e);
                    throw;
                }
            }
            if (!ReferenceEquals(restCache, null)) total++;
            restCache = null;
            if (upperCache != null) total++;
            upperCache = null;
            _strictness = -11;
            return total;
        }

        public override string ToDebugString()
        {
            return SpecialName;
        }

        public override int RunLowMemHooks()
        {
            if (nodeOuter != null) return 0;
            int total = 0;
            if (splittedCache != null)
            {
                lock (this)
                {
                    Unifiable[] prev = splittedCache;
                    splittedCache = null;
                    try
                    {
                        total += 1;
                        foreach (Unifiable u in prev)
                        {
                            if (u != null && ReferenceEquals(u, this))
                            {
                                total += u.RunLowMemHooks();
                            }
                        }
                    }
                    finally
                    {
                        splittedCache = prev;
                    }
                }
            }
            if (restCache != null)
            {
                Unifiable restCache0 = restCache;
                restCache = null;
                total += restCache0.RunLowMemHooks();
            }
            double localStrictnes = _strictness;
            total += SpoilCache();
            _strictness = localStrictnes;
            return total;
        }

        internal string str;

        [NonSerialized]
        public Unifiable[] splittedCache;
        [NonSerialized]
        protected Unifiable restCache;
        [NonSerialized]
        protected string upperCache;
        [NonSerialized]
        public object valueCache;
        [NonSerialized]
        public object nodeOuter;
        private double _strictness = -11;

        public override double Strictness
        {
            get
            {
                if (_strictness == -11)
                {
                    _strictness = -Looseness;
                }
                return _strictness;
            }
        }
        public override bool WillMatch(string word, SubQuery query)
        {
            return WillMatch0(word.ToUpper(), query);
        }
        public override bool WillMatch0(string word, SubQuery query)
        {
           // if (IsAnyText) return true;
            //if (ToKey() == word.ToUpper()) return true;
            if (IsLitteralText && !IsLazy && !IsCatchAll)
            {
                return word == ToUpper();
            }
            if (str == "*" || str == "_") return true;
            if (IsLazy)
            {
                var toString = ToValue(query);
                if (toString == null)
                {
                    return false;
                }
                toString = toString.ToUpper();
                if (toString == word)
                {
                    return true;
                }
                return false;
            }
            return false;
        }

        public double Looseness
        {
            get
            {
                if (str == null) return 0; // 2.1;
                if (str == "") return 0; // 2.1;
                if (str == "*") return 6.0;
                if (str == "_") return 4.2;
                char c0 = str[0];
                char cN = str[str.Length - 1];
                if (c0 == '*' && '*' == cN) return 5; // 0.0;
                if (c0 == '_' && '_' == cN) return 4;
                if (c0 == '<' && cN == '>') return 3;
                if (c0 == '~' || cN == '~') return 3.2;
                if (c0 == '#') return 0.3;
                if (c0 == '*' || cN == '*') return 4;
                if (c0 == '_' || cN == '_') return 4;
                return -0.5;
            }
        }

        public override int CompareTo(Unifiable other0)
        {
            StringUnifiable other = other0 as StringUnifiable;
            if (other == null) return -1;
            string otherstr = other.str;
            if (otherstr == str) return 0;
            char c0 = str[0];
            char cN = str[str.Length - 1];
            double strictness = Strictness;
            double otherStrictness = other.Strictness;
            if (strictness == otherStrictness)
            {
                return ToUpper().CompareTo(other.ToUpper());
            }
            return strictness.CompareTo(otherStrictness);
        }

        public override bool AddCategory(IndexTarget template)
        {
            if (NOCateIndex) return false;
            IndexTargetList categoryInfos;
            lock (categoryInfosDictionary)
            {
                categoryInfos = CategoryInfos;
                if (categoryInfos == null)
                {
                    CategoryInfos = new IndexTargetListImpl {template};
                    return true;
                }
            }
            lock (categoryInfos)
            {
                int countAlready = categoryInfos.Count;
                if (countAlready > MaxCategories)
                {
                    return false;
                }
                categoryInfos.Add(template);
                countAlready++;
                if (countAlready%PrintCategories == 0)
                {
                    writeToLog("AddCategory '" + SpecialName + "' Count=" + categoryInfos.Count + " for " + template);
                }
            }
            return false;
        }

        public override bool RemoveCategory(IndexTarget template)
        {
            if (NOCateIndex) return false;
            if (template == null) return false;
            IndexTargetList categoryInfos;
            lock (categoryInfosDictionary)
            {
                categoryInfos = CategoryInfos;
                if (categoryInfos == null) return false;
            }
            lock (categoryInfos)
            {
                int countAlready = categoryInfos.Count;
                switch (countAlready)
                {
                    case 0:
                        CategoryInfos = null;
                        return false;
                    case 1:
                        {
                            if (categoryInfos.Remove(template))
                            {
                                CategoryInfos = null;
                                return true;
                            }
                            return false;
                        }
                    default:
                        return categoryInfos.Remove(template);
                }
            }
        }

        public override Unifiable FullPath
        {
            get
            {
                return this;
                throw new NotImplementedException();
            }
        }

        public override bool IsCatchAll
        {
            get { return str == "*" || str == "_"; }
        }

        public override Unifiable[] Possibles
        {
            get
            {
                var ar = ToArray();
                if (splittedCache != null && splittedCache.Length == 1) return splittedCache;
                List<Unifiable> possibles = new List<Unifiable>();
                bool expanded = false;
                for (int i = 0; i < ar.Length; i++)
                {
                    Unifiable item = ar[i];
                    if (IsMulti(item))
                    {
                        foreach (var e in item.Possibles)
                        {
                            expanded = true;
                            ar[i] = e;
                            var uni = Join(" ", ar, 0, -1);
                            possibles.Add(uni);
                        }
                        // reset
                        ar[i] = item;
                    }
                }
                if (!expanded) return new[] { this };
                return possibles.ToArray();
            }
        }

        private string _str
        {
            set
            {
                if (value != null)
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
            try
            {
                if (upperCache != null) return upperCache;
            }
            finally
            {
                if (upperCache == null) upperCache = ToUpper0();
            }
            return upperCache;
        }

        public string ToUpper0()
        {
            string upperCache = null;
            {
                string schached = str;
                if (StaticAIMLUtils.ContainsXml(schached))
                {
                    Unifiable[] vv = ToArray();
                    if (vv.Length == 0)
                    {
                        upperCache = ToUpper(schached);
                        return upperCache;
                    }
                    if (vv.Length == 1)
                    {
                        upperCache = vv[0].AsString();
                        return UpcaseXMLSource ? ToUpper(upperCache) : upperCache;
                    }
                    StringAppendableUnifiableImpl fupperCache = CreateAppendable();
                    foreach (Unifiable part in vv)
                    {
                        fupperCache.Append(part.ToUpper());
                    }
                    string fup = fupperCache.AsString();
                    if (fup.Length != schached.Length)
                    {
                        return fup;
                    }
                    upperCache = fup;
                    return upperCache;
                }
                upperCache = ToUpper(schached);
            }
            return upperCache;
        }


        public override void SetFromString(string value)
        {
            SpoilCache();
            SetAllFromString(value);
            SetFlagsFromString();
        }

        protected StringUnifiable()
        {
            str = "";
        }

        private void SetAllFromString(string value)
        {
            _str = value == null ? null : string.Intern(value);
        }

        public StringUnifiable(string value)
        {
            SetAllFromString(value);
        }

        public StringUnifiable(string v, bool tf)
        {
            SetAllFromString(v);
            if (tf)
            {
                SetFlagsFromString();
            }
        }

        private void SetFlagsFromString()
        {
            {
#if ACTUALLY_USING_ClassifyFlagTypes
                int vLength = v.Length;
                ClassifyFlagTypes(v, vLength);
#endif
                Flags = FlagsForString(str);
            }
        }

#if ACTUALLY_USING_ClassifyFlagTypes
        static internal bool FastFlags = true;
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
                if (v.StartsWith("~") || v.StartsWith("<bot"))
                {
                    Flags |= UFlags.NO_BINDS_STARS;
                    v = v.Substring(1);
                    vLength -= 1;
                    continue;
                }
                if (v == "T")
                {
                    Flags |= UFlags.IS_TRUE;
                    if (FastFlags) return Flags;
                    v = v.Substring(1);
                    vLength -= 1;
                    continue;
                }
                if (v == "NIL")
                {
                    Flags |= UFlags.IS_FALSE;
                    if (FastFlags) return Flags;
                    v = v.Substring(3);
                    vLength -= 3;
                    continue;
                }
                if (v.StartsWith("TAG-"))
                {
                    Flags |= UFlags.IS_TAG;
                    if (FastFlags) return Flags | UFlags.IS_EXACT;
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
#endif

        internal static UFlags FlagsForString(string str)
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
                    str = Trim(str);
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
                        case '_':
                            Flags |= UFlags.SHORT_WILDCARD;
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
                        case '<':
                            {
                                Flags |= UFlags.LAZY_XML;

                                if (UFlags.LAZY_XML == Flags)
                                {
                                    if (str.Contains("_")) Flags |= UFlags.SHORT_WILDCARD;
                                    else if (str.Contains("*")) Flags |= UFlags.LONG_WILDCARD;
                                    //else
                                    //{
                                    //    Flags |= UFlags.NO_BINDS_STARS;
                                    //}
                                }
                                if (found.Contains(">#$"))
                                    Flags |= UFlags.REG_CLASS | UFlags.IS_TRUE | UFlags.IS_EXACT;
                                if (found.Contains("BINDSTAR=\"TRUE\"") || found.Contains("BIND>") ||
                                    found.Contains("STAR>")) Flags |= UFlags.BINDS_STARS;
                                else Flags |= UFlags.NO_BINDS_STARS;
                                break;
                            }
                        default:
                            if (char.IsLetter(c))
                            {
                                Flags |= UFlags.IS_EXACT;
                            }
                            else if (char.IsNumber(c) || c == '%')
                            {
                                Flags |= UFlags.IS_EXACT;
                            }
                            else if (char.IsPunctuation(c) || c == '%')
                            {
                                Flags |= UFlags.IS_PUNCT;
                            }
                            else
                            {
                                //  Flags |= UFlags.IS_EXACT;
                            }
                            break;
                    }


                    if (Flags == UFlags.NO_FLAGS)
                    {
                        if (char.IsLetter(c))
                        {
                            Flags |= UFlags.IS_EXACT;
                        }
                        else if (char.IsNumber(c) || c == '%')
                        {
                            Flags |= UFlags.IS_EXACT;
                        }
                        else if (char.IsPunctuation(c) || c == '%')
                        {
                            Flags |= UFlags.IS_PUNCT;
                        }
                        Flags |= UFlags.IS_EXACT;
                    }


                    int wh = str.IndexOf(' ');
                    if (wh > 1) Flags |= UFlags.MORE_THAN_ONE;
                    else Flags |= UFlags.ONLY_ONE;
                    Flags |= UFlags.IS_EXACT;

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
            if (param1 is String) return (String) param1;
            return "" + param1;
        }

        public override Unifiable Trim()
        {
            if (str == null)
            {
                if (DebugNulls) writeToLog("ERROR NULL.Tirm()!!");
                return NULL;
            }
            string str2 = StaticAIMLUtils.CleanWhitepaces(str);
            if (str2 == str) return this;
            return Trim(str);
        }

        public override string AsString()
        {
            if (str == null)
            {
                if (DebugNulls) writeToLog("ERROR AsString  -U-NULL- !! DEBUG9");
                else throw new NullReferenceException("ERROR AsString  -U-NULL- !! DEBUG9");
                return null;
            }
            return str;
        }

        public override Unifiable ToCaseInsensitive()
        {
            return Create(ToUpper(str));
        }

        public virtual char[] ToCharArray()
        {
            return str.ToCharArray();
        }

        public override bool Equals(object obj)
        {
            if (obj is BestUnifiable) return ((BestUnifiable)obj).Equals(this);
            if (obj is Unifiable) return ((Unifiable) obj) == this;
            string os = astr(obj);
            if (str == os) return true;
            if (str.ToLower() == os.ToLower())
            {
                return true;
            }
            return false;
        }

        public override object AsNodeXML
        {
            get
            {
                return GetNode();
                /*
            if (upperCache != null) return upperCache;
            return ToLower(str.
                               Replace("/ ", "/").
                Replace(" >", ">").Replace("> ", ">").
                               Replace(" <", "<").Replace("< ", "<"));*/
            }
        }

        public override bool SameMeaningCS(Unifiable s, bool caseSensitive)
        {
            if (s is BestUnifiable) return s.SameMeaningCS(this, caseSensitive);
            if (ReferenceEquals(this, s)) return true;
            bool null2 = ReferenceEquals(s, null);
            if (null2) return false;
            if (str == s.SpecialName)
            {
                return true;
            }
            if (caseSensitive)
            {
                if (str == s.AsString())
                {
                    return true;
                }
                return false;
            }
            if (ToUpper() == ToUpper(s))
            {
                return true;
            }
            return false;
        }

        public override string ToString()
        {
            if (str == null)
            {
                if (DebugNulls) writeToLog("ToSTring=NULL");
                // ReSharper disable AssignNullToNotNullAttribute
                return null;
                // ReSharper restore AssignNullToNotNullAttribute
            }
            return str;
        }

        public override int GetHashCode()
        {
            if (IsWildCard) return -1;
            if (str == null) return 0;
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

        public override bool IsFalse()
        {
            if (IsFlag(UFlags.IS_FALSE)) return true;
            if (IsFlag(UFlags.IS_TRUE)) return false;
            return false;
        }

        public override bool IsWildCard
        {
            get
            {
                if (string.IsNullOrEmpty(str)) return false;
                char c = str[str.Length - 1];
                if (c == '_' || c == '*') return true;
                if (c == '>')
                {
                    return true;
                }
                return IsLazy;
            }
        }

        public override Unifiable[] ToArray()
        {
            if (splittedCache == null)
            {
                splittedCache = Splitter(str);
            }
            return splittedCache;
        }

        public Unifiable[] Splitter(string str0)
        {
            str0 = Trim(str0);
            StringUnifiable stringAppendable = null; // MakeStringUnifiable(str, true);
            string strTrim = str0; // stringAppendable.Trim();
            if (!strTrim.Contains(">") || !strTrim.Contains("<"))
            {
                var ss = SplitSimpleString(strTrim);
                return arrayOf(ss);
            }

            try
            {
                if (false)
                    strTrim = StaticAIMLUtils.ReplaceMap(strTrim,
                                         new[]
                                             {
                                                 new[] {"&gt;", "<gt />"},
                                                 new[] {"&lt;", "<lt />"},
                                                 new[] {"&quot;", "<qt />"},
                                                 new[] {"&amp;", "<amp />"},
                                             });
                XmlNode firstChild = StaticAIMLUtils.getDocNode("<li>" + strTrim + "</li>", false, false, StaticAIMLUtils.StringOnlyDoc);
                if (firstChild.ChildNodes.Count == 1)
                {
                    return new Unifiable[] {this};
                }
                List<Unifiable> u = new List<Unifiable>();
                CreateUnifableForList(firstChild.ChildNodes, null, u);
                if (u.Count == 0) return DontStore;
                if (u.Count == 1)
                {
                    return new Unifiable[] {u[0]}; //DontStore;}
                }
                return u.ToArray();
            }
            catch (Exception e)
            {
                AltBot.writeDebugLine("" + e.Message + ": " + " '" + str0 + "'");
                StringUnifiable su = MakeUnifiableFromString(str0, false) as StringUnifiable;
                Unifiable[] suu = new Unifiable[] {su};
                su.splittedCache = suu;
                return suu;
                throw;
            }
        }

        static IEnumerable SplitSimpleString(string strTrim)
        {
            return strTrim.Split(BRKCHARS, StringSplitOptions.RemoveEmptyEntries);
        }

        public void CreateUnifableForList(IEnumerable childNodes, StringAppendableUnifiableImpl stringAppendable,
                                          List<Unifiable> u)
        {
            try
            {
                int cc = -1; //
                //childNodes.Count;
                ICollection icol = childNodes as ICollection;
                if (icol != null) cc = icol.Count;
                else
                {
                    XmlNodeList xcol = childNodes as XmlNodeList;
                    if (xcol != null) cc = xcol.Count;
                }
                if (cc == 0) return;
                bool canUseComment = cc == 1;
                foreach (object onode in childNodes)
                {
                    XmlNode node = onode as XmlNode;
                    if (node == null)
                    {
                        u.Add(Create(onode));
                    }
                    else if (node.NodeType == XmlNodeType.Text)
                    {
                        string splitMe = Trim(StaticAIMLUtils.TextNodeValue(node, true));
                        if (splitMe == str)
                        {
                            writeToLog("ERROR: this inside of itself '" + str + "'");
                            u.Add(this);
                            continue;
                        }
                        var splitter = Splitter(splitMe);
                        u.AddRange(splitter);
                    }
                    else if (node.NodeType == XmlNodeType.Element)
                    {
                        var create = CreateFromXml(node);
                        u.Add(create);
                    }
                    else if (canUseComment)
                    {
                        var create = CreateFromXml(node);
                        u.Add(create);
                    }
                    else
                    {
                        if (node.NodeType == XmlNodeType.Comment) continue;
                        if (node.NodeType == XmlNodeType.Whitespace) continue;
                    }
                }
                if (stringAppendable != null)
                    foreach (Unifiable unifiable in u)
                    {
                        stringAppendable.Append(unifiable);
                    }
            }
            catch (Exception e)
            {
                AltBot.writeDebugLine("" + e.Message + ": " + " " + e.StackTrace + "\n" +
                                      DescribeUnifiable(stringAppendable));
                throw;
            }
        }

        public override bool IsTag(string that)
        {
            return str == "TAG-" + that || str.StartsWith("<" + that.ToLower() + " ");
        }

        public override void Append(string p)
        {
            throw new Exception("this " + AsString() + " cannot be appended with " + p);
        }

        public override void Append(Unifiable p)
        {
            if (p != null) Append(p.AsString());
        }

        public override Unifiable Frozen(SubQuery subquery)
        {
            return Create(str);
        }

        public override Unifiable ToPropper()
        {
            int len = str.Length;

            if (len == 0) return this;
            string newWord = ToUpper(str.Substring(0, 1));
            if (len == 1)
            {
                if (newWord == str) return this;
            }
            newWord += str.Substring(1).ToLower();
            return newWord;
        }

        public override Unifiable Rest
        {
            get
            {
                if (String.IsNullOrEmpty(str)) return Empty;
                splittedCache = ToArray();
                if (splittedCache.Length == 0)
                {
                    return Empty;
                }
                if (restCache == null)
                    return restCache = Join(" ", splittedCache, 1, splittedCache.Length - 1);
                return restCache;

                if (String.IsNullOrEmpty(str)) return Empty;
                int i = str.IndexOfAny(BRKCHARS);
                if (i == -1) return Empty;
                restCache = str.Substring(i + 1);
                return Create(Trim(restCache));
            }
        }

        public static bool DebugNulls;

        public override Unifiable First
        {
            get
            {
                if (String.IsNullOrEmpty(str)) return Empty;
                //int i = str.IndexOfAny(BRKCHARS);
                //if (i == -1) return Create(str);
                Unifiable[] s = ToArray();
                if (s == null) return null;
                if (s.Length < 1) return Empty;
                return s[0];
                //string rest = str.Substring(0, i - 1);
                //return Create(rest.Trim());
            }
        }

        public override bool IsHighPriority
        {
            get
            {
                if (str == "_") return true;
                if (str.EndsWith("_")) return true;
                //if (IsLazy) return true;
                return false;
            }
        }

        public override bool IsLazy
        {
            get
            {
                if (IsMarkerTag) return false;
                if (str == null) return false;
                if (str == "") return false;
                char fc = str[0];
                if (fc == '~')
                {
                    return true;
                }
                if (fc == '<')
                {
                    return true;
                    if (str.Contains("star") || str.Contains("match="))
                    {
                        return true;
                    }
                    if (str.Contains("var="))
                    {
                        return true;
                    }
                    if (str.Contains("name="))
                    {
                        return true;
                    }
                }
                return false;
            }
        }

        public override bool IsLitteral
        {
            get
            {
                if (IsLazy) return false;
                if (IsMarkerTag) return true;
                if (IsWildCard) return false;
                return true;
            }
        }


        public override bool IsAnyText
        {
            get
            {
                if (!IsWildCard) return false;
                if (IsLitteralText) return false;
                return true;
            }
        }

        public override bool IsLitteralText
        {
            get
            {
                if (IsLazy) return false;
                if (IsMarkerTag) return false;
                if (IsWildCard) return false;
                return true;
            }
        }

        public virtual bool IsMarkerTag
        {
            get { return IsFlag(UFlags.IS_TAG); }
        }

        public override bool StoreWildCard()
        {
            if (IsFlag(UFlags.NO_BINDS_STARS)) return false;
            return !str.StartsWith("~") && !str.StartsWith("<bot ");
        }

        public override bool ConsumePath(int at, string[] tokens, out string fw, out Unifiable after, out int newAt,
                                         SubQuery query)
        {
            int tokenLen = tokens.Length;
            if (tokenLen == 0)
            {
                bool WasEmpty = IsNullOrEmpty(this);
                fw = "";
                after = Empty;
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
            bool prev = NamedValuesFromSettings.UseLuceneForGet;
            try
            {
                NamedValuesFromSettings.UseLuceneForGet = false;
                if (UniifyPath0(at, tokens, tokenLen, ovs, newAt, minLen, query, out after))
                {
                    return true;
                }
            }
            finally
            {
                NamedValuesFromSettings.UseLuceneForGet = prev;
            }
            if (MustBeFast) return false;

            int len = tokens.Length;
            Unifiable[] myA = ToArray();
            int upTo = myA.Length;
            // if (upTo == 0) return false;
            int min = 1;
            Unifiable matchMe = this;
            if (!IsLazy)
            {
                upTo = matchMe.ToUpper().Split(new[] {' '}).Length;
                min = upTo;
            }
            else
            {
                matchMe = ToValue(query);
                upTo = matchMe.ToUpper().Split(new[] {' '}).Length;
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

        private bool UniifyPath0(int at, string[] tokens, int tokenLen, Unifiable ovs, int usedAt, int minLen,
                                 SubQuery query, out Unifiable after)
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

        protected override string GenerateSpecialName
        {
            get { return ToUpper(AsString()); }
        }

        public override float Unify(Unifiable other, SubQuery query)
        {
            if (ReferenceEquals(this, other)) return UNIFY_TRUE;
            if (ReferenceEquals(null, other)) return UNIFY_FALSE;

            string su = ToUpper();
            string ou = other.ToUpper();
            if (su == ou) return UNIFY_TRUE;
            bool otherIsLitteral = other.IsLitteral;
            if (IsLitteral)
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
            if (IsAnyText)
            {
                writeToLog("CALL CALL/WILL UNIFY");
                return !IsNullOrEmpty(other) ? UNIFY_TRUE : UNIFY_FALSE;
            }
            else if (CanMatchZero)
            {
                writeToLog("CALL CALL/WILL UNIFY");
                return other.CanMatchZero ? UNIFY_TRUE : UNIFY_FALSE;
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
                if (IsLazy)
                {
                    return UnifyTagHandler(other, query) ? UNIFY_TRUE : UNIFY_FALSE;
                }
                if (IsWildCard)
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
                UFlags flags = ov.Flags;
                ///bot.GetTagHandler(templateNode, subquery, request, result, request.user);
                if (IsCachedMatch(ov, query))
                {
                    writeToLog("UnifyLazy: CACHED" + ov + " in " + query);
                    return true;
                }
                if (valueCache is String)
                {
                    String sv = (String) valueCache;
                    if (IsStringMatch(sv, ov.ToValue(query)))
                    {
                        writeToLog("UnifyLazy: SUCCEED T2 " + ov + " in " + query);
                        return true;
                    }
                    return false;
                }
                if (valueCache is StringUnifiable)
                    if (ReferenceEquals(ov, valueCache))
                    {
                        writeToLog("UnifyLazy: SUCCEED T1 " + ov + " in " + query);
                        return true;
                    }
                AIMLTagHandlerU tagHandlerU = GetTagHandler(query);
                if (tagHandlerU.CallCanUnify(ov) == UNIFY_TRUE)
                {
                    writeToLog("UnifyLazy: SUCCEED" + ov + " in " + query);
                    return true;
                }
                else
                {
                    return false;
                }
                Unifiable outputSentence = tagHandlerU.CompleteAimlProcess();
                if (ov.CanUnify(outputSentence, query))
                {
                    return true;
                }
                if (outputSentence == null) return false;
                string value = outputSentence.AsString();
                if (ov.ToUpper() == value.ToUpper())
                {
                    valueCache = value; // ToValue(query);
                    writeToLog("UnifyLazy: SUCCEED" + ov + " in " + query);
                    return true;
                }
                else
                {
                    valueCache = value; // ToValue(query);
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
        public AIMLTagHandlerU GetTagHandler(SubQuery subquery)
        {
            if (valueCache is AIMLTagHandlerU)
            {
                AIMLTagHandlerU tagHandlerU = (AIMLTagHandlerU) valueCache;
                tagHandlerU.ResetValues(false);
                return tagHandlerU;
            }
            XmlNode getNode1 = GetNode();
            return subquery.GetTagHandler(getNode1);
        }

        public virtual XmlNode GetNode()
        {
            if (nodeOuter != null) return nodeOuter as XmlNode;
            if (valueCache is XmlNode) return (XmlNode)valueCache;
            try
            {
                if (str.Contains("><"))
                {
                    return GetNode2();
                }
                XmlNode nodeOuter0 = StaticAIMLUtils.getDocNode("<li>" + str + "</li>", false, false, null);
                if (nodeOuter0.ChildNodes.Count == 1)
                {
                    nodeOuter0 = nodeOuter0.FirstChild;
                    LineInfoElement.chopParent(nodeOuter0);
                    splittedCache = new Unifiable[] {this};
                    this.nodeOuter = nodeOuter0;
                    return nodeOuter0;
                }
                splittedCache = arrayOf(nodeOuter0.ChildNodes);
                XmlDocument OwnerDocument = nodeOuter0.OwnerDocument;
                //LineInfoElementImpl.unsetReadonly(nodeOuter);
                if (OwnerDocument != null)
                {
                    nodeOuter0 = OwnerDocument.CreateTextNode(str);
                }
                this.nodeOuter = nodeOuter0;
                return nodeOuter0;
            }
            catch (Exception e)
            {
                return StaticAIMLUtils.getTemplateNode(str);
            }
        }

        private XmlNode GetNode2()
        {
            if (nodeOuter != null) return nodeOuter as XmlNode;
            if (valueCache is XmlNode) return (XmlNode)valueCache;
            try
            {
                XmlNode node = StaticAIMLUtils.getTemplateNode(str);
                LineInfoElement.unsetReadonly(node);
                return node;
            }
            catch (Exception e)
            {
                writeToLog("" + e);
                throw;
            }
        }

        public override void Clear()
        {
            throw new IndexOutOfRangeException();
            _str = "";
        }

        public override sealed string ToValue(SubQuery query)
        {
            if (valueCache is string) return (String) valueCache;
            return ToValue0(query);
            //return "" + valueCache;
        }

        protected string ToValue0(SubQuery query)
        {
            if (IsLitteral) return str;
            if (str.Length < 2)
            {
                return str;
            }
            if (IsLazy)
            {
                //todo 
                if (query == null) return AsString();
                AIMLTagHandlerU tagHandlerU = GetTagHandler(query);
                ThreadStart undo = tagHandlerU.EnterUnify();
                try
                {
                    Unifiable outputSentence = tagHandlerU.CompleteAimlProcess();
                    if (!IsNullOrEmpty(outputSentence))
                    {
                        valueCache = outputSentence.AsString();
                        return (string) valueCache;
                    }
                    writeToLog("Failed Eval " + str);
                }
                finally
                {
                    if (undo != null) undo();
                }
                ///bot.GetTagHandler(templateNode, subquery, request, result, request.user);
            }
            if (IsWildCard)
            {
                return str;
            }
            return AsString();
        }

        public override void OfferNode(XmlNode xmlNode, string inner)
        {
            if (nodeOuter == null || splittedCache == null)
            {
                str = inner ?? str;
                nodeOuter = nodeOuter ?? xmlNode;
                if (false)
                {
                    List<Unifiable> u = new List<Unifiable>(); 
                    StringAppendableUnifiableImpl stringAppendable = null; // CreateAppendable();}
                    CreateUnifableForList(xmlNode.ChildNodes, stringAppendable, u);
                    splittedCache = splittedCache ?? u.ToArray();
                }
                //valueCache = stringAppendable.AsString();
            } else
            {
                
            }
        }
    }
}