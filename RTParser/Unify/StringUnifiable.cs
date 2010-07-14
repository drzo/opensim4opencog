using System;
using System.Collections.Generic;
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

        public string _str;
        protected string str
        {
            get
            {
                return _str;
            }
             
            set
            {
                _str = value;
            }
        }

        public StringUnifiable()
        {
            str = "";
        }

        public StringUnifiable(string value)
        {
            str = value;
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
            string str2 = str.Trim().Replace("  "," ").Replace("  "," ");
            if (str2==str) return this;
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
            return str;
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
            if (String.IsNullOrEmpty(str)) return true;
            string found = str.Trim().ToUpper();
            return found == "" || found == "NIL" || found == "()" || found == "FALSE" || found == "NO" || found == "OFF";
        }

        public override bool IsWildCard()
        {
            if (IsMarkerTag()) return false;
            if (str == "*" || str == "_")
            {
                return true;
            }
            if (str.StartsWith("<"))
            {
                return IsLazyStar();
            }
            return false;
        }

        public override Unifiable[] ToArray()
        {
            if (splitted != null)
            {
                return splitted;
            }
            if (splitted == null) splitted = Splitter(str);
            return splitted;
        }

        public static Unifiable[] Splitter(string str)
        {
            string strTrim = str.Trim().Replace("  "," ").Replace("  "," ");
            if (!strTrim.Contains("<"))
                return arrayOf(strTrim.Split(BRKCHARS, StringSplitOptions.RemoveEmptyEntries));
            XmlDocument doc = new XmlDocument();
            List<Unifiable> u = new List<Unifiable>();

            try
            {
                doc.LoadXml("<node>" + strTrim + "</node>");
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
                RTPBot.writeDebugLine("" + e.Message + ": " +strTrim);
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

            if (!IsAppendable)
            {
                throw new Exception("this " + AsString() + " cannot be appended with " + p);
            }
            if (p==null) return;
            if (str == "")
                str = p.AsString().Trim();
            else
            {
                str += " ";
                str += p.AsString().Trim();
            }
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

        protected Unifiable[] splitted = null;
        protected Unifiable rest = null;
        public override Unifiable Rest()
        {

            splitted = ToArray();
            if (rest == null) rest = Join(" ", splitted, 1, splitted.Length - 1);
            return rest;

            if (String.IsNullOrEmpty(this.str)) return Unifiable.Empty;
            int i = str.IndexOfAny(BRKCHARS);
            if (i == -1) return Empty;
            rest = str.Substring(i + 1);
            return Create(rest.Trim());
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

        public override MatchWidth Width
        {
            get
            {
                if (IsLongWildCard())
                {
                    return MatchWidth.MORE_THAN_ONE;
                }
                if (IsFiniteWildCard()) return MatchWidth.ONE_OR_TWO;
                return MatchWidth.ONLY_ONE;
            }
        }

        public override bool IsLongWildCard()
        {
            if (str == ("*")) return true;
            if (str == ("^")) return true;
            if (this.IsMarkerTag()) return false;
            if (IsLazyStar()) return true;
            return false;
        }

        public override bool IsLazy()
        {
            if (this.IsMarkerTag()) return false;
            if (str == "") return false;
            if (str[0]=='~')
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
            //string test = str.Trim().ToUpper();
            return str.StartsWith("TAG-");
        }

        override public bool StoreWildCard()
        {
            return !str.StartsWith("~");
        }

        override public bool ConsumeFirst(Unifiable fullpath, out Unifiable left, out Unifiable right, SubQuery query)
        {
            Unifiable[] array = fullpath.ToArray();
            left = Unifiable.Empty;
            right = fullpath;
            int len = array.Length;
            if (len == 0) return false;
            if (str == "_")
            {
                if (len > 1)
                {
                  
                }
                return false;
            }
            Unifiable[] myA = ToArray();
            int upTo = myA.Length;
            int min = 1;
            Unifiable matchMe = this;
            if (!IsLazy())
            {
                upTo = matchMe.ToUpper().Split(new char[] { ' ' }).Length;
                min = upTo;
                return false;
            }
            else
            {
                matchMe = ToValue(query);
                upTo = matchMe.ToUpper().Split(new char[] { ' ' }).Length;
                min = upTo;
            }
            if (upTo > len)
            {
                upTo = len;
            }
            //if (upTo > 1) writeToLog("ConsumeFirst Try: " + fullpath);

            for (int j = min; j <= upTo; j++)
            {
                left = Join(" ", array, 0, j);
                if (matchMe.WillUnify(left, query))
                {
                    if (j > 1) writeToLog("ConsumeFirst Success!: " + fullpath);
                    rest = Join(" ", array, j, len - j);
                    return true;
                }
            }
            return false;
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
                    try
                    {
                        ///bot.GetTagHandler(templateNode, subquery, request, result, request.user);
                        AIMLTagHandler tagHandler = GetTagHandler(query);
                        if (tagHandler.CanUnify(other) == UNIFY_TRUE)
                        {
                            writeToLog("UnifyLazy: SUCCEED" + other + " in " + query);
                            return UNIFY_TRUE;
                        }
                        Unifiable outputSentence = tagHandler.CompleteProcess();
                        string value = outputSentence.AsString();
                        if (ov.ToUpper() == value.ToUpper())
                        {
                            writeToLog("UnifyLazy: SUCCEED" + other + " in " + query);
                            return UNIFY_TRUE;
                        }
                        return UNIFY_FALSE;
                    }
                    catch (Exception e)
                    {
                        writeToLog("UnifyLazy ERROR! " + e);

                    }
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

        private SubQuery savedSQ;
        AIMLTagHandler savedTagHandler;
        public XmlNode node;
        public AIMLTagHandler GetTagHandler(SubQuery subquery)
        {
            if (savedTagHandler != null)
            {
                if (savedSQ == subquery)
                {
                    return savedTagHandler;
                }
            }
            if (node == null) node = GetNode();
            RTPBot bot = null;
            User user = null;
            Request request = null;
            Result result = null;
            // if (node.ChildNodes.Count == 0) ;            
            if (subquery != null)
            {
                result = subquery.Result;
                request = subquery.Request ?? result.request;
                result = result ?? request.result;
                user = result.user;
                bot = request.Proccessor;
                savedTagHandler = bot.GetTagHandler(user, subquery, request, result, node, null);
                savedSQ = subquery;
            }
            return savedTagHandler;
        }

        public virtual XmlNode GetNode()
        {
            try
            {
                return AIMLTagHandler.getNode(str);
            } catch(Exception e)
            {
                return AIMLTagHandler.getNode("<template>" + str + "</template>");
            }
        }

        public override bool IsEmpty
        {
            get
            {
                string s = str;
                if (string.IsNullOrEmpty(s)) return true;
                s = s.Trim();
                if (s.Length != 0) return false;
                writeToLog("IsEmpty: " + str);
                return true;                 
            }
        }

        private bool ppendable;

        public bool IsAppendable
        {
            get { return ppendable; }
            set { ppendable=value; }
        }

        public override void Clear()
        {
            throw new IndexOutOfRangeException();
            str = "";
        }

        public override string ToValue(SubQuery query)
        {
            if (IsLitteral()) return str;
            if (str.Length < 2) return str;
            if (IsLazy())
            {
                //todo 
                if (query == null) return AsString();
                AIMLTagHandler tagHandler = GetTagHandler(query);
                Unifiable outputSentence = tagHandler.CompleteProcess();
                if (!outputSentence.IsEmpty) return outputSentence.AsString();
                writeToLog("Failed Eval " + str);
                ///bot.GetTagHandler(templateNode, subquery, request, result, request.user);
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