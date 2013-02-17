using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.Xml.XPath;
using AltAIMLbot;
using AltAIMLbot.Utils;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using AltAIMLbot.AIMLTagHandlers;

namespace AltAIMLbot.Utils
{
    [Serializable]
    public class ConversationCondition
    {
        public bool IsConditionTrue(SubQuery subQuery)
        {
            string tagName = this.TagName;
            Func<int, int,User, Unifiable> getThat = subQuery.GetMatcher(tagName);
            int[] idx2 = GetIndex2(IndexVal);
            User responder = null;
            var val = getThat(idx2[0], idx2[1], responder);
            try
            {
                var res = UnifibleTagHandler.UnifyStars((string)Pattern, (string)val);
                return res != null;                
            }
            catch (Exception e)
            {
                writeToLog("ERROR: " + e);
                return false;
                throw;
            }
        }

        private static int[] GetIndex2(string at1)
        {
            if (string.IsNullOrEmpty(at1) || at1 == "1,1") return THAT00;
            if (at1 == "1" || at1 == "1,*") return THAT0S;
            if (at1 == "*" || at1 == "*,*") return THATSS;
            if (at1 == "*,1") return THATS0;
            int at1Contains = at1.IndexOf(",");
            if (at1Contains!=-1)
            {
                string[] ds = at1.Split(",".ToCharArray(), StringSplitOptions.None);                
                return new int[] { Parse(ds[0]), Parse(ds[1]) };
            }
            return new int[] {Parse(at1), -1};

        }

        private static int Parse(string s)
        {
            if (s == "" || s == "*" || s == "0")
            {
                return -1;
            }
            return int.Parse(s) - 1;
        }

        private static int[] THAT00 = new int[2] { 0, 0 };
        private static int[] THAT0S = new int[2] { 0, -1 };
        private static int[] THATS0 = new int[2] { -1, 0 };
        private static int[] THATSS = new int[2] { -1, -1 };

        public HashSet<object> IndexedOn = null;

        public bool IsPrecond;
        public string TagName;
        public string Pattern;
        public string IndexVal;
        public bool IndexPosition;
        [NonSerialized]
        public object SourceNode;
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            if (IsPrecond) sb.Append("precond");
            else sb.Append("condition");
            if (TagName != null) sb.Append("=\"" + TagName.ToUpper() + "\"");
            if (IndexVal != null) sb.Append(" pos=" + IndexVal );
            if (IndexPosition) sb.Append(" idx=" + IndexPosition);
            if (Pattern != null) sb.Append(" match=\"" + Pattern + "\" ");
            var sSourceNode = SourceNode as XmlNode;
            if (sSourceNode != null)
            {
                sb.AppendLine(StaticXMLUtils.LocationInfo(sSourceNode) + sSourceNode.OuterXml);
            }
            return sb.ToString();

        }

        public ConversationCondition(bool isPrecond, string tagName, string pattern, string indexVal, bool position1, XmlNode proof)
        {
            IsPrecond = isPrecond;
            TagName = tagName;
            Pattern = pattern;
            IndexVal = indexVal;
            IndexPosition = position1;
            SourceNode = proof;
        }

        public ConversationCondition(XmlNode node)
        {
            SetValue(node);
        }

        public void SetValue(XmlNode node)
        {
            SourceNode = node;
            bool isRequired;
            bool indexPosition;
            string indexVal;
            TagName = node.LocalName;
            Pattern = AIMLLoaderU.TryGetPrecondionThat(TagName, node,
                                                      out isRequired, out indexVal, out indexPosition);
            IsPrecond = isRequired;
            IndexPosition = indexPosition;
            IndexVal = indexVal;
        }

        /// <summary>
        /// Name/Value requirment
        /// </summary>
        /// <param name="name"></param>
        /// <param name="value"></param>
        /// <param name="proof"></param>
        public ConversationCondition(string name, Unifiable value, XmlNode proof)
            : this(true, name, value, "1", true, proof)
        {
        }
        private void writeToLog(string s, params object[] args)
        {
            AltBot.writeDebugLine("" + this + " " + s, args);
        }



        internal long RunLowMemHooks()
        {
            if (SourceNode != null)
            {
                SourceNode = null;
                return 1;
            }
            return 0;
        }
    }
}