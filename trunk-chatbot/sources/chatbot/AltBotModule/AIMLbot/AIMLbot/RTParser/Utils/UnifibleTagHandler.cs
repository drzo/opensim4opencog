using System;
using System.Collections.Generic;
using System.Data;
using System.Text.RegularExpressions;
using System.Xml;
using AltAIMLbot.Utils;
using AltAIMLbot;

namespace AltAIMLbot.Utils
{
    public abstract class UnifibleTagHandler : AIMLTagHandlerU, IUnifibleTagHandler
    {
        internal const float AND_FALSE = 1;
        internal const float AND_TRUE = 0;
        internal const float ISA_FALSE = 1;
        internal const float ISA_TRUE = 0;
        internal const float OPT_FALSE = 1;
        internal const float OPT_TRUE = 0;
        internal const float OR_FALSE = 1;
        internal const float OR_TRUE = 0;
        internal const float STAR_FALSE = 1;
        internal const float STAR_TRUE = 0;
        protected static readonly List<Unifiable> SUCCEED_NOVARS = new List<Unifiable>();

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be processed</param>
        public UnifibleTagHandler(AltBot bot,
                                  User user,
                                  SubQuery query,
                                  Request request,
                                  Result result,
                                  XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        public static List<Unifiable> UnifyStars(string target, string source)
        {
            var matchVars = new List<object>();
            string me = RegExFrom(target, matchVars);
            MatchCollection mc = Regex.Matches(source, me, RegexOptions.IgnoreCase);
            if (mc.Count == 0)
            {
                if (matchVars.Count == 0) return SUCCEED_NOVARS;
                AltBot.writeDebugLine("DEBUG9: UnifyStars '" + me + "'!='" + source + "'");
                return null;
            }
            if (mc.Count != matchVars.Count)
            {
                if (mc.Count == 1)
                {
                    Match mc1 = mc[0];
                    if (mc1.Success)
                    {
                        int mcLength = mc1.Length;
                        if (mcLength > 0)
                        {
                            if (mcLength == source.Length)
                            {
                                return new List<Unifiable>() {source};
                            }
                        }
                    }
                    AltBot.writeDebugLine("ERROR: UnifyStars '" + me + "'!='" + source + "'");
                    return null;
                }
            }
            int mi = 0;
            var results = new List<Unifiable>(mc.Count);
            foreach (Match match in mc)
            {
                string mv = match.Value;
                object mp = matchVars[mi];
                if (UnifyValues(mp, mv))
                {
                    results.Add(mv);
                }
                else
                {
                    return null;
                }
            }
            return results;
        }

        private static bool UnifyValues(object mp, string mv)
        {
            string mpts = mp.ToString();
            if (mpts == "*") return true;
            if (mpts == "_") return true;
            return true;
        }

        private static string RegExFrom(XmlNodeList nodeList, List<object> matchVars)
        {
            string left = "";
            foreach (object e in nodeList)
            {
                XmlNode node = (XmlNode) e;
                if (node.NodeType == XmlNodeType.Text)
                {
                    left += " " + RegExFrom(TextNodeValue(node), matchVars);
                }
                else
                {
                    left += " (.*)";
                    matchVars.Add(node);
                }
            }
            return left;
        }

        private static string RegExFrom(string target, List<object> matchVars)
        {
            int fi = target.IndexOfAny(new[] {'*', '_', '<'});
            if (fi == -1) return target;
            string left = target.Substring(0, fi);
            char fc = target[fi];
            switch (fc)
            {
                case '*':
                    matchVars.Add("*");
                    return left + @"\b([A-Za-z 0-9]+)\b" + RegExFrom(target.Substring(fi + 1), matchVars);
                case '_':

                    matchVars.Add("_");
                    return left + @"\b([A-Za-z0-9]+)\b" + RegExFrom(target.Substring(fi + 1), matchVars);
                case '<':
                    string targetSubstring = target.Substring(fi - 1);
                    XmlNode node = getNode("<node>" + targetSubstring + "</node>");
                    return left + RegExFrom(node.ChildNodes, matchVars);
                default:
                    break;
            }
            return target;
        }

        public abstract override float CanUnify(Unifiable with);

        public virtual float DefaultCanUnify(Unifiable with)
        {
            return ComputeInnerOrNull().Unify(with, this.query);
        }

        protected void SetWith(XmlNode childNode, Unifiable with)
        {
            MEMBER = new Unifiable[] { with };
        }

        protected bool TextWith(XmlNode templateNode, Unifiable with, out bool tf, out string toXMLValueNotOuter)
        {
            toXMLValueNotOuter = ToXMLValueNotOuter(templateNode);
            if (toXMLValueNotOuter != null)
            {
                string srch = (" " + with.ToValue(query) + " ").ToUpper();
                tf = (" " + toXMLValueNotOuter + " ").ToUpper().Contains(srch);
                SetWith(templateNode, with);
                return true;
            }
            tf = false;
            return false;
        }

        protected Unifiable[] MEMBER = null;

        protected override Unifiable ProcessChangeU()
        {
            if (MEMBER != null && MEMBER.Length > 0) return MEMBER[0];
            var v1 = ComputeInner();
            var v2 = templateNodeInnerText;
            if ((string)v1==(string)v2) return v2;
            writeToLogWarn("Not sure if i should return '{0}' isntead of '{1}'", v1, v2);
            return v2;
        }

        protected Unifiable AsOneOf()
        {
            writeToLogWarn("AsOneOf Many choices");
            throw new NotImplementedException();
        }
        protected abstract Unifiable ComputeInnerOrNull();

        public string ComputeInner()
        {
            string re = "";
            if (templateNode.NodeType == XmlNodeType.Text)
            {
                return ValueText(templateNodeInnerText);
            }
            if (templateNode.HasChildNodesNonText())
            {
                // recursively check
                foreach (XmlNode childNode in templateNode.ChildNodes)
                {
                    re += ToXmlValue(childNode);
                }
            }
            else
            {
                re = ComputeInnerOrNull();
                string res = (string)re;
                if (string.IsNullOrEmpty(res))
                {
                    re = Recurse();
                }
                if (!InUnify)
                {
                    templateNodeInnerText = re;
                }
            }
            return re;
        }
        
        override protected Unifiable Recurse()
        {
            var vorNull = ComputeInnerOrNull();
            if (!Unifiable.IsNull(vorNull))
            {
                return vorNull;
            }
            writeToLogWarn("Why are we in Recurse?");
            return base.Recurse();
        }

        virtual protected float ChildUnify(Unifiable with, XmlNode childNode)
        {            
            bool wasTrue;
            float partCallCanUnify;
            string useLess;
            if (TextWith(childNode, with, out wasTrue, out useLess))
            {
                partCallCanUnify = wasTrue ? AND_TRUE : AND_FALSE;
            }
            else
            {
                if (childNode == templateNode)
                {
                    throw new Exception("This is inside iteself!");
                }
                AIMLTagHandlerU part = GetChildTagHandler(childNode);
                partCallCanUnify = part.CallCanUnify(with);
            }
            return partCallCanUnify;
        }

    }

    public interface IUnifibleTagHandler
    {
        float CanUnify(Unifiable with);
    }
}