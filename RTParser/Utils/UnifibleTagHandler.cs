using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Threading;
using System.Xml;

namespace RTParser.Utils
{
    public abstract class UnifibleTagHandler : RTParser.Utils.AIMLTagHandler
    {
        static public List<Unifiable> UnifyStars(string target, string source)
        {
            var matchVars = new List<object>();
            string me = RegExFrom(target, matchVars);
            MatchCollection mc = Regex.Matches(source, me, RegexOptions.IgnoreCase);
            if (mc.Count == 0)
            {
                if (matchVars.Count == 0) return SUCCEED_NOVARS;
                RTPBot.writeDebugLine("DEBUG9: UnifyStars '" + me + "'!='" + source + "'");
                return null;
            }
            if (mc.Count != matchVars.Count)
            {
                RTPBot.writeDebugLine("ERROR: UnifyStars '" + me + "'!='" + source + "'");
                return null;
            }
            int mi = 0;
            List<Unifiable> results = new List<Unifiable>(mc.Count);
            foreach (Match match in mc)
            {
                var mv = match.Value;
                var mp = matchVars[mi];
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
            var mpts = mp.ToString();
            if (mpts == "*") return true;
            if (mpts == "_") return true;
            return true;
            
        }

        readonly protected static List<Unifiable> SUCCEED_NOVARS = new List<Unifiable>();

        private static string RegExFrom(XmlNodeList nodeList, List<object> matchVars)
        {
            string left = "";
            foreach (var e in nodeList)
            {

                XmlNode node = (XmlNode) e;
                if (node.NodeType==XmlNodeType.Text)
                {
                    left += " " + RegExFrom(node.InnerText, matchVars);
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
            int fi = target.IndexOfAny( new char[] {'*','_','<'});
            if (fi == -1) return target;
            string left = target.Substring(0, fi);
            char fc = target[fi];
            switch (fc)
            {
                case '*':
                    matchVars.Add("*");
                    return left + "(.*)" + RegExFrom(target.Substring(fi), matchVars);
                case '_':
                    
                    matchVars.Add("_");
                    return left + "(.*)" + RegExFrom(target.Substring(fi), matchVars);
                case '<':
                    string targetSubstring = target.Substring(fi - 1);
                    XmlNode node = AIMLTagHandler.getNode("<node>" + targetSubstring + "</node>");
                    return left + RegExFrom(node.ChildNodes, matchVars);
                default:
                    break;
            }
            return target;
        }

        internal const float OR_TRUE = 0;
        internal const float OR_FALSE = 1;
        internal const float STAR_TRUE = 0;
        internal const float STAR_FALSE = 1;
        internal const float OPT_TRUE = 0;
        internal const float OPT_FALSE = 1;
        internal const float ISA_TRUE = 0;
        internal const float ISA_FALSE = 1;
        internal const float AND_TRUE = 0;
        internal const float AND_FALSE = 1;


        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be processed</param>
        public UnifibleTagHandler(RTParser.RTPBot bot,
                                      RTParser.User user,
                                      RTParser.Utils.SubQuery query,
                                      RTParser.Request request,
                                      RTParser.Result result,
                                      XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        public override float CanUnify(Unifiable with)
        {
            return base.CanUnify(with);
        }

    }
}