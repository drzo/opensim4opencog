using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using RTParser.Utils;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The star element indicates that an AIML interpreter should substitute the value "captured" 
    /// by a particular wildcard from the pattern-specified portion of the match path when returning 
    /// the template. 
    /// 
    /// The star element has an optional integer index attribute that indicates which wildcard to use. 
    /// The minimum acceptable value for the index is "1" (the first wildcard), and the maximum 
    /// acceptable value is equal to the number of wildcards in the pattern. 
    /// 
    /// An AIML interpreter should raise an error if the index attribute of a star specifies a wildcard 
    /// that does not exist in the category element's pattern. Not specifying the index is the same as 
    /// specifying an index of "1". 
    /// 
    /// The star element does not have any content. 
    /// </summary>
    public class star : StarTagHandler
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be processed</param>
        public star(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode, 1)
        {
            StarDict = () => TheQuery.InputStar;
        }
    }

    public class StarTagHandler : RTParser.Utils.UnifibleTagHandler
    {
        protected override bool ExpandingSearchWillYieldNoExtras { get { return true; } }
        protected virtual IList<Unifiable> GetStarDict()
        {
            return StarDict();
        }

        protected int DefaultIndex { get; set; }

        protected Func<IList<Unifiable>> StarDict;
        internal const float STAR_TRUE = 0;
        internal const float STAR_FALSE = 1;

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be processed</param>
        public StarTagHandler(RTParser.RTPBot bot,
                                      RTParser.User user,
                                      RTParser.Utils.SubQuery query,
                                      RTParser.Request request,
                                      RTParser.Result result,
                                      XmlNode templateNode, int idx)
            : base(bot, user, query, request, result, templateNode)
        {
            DefaultIndex = idx;
        }

        public override float CanUnify(Unifiable with)
        {
            if (templateNode.NodeType == XmlNodeType.Text)
            {
                string srch = (" " + with.ToValue(query) + " ").ToUpper();
                return ((" " + templateNode.InnerText + " ").ToUpper().Equals(srch)) ? STAR_TRUE : STAR_FALSE;
            }
            if (templateNode.HasChildNodes)
            {
                {
                    Unifiable rest = with;
                    // recursively check
                    foreach (XmlNode childNode in templateNode.ChildNodes)
                    {

                        with = rest.First();
                        rest = rest.Rest();
                        try
                        {
                            if (childNode.NodeType == XmlNodeType.Text)
                            {
                                string srch = (" " + with.ToValue(query) + " ").ToUpper();
                                return ((" " + childNode.InnerText + " ").ToUpper().Equals(srch))
                                           ? STAR_TRUE
                                           : STAR_FALSE;
                            }
                            AIMLTagHandler part = GetChildTagHandler(childNode);
                            if (part.CallCanUnify(with) > 0) return STAR_FALSE;
                        }
                        catch (Exception e)
                        {
                            RTPBot.writeDebugLine("" + e);
                        }
                    }
                    return IsNullOrEmpty(rest) ? STAR_TRUE : STAR_FALSE;
                }
            }
            return IsNullOrEmpty(with) ? STAR_TRUE : STAR_FALSE;
        }

        /// <summary>
        /// The method that does the actual processing of the text.
        /// </summary>
        /// <returns>The resulting processed text</returns>
        protected override Unifiable ProcessChange()
        {
            IList<Unifiable> stars = GetStarDict();

            int starsCount = stars.Count;
            string value = GetAttribValue("index", "" + DefaultIndex);

            string starName = this.templateNode.Name.ToLower();
            if (CheckNode("star,thatstar,inputstar,topicstar"))
            {
                if (starsCount > 0)
                {
                    int index = Convert.ToInt32(value);
                    try
                    {
                        if (index <= starsCount && index > 0)
                        {
                            return stars[index - 1];
                        }
                        else
                        {
                            writeToLogWarn("{0} out of bounds 0<{1}<{2} reference caused by input: '{3}'", starName,
                                           value, starsCount, request);
                        }
                    }
                    catch
                    {
                        writeToLogWarn("{0} non-integer '{1}' of {2} reference caused by request: '{3}'", starName, value, starsCount, request);
                    }
                }
                else
                {
                    writeToLogWarn("A star tag tried to reference an empty '{0}' collection '{1}' of {2} reference caused by input: '{3}'", starName, value, starsCount, request);
                }
            }
            Unifiable ret = GetAttribValue("default", templateNodeInnerText);
            return ret;
        }

    }
}
