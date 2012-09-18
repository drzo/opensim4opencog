using System;
using System.Text;
using System.Xml;
using AltAIMLParser;
using RTParser;

namespace AltAIMLbot.AIMLTagHandlers
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
    public class star : Utils.AIMLTagHandler
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
        public star(AltBot bot,
                        User user,
                        Utils.SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            isRecursive = false;
        }

        protected override string ProcessChange()
        {
            if (this.TemplateNodeName == "star")
            {
                return GetStar("pattern");
            }
            return string.Empty;
        }

        protected string GetStar(string starname)
        {
            int index = GetAttribValue("index", () => 1);
            try
            {
                var starValues = query.GetStars(starname);
                int valuesCount = starValues.Count;
                if (valuesCount >= index)
                {
                    if (index > 0)
                    {
                        return starValues[valuesCount - index];
                    }
                    else
                    {
                        writeToLogError("An " + starname + " tag with a badly formed index (" + index +
                                        ") was encountered processing the input: " + this.request.rawInput);
                    }
                }
                else
                {
                    writeToLogError("An out of bounds index (" + index + ") to " + starname + " count=" + valuesCount +
                                    " was encountered when processing the input: " + this.request.rawInput);
                }
            }
            catch (Exception e)
            {
                writeToLogError("A thatstar tag with a badly formed index (" + index +
                                ") was encountered processing the input: " + this.request.rawInput + " caused " + e);
            }
            return null;
        }
    }
}
