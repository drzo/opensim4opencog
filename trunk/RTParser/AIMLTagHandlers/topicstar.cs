using System;
using System.Xml;
using System.Text;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The topicstar element tells the AIML interpreter that it should substitute the contents of 
    /// a wildcard from the current topic (if the topic contains any wildcards).
    /// 
    /// The topicstar element has an optional integer index attribute that indicates which wildcard 
    /// to use; the minimum acceptable value for the index is "1" (the first wildcard). Not 
    /// specifying the index is the same as specifying an index of "1". 
    /// 
    /// The topicstar element does not have any content. 
    /// </summary>
    public class topicstar : RTParser.Utils.AIMLTagHandler
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
        public topicstar(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        protected override Unifiable ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "topicstar")
            {
                try
                {
                    int result = Convert.ToInt32(GetAttribValue("index", "1"));
                    if (result >= 0)
                    {
                        if (result < query.TopicStar.Count)
                        {
                            return (Unifiable)this.query.TopicStar[result - 1];
                        }
                    }
                    else
                    {
                        this.Proc.writeToLog("ERROR! An out of bounds index " + result + " to TopicStar was encountered when processing the input: " + this.request.rawInput);
                    }
                    return GetAttribValue("default", Unifiable.Empty);
                }
                catch
                {
                    this.Proc.writeToLog("ERROR! A TopicStar tag with a bady formed index (" + this.templateNode.OuterXml + ") was encountered processing the input: " + this.request.rawInput);
                }
            }
            return Unifiable.Empty;
        }
    }
}
