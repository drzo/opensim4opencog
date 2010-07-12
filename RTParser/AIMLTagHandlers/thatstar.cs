using System;
using System.Xml;
using System.Text;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The thatstar element tells the AIML interpreter that it should substitute the contents of a 
    /// wildcard from a pattern-side that element. 
    /// 
    /// The thatstar element has an optional integer index attribute that indicates which wildcard 
    /// to use; the minimum acceptable value for the index is "1" (the first wildcard). 
    /// 
    /// An AIML interpreter should raise an error if the index attribute of a star specifies a 
    /// wildcard that does not exist in the that element's pattern content. Not specifying the index 
    /// is the same as specifying an index of "1". 
    /// 
    /// The thatstar element does not have any content. 
    /// </summary>
    public class thatstar : RTParser.Utils.AIMLTagHandler
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
        public thatstar(RTParser.RTPBot bot,
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
            if (this.templateNode.Name.ToLower() == "thatstar")
            {
                try
                {
                    int result = Convert.ToInt32(GetAttribValue("index", "1")) - 1;
                    if (result <= query.ThatStar.Count)
                    {
                        if (result >= 0)
                        {
                            return (Unifiable) this.query.ThatStar[result];
                        }
                    }
                    else
                    {
                        writeToLog("ERROR! An out of bounds index " + result + " to thatstar was encountered when processing the input: " + this.request.rawInput);
                    }
                    return GetAttribValue("default", Unifiable.Empty);
                }
                catch (Exception exception)
                {
                    writeToLog("ERROR! A thatstar tag with a bady formed index (" + this.templateNode.OuterXml +
                               ") was encountered processing the input: " + this.request.rawInput + " " + exception);
                }
            }
            return Unifiable.Empty;
        }
    }
}
