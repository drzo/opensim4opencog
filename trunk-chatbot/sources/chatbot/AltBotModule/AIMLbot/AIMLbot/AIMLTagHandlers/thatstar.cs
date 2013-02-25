using System.Xml;
using AltAIMLbot.Normalize;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
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
    public class thatstar : StarTagHandler
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be Processed</param>
        public thatstar(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode, 1)
        {
            StarDict = () => CurrentQuery.ThatStars;
        }
        protected override Unifiable Format(Unifiable unifiable)
        {
            if(templateNode.ParentNode.Name=="srai")
            {
                var vv = ApplySubstitutions.Substitute(substs.Person2Substitutions, unifiable);
                return vv;
            }
            return unifiable;
        }
    }
}
