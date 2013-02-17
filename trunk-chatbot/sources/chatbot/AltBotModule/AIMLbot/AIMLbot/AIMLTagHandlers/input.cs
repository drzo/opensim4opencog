using System.Xml;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The input element tells the AIML interpreter that it should substitute the contents of a 
    /// previous user input. 
    /// 
    /// The template-side input has an optional index attribute that may contain either a single 
    /// integer or a comma-separated pair of integers. The minimum value for either of the integers 
    /// in the index is "1". The index tells the AIML interpreter which previous user input should 
    /// be returned (first dimension), and optionally which "sentence" (see [8.3.2.]) of the previous 
    /// user input. 
    /// 
    /// The AIML interpreter should raise an error if either of the specified index dimensions is 
    /// invalid at run-time. 
    /// 
    /// An unspecified index is the equivalent of "1,1". An unspecified second dimension of the index 
    /// is the equivalent of specifying a "1" for the second dimension. 
    /// 
    /// The input element does not have any content. 
    /// </summary>
    public class input : AIMLConstraintTagHandler
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
        public input(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : this(bot, user, query, request, result, templateNode, 1)
        {
        }

        public input(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                XmlNode templateNode, int offset)
            : base(bot, user, query, request, result, templateNode, offset)
        {
            offetFrom = offset;
        }

        protected override Unifiable ProcessChangeU()
        {
            if (CheckNode("input,justthat,beforethat,request"))
            {
                //else if (this.templateNode.Attributes.Count == 1)
                {
                    var at1 = GetAttribValue("index", null);//.Trim();
                    //if (at1 != null)
                    {
                        //if (at1.Length > 0)
                        {
                            return CheckValue(GetIndexes(at1, request.Responder, user.getInputSentence,
                                              (str, args) => localError(at1, str)));
                        }
                    }
                }
            }
            return Unifiable.Empty;
        }
    }
}
