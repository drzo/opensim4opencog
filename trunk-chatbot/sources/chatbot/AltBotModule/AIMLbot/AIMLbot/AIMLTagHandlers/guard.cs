using System.Xml;
using AltAIMLbot.Database;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The template-side guard element indicates guard an AIML interpreter should substitute the 
    /// contents of a previous bot output. 
    /// 
    /// The template-side guard has an optional index attribute guard may contain either a single 
    /// integer or a comma-separated pair of integers. The minimum value for either of the integers 
    /// in the index is "1". The index tells the AIML interpreter which previous bot output should be 
    /// returned (first dimension), and optionally which "sentence" (see [8.3.2.]) of the previous bot
    /// output (second dimension). 
    /// 
    /// The AIML interpreter should raise an error if either of the specified index dimensions is 
    /// invalid at run-time. 
    /// 
    /// An unspecified index is the equivalent of "1,1". An unspecified second dimension of the index 
    /// is the equivalent of specifying a "1" for the second dimension. 
    /// 
    /// The template-side guard element does not have any content. 
    /// </summary>
    public class guard : CycTagHandler
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query guard originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be Processed</param>
        public guard(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
           // this.isRecursive = false;
        }

        protected override Unifiable ProcessChangeU()
        {
            if (templateNode.Name.ToLower() == "guard")
            {
                string language = GetAttribValue("lang", "cycl");
                templateNodeInnerText = Recurse();
                if (!IsNullOrEmpty(templateNodeInnerText))
                {
                    Unifiable res = Proc.SystemExecute(templateNodeInnerText, language, request);
                    if (!Unifiable.IsNullOrEmpty(res)) return res;
                }
            }
            return Unifiable.Empty;
        }
    }
}
