using System.Xml;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The think element instructs the AIML interpreter to perform all usual Processing of its 
    /// contents, but to not return any value, regardless of whether the contents produce output.
    /// 
    /// The think element has no attributes. It may contain any AIML template elements.
    /// </summary>
    //public class think : AIMLFormatingTagHandler
    public class think : Utils.AIMLTagHandler
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
        public think(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            IsStarAtomically = false;
        }
        /*
        /// <summary>
        /// The method that does the actual Processing of the text.
        /// </summary>
        /// <returns>The resulting Processed text</returns>
        protected override Unifiable Format(Unifiable templateNodeInnerText)
        {
            CheckNode("think");
            writeToLog("THOUGHT: '" + Unifiable.DescribeUnifiable(templateNodeInnerText) + "'");
            if (IsNull(templateNodeInnerText))
            {
                return FAIL;
            }
            var vv = GetAttribValue<Unifiable>(templateNode, "retval", null);
            if (vv != null) return vv;
            //if (true) return THINKYTAG;
            return Succeed(" THOUGHT: '" + templateNodeInnerText + "'");
        }
*/
        static public Unifiable THINKYTAG
        {
            get
            {
                return " , ";
                return "TAG-THINK";
            }
        }
/*
        protected override Unifiable templateNodeInnerText
        {
            get
            {
                return base.templateNode.InnerText;
            }
            set
            {
                base.templateNode.InnerText = value;
            }
        }
        */
        protected override Unifiable ProcessChangeU() {
            string innerThought = Recurse();
            return Succeed(innerThought);
        }


    }
}
