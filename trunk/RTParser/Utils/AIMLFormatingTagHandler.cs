using System;
using System.Xml;
using RTParser.AIMLTagHandlers;

namespace RTParser.Utils
{
    public abstract class AIMLFormatingTagHandler : AIMLTagHandler
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
        public AIMLFormatingTagHandler(RTParser.RTPBot bot,
                                       RTParser.User user,
                                       RTParser.Utils.SubQuery query,
                                       RTParser.Request request,
                                       RTParser.Result result,
                                       XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            isRecursive = true;
        }

        #region Overrides of AIMLTagHandler

        /// <summary>
        /// The method that does the actual processing of the text.
        /// </summary>
        /// <returns>The resulting processed text</returns>
        protected override Unifiable ProcessChange()
        {
            if (isRecursive && !ReadOnly)
            {
                return Format(TransformAtomically(null, true));
            }
            return TransformAtomically(Format, false);
        }

        #endregion
        

        /// <summary>
        /// The subclass only needs to process the non atomic inner text
        /// </summary>
        /// <param name="text"></param>
        /// <returns></returns>
        protected abstract Unifiable Format(Unifiable text);
    }
}