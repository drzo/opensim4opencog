using System.Xml;
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLParser;

namespace AltAIMLbot.Utils
{
    public abstract class AIMLFormatingTagHandler : AIMLTagHandlerU
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
        public AIMLFormatingTagHandler(AltBot bot,
                                       User user,
                                       SubQuery query,
                                       Request request,
                                       Result result,
                                       XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            isRecursive = true;
            IsStarAtomically = true;
        }

        #region Overrides of AIMLTagHandler

        /// <summary>
        /// The method that does the actual processing of the text.
        /// </summary>
        /// <returns>The resulting processed text</returns>
        protected override Unifiable ProcessChangeU()
        {
            if (RecurseResultValid) return RecurseResult;
            if (isRecursive && !ReadOnly)
            {
                return RecurseResult = Format(TransformAtomically(FormatEach, true));
            }
            return RecurseResult = TransformAtomically(Format, false);
        }

        protected virtual Unifiable FormatEach(Unifiable text)
        {
            return text;
        }

        public override Unifiable CompleteProcessU()
        {
            if (RecurseResultValid) return RecurseResult;
            var vv = ProcessAimlChange();
            if (!Unifiable.IsNullOrEmpty(vv))
            {
                RecurseResult = vv;
                return vv;
            }
            RecurseResult = vv;
            return vv;
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