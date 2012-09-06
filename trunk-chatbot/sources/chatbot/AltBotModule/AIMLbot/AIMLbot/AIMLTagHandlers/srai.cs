using System;
using System.Xml;
using System.Text;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The srai element instructs the AIML interpreter to pass the result of processing the contents 
    /// of the srai element to the AIML matching loop, as if the input had been produced by the user 
    /// (this includes stepping through the entire input normalization process). The srai element does 
    /// not have any attributes. It may contain any AIML template elements. 
    /// 
    /// As with all AIML elements, nested forms should be parsed from inside out, so embedded srais are 
    /// perfectly acceptable. 
    /// </summary>
    public class srai : AltAIMLbot.Utils.AIMLTagHandler
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
        public srai(AltAIMLbot.AltBot bot,
                        AltAIMLbot.User user,
                        AltAIMLbot.Utils.SubQuery query,
                        AltAIMLbot.Request request,
                        AltAIMLbot.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        protected override string ProcessChange()
        {
            if (this.TemplateNodeName == "srai")
            {
                string graphName = GetAttribValue("graph", "*");

                if (this.request.depth < this.request.depthMax)
                {
                    if (this.TemplateNodeHasText)
                    {
                        if (TemplateNodeAttributes.Count > 0)
                        {
                            string myTopic = GetAttribValue("topic", "");
                            string myState = GetAttribValue("state", "");
                            if ((myTopic.Length > 0) || (myState.Length > 0))
                            {
                                // Extended SRAI with explicit topic and state attributes

                                // remember this level
                                string localTopic = this.user.Topic;
                                string localState = this.user.State;
                                // pass through anything not specified
                                if (myTopic.Length == 0) { myTopic = localTopic; }
                                if (myState.Length == 0) { myState = localState; }
                                // insert the args
                                this.user.Predicates.updateSetting("topic", myTopic);
                                this.user.Predicates.updateSetting("state", myState);
                                // make the call           
                                try
                                {
                                    Request subRequest0 = new Request(this.TemplateNodeInnerText, this.user, this.bot);
                                    subRequest0.StartedOn = this.request.StartedOn; // make sure we don't keep adding time to the request
                                    subRequest0.depth = this.request.depth + 1;
                                    Result subResult0 = this.bot.Chat(subRequest0, graphName);
                                    this.request.hasTimedOut = subRequest0.hasTimedOut;
                                    Console.WriteLine(" --- SRAI: RETURNA [{0}]", subResult0.Output);
                                    return subResult0.Output;
                                }
                                finally
                                {
                                    // restore this level values
                                    this.user.Predicates.updateSetting("topic", localTopic);
                                    this.user.Predicates.updateSetting("state", localState);
                                    
                                }
                            }
                        }
                        //else
                        //{
                        // Plain old SRAI
                        Request subRequest = new Request(this.TemplateNodeInnerText, this.user, this.bot);
                        subRequest.StartedOn = this.request.StartedOn; // make sure we don't keep adding time to the request
                        subRequest.depth = this.request.depth + 1;
                        Result subResult = this.bot.Chat(subRequest, graphName);
                        this.request.hasTimedOut = subRequest.hasTimedOut;
                        Console.WriteLine(" --- SRAI: RETURNB [{0}]", subResult.Output);
                        return subResult.Output;
                        //}
                    }
                }

                Console.WriteLine(" --- SRAI: RETURNC EMPTY!!!");
                return string.Empty;
            }
            else
            {
                Console.WriteLine(" --- SRAI: RETURND RECURSION DEPTH EXCEEDED!!!");
                return string.Empty;
            }
        }
    }
}
