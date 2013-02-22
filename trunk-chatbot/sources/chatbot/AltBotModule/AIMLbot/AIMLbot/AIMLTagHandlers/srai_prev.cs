using System;
using System.Collections.Generic;
using System.Xml;
using System.Text;
using AltAIMLParser;
using AltAIMLbot;
using AltAIMLbot.Utils;

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
    public class srai_prev : Utils.AIMLTagHandler
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
        public srai_prev(AltBot bot,
                        User user,
                        Utils.SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        protected override Unifiable ProcessChangeU()
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
                            // Extended SRAI with explicit topic and state attributes
                            Dictionary<string, string> savedVars = new Dictionary<string, string>();
                            foreach (XmlAttribute attribute in TemplateNodeAttributes)
                            {
                                string name = attribute.Name;
                                string newValue = attribute.Value;
                                // remember this level
                                savedVars[name] = this.user.Predicates.grabSetting(name);
                                // insert the args
                                this.user.Predicates.updateSetting(name, newValue);
                            }
                            {        
                                try
                                {
                                    Request subRequest0 = new Request(this.TemplateNodeInnerText, this.user, request.That,
                                        this.bot, false, request.RequestType | RequestKind.SraiTag, request);
                                    subRequest0.StartedOn = this.request.StartedOn; // make sure we don't keep adding time to the request
                                    subRequest0.depth = this.request.depth + 1;
                                    Result subResult0 = this.bot.Chat(subRequest0, graphName);
                                    this.request.hasTimedOut = subRequest0.hasTimedOut;
                                    string outstring0 = subResult0.Output.AsString();
                                    if (outstring0 == null) outstring0 = string.Empty;
                                    Console.WriteLine(" --- SRAI: RETURNA [{0}]", outstring0);
                                    return outstring0;
                                }
                                finally
                                {
                                    // restore this level values
                                    foreach (var savedVar in savedVars)
                                    {
                                        this.user.Predicates.updateSetting(savedVar.Key, savedVar.Value);    
                                    }                                    
                                }
                            }
                        }
                        //else
                        //{
                        // Plain old SRAI
                        string ourInput =this.TemplateNodeInnerText;
                        Request subRequest = new Request(ourInput, this.user, request.LoadOptions, request.Responder, this.bot, request, request.Graph, false,
                                                         request.RequestType | RequestKind.SraiTag);
                        subRequest.StartedOn = this.request.StartedOn; // make sure we don't keep adding time to the request
                        subRequest.depth = this.request.depth + 1;
                        Result subResult = this.bot.Chat(subRequest, graphName);
                        this.request.hasTimedOut = subRequest.hasTimedOut;
                        string outstring = subResult.Output.AsString();
                        if (outstring == null) outstring = string.Empty;
                        //Console.WriteLine(" --- SRAI: RETURNB [{0}]", subResult.Output);
                        Console.WriteLine(" --- SRAI: RETURNB [{0}]", outstring);
                        return outstring;
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
