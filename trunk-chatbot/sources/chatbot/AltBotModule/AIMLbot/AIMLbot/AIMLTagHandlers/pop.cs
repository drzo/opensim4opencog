using System;
using System.Collections.Generic;
using System.Xml;
using System.Text;
using AltAIMLParser;
using AltAIMLbot;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class pop : AIMLTagHandler
    {

        public pop(AltBot bot,
                User user,
                SubQuery query,
                Request request,
                Result result,
                XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }



        protected override Unifiable ProcessChangeU()
        {
            if (templateNode.Name.ToLower() == "pop")
            {
                // If there is a conversation memo then pop it
                // otherwise take the tag content as a srai (to trip say a random reply)

                try
                {
                    if (Proc.conversationStack.Count > 0)
                    {
                        Unifiable converseMemo = Proc.conversationStack.Pop();
                        return converseMemo;
                    }
                    else
                    {
                        Unifiable starContent = Recurse();
                        //return callSRAI(starContent);
                        return popSrai(starContent);
                    }

                }
                catch
                {

                }

            }
            return Unifiable.Empty;

        }

        protected string popSrai(string innerContent)
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
                                Request subRequest0 = new Request(innerContent, this.user, request.That, this.bot, false, request.RequestType | RequestKind.SraiTag, request);
                                subRequest0.StartedOn = this.request.StartedOn; // make sure we don't keep adding time to the request
                                subRequest0.depth = this.request.depth + 1;
                                Result subResult0 = this.bot.Chat(subRequest0, graphName);
                                this.request.hasTimedOut = subRequest0.hasTimedOut;
                                string outstring0 = subResult0.Output.AsString();
                                if (outstring0 == null) outstring0 = string.Empty;
                                Console.WriteLine(" --- POPSRAI: RETURNA [{0}]", outstring0);
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
                    string ourInput = this.Recurse();
                    Request subRequest = new Request(ourInput, this.user, request, request.Responder, this.bot, request, request.Graph, false,
                                                         request.RequestType | RequestKind.SraiTag);
                    subRequest.StartedOn = this.request.StartedOn; // make sure we don't keep adding time to the request
                    subRequest.depth = this.request.depth + 1;
                    Result subResult = this.bot.Chat(subRequest, graphName);
                    this.request.hasTimedOut = subRequest.hasTimedOut;
                    string outstring = subResult.Output.AsString();
                    if (outstring == null) outstring = string.Empty;
                    //Console.WriteLine(" --- SRAI: RETURNB [{0}]", subResult.Output);
                    Console.WriteLine(" --- POPSRAI: RETURNB [{0}]", outstring);
                    return outstring;
                    //}
                }
            }
            else
            {
                Console.WriteLine(" --- POPSRAI: RETURND RECURSION DEPTH EXCEEDED!!!");
                return string.Empty;
            }
            return string.Empty;
        }
    }
}