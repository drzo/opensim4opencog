using System;
using System.Collections.Generic;
using System.Xml;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The random element instructs the AIML interpreter to return exactly one of its contained li 
    /// elements randomly. The random element must contain one or more li elements of type 
    /// defaultListItem, and cannot contain any other elements.
    /// </summary>
    public class random : AIMLTagHandlerU
    {
        static Random r = new Random();
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be Processed</param>
        public random(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            isRecursive = false;
        }


        protected override Unifiable ProcessChangeU()     
        {
            if (RecurseResultValid)
            {
                return RecurseResult;
            }
            ResetValues(true);
            int maxConditions = GetAttribValue<int>(templateNode, "count", 1);
            int minConditions = GetAttribValue<int>(templateNode, "mincount", 1);
            var nodes = SelectNodes(templateNode.ChildNodes);
            if (CheckNode("random"))
            {
                Unifiable appendable = Unifiable.CreateAppendable();
                if (templateNode.HasChildNodes)
                {
                    // only grab <li> nodes
                    List<XmlNode> listNodes = new List<XmlNode>();
                    foreach (XmlNode childNode in nodes)
                    {
                        if (childNode.Name == "li")
                        {
                            listNodes.Add(childNode);
                        }
                    }
                    // randomly grab <li> nodes and use them
                    List<XmlNode> useNodes = new List<XmlNode>();
                    while (maxConditions-- > 0 && listNodes.Count > 0)
                    {
                        XmlNode chosenNode = listNodes[r.Next(0, listNodes.Count - 1)];
                        listNodes.Remove(chosenNode);
                        var childResult = ProcessChildNode(chosenNode);
                        if (childResult != null)
                        {
                            useNodes.Add(chosenNode);
                            QueryHasSuceededN++;
                            appendable.Append(childResult);
                        }
                    }
                    if (QueryHasSuceededN < minConditions)
                    {
                        QueryHasFailedN++;
                        return null;
                    }
                    return appendable;
                }
            }
            return Unifiable.Empty;
        }
    }
}
