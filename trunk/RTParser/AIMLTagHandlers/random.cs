using System;
using System.Xml;
using System.Text;
using System.Collections.Generic;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The random element instructs the AIML interpreter to return exactly one of its contained li 
    /// elements randomly. The random element must contain one or more li elements of type 
    /// defaultListItem, and cannot contain any other elements.
    /// </summary>
    public class random : RTParser.Utils.AIMLTagHandler
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
        public random(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            this.isRecursive = false;
        }


        protected override Unifiable ProcessChange()     
        {
            if (RecurseResultValid)
            {
                return RecurseResult;
            }
            int maxConditions = GetAttribValue<int>(templateNode, "count", 1);
            var nodes = SelectNodes(templateNode.ChildNodes);
            if (this.templateNode.Name.ToLower() == "random")
            {
                if (this.templateNode.HasChildNodes)
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
                    // randomly grab <li> nodes
                    List<XmlNode> useNodes = new List<XmlNode>();
                    while (maxConditions-- > 0 && listNodes.Count > 0)
                    {
                        Random r = new Random();
                        XmlNode chosenNode = listNodes[r.Next(listNodes.Count)];
                        listNodes.Remove(chosenNode);
                        useNodes.Add(chosenNode);
                    }
                    return OutputFromNodes(useNodes, (n) => true);
                }
            }
            return Unifiable.Empty;
        }
    }
}
