using System;
using System.Xml;
using System.Text;
using System.Collections.Generic;
using AltAIMLParser;
using RTParser;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The random element instructs the AIML interpreter to return exactly one of its contained li 
    /// elements randomly. The random element must contain one or more li elements of type 
    /// defaultListItem, and cannot contain any other elements.
    /// </summary>
    public class random : Utils.AIMLTagHandler
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
        public random(AltBot bot,
                        User user,
                        Utils.SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            this.isRecursive = false;
        }

        protected override string ProcessChange()
        {
            if (this.TemplateNodeName == "random")
            {
                if (this.templateNode.HasChildNodes)
                {
                    // only grab <li> nodes
                    List<XmlNode> listNodes = new List<XmlNode>();
                    foreach (XmlNode childNode in this.templateNode.ChildNodes)
                    {
                        if (childNode.Name.ToLower () == "li")
                        {
                            listNodes.Add(childNode);
                        }
                    }
                    if (listNodes.Count > 0)
                    {
                        //Random r = new Random();
                        //XmlNode chosenNode = (XmlNode)listNodes[r.Next(listNodes.Count)];
                        //string result = chosenNode.InnerXml;
                        string result = this.bot.myRandMem.selectOne(listNodes);

                        // temporary PATCH
                        if ((result.Contains("<star"))&&(!result.Contains(" <star")))
                        {
                            result=result.Replace("<star", " <star");
                        }

                        Console.WriteLine(" Random Selected:{0}", result);
                        return result;
                    }
                    else
                    {
                        Console.WriteLine(" Random listNodes.Count <=0!!!");
                    }

                }
                else
                {
                    Console.WriteLine(" Random HasChildNodes == FALSE !!!");
                }

            }
            return string.Empty;
        }
    }
}
