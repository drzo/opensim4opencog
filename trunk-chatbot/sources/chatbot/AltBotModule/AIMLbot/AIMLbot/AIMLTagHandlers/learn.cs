using System;
using System.Xml;
using System.Text;
using System.IO;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The learn element instructs the AIML interpreter to retrieve a resource specified by a URI, 
    /// and to process its AIML object contents.
    /// </summary>
    public class learn : AltAIMLbot.Utils.AIMLTagHandler
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
        public learn(AltAIMLbot.AltBot bot,
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
            if (this.TemplateNodeName == "learn")
            {
                // currently only AIML files in the local filesystem can be referenced
                // ToDo: Network HTTP and web service based learning
                if (this.TemplateNodeHasText)
                {
                    string path = this.TemplateNodeInnerText;
                    FileInfo fi = new FileInfo(path);
                    if (fi.Exists)
                    {
                        XmlDocument doc = new XmlDocument();
                        try
                        {
                            doc.Load(path);
                            this.bot.loadAIMLFromXML(doc, path);
                        }
                        catch
                        {
                            this.bot.writeToLog("ERROR! Attempted (but failed) to <learn> some new AIML from the following URI: " + path);
                        }
                    }
                }
            }
            return string.Empty;
        }
    }
}
