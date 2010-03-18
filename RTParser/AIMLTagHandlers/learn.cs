using System;
using System.Xml;
using System.Text;
using System.IO;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The learn element instructs the AIML interpreter to retrieve a resource specified by a URI, 
    /// and to process its AIML object contents.
    /// </summary>
    public class learn : RTParser.Utils.AIMLTagHandler
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
        public learn(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        protected override Unifiable ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "learn")
            {
                // currently only AIML files in the local filesystem can be referenced
                // ToDo: Network HTTP and web service based learning
                if (!templateNodeInnerText.IsEmpty)
                {
                    Unifiable path = templateNodeInnerText;
                    try
                    {
                        XmlDocument doc = new XmlDocument();
                        try
                        {
                            FileInfo fi = new FileInfo(path);
                            if (fi.Exists)
                            {
                                doc.Load(path);
                            }
                            else
                            {
                                XmlTextReader reader = new XmlTextReader(path);
                                doc.Load(reader);
                            }
                        }
                        catch (Exception e)
                        {
                            String s =
                                "ERROR! Attempted (but failed) to <learn> some new AIML from the following URI: " + path +
                                " error " + e;
                            this.Proc.writeToLog(s);
                            return s;
                        }

                        doc.Load(path);
                        this.Proc.loadAIMLFromXML(doc, path);
                    }
                    catch (Exception e2)
                    {
                        String s =
                            "ERROR! Attempted (but failed) to <learn> some new AIML from the following URI: " + path +
                            " error " + e2;
                        this.Proc.writeToLog(s);
                        return s;
                    }

                }
            }
            return Unifiable.Empty;
        }
    }
}
