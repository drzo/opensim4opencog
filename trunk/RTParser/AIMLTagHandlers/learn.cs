using System;
using System.Xml;
using System.Text;
using System.IO;
using RTParser.Utils;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The learn element instructs the AIML interpreter to retrieve a resource specified by a URI, 
    /// and to process its AIML object contents.
    /// supports network HTTP and web service based AIML learning (as well as local filesystem)
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
                LoaderOptions opts = new LoaderOptions(request.Graph);

                opts.recurse = Unifiable.IsLogicTF(GetAttribValue("recurse", Unifiable.Empty));
                //recurse here?
                GraphMaster g = request.Graph;
                var g0 = g;
                String gn = GetAttribValue("graph", null);
                if (gn != null)
                {
                    g = Proc.GetGraph(gn, request.Graph);
                }
                request.Graph = g;
                try
                {
                    Unifiable templateNodeInnerText = Recurse();
                    if (!templateNodeInnerText.IsEmpty)
                    {
                        Unifiable path = templateNodeInnerText;
                        try
                        {
                            opts.Filename = path;
                            Proc.loadAIMLFromURI(path, opts, request);
                        }
                        catch (Exception e2)
                        {
                            String s =
                                "ERROR! Attempted (but failed) to <learn> some new AIML from the following URI: " + path + " error " + e2;
                            writeToLog(s);
                        }

                    }
                }
                finally
                {
                    request.Graph = g0;
                } 
            }
            return Unifiable.Empty;
        }
    }
}
