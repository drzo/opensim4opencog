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
    public class learn : RTParser.Utils.LoadingTagHandler
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
            IsStarted = true;
            isRecursive = true;
            var recursiveResult = Unifiable.CreateAppendable();

            if (templateNode.HasChildNodes)
            {

                XmlNode attach = AIMLLoader.CopyNode("aiml", templateNode, false);
                attach.RemoveAll();
                // recursively check
                foreach (XmlNode childNode in templateNode.ChildNodes)
                {
                    attach.AppendChild(EvalChild(childNode));
                }
                templateNode = attach;
            }
            return recursiveResult;
        }

        private XmlNode EvalChild(XmlNode templateNode)
        {
            XmlNode attach = templateNode.CloneNode(false);// //AIMLLoader.CopyNode(templateNode, false);
            LineInfoElement.unsetReadonly(attach);
            if (templateNode.HasChildNodes)
            {
                // recursively check
                foreach (XmlNode childNode in templateNode.ChildNodes)
                {
                    if (childNode.LocalName=="eval")
                    {
                        LineInfoElement tchiuld = getNode("<template>" + childNode.InnerXml + "</template>", childNode);
                        LineInfoElement.unsetReadonly(tchiuld);
                        Unifiable processChildNode = ProcessChildNode(tchiuld, ReadOnly, false);
                        SaveResultOnChild(childNode, processChildNode);
                        LineInfoElement readNode = getNode("<node>" + Unifiable.InnerXmlText(childNode) + "</node>", childNode);
                        LineInfoElement.unsetReadonly(readNode);
                        if (readNode.ChildNodes.Count == 1)
                        {
                            XmlNode chilz = readNode.ChildNodes[0];
                            LineInfoElement.chopParent(chilz);
                            attach.AppendChild(chilz);
                            continue;
                        }
                        foreach (XmlNode child in readNode.ChildNodes)
                        {
                            LineInfoElement.unsetReadonly(child);
                            attach.AppendChild(child.CloneNode(true));
                        }
                        continue;
                    }
                    attach.AppendChild(EvalChild(childNode));
                }
            }
            return attach;
        }

        protected override Unifiable ProcessLoad(LoaderOptions loaderOptions)
        {
            if (CheckNode("learn,load"))
            {
               // LoaderOptions loaderOptions = loaderOptions0;// ?? LoaderOptions.GetDefault(request);

                loaderOptions.recurse = Unifiable.IsLogicTF(GetAttribValue("recurse", loaderOptions.recurse ? "True" : "False"), query);
                //recurse here?
                GraphMaster g = request.Graph;
                var g0 = g;
                String graphName = GetAttribValue("graph", null);
                if (graphName != null)
                {
                    g = Proc.GetGraph(graphName, g0);
                    if (g != null) request.Graph = g;
                }

                try
                {
                    string s = templateNode.InnerXml.TrimStart("+ ".ToCharArray());
                    Unifiable templateNodeInnerText = s;
                    if (s.Length > 0)
                    {
                      //  templateNodeInnerText = Recurse();
                    } else
                    {
                       // templateNodeInnerText = s;
                    }
                    //if (!templateNodeInnerText.IsEmpty)
                    {
                        Unifiable path = GetAttribValue("filename,uri,file,url,dir,path,pathname,directory",templateNodeInnerText);
                        try
                        {
                            request.LoadingFrom = DocumentInfo();
                            loaderOptions = request.LoadOptions;
                            if (s.Contains("<"))
                            {
                                request.Loader.loadAIMLNode(templateNode, loaderOptions, request);
                                return s;
                            }
                            else if (path == "")
                            {
                                writeToLogWarn("ERROR! Attempted (but failed) to <learn> some new AIML from the following URI: '{0}' - '{1}'", path, s);
                            }
                            else
                            {
                                loaderOptions.LoadingFrom0 = DocumentInfo();
                                loaderOptions.Loading0 = path;
                                loaderOptions.CtxGraph = request.Graph;
                                request.Loader.loadAIMLURI(path, loaderOptions);
                                return path; // Succeed();
                            }
                        }
                        catch (Exception e2)
                        {
                            Proc.writeToLog(e2);
                            writeToLogWarn("ERROR! Attempted (but failed) to <learn> some new AIML from the following URI: {0} error {1}", path, e2);
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
