using System;
using System.Xml;

namespace RTParser.Utils
{
    abstract public class LoadingTagHandler : RTParser.Utils.AIMLTagHandler
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="Proc">The Proc involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be processed</param>
        public LoadingTagHandler(RTParser.RTPBot bot,
                                 RTParser.User user,
                                 RTParser.Utils.SubQuery query,
                                 RTParser.Request request,
                                 RTParser.Result result,
                                 XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            isRecursive = false;
        }

        protected override Unifiable ProcessChange()
        {
            IsStarted = true;
            isRecursive = true;
            var recursiveResult = Unifiable.CreateAppendable();
            if (templateNode.HasChildNodes)
            {
                // recursively check
                foreach (XmlNode childNode in templateNode.ChildNodes)
                {

                    Unifiable processChildNode = ProcessChildNode(childNode, ReadOnly, false);
                    SaveResultOnChild(childNode, processChildNode);
                    recursiveResult.Append(processChildNode);
                }
            }
            return recursiveResult;
        }

        public override void SaveResultOnChild(XmlNode node, string value)
        {
            //if (value == null) return;
            //if (value == "") return;
            value = CheckValue(value);
            if (value == null || value.Trim() == "")
            {
                writeToLog("-!SaveResultOnChild AIMLTRACE " + value + " -> " + node.OuterXml);
            }
            if (node.NodeType == XmlNodeType.Comment) return;

            if (node is XmlText) node.InnerText = value;
            else
                node.InnerXml = "+" + value;
        }

        public override Unifiable CompleteProcess()
        {
            if (!IsStarted)
            {
                ProcessChange();
            }
            var saveOpts = request.LoadOptions;
            var loaderOptions = saveOpts;
            Unifiable vv = null;
            GraphMaster GM = loaderOptions.CtxGraph;
            int size = GM.Size;
            try
            {
                request.LoadingFrom = loaderOptions.LoadingFrom0 = DocumentInfo();
                vv = ProcessLoad(loaderOptions);
                if (!Unifiable.IsNullOrEmpty(vv))
                {
                    RecurseResult = vv;
                }
            }
            finally
            {
                request.LoadOptions = saveOpts;
            }
            int newSize = GM.Size;
            int change = newSize - size;
            string ch = "Loaded " + GM + " was " + size;
            if (RecurseResult == (string)null)
            {
                return ch;
            }
            return ch + " " + RecurseResult;
        }

        protected abstract Unifiable ProcessLoad(LoaderOptions loaderOptions);

        public override Unifiable CheckValue(Unifiable value)
        {
            if (Object.ReferenceEquals(value, Unifiable.Empty)) return value;
            if (value == null)
            {
                writeToLogWarn("ChackValue NULL");
                return Unifiable.Empty;
            }
            else
            {
                if (Unifiable.IsNullOrEmpty(value))
                {
                    writeToLogWarn("CheckValue EMPTY = '" + value + "'");
                    return Unifiable.Empty;
                }
                string v = value.AsString();
                if (!value.AsString().Contains("<a href"))
                {
                    if (v.Contains("<"))
                    {
                        writeToLogWarn("CheckValue XML = '" + value + "'");
                    }
                    else if (v.Contains("&"))
                    {
                        writeToLogWarn("CheckValue HTML = '" + value + "'");
                    }
                }
                return value;
            }
        }

    }
}