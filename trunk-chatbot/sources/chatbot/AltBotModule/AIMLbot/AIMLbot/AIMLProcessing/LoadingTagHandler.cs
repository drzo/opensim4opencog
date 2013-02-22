using System.Xml;
using AltAIMLbot.Utils;
using AltAIMLbot;

namespace AltAIMLbot.Utils
{
    public abstract class LoadingTagHandler : AIMLTagHandler
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
        public LoadingTagHandler(AltBot bot,
                                 User user,
                                 SubQuery query,
                                 Request request,
                                 Result result,
                                 XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            isRecursive = false;
        }
        public bool IsLoadReady;
        protected bool ProcessInnerXmlAsLoad = false;
        
        protected virtual Unifiable PreProcessChange()
        {
            return ProcessChangeU();
        }

        sealed protected override Unifiable ProcessChangeU()
        {
            if (IsLoadReady)
            {
                writeToLogWarn("LoadIsReady AGAIN?!");
            }
            IsLoadReady = true;

            StringAppendableUnifiableImpl recursiveResult = Unifiable.CreateAppendable();
            if (templateNode.HasChildNodes)
            {
                // recursively check
                foreach (XmlNode childNode in templateNode.ChildNodes)
                {
                    Unifiable processChildNode = ProcessChildNode(childNode);
                    if (processChildNode == null)
                    {
                        writeToLogWarn("processChildNode==NULL");
                    }
                    SaveResultOnChild(childNode, processChildNode);
                    recursiveResult.Append(processChildNode);
                }
            }
            return recursiveResult;
        }

        public override Unifiable RecurseChildren()
        {
            if (FinalResultValid) return FinalResult;
            if (!IsLoadReady)
            {
                PreProcessChange();
            }
            LoaderOptions saveOpts = request.LoadOptions;
            LoaderOptions loaderOptions = saveOpts.Copy();
            request.LoadOptions = loaderOptions;
            Unifiable vv = null;
            GraphMaster GM = loaderOptions.CtxGraph;
            int size = GM.Size;
            try
            {
                request.CurrentlyLoadingFrom = loaderOptions.CurrentlyLoadingFrom = DocumentInfo();
                vv = ProcessLoad(loaderOptions);
                if (!IsNullOrEmpty(vv))
                {
                    FinalResult = vv;
                }
            }
            finally
            {
                request.LoadOptions = saveOpts;
            }
            int newSize = GM.Size;
            int change = newSize - size;
            string ch = "Loaded " + change + " into " + GM + " was " + size;
            if (FinalResult == (string) null)
            {
                return Succeed(ch);
            }
            return Succeed(ch + " " + FinalResult);
        }

        protected abstract Unifiable ProcessLoad(LoaderOptions loaderOptions);

        public override Unifiable CheckValue(Unifiable value)
        {
            if (ReferenceEquals(value, Unifiable.EmptyRef)) return value;
            if (value == null)
            {
                writeToLogWarn("ChackValue NULL");
                return null;
            }
            else
            {
                if (Unifiable.IsNullOrEmpty(value))
                {
                    writeToLogWarn("CheckValue EMPTY = '" + value + "'");
                    return Unifiable.Empty;
                }
                if (CompleteEvaluatution(value, this, out value))
                {
                    //RecurseResult = vv;
                    return value;
                } 
                return value;
            }
        }
    }
}