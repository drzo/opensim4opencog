using System.Xml;
using AltAIMLbot.Utils;
using AltAIMLbot;

namespace AltAIMLbot.Utils
{
    public abstract class LoadingTagHandler : AIMLTagHandler, NoReturnResult
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
        protected bool ProcessInnerXmlAsLoad = false;
        public XmlNode loadTemplate;

        public abstract XmlNode PrepairTemplateNodeToBecomeSource();

        protected override Unifiable ProcessChangeU()
        {
            if (loadTemplate == null)
            {
                writeToLogWarn("Should already contain a loadTemplate! ");
                loadTemplate = PrepairTemplateNodeToBecomeSource();
            }
            var saveOpts = request.LoadOptions;
            Unifiable vv = null;
            GraphMaster GM = request.Graph;
            int size = GM.Size;
            try
            {
                request.CurrentlyLoadingFrom = DocumentInfo();
                request.Loader.loadAIMLNode(loadTemplate);
            }
            finally
            {
                request.LoadOptions = saveOpts;
            }
            int newSize = GM.Size;
            int change = newSize - size;
            string ch = "Loaded " + change + " into " + GM + " was " + size;
            if (FinalResult == (string)null)
            {
                return Succeed(ch);
            }
            return Succeed(ch + " " + FinalResult);
        }

        sealed public override Unifiable RecurseChildren()
        {
            if (loadTemplate == null)
            {
                loadTemplate = PrepairTemplateNodeToBecomeSource();
            }
            if (loadTemplate != null)
            {
                return loadTemplate.OuterXml;
            }
            return templateNode.OuterXml;
        }

        protected Unifiable ProcessLoad(LoaderOptions loaderOptions)
        {
            return Unifiable.Empty;
        }

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
                if (CompleteEvaluation(value, this, out value))
                {
                    //RecurseResult = vv;
                    return value;
                } 
                return value;
            }
        }
    }
}