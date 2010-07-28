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


        #region Overrides of TextTransformer

        /// <summary>
        /// The method that does the actual processing of the text.
        /// </summary>
        /// <returns>The resulting processed text</returns>
        protected override Unifiable ProcessChange()
        {
            RecurseResult = base.RecurseProcess();
            return RecurseResult;
        }

        #endregion

        public override Unifiable CompleteProcess()
        {
           // var res = base.CompleteProcess();

            var saveOpts = request.LoadOptions;
            var loaderOptions = saveOpts;
            try
            {
                request.LoadingFrom = loaderOptions.LoadingFrom0 = DocumentInfo();
                ProcessLoad(loaderOptions);
            }
            finally
            {
                request.LoadOptions = saveOpts;
            }

            if (RecurseResult == (string)null) return Unifiable.Empty;
            return RecurseResult;
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