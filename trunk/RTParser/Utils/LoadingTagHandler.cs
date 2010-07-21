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
        sealed protected override Unifiable ProcessChange()
        {
            return base.RecurseProcess();
        }
        /// <summary>
        /// Do a transformation on the Unifiable found in the InputString attribute
        /// </summary>
        /// <returns>The resulting transformed Unifiable</returns>
        sealed public override Unifiable Transform()
        {
            return templateNodeInnerText;
            /*

            if (!this.inputString.IsEmpty)
            {
                return this.RecurseProcess();
            }
            else
            {
                return Unifiable.Empty;
            }*/
        }

        #endregion

        sealed public override Unifiable CompleteProcess()
        {
            var res = base.CompleteProcess();

            LoaderOptions loaderOptions = LoaderOptions.GetDefault(request);
            string filename = loaderOptions.Filename;
            try
            {
                string o2 = loaderOptions.Filename = DocumentInfo();
                ProcessLoad(loaderOptions);
            }
            finally
            {
                loaderOptions.Filename = filename;
            }

            return res;
        }

        protected abstract Unifiable ProcessLoad(LoaderOptions loaderOptions);
    }
}