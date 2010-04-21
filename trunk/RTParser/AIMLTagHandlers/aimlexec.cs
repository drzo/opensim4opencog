using System;
using System.Threading;
using System.Xml;
using System.Text;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The srai element instructs the AIML interpreter to pass the result of processing the contents 
    /// of the srai element to the AIML matching loop, as if the input had been produced by the user 
    /// (this includes stepping through the entire input normalization process). The srai element does 
    /// not have any attributes. It may contain any AIML template elements. 
    /// 
    /// As with all AIML elements, nested forms should be parsed from inside out, so embedded srais are 
    /// perfectly acceptable. 
    /// </summary>
    public class aimlexec : RTParser.Utils.AIMLTagHandler
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
        public aimlexec(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            isRecursive = false;
        }

        private static int depth = 0;
        public override Unifiable CompleteProcess()
        {
            Unifiable f = Recurse();           
            try
            {
                depth++;
                int d = request.GetCurrentDepth();
                if (d > 30)
                {
                    Console.WriteLine("WARNING Depth pretty deep " + f + " returning empty");
                    return Unifiable.Empty;
                }
                if (depth > 30)
                {
                    Console.WriteLine("WARNING Depth pretty deep " + f + " returning empty");
                    return Unifiable.Empty;
                }
                XmlNode node = new XmlDocument().ReadNode(new XmlTextReader(new System.IO.StringReader(f.ToString())));
                bool found0;
                Proc.proccessResponse(query, request, result, (XmlNode)node, null, out found0);
                return Unifiable.Empty;
            }
            finally
            {
                depth--;
            }
        }

        protected override Unifiable ProcessChange()
        {
            return templateNodeInnerText;
            if (false && this.templateNode.Name.ToLower() == "formal")
            {
                Unifiable result = Unifiable.CreateAppendable();
                Unifiable rest = templateNodeInnerText;
                while (!rest.IsEmpty)
                {
                    // Unifiable[] words = templateNodeInnerText.AsString().Split(new char[]{''});
                    Unifiable word = templateNodeInnerText.First();
                    rest = rest.Rest();
                    {
                        Unifiable newWord = word;
                        result.Append(newWord + " ");
                    }
                }
                return result.ToString().Trim();
            }
            return templateNodeInnerText;
        }
    }
}
