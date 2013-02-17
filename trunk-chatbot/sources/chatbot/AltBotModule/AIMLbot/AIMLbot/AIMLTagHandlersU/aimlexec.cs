using System.IO;
using System.Xml;
using AltAIMLbot.Utils;
using MushDLR223.Utilities;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The srai element instructs the AIML interpreter to pass the result of Processing the contents 
    /// of the srai element to the AIML matching loop, as if the input had been produced by the user 
    /// (this includes stepping through the entire input normalization Process). The srai element does 
    /// not have any attributes. It may contain any AIML template elements. 
    /// 
    /// As with all AIML elements, nested forms should be parsed from inside out, so embedded srais are 
    /// perfectly acceptable. 
    /// </summary>
    public class aimlexec : AIMLTagHandlerU
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be Processed</param>
        public aimlexec(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            isRecursive = false;
        }

        //private static int depth = 0;
        public override Unifiable CompleteProcessU()
        {
            try
            {
                ResetValues(true);
                isRecursive = true;
                Unifiable f = Recurse();
                //depth++;
               /* int d = request.GetCurrentDepth();
                if (d > 30)
                {
                    writeToLogWarn("WARNING Depth pretty deep " + f + " returning empty");
                    return Unifiable.Empty;
                }
                if (depth > 30)
                {
                    writeToLogWarn("WARNING Depth pretty deep " + f + " returning empty");
                    return Unifiable.Empty;
                }*/
                var tn = templateNode;
                foreach (var cn in tn.ChildNodes)
                {

                }
                string s = Unifiable.ToVMString(f);
                XmlNode node =
                    new XmlDocumentLineInfo(s, false).ReadNode(
                        XmlDocumentLineInfo.CreateXmlTextReader(new StringReader(s)));
                bool templateSucceeded;
                bool createdOutput;
                templateInfo = GetTemplateInfo();
                request.LastHandler = Proc.TagHandling.proccessResponse(query, request, result, (XmlNode)node, templateInfo.Guard, out createdOutput,
                                      out templateSucceeded, this, templateInfo, ReadOnly, true);
                return Unifiable.Empty;
            }
            finally
            {
             //   depth--;
            }
        }

        protected override Unifiable ProcessChangeU()
        {
            return templateNodeInnerText;
            if (false && templateNode.Name.ToLower() == "aimlexec")
            {
                Unifiable result = Unifiable.CreateAppendable();
                Unifiable rest = templateNodeInnerText;
                while (!IsNullOrEmpty(rest))
                {
                    // Unifiable[] words = templateNodeInnerText.AsString().Split(new char[]{''});
                    Unifiable word = templateNodeInnerText.First;
                    rest = rest.Rest;
                    {
                        Unifiable newWord = word;
                        result.Append(newWord + " ");
                    }
                }
                return Trim(Unifiable.ToVMString(result));
            }
            return templateNodeInnerText;
        }
    }
}
