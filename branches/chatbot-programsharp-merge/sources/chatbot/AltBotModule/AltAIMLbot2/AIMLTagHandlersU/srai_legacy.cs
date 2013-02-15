
#if false
namespace RTParser.AIMLTagHandlerUs
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
    public class srai_legacy : RTParser.Utils.AIMLTagHandlerU
    {
        RTParser.AltBot mybot;
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be Processed</param>
        public srai_legacy(RTParser.AltBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            mybot = bot;
        }

        public override void writeToLog(string unifiable, params object[] objs)
        {
            if (Parent != null && Parent != this)
            {
                Parent.writeToLog(unifiable, objs);
            }
            else
            {
                base.writeToLog(unifiable, objs);
            }
            if (false && string.Format(unifiable,objs).Contains("&"))
            {
                base.writeToLog("!ERRROR");
            }
        }


        // unsuesd/... just code refernce
        protected string  OriginalProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "srai")
            {
                if (this.templateNode.InnerText.Length > 0)
                {
                    Request subRequest = new MasterRequest(this.templateNode.InnerText, (AIMLbot.User)this.user, (AIMLbot.Bot)this.Proc);
                    subRequest.StartedOn = this.request.StartedOn; // make sure we don't keep adding time to the request
                    Result subQuery = this.Proc.Chat(subRequest);
                    this.request.hasTimedOut = subRequest.hasTimedOut;
                    return subQuery.Output;
                }
            }
            return string.Empty;
        }

        static int depth = 0;
        protected override Unifiable ProcessChangeU()
        {
            try
            {
                int d = request.GetCurrentDepth();
                object prefix = string.Format("{0}: SRAI({1}/{2})", request.Graph, depth, d);
                if (d > request.SraiDepth.Max)
                {
                    writeToLog(prefix + "WARNING Depth pretty deep " + templateNode + " returning empty");
                    return Unifiable.Empty;
                }
                if (depth > 30)
                {
                    writeToLog(prefix + "WARNING Depth pretty deep " + templateNode + " returning empty");
                    return Unifiable.Empty;
                }
                if (this.templateNode.Name.ToLower() == "srai")
                {
                    Unifiable templateNodeInnerValue = Recurse();
                    if (!templateNodeInnerValue.IsEmpty)
                    {
                        AIMLbot.Request subRequest = request.CreateSubRequest(templateNodeInnerValue, this.user,
                                                                              this.Proc, (AIMLbot.Request) request);


                        subRequest.Graph = request.Graph.Srai;

                        var ti = query.CurrentTemplate;
                        if (ti != null) subRequest.UsedTemplates.Add(ti);

                        // make sure we don't keep adding time to the request
                        bool showDebug = true;
                        string subRequestrawInput = subRequest.rawInput;
                        if (subRequestrawInput.Contains("SYSTEMANIM") || subRequestrawInput.Contains("HANSANIM"))
                        {
                            showDebug = false;
                        }

                        if (showDebug)
                            writeToLog(prefix + " CALLING '" + subRequestrawInput + "'");


                        if (mybot.chatTrace)
                        {
                            //  mybot.bot.writeChatTrace("\"L{0}\" -> \"S{1}\" ;\n", depth - 1, depth-1);
                            // mybot.bot.writeChatTrace("\"L{0}\" -> \"S{1}\" ;\n", depth, depth);
                            //mybot.bot.writeChatTrace("\"S{0}\" -> \"SRC:{1}\" ;\n", depth, CatTextInfo());

                            //mybot.bot.writeChatTrace("\"S{0}\" -> \"S{1}\" ;\n", depth - 1, depth);
                            //mybot.bot.writeChatTrace("\"S{0}\" -> \"SIN:{1}\" ;\n", depth, subRequestrawInput);
                            //mybot.bot.writeChatTrace("\"SIN:{0}\" -> \"LN:{1}\" ;\n", subRequestrawInput, CatTextInfo());
                        }
                        if (depth > 200)
                        {
                            writeToLog(prefix + " FAILING TOOOO DEEEEP '" + subRequestrawInput + "'");
                            return Unifiable.Empty;
                        }
                        AIMLbot.Result subResult;
                        var prev = subRequest.GraphsAcceptingUserInput;
                        var prevSO = user.SuspendAdd;
                        try
                        {
                            subRequest.GraphsAcceptingUserInput = true;
                            //var newresult = new AIMLbot.Result(request.user, Proc, request);
                            //subRequest.result = newresult;
                            user.SuspendAdd = true;
                            if (request.IsTraced) subRequest.IsTraced = !showDebug;
                            subResult = this.Proc.Chat0(subRequest, subRequest.Graph);
                        }
                        finally
                        {
                            user.SuspendAdd = prevSO;
                            subRequest.GraphsAcceptingUserInput = prev;
                        }
                        this.request.hasTimedOut = subRequest.hasTimedOut;
                        var subQueryRawOutput = subResult.RawOutput.Trim();
                        if (Unifiable.IsNullOrEmpty(subQueryRawOutput))
                        {
                            if (showDebug)
                                writeToLog(prefix + " MISSING RETURN ");
                            if (mybot.chatTrace)
                            {
                                mybot.writeChatTrace("\"L{0}\" -> \"S{1}\" ;\n", depth, depth);
                                mybot.writeChatTrace("\"S{0}\" -> \"SIN:{1}\" ;\n", depth, subRequestrawInput);
                                //mybot.writeChatTrace("\"SIN:{0}\" -> \"LN:{1}\" ;\n", subRequestrawInput, CatTextInfo());
                                mybot.writeChatTrace("\"SIN:{0}\" -> \"PATH:{1}\" [label=\"{2}\"] ;\n",
                                                     subRequestrawInput, depth, subResult.NormalizedPaths);
                                mybot.writeChatTrace("\"PATH:{0}\" -> \"LN:{1}\" [label=\"{2}\"] ;\n", depth, depth,
                                                     LineNumberTextInfo());

                                mybot.writeChatTrace("\"LN:{0}\" -> \"RPY:MISSING({1})\" ;\n", depth, depth);
                            }

                            return Unifiable.Empty;
                        }
                        else
                        {
                            string sss = result.ToString();
                            if (showDebug)
                            {
                                writeToLog("{0} SUCCESS RETURN {1}  {2} '{3}'", prefix, subRequestrawInput,
                                           subResult.Score, subQueryRawOutput);
                                if (query != null)
                                {
                                    if (query.CurrentTemplate != null)
                                    {
                                        writeToLog("SCORE {0}*{1}->{2} ",
                                                   subResult.Score, query.CurrentTemplate.Rating,
                                                   query.CurrentTemplate.Rating *= subResult.Score);

                                    }
                                }
                            }
                            this.request.AddSubResult(result);
                            this.request.AddSubResult(subResult);
                        }

                        if (mybot.chatTrace)
                        {
                            mybot.writeChatTrace("\"L{0}\" -> \"S{1}\" ;\n", depth, depth);
                            mybot.writeChatTrace("\"S{0}\" -> \"SIN:{1}\" ;\n", depth, subRequestrawInput);
                            mybot.writeChatTrace("\"SIN:{0}\" -> \"PATH:{1}\" [label=\"{2}\"] ;\n", subRequestrawInput,
                                                 depth, subResult.NormalizedPaths);
                            mybot.writeChatTrace("\"PATH:{0}\" -> \"LN:{1}\" [label=\"{2}\"] ;\n", depth, depth,
                                                 LineNumberTextInfo());
                            mybot.writeChatTrace("\"LN:{0}\" -> \"RPY:{1}\" ;\n", depth, subQueryRawOutput);
                        }
                        return subResult.Output;
                    }
                    else
                    {
                        if (templateNodeInnerValue.IsEmpty)
                        {
                            writeToLog("InnerValue.IsEmpty! " + initialString);
                            return templateNodeInnerValue;
                        }
                    }
                }
                return Unifiable.Empty;
            }
            finally
            {
                depth--;
            }
        }
    }
}
#endif