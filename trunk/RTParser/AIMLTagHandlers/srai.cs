using System;
using System.Threading;
using System.Xml;
using System.Text;
using System.IO;
using MushDLR223.ScriptEngines;
using RTParser.Utils;

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
    public class srai : RTParser.Utils.AIMLTagHandler
    {
        RTParser.RTPBot mybot;
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be processed</param>
        public srai(RTParser.RTPBot bot,
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

        public static bool UseOriginalProcess = false;

        // unsuesd/... just code refernce
        protected string OriginalProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "srai")
            {
                if (this.templateNode.InnerText.Length > 0)
                {
                    Request subRequest = new AIMLbot.Request(this.templateNode.InnerText, (AIMLbot.User)this.user, (AIMLbot.Bot)this.Proc);
                    subRequest.StartedOn = this.request.StartedOn; // make sure we don't keep adding time to the request
                    Result subQuery = this.Proc.Chat(subRequest);
                    this.request.hasTimedOut = subRequest.hasTimedOut;
                    return subQuery.Output;
                }
            }
            return string.Empty;
        }

        protected override Unifiable ProcessChange()
        {
            string s;
            if (ResultReady(out s)) return s;
            IsStarted = true;
            if (Unifiable.IsNullOrEmpty(RecurseResult))
            {
                try
                {
                    user.Enter(this);
                    int userDepth = user.depth;
                    int sraiDepth = request.GetCurrentDepth();
                    if (userDepth > 30 || sraiDepth > request.SraiDepth.Max)
                    {
                        query.prefix = string.Format("{0}: SRAIDEPTH(user:{1}/request:{2})", request.Graph, userDepth, sraiDepth);
                        writeToLog("WARNING Depth pretty deep " + templateNode + " returning empty");
                        return Unifiable.Empty;
                    }
                    RecurseResult = UseOriginalProcess ? (Unifiable) OriginalProcessChange() : ProcessChange0();
                }
                catch (Exception)
                {
                    user.Exit(this);
                    templateNode.InnerXml = "+" + RecurseResult;
                }
            }
            return RecurseResult;
        }
        public override Unifiable CompleteProcess()
        {
            return ProcessChange();
            return base.CompleteProcess();
        }
        public override string Transform()
        {
            return base.Transform();
        }
        public override Unifiable RecurseProcess()
        {
            return ProcessChange();
        }
        protected Unifiable ProcessChange0()
        {
            string s;
            if (ResultReady(out s)) return s;
            if (CheckNode("srai"))
            {
                bool chatTraced = Proc.chatTrace;
                Proc.chatTrace = false;
                try
                {
                    var templateNodeInnerValue = Recurse();
                    return ProcessChangeSrai(request, query, templateNodeInnerValue, templateNode, initialString, writeToLog);
                }
                catch (Exception e)
                {
                    writeToLog("ERROR: " + e);
                    throw;
                }
                finally
                {
                    Proc.chatTrace = chatTraced;
                }
            }
            return Unifiable.Empty;
        }

        internal static Unifiable ProcessChangeSrai(Request request, SubQuery query,
            Unifiable templateNodeInnerValue, XmlNode templateNode, string initialString, OutputDelegate writeToLog)
        {
            try
            {
                writeToLog = writeToLog ?? DEVNULL;
                RTPBot mybot = request.TargetBot;
                User user = request.Requester;
                int depth = user.depth;
                var thisrequest = request;
                Result thisresult = request.CurrentResult;
                string prefix = query.prefix;
                //string s;
                //if (ResultReady(out s)) return s;
                /*
                int d = request.GetCurrentDepth();
                if (d > request.SraiDepth.Max)
                {
                    writeToLog(prefix + " WARNING Depth pretty deep " + templateNode + " returning empty");
                    return Unifiable.Empty;
                }
                 */
                //if (CheckNode("srai"))
                {
                    //Unifiable templateNodeInnerValue = Recurse();
                    if (!templateNodeInnerValue.IsEmpty)
                    {
                        AIMLbot.Request subRequest = request.CreateSubRequest(templateNodeInnerValue, user,
                                                                              mybot, (AIMLbot.Request) request);


                        string requestGraphSrai = request.Graph.Srai;
                        subRequest.Graph = request.GetGraph(requestGraphSrai);

                        var ti = query.CurrentTemplate;
                        if (ti != null) subRequest.UsedTemplates.Add(ti);

                        // make sure we don't keep adding time to the request
                        bool showDebug = DebugSRAIs;
                        string subRequestrawInput = subRequest.rawInput;
                        if (showDebug && subRequestrawInput.Contains("SYSTEMANIM") || subRequestrawInput.Contains("HANSANIM"))
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
                            subResult = mybot.Chat0(subRequest, subRequest.Graph);
                        }
                        finally
                        {
                            user.SuspendAdd = prevSO;
                            subRequest.GraphsAcceptingUserInput = prev;
                        }


                        thisrequest.hasTimedOut = subRequest.hasTimedOut;
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
                                                     AIMLLoader.TextAndSourceInfo(templateNode));

                                mybot.writeChatTrace("\"LN:{0}\" -> \"RPY:MISSING({1})\" ;\n", depth, depth);
                            }

                            return Unifiable.Empty;
                        }
                        else
                        {
                            string sss = thisresult.ToString();
                            if (showDebug)
                            {
                                writeToLog("{0} SUCCESS RETURN {1}  {2} '{3}'", prefix, subRequestrawInput,
                                           subResult.Score, subQueryRawOutput);
// ReSharper disable ConditionIsAlwaysTrueOrFalse
                                if (query != null)
// ReSharper restore ConditionIsAlwaysTrueOrFalse
                                {
                                    if (query.CurrentTemplate != null)
                                    {
                                        writeToLog("SCORE {0}*{1}->{2} ",
                                                   subResult.Score, query.CurrentTemplate.Rating,
                                                   query.CurrentTemplate.Rating *= subResult.Score);

                                    }
                                }
                            }
                            thisrequest.AddSubResult(thisresult);
                            thisrequest.AddSubResult(subResult);
                        }

                        if (mybot.chatTrace)
                        {
                            mybot.writeChatTrace("\"L{0}\" -> \"S{1}\" ;\n", depth, depth);
                            mybot.writeChatTrace("\"S{0}\" -> \"SIN:{1}\" ;\n", depth, subRequestrawInput);
                            mybot.writeChatTrace("\"SIN:{0}\" -> \"PATH:{1}\" [label=\"{2}\"] ;\n", subRequestrawInput,
                                                 depth, subResult.NormalizedPaths);
                            mybot.writeChatTrace("\"PATH:{0}\" -> \"LN:{1}\" [label=\"{2}\"] ;\n", depth, depth,
                                                 AIMLLoader.TextAndSourceInfo(templateNode));
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
                //depth--;
            }
        }
    }
}
