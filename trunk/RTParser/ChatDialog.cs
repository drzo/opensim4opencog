using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Threading;
using System.Xml;
using LAIR.ResourceAPIs.WordNet;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using RTParser.AIMLTagHandlers;
using RTParser.Database;
using RTParser.Normalize;
using RTParser.Utils;
using RTParser.Variables;
using UPath = RTParser.Unifiable;
using UList = System.Collections.Generic.List<RTParser.Utils.TemplateInfo>;


namespace RTParser
{
    public partial class RTPBot
    {
        public bool AlwaysUseImmediateAimlInImput = true;
        public static bool RotateUsedTemplate = true;
        public readonly TaskQueueHandler HeardSelfSayQueue = new TaskQueueHandler("AIMLBot HeardSelf",
                                                                                   TimeSpan.FromMilliseconds(10),
                                                                                   TimeSpan.FromSeconds(10), true);
        private readonly Object chatTraceO = new object();
        public List<Thread> ThreadList { get { return HeardSelfSayQueue.InteruptableThreads; } }
        public bool chatTrace = true;
        private StreamWriter chatTraceW;
        private JoinedTextBuffer currentEar = new JoinedTextBuffer();
        private int streamDepth;
        public Unifiable botJustSaid;

        // last user talking to bot besides itself
        private User _lastUser;
        public User LastUser
        {
            get
            {
                if (IsInteractiveUser(_lastUser)) return _lastUser;
                User LU = _lastResult != null ? _lastResult.user : null;
                if (IsInteractiveUser(LU)) return LU;
                return null;
            }
            set
            {
                User LU = LastUser;
                if (!IsInteractiveUser(LU) || IsInteractiveUser(value))
                {
                    _lastUser = value;
                }
            }
        }

        // last result from the last talking to bot besides itself
        private Result _lastResult;
        public Result LastResult
        {
            get
            {
                if (_lastResult != null)
                {
                    if (IsInteractiveUser(_lastResult.user)) return _lastResult;
                }
                else
                {
                    User LU = LastUser;
                    if (IsInteractiveUser(LU)) return LU.LastResult;
                }
                return _lastResult;
            }
            set
            {
                Result LR = LastResult;
                if (value == null) return;
                if (LR == null)
                {
                    _lastResult = value;
                }
                LastUser = value.user;

                if (LR != null && LR.user != BotAsUser)
                {
                    if (value.user != LR.user)
                    {
                        _lastUser = value.user ?? _lastUser;
                    }
                }
            }
        }

        /// <summary>
        /// The directory to look in for the WordNet3 files
        /// </summary>
        public string PathToWordNet
        {
            get { return GetPathSetting("wordnetdirectory", "wordnet30"); }
        }

        /// <summary>
        /// The directory to look in for the Lucene Index files
        /// </summary>
        public string PathToLucene
        {
            get { return GetPathSetting("lucenedirectory", "lucenedb"); }
        }

        /// <summary>
        /// The number of categories this Proccessor has in its graphmaster "brain"
        /// </summary>
        public int Size
        {
            get { return GraphMaster.Size + HeardSelfSayGraph.Size; }
        }

        private GraphMaster _g;
        private GraphMaster _h;


        /// <summary>
        /// The "brain" of the Proccessor
        /// </summary>
        public GraphMaster GraphMaster
        {
            get
            {
                if (_g != null) return _g;
                if (String.IsNullOrEmpty(NamePath))
                {
                    writeToLog("No graphmapster!");
                    return null;
                }
                return GetGraph(NamePath, _g);
            }
        }

        public GraphMaster HeardSelfSayGraph
        {
            get
            {
                if (_h != null) return _h;
                if (String.IsNullOrEmpty(NamePath))
                {
                    writeToLog("No HeardSelfSayGraph!");
                    return null;
                }
                return GetGraph(NamePath + "_heardselfsay", _h);
            }
        }


        /// <summary>
        /// The Markovian "brain" of the Proccessor for generation
        /// </summary>
        public MBrain MBrain
        {
            get { return mbrain; }
        }

        private readonly MBrain mbrain = new MBrain();

        public MBrain STM_Brain
        {
            get { return stm_brain; }
        }

        private readonly MBrain stm_brain = new MBrain();

        /// <summary>
        /// Proccessor for phonetic HMM
        /// </summary>
        // emission and transition stored as double hash tables
        public PhoneticHmm pHMM
        {
            get { return phoneHMM; }
        }

        private readonly PhoneticHmm phoneHMM = new PhoneticHmm();

        /// <summary>
        /// Proccessor for action Markov State Machine
        /// </summary>
        // emission and transition stored as double hash tables
        public actMSM pMSM
        {
            get { return botMSM; }
        }

        private readonly actMSM botMSM = new actMSM();
        //public string lastDefMSM;
        //public string lastDefState;

        public Stack<string> conversationStack = new Stack<string>();
        public Hashtable wordAttributeHash = new Hashtable();

        public WordNetEngine wordNetEngine;
        // = new WordNetEngine(HostSystem.Combine(Environment.CurrentDirectory, this.GlobalSettings.grabSetting("wordnetdirectory")), true);

        //public string indexDir = @"C:\dev\Lucene\";
        public string fieldName = "TEXT_MATTER";
        public MyLuceneIndexer LuceneIndexer;


        #region Conversation methods

        private Result GlobalChatWithUser(string input, string user, OutputDelegate traceConsole, bool saveResults)
        {
            traceConsole = traceConsole ?? writeDebugLine;
            User CurrentUser = LastUser;
            pMSM.clearEvidence();
            pMSM.clearNextStateValues();
            //  myUser.TopicSetting = "collectevidencepatterns";
            RequestImpl r = GetRequest(input, user);
            r.IsTraced = true;
            r.writeToLog = traceConsole;
            Result res = Chat0(r, r.Graph);
            string useOut = null;
            //string useOut = resOutput;
            if (!res.IsEmpty)
            {
                useOut = res.Output;
                CurrentUser = res.user;
                string oTest = ToEnglish(useOut);
                if (oTest != null && oTest.Length > 2)
                {
                    useOut = oTest;
                }
            }
            if (string.IsNullOrEmpty(useOut))
            {
                useOut = "Interesting.";
                res.TemplateRating = Math.Max(res.Score, 0.5d);
            }
            if (saveResults)
            {
                try
                {
                    LastResult = res;
                    LastUser = CurrentUser;
                }
                catch (Exception exception)
                {
                    traceConsole("" + exception);
                }
            }
            traceConsole(useOut);
            return res;
        }

        public RequestImpl GetRequest(string rawInput, string username)
        {
            User findOrCreateUser = FindOrCreateUser(username);
            AIMLbot.Request r = new AIMLbot.Request(rawInput, findOrCreateUser, this, null);
            findOrCreateUser.CurrentRequest = r;
            r.IsTraced = true;
            return r;
        }

        /// <summary> 
        /// Given some raw input string username/unique ID creates a response for the user
        /// </summary>
        /// <param name="rawInput">the raw input</param>
        /// <param name="UserGUID">a usersname</param>
        /// <returns>the result to be output to the user</returns>        
        public string ChatString(string rawInput, string username)
        {
            RequestImpl r = GetRequest(rawInput, username);
            r.IsTraced = true;
            return Chat(r).Output;
        }

        /// <summary>
        /// Given some raw input and a unique ID creates a response for a new user
        /// </summary>
        /// <param name="rawInput">the raw input</param>
        /// <param name="UserGUID">an ID for the new user (referenced in the result object)</param>
        /// <returns>the result to be output to the user</returns>
        public Result Chat(string rawInput, string UserGUID)
        {
            Request request = GetRequest(rawInput, UserGUID);
            request.IsTraced = true;
            return Chat(request);
        }
        /// <summary>
        /// Given a request containing user input, produces a result from the Proccessor
        /// </summary>
        /// <param name="request">the request from the user</param>
        /// <returns>the result to be output to the user</returns>
        /// 
        public AIMLbot.Result Chat(Request request)
        {
            try
            {
                return Chat(request, request.Graph ?? GraphMaster);
            }
            finally
            {
                AddHeardPreds(request.rawInput, HeardPredicates);
            }
        }

        public AIMLbot.Result Chat(Request request, GraphMaster G)
        {
            GraphMaster prev = request.Graph;
            request.Graph = G;
            try
            {
                AIMLbot.Result v = Chat0(request, G);
                return v;
            }
            finally
            {
                request.Graph = prev;
            }
        }

        public AIMLbot.Result Chat0(Request request, GraphMaster G)
        {
            bool isTraced = request.IsTraced || G == null;
            User user = request.Requester ?? LastUser;
            UndoStack undoStack = UndoStack.GetStackFor(request);
            AIMLbot.Result res;
            undoStack.pushValues(user.Predicates, "rawinput", request.rawInput);
            undoStack.pushValues(user.Predicates, "input", request.rawInput);
            lock (user.QueryLock)
            {
                res = ChatWithUser(request, user, G);
                // ReSharper disable ConditionIsAlwaysTrueOrFalse
                if (res.OutputSentenceCount == 0)
                // ReSharper restore ConditionIsAlwaysTrueOrFalse
                {
                    request.IncreaseLimits(1);
                    res = ChatWithUser(request, user, G);
                }
            }
            undoStack.UndoAll();
            return res;
        }

        public AIMLbot.Result ChatWithUser(Request request, User user, GraphMaster G)
        {
            var originalRequestor = request.Requester;
            try
            {
                request.Requester = user;
                Result result = ChatWithRequest(request, G);

                // populate the Result object
                PopulateUserWithResult(originalRequestor, request, result);
                return (AIMLbot.Result) result;
            }
            finally
            {
                request.Requester = originalRequestor;
            }
        }

        public AIMLbot.Result ChatWithRequest(Request request, GraphMaster G)
        {
            User originalRequestor = request.Requester;
            //LastUser = user;            
            AIMLbot.Result result;
            OutputDelegate writeToLog = request.writeToLog ?? DEVNULL;

            bool isTraced = request.IsTraced || G == null;
            //chatTrace = null;

            streamDepth++;

            string rawInputString = request.rawInput.AsString();

            if (rawInputString.StartsWith("@"))
            {
                result = request.CreateResult(request);
                if (chatTrace) result.IsTraced = isTraced;
                bool myBotBotDirective = BotDirective(request, rawInputString, result.WriteLine);
                if (myBotBotDirective)
                {
                    return result;
                }
            }

            var orig = request.BotOutputs;
            if (AlwaysUseImmediateAimlInImput && ContainsAiml(rawInputString))
            {
                try
                {
                    result = ImmediateAiml(getNode(rawInputString), request, Loader, null);
                    request.rawInput = result.Output;
                }
                catch (Exception e)
                {
                    isTraced = true;
                    this.writeToLog(e);
                    writeToLog("ImmediateAiml: ERROR: " + e);
                }
            }

            if (request.GraphsAcceptingUserInput)
            {
                // Normalize the input
                AIMLLoader loader = Loader;
                if (!StaticLoader || loader == null)
                {
                    loader = new AIMLLoader(this, request);
                }
                Loader = loader;
                //RTParser.Normalize.SplitIntoSentences splitter = new RTParser.Normalize.SplitIntoSentences(this);
                var rawSentences = new[] { request.rawInput }; //splitter.Transform(request.rawInput);
                result = request.CreateResult(request);
                if (chatTrace) result.IsTraced = isTraced;
                LoadInputPaths(request, loader, rawSentences, result);
                int NormalizedPathsCount = result.NormalizedPaths.Count;

                if (isTraced && NormalizedPathsCount != 1)
                {
                    foreach (Unifiable path in result.NormalizedPaths)
                    {
                        writeToLog("  i: " + path.LegacyPath);
                    }
                    writeToLog("NormalizedPaths.Count = " + NormalizedPathsCount);
                }

                G = G ?? GraphMaster;
                // grab the templates for the various sentences from the graphmaster
                foreach (Unifiable path in result.NormalizedPaths)
                {
                    QueryList ql = G.gatherQueriesFromGraph(path, request, MatchState.UserInput);
                    if (ql.TemplateCount > 0)
                    {
                        request.TopLevel = ql;
                        result.AddSubqueries(ql);
                    }
                }


                int sqc = result.SubQueries.Count;

                bool printedSQs = false;
                //todo pick and chose the queries
                // if (result.SubQueries.Count != 1)
                {
                    if (isTraced)
                    {
                        string s = "AIMLTRACE: SubQueries.Count = " + result.SubQueries.Count;
                        foreach (SubQuery path in result.SubQueries)
                        {
                            s += Environment.NewLine;
                            s += "  " + Unifiable.ToVMString(path.FullPath);
                        }
                        printedSQs = true;
                        writeToLog(s);
                        DLRConsole.SystemFlush();
                    }
                }

                // process the templates into appropriate output
                int solutions;
                bool hasMoreSolutions;
                ProccessTemplates(request, result, out solutions, out hasMoreSolutions);

                if (result.OutputSentenceCount == 0 && sqc > 0)
                {
                    isTraced = true;
                    //todo pick and chose the queries
                    // if (result.SubQueries.Count != 1)
                    {
                        if (!printedSQs)
                        {
                            string s = "AIMLTRACE! : OutputSenteceCount == 0 while sqc=" + result.SubQueries.Count;
                            foreach (SubQuery path in result.SubQueries)
                            {
                                s += Environment.NewLine;
                                s += "  " + Unifiable.ToVMString(path.FullPath);
                            }
                            writeToLog(s);
                            DLRConsole.SystemFlush();
                        }
                    }
                    request.IncreaseLimits(1);
                    ProccessTemplates(request, result, out solutions, out hasMoreSolutions);
                    if (result.OutputSentenceCount == 0)
                    {
                        request.IncreaseLimits(1);
                        writeToLog("AIMLTRACE: bumping up limits 2 times " + request);
                        ProccessTemplates(request, result, out solutions, out hasMoreSolutions);
                    }
                }

                if (isTraced || result.OutputSentenceCount != 1)
                {
                    if (isTraced)
                    {
                        DLRConsole.SystemFlush();
                        string s = "AIMLTRACE: result.OutputSentenceCount = " + result.OutputSentenceCount;
                        foreach (string path in result.OutputSentences)
                        {
                            s += Environment.NewLine;
                            s += "  " + Unifiable.ToVMString(path);
                        }
                        s += Environment.NewLine;
                        writeToLog(s);
                        DLRConsole.SystemFlush();
                    }

                    foreach (SubQuery path in result.SubQueries)
                    {
                        //string s = "AIMLTRACE QUERY:  " + path.FullPath;

                        //writeToLog("\r\n tt: " + path.Request.Graph);
                        if (chatTrace)
                        {
                            //bot.writeChatTrace("\"L{0}\" -> \"{1}\" ;\n", result.SubQueries.Count, path.FullPath.ToString());
                            writeChatTrace("\"L{0}\" -> \"L{1}\" ;\n", result.SubQueries.Count - 1,
                                           result.SubQueries.Count);
                            writeChatTrace("\"L{0}\" -> \"REQ:{1}\" ;\n", result.SubQueries.Count, request.ToString());
                            writeChatTrace("\"REQ:{0}\" -> \"PATH:{1}\" [label=\"{2}\"];\n", request.ToString(),
                                           result.SubQueries.Count, path.FullPath);
                            writeChatTrace("\"REQ:{0}\" -> \"RPY:{1}\" ;\n", request.ToString(), result.RawOutput);
                        }
                    }
                }
            }
            else
            {
                string nai = NotAcceptingUserInputMessage;
                if (isTraced)
                    this.writeToLog("ERROR {0} getting back {1}", request, nai);
                result = request.CreateResult(request);
                request.AddOutputSentences(null, nai, result);
                //result.IsComplete = true;
                //return result;
            }
            User popu = originalRequestor ?? request.Requester ?? result.user;
            result.Duration = DateTime.Now - request.StartedOn;
            result.IsComplete = true;
            popu.addResultTemplates(request);
            streamDepth--;
            return result;
        }
        internal void PopulateUserWithResult(User user, Request request, Result result)
        {
            User popu = user ?? request.Requester ?? result.user;
            // only the toplevle query popuklates the user object
            if (result.ParentResult == null)
            {
                // toplevel result
                popu.addResult(result);
                if (RotateUsedTemplate)
                {
                    result.RotateUsedTemplates();
                }
            }
        }

        private void LoadInputPaths(Request request, AIMLLoader loader, Unifiable[] rawSentences, AIMLbot.Result result)
        {
            int maxInputs = request.MaxInputs;
            int numInputs = 0;
            int sentenceNum = 0;
            int topicNum = 0;
            int thatNum = 0;
            string lastInput = "";
            {
                foreach (Unifiable sentence0 in rawSentences)
                {
                    string sentence = sentence0;
                    result.InputSentences.Add(sentence);
                    sentence = sentence.Trim();
                    while (sentence.EndsWith(".") || sentence.EndsWith(","))
                    {
                        sentence = sentence.Substring(0, sentence.Length - 1).Trim();
                    }
                    if (sentence.Length == 0)
                    {
                        writeToLog("skipping input sentence " + sentence0);
                        continue;
                    }
                    sentenceNum++;
                    topicNum = 0;
                    if (maxInputs == 1)
                    {
                        Unifiable requestThat = request.That;
                        requestThat.AsString();
                        Unifiable path = loader.generatePath(sentence,
                            //thatNum + " " +
                                                             requestThat, request.Flags,
                            //topicNum + " " +
                                                             request.Requester.TopicSetting, true);
                        if (path.IsEmpty)
                        {
                            path = loader.generatePath(sentence,
                                //thatNum + " " +
                                                       requestThat, request.Flags,
                                //topicNum + " " +
                                                       request.Requester.TopicSetting, false);
                        }
                        if (path.IsEmpty) continue;
                        numInputs++;
                        result.NormalizedPaths.Add(path);
                        if (numInputs >= maxInputs) return;
                        continue;
                    }
                    foreach (Unifiable topic0 in request.Topics)
                    {
                        Unifiable topic = topic0;
                        topicNum++;
                        if (topic.IsLongWildCard())
                        {
                            topic = NOTOPIC;
                        }
                        thatNum = 0;
                        foreach (Unifiable that in request.BotOutputs)
                        {
                            thatNum++;
                            string thats = that.AsString();
                            Unifiable path = loader.generatePath(sentence, //thatNum + " " +
                                                                 thats, request.Flags,
                                //topicNum + " " +
                                                                 topic, true);
                            if (that.IsLongWildCard())
                            {
                                if (thatNum > 1)
                                {
                                    continue;
                                }
                                if (topic.IsLongWildCard())
                                {
                                    topic = "NOTHAT";
                                }
                            }
                            string thisInput = path.LegacyPath.AsString().Trim().ToUpper();
                            if (thisInput == lastInput) continue;

                            lastInput = thisInput;
                            numInputs++;
                            result.NormalizedPaths.Add(path);
                            if (numInputs >= maxInputs) return;
                        }
                    }
                }
            }
        }

        private string swapPerson(string inputString)
        {
            if (Loader == null)
            {
                Loader = new AIMLLoader(this, GetBotRequest("swapPerson " + inputString));
            }
            string temp = Loader.Normalize(inputString, true);
            //temp = ApplySubstitutions.Substitute(this, this.PersonSubstitutions, temp);
            temp = ApplySubstitutions.Substitute(Person2Substitutions, temp);
            return temp;
        }

        public Unifiable CleanupCyc(string text)
        {
            if (text == null) return null;
            if (text == "")
            {
                return "";
            }
            text = text.Trim();
            if (text == "")
            {
                writeToLog(" had white string ");
                return "";
            }
            if (TheCyc != null)
            {
                text = TheCyc.CleanupCyc(text);
            }
            return text.Replace("#$", " ").Replace("  ", " ").Trim();
        }


        private void ProccessTemplates(Request request, Result result, out int solutions, out bool hasMoreSolutions)
        {
            hasMoreSolutions = true;
            solutions = 0;
            foreach (SubQuery query in result.SubQueries)
            {
                result._CurrentQuery = query;
                var queryTemplates = query.Templates;
                if (queryTemplates != null && queryTemplates.Count > 0)
                {
                    foreach (TemplateInfo s in queryTemplates)
                    {
                        // Start each the same
                        s.Rating = 1.0;
                        try
                        {
                            s.Query = query;
                            query.CurrentTemplate = s;
                            bool createdOutput;
                            bool templateSucceeded;
                            XmlNode sOutput = s.ClonedOutput;
                            proccessResponse(query, request, result, sOutput, s.Guard, out createdOutput,
                                             out templateSucceeded, null, s, false, false);


                            if (createdOutput)
                            {
                                solutions++;
                            }
                            if (request.IsComplete(result))
                            {
                                hasMoreSolutions = false;
                                return;
                            }
                            //break; // KHC: single vs. Multiple
                            if ((createdOutput) && (((QuerySettingsReadOnly)request).ProcessMultipleTemplates == false))
                                break;
                        }
                        catch (Exception e)
                        {
                            writeToLog(e);
                            if (WillCallHome)
                            {
                                phoneHome(e.Message, request);
                            }
                            writeToLog("WARNING! A problem was encountered when trying to process the input: " +
                                       request.rawInput + " with the template: \"" + s + "\"");
                        }
                    }
                }
            }
        }


        public void writeChatTrace(string message, params object[] args)
        {
            if (!chatTrace) return;
            try
            {
                lock (chatTraceO)
                {
                    if (chatTraceW == null)
                    {
                        chatTraceW = new StreamWriter("bgm\\chatTrace.dot");
                        chatTraceW.WriteLine("digraph G {");
                        streamDepth = 1;
                    }
                    if (streamDepth < 0) streamDepth = 0;
                    int w = streamDepth;
                    while (w-- < 0)
                    {
                        chatTraceW.Write("  ");
                    }
                    if (args != null && args.Length != 0) message = String.Format(message, args);
                    chatTraceW.WriteLine(message);
                    //writeDebugLine(message);
                    if (streamDepth <= 0 && chatTraceW != null)
                    {
                        chatTraceW.WriteLine("}");
                        chatTraceW.Close();
                        streamDepth = 0;
                        chatTraceW = null;
                    }
                }
            }
            catch (Exception)
            {
            }
        }

        public AIMLbot.Result ImmediateAiml(XmlNode templateNode, Request request0,
                                            AIMLLoader loader, AIMLTagHandler handler)
        {
            bool prev = request0.GraphsAcceptingUserInput;
            try
            {
                request0.GraphsAcceptingUserInput = true;
                return ImmediateAIML0(request0, templateNode, handler);
            }
            finally
            {
                request0.GraphsAcceptingUserInput = prev;
            }
        }

        private AIMLbot.Result ImmediateAIML0(Request parentRequest, XmlNode templateNode, AIMLTagHandler handler)
        {
            string requestName = "<underline>" + templateNode.OuterXml + "</underline>";
            RTPBot request0Proccessor = this;
            GuardInfo sGuard = null;
            Request request = null;
            User user = BotAsUser;

            if (parentRequest != null)
            {
                user = parentRequest.Requester;
                requestName = parentRequest.rawInput + " " + requestName;
            }

            //  if (request == null)

            request = parentRequest;
            // new AIMLbot.Request(requestName, user, request0Proccessor, (AIMLbot.Request)parentRequest);

            if (parentRequest != null)
            {
                request.Graph = parentRequest.Graph;
                request.depth = parentRequest.depth + 1;
            }

            AIMLbot.Result result = request.CreateResult(request);
            request.CurrentResult = result;
            if (request.CurrentResult != result)
            {
                writeToLog("ERROR did not set the result!");
            }
            if (request.Graph != result.Graph)
            {
                writeToLog("ERROR did not set the result!");
            }
            TemplateInfo templateInfo = null; //
            if (false)
            {
                templateInfo = new TemplateInfo(templateNode, null, null, null);
            }
            bool templateSucceeded;
            bool createdOutput;
            SubQuery query = request.CurrentQuery;
            bool doUndos = false;
            bool copyChild = false;
            if (query == null)
            {
                query = new SubQuery(requestName, result, request);
                request.IsTraced = true;
                doUndos = true;
                copyChild = true;
            }
            if (copyChild)
            {
                if (templateInfo != null)
                    templateNode = templateInfo.ClonedOutput;
                copyChild = false;
            }
            proccessResponse(query, request, result, templateNode, sGuard, out createdOutput, out templateSucceeded,
                             handler, templateInfo, copyChild, false); //not sure if should copy parent
            if (doUndos) query.UndoAll();
            return result;
        }

        public IXmlLineInfo proccessResponse(SubQuery query,
                                             Request request, Result result,
                                             XmlNode templateNode, GuardInfo sGuard,
                                             out bool createdOutput, out bool templateSucceeded,
                                             AIMLTagHandler handler, TemplateInfo templateInfo,
                                             bool copyChild, bool copyParent)
        {
            request.CurrentResult = result;
            query = query ?? request.CurrentQuery;
            templateInfo = templateInfo ?? query.CurrentTemplate;
            //request.CurrentQuery = query;
            if (!request.CanUseTemplate(templateInfo, result))
            {
                templateSucceeded = false;
                createdOutput = false;
                return null;
            }
            UndoStack undoStack = UndoStack.GetStackFor(query);
            try
            {
                return proccessResponse000(query, request, result, templateNode, sGuard,
                                           out createdOutput, out templateSucceeded,
                                           handler, templateInfo, copyChild, copyParent);
            }
            finally
            {
                undoStack.UndoAll();
            }
        }

        public IXmlLineInfo proccessResponse000(SubQuery query, Request request, Result result,
                                                XmlNode sOutput, GuardInfo sGuard,
                                                out bool createdOutput, out bool templateSucceeded,
                                                AIMLTagHandler handler, TemplateInfo templateInfo,
                                                bool copyChild, bool copyParent)
        {
            bool isTraced = request.IsTraced || result.IsTraced || !request.GraphsAcceptingUserInput;
            //XmlNode guardNode = AIMLTagHandler.getNode(s.Guard.InnerXml);
            bool usedGuard = sGuard != null && sGuard.Output != null;
            sOutput = sOutput ?? templateInfo.ClonedOutput;
            string output = sOutput.OuterXml;
            XmlNode templateNode = sOutput;
            bool childOriginal = true;
            if (usedGuard)
            {
                string guardStr = "<template>" + sGuard.Output.InnerXml + " GUARDBOM " + sOutput.OuterXml +
                                  "</template>";
                templateNode = getNode(guardStr, sOutput);
                childOriginal = false;
            }

            bool protectChild = copyChild || childOriginal;
            AIMLTagHandler tagHandler;
            string outputSentence = processNode(templateNode, query,
                                                request, result, request.Requester, handler,
                                                protectChild, copyParent, out tagHandler);

            templateSucceeded = !IsFalse(outputSentence);

            int f = outputSentence.IndexOf("GUARDBOM");
            if (f < 0)
            {
                string o = ToEnglish(outputSentence);
                if (IsOutputSentence(o))
                {
                    if (isTraced)
                    {
                        string aIMLLoaderParentTextAndSourceInfo = ParentTextAndSourceInfo(templateNode);
                        if (aIMLLoaderParentTextAndSourceInfo.Length > 300)
                        {
                            aIMLLoaderParentTextAndSourceInfo = TextFilter.ClipString(
                                aIMLLoaderParentTextAndSourceInfo, 300);
                        }
                        writeToLog("AIMLTRACE '{0}' IsOutputSentence={1}", o, aIMLLoaderParentTextAndSourceInfo);
                    }
                    createdOutput = true;
                    templateSucceeded = true;
                    request.AddOutputSentences(templateInfo, o, result);
                }
                else
                {
                    createdOutput = false;
                }
                if (!createdOutput && isTraced && request.GraphsAcceptingUserInput)
                    writeToLog("UNUSED '{0}' TEMPLATE={1}", o, ParentTextAndSourceInfo(templateNode));
                return tagHandler;
            }

            try
            {
                string left = outputSentence.Substring(0, f).Trim();
                templateSucceeded = !IsFalse(left);
                if (!templateSucceeded)
                {
                    createdOutput = false;
                    return tagHandler;
                }
                string lang = GetAttribValue(sGuard.Output, "lang", "cycl").ToLower();

                try
                {
                    Unifiable ss = SystemExecute(left, lang, request);
                    if (IsFalse(ss) || IsNullOrEmpty(ss))
                    {
                        if (isTraced)
                            writeToLog("GUARD FALSE '{0}' TEMPLATE={1}", request,
                                       ParentTextAndSourceInfo(templateNode));
                        templateSucceeded = false;
                        createdOutput = false;
                        return tagHandler;
                    }
                    else
                    {
                        templateSucceeded = true;
                    }
                }
                catch (Exception e)
                {
                    writeToLog(e);
                    templateSucceeded = false;
                    createdOutput = false;
                    return tagHandler;
                }

                //part the BOM
                outputSentence = outputSentence.Substring(f + 9);
                string o = ToEnglish(outputSentence);
                if (IsOutputSentence(o))
                {
                    if (isTraced)
                        writeToLog(query.Graph + ": GUARD SUCCESS '{0}' TEMPLATE={1}", o,
                                   ParentTextAndSourceInfo(templateNode));
                    templateSucceeded = true;
                    createdOutput = true;
                    request.AddOutputSentences(templateInfo, o, result);
                    return tagHandler;
                }
                else
                {
                    writeToLog("GUARD SKIP '{0}' TEMPLATE={1}", outputSentence,
                               ParentTextAndSourceInfo(templateNode));
                }
                templateSucceeded = false;
                createdOutput = false;
                return tagHandler;
            }
            catch (Exception ex)
            {
                writeToLog(ex);
                templateSucceeded = false;
                createdOutput = false;
                return tagHandler;
            }
        }

        private bool IsOutputSentence(string sentence)
        {
            if (sentence == null) return false;
            string o = ToEnglish(sentence);
            if (o == null) return false;
            return o.Length > 0;
        }

        public string ToHeard(string message)
        {
            if (message == null) return null;
            message = message.Trim();
            if (message == "") return "";
            if (message.Contains("<"))
            {
                string messageIn = message;
                message = ForInputTemplate(message);
                //if (messageIn != message) writeDebugLine("heardSelfSay - ForInputTemplate: " + messageIn + " -> " + message);
            }

            if (message == "") return "";
            if (message.Contains("<"))
            {
                string messageIn = message;
                message = ToEnglish(message);
                if (messageIn != message) writeDebugLine("heardSelfSay - ToEnglish: " + messageIn + " -> " + message);
            }

            if (message == "" || message.Contains("<"))
            {
                writeDebugLine("heardSelfSay - ERROR: heard ='" + message + "'");
                return message;
            }
            return message;
        }

        public string ToEnglish(string sentenceIn)
        {
            if (sentenceIn == null)
            {
                return null;
            }
            sentenceIn = ReTrimAndspace(sentenceIn);
            if (sentenceIn == "")
            {
                return "";
            }

            string sentence = VisibleRendering(getNode("<template>" + sentenceIn + "</template>").ChildNodes,
                                               PatternSideRendering);

            sentence = ReTrimAndspace(sentence);
            if (DifferentBesidesCase(sentenceIn, sentence))
            {
                writeToLog("VisibleRendering: " + sentenceIn + " -> " + sentence);
                sentenceIn = sentence;
            }

            if (sentence == "")
            {
                return "";
            }
            sentence = ApplySubstitutions.Substitute(InputSubstitutions, sentenceIn);
            sentence = ReTrimAndspace(sentence);
            if (DifferentBesidesCase(sentenceIn, sentence))
            {
                writeToLog("InputSubst: " + sentenceIn + " -> " + sentence);
                sentenceIn = sentence;
            }

            sentence = CleanupCyc(sentenceIn);
            sentence = ReTrimAndspace(sentence);
            if (DifferentBesidesCase(sentenceIn, sentence))
            {
                writeToLog("CleanupCyc: " + sentenceIn + " -> " + sentence);
                sentenceIn = sentence;
            }

            sentence = ApplySubstitutions.Substitute(OutputSubstitutions, sentenceIn);
            sentence = ReTrimAndspace(sentence);
            if (DifferentBesidesCase(sentenceIn, sentence))
            {
                writeToLog("OutputSubst: " + sentenceIn + " -> " + sentence);
                sentenceIn = sentence;
            }

            if (!checkEndsAsSentence(sentenceIn))
            {
                sentenceIn += ".";
            }

            return sentenceIn;
        }

        static bool DifferentBesidesCase(string sentenceIn, string sentence)
        {
            return sentence.ToLower() != sentenceIn.ToLower();
        }

        static string ReTrimAndspace(string substitute)
        {
            if (substitute == null) return null;
            var csubstitute = substitute.ToCharArray();
            return substitute.Replace("  ", " ").Replace("  ", " ").Replace("  ", " ").Trim();
        }

        /// Checks that the provided sentence ends with a sentence splitter
        /// </summary>
        /// <param name="sentence">the sentence to check</param>
        /// <returns>True if ends with an appropriate sentence splitter</returns>
        public bool checkEndsAsSentence(string sentence)
        {
            sentence = sentence.Trim();
            if (sentence.EndsWith("?")) return true;
            foreach (Unifiable splitter in Splitters)
            {
                if (sentence.EndsWith(splitter))
                {
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// Recursively evaluates the template nodes returned from the Proccessor
        /// </summary>
        /// <param name="node">the node to evaluate</param>
        /// <param name="query">the query that produced this node</param>
        /// <param name="request">the request from the user</param>
        /// <param name="result">the result to be sent to the user</param>
        /// <param name="user">the user who originated the request</param>
        /// <returns>the output Unifiable</returns>
        public string processNode(XmlNode node, SubQuery query,
                                  Request request, Result result, User user,
                                  AIMLTagHandler parent, bool protectChild, bool copyParent,
                                  out AIMLTagHandler tagHandler)
        {
            // check for timeout (to avoid infinite loops)
            if (request != null && DateTime.Now > request.TimesOutAt)
            {
                request.writeToLog(
                    "WARNING! Request timeout. User: {0} raw input: \"{1}\" processing template: \"{2}\"",
                    request.Requester.UserID, request.rawInput,
                    (query == null ? "-NOQUERY-" : query.Templates.Count.ToString()));
                request.hasTimedOut = true;
                tagHandler = null;
                return Unifiable.Empty;
            }


            XmlNode oldNode = node;
            // copy the node!?!
            if (protectChild)
            {
                copyParent = true;
                LineInfoElementImpl newnode = CopyNode(node, copyParent);
                newnode.ReadOnly = false;
                node = newnode;
            }

            // process the node
            tagHandler = GetTagHandler(user, query, request, result, node, parent);
            if (ReferenceEquals(null, tagHandler))
            {
                if (node.NodeType == XmlNodeType.Comment) return Unifiable.Empty;
                if (node.NodeType == XmlNodeType.Text)
                {
                    string s = node.InnerText.Trim();
                    if (String.IsNullOrEmpty(s))
                    {
                        return Unifiable.Empty;
                    }
                    return s;
                }
                EvalAiml(node, request, request.WriteLine);
                return node.InnerXml;
            }

            tagHandler.SetParent(parent);
            Unifiable cp = tagHandler.CompleteAimlProcess();
            return cp;
        }
        #endregion

        private void SetupConveration()
        {
            HeardSelfSayQueue.Start();
            AddBotCommand("do16", () =>
                                      {
                                          Enqueue(() => Sleep16Seconds(10));
                                          Enqueue(() => Sleep16Seconds(1));
                                          Enqueue(() => Sleep16Seconds(30));
                                          Enqueue(() => Sleep16Seconds(5));
                                          Enqueue(() => Sleep16Seconds(5));
                                          Enqueue(() => Sleep16Seconds(45));
                                      });

            string names_str = "markovx.trn 5ngram.ngm";
            var nameset = names_str.Split(' ');
            foreach (string name in nameset)
            {
                int loadcount = 0;
                string file = HostSystem.Combine("trn", name);
                if (HostSystem.FileExists(file))
                {
                    StreamReader sr = new StreamReader(file);
                    writeToLog(" **** Markovian Brain LoadMarkovLTM: '{0}'****", file);
                    MBrain.Learn(sr);
                    sr.Close();
                    writeToLog(" **** Markovian Brain initialized.: '{0}' **** ", file);
                    loadcount++;
                }

                file = HostSystem.Combine("ngm", name);
                if (HostSystem.FileExists(file))
                {
                    StreamReader sr = new StreamReader(file);
                    writeToLog(" **** Markovian Brain LoadMarkovLTM: '{0}'**** ", file);
                    MBrain.LearnNgram(sr);
                    sr.Close();
                    writeToLog(" **** Markovian Brain N-Gram initialized '{0}'. **** ", file);
                    loadcount++;
                }

                if (loadcount == 0)
                {
                    writeToLog(
                        " **** WARNING: No Markovian Brain Training nor N-Gram file found for '{0}' . **** ", name);
                }
            }

            if (pHMM.hmmCorpusLoaded == 0)
            {
                string file = HostSystem.Combine("bgm", "corpus.txt");
                //if (HostSystem.DirExists(file))
                if (HostSystem.FileExists(file))
                {
                    writeToLog("Load Corpus Bigrams: '{0}'", file);
                    StreamReader sr = new StreamReader(file);
                    pHMM.LearnBigramFile(sr);
                    sr.Close();
                    pHMM.hmmCorpusLoaded++;
                    writeToLog("Loaded Corpus Bigrams: '{0}'", file);
                }
            }
            Console.WriteLine("*** Start WN-Load ***");
            wordNetEngine = new WordNetEngine(PathToWordNet, true);
            Console.WriteLine("*** DONE WN-Load ***");

            Console.WriteLine("*** Start Lucene ***");
            LuceneIndexer = new MyLuceneIndexer(PathToLucene, fieldName);
            LuceneIndexer.TheBot = this;
            LuceneIndexer.wordNetEngine = wordNetEngine;
            Console.WriteLine("*** DONE Lucene ***");
        }

        private int napNum = 0;

        private void Sleep16Seconds(int secs)
        {
            napNum++;
            DateTime start = DateTime.Now;
            var errOutput = DLRConsole.SYSTEM_ERR_WRITELINE;
            string thisTime = " #" + napNum;
            try
            {
                errOutput("START Sleep" + secs + "Seconds " + thisTime + " \ntime=" + DateTime.Now.Subtract(start).TotalMilliseconds);
                Thread.Sleep(TimeSpan.FromSeconds(secs));
                errOutput("COMPLETE Sleep" + secs + "Seconds " + thisTime + " \ntime=" + DateTime.Now.Subtract(start).TotalSeconds);
                Enqueue("ENQUE Sleep" + secs + "Seconds #" + thisTime, () => Sleep16Seconds(secs));

            }
            catch (ThreadAbortException e)
            {
                errOutput("ThreadAbortException Sleep" + secs + "Seconds " + e + " " + thisTime + " \ntime=" + DateTime.Now.Subtract(start).TotalSeconds);
            }
            catch (ThreadInterruptedException e)
            {
                errOutput("ThreadInterruptedException Sleep" + secs + "Seconds " + e + " " + thisTime + " \ntime=" + DateTime.Now.Subtract(start).TotalSeconds);
            }
            catch (Exception e)
            {
                errOutput("Exception Sleep" + secs + "Seconds " + e + " " + thisTime + " \ntime=" + DateTime.Now.Subtract(start).TotalSeconds);
                throw;
            }
            finally
            {
                errOutput("Finanaly Sleep" + secs + "Seconds #" + thisTime + " \ntime=" + DateTime.Now.Subtract(start).TotalSeconds);
            }
        }


        internal void Enqueue(ThreadStart action)
        {
            HeardSelfSayQueue.Enqueue(HeardSelfSayQueue.NamedTask("Enqueue_" + napNum, action));
        }
        internal void Enqueue(string named, ThreadStart action)
        {
            HeardSelfSayQueue.Enqueue(HeardSelfSayQueue.NamedTask(named, action));
        }
    }
}