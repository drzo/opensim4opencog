using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Threading;
using System.Xml;
using AIMLbot;
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
using PatternInfo = RTParser.Unifiable;
using ThatInfo = RTParser.Unifiable;
using TopicInfo = RTParser.Unifiable;
using GuardInfo = RTParser.Unifiable;
using ResponseInfo = RTParser.Unifiable;

namespace RTParser
{
    interface IChatterBot
    {
        SystemExecHandler ChatWithHandler(string userName);
    }

    public partial class RTPBot
    {
        public static bool BE_COMPLETE_NOT_FAST = false;
        public static int SraiDepthMax = 10;
        public bool AlwaysUseImmediateAimlInImput = true;
        public static bool RotateUsedTemplate = true;
        public bool DontUseSplitters = true;
        /*
        public readonly TaskQueueHandler HeardSelfSayQueue0 = new TaskQueueHandler("AIMLBot HeardSelf",
                                                                                   TimeSpan.FromMilliseconds(10),
                                                                                   TimeSpan.FromSeconds(10), true);*/
        public TaskQueueHandler HeardSelfSayQueue
        {
            get
            {
              //  if (BotAsUser == null) return HeardSelfSayQueue0;
                return BotAsUser.GetTaskQueueHandler("HeardSelfSay");
            }
        }
        private readonly Object chatTraceO = new object();
        public List<Thread> ThreadList { get { return HeardSelfSayQueue.InteruptableThreads; } }
        public bool chatTrace = true;
        private StreamWriter chatTraceW;
        private JoinedTextBuffer currentEar = new JoinedTextBuffer();
        private int streamDepth;
        //public Unifiable responderJustSaid;

        // last user talking to bot besides itself
        private User _lastUser;
        public User LastUser
        {
            get
            {
                if (BotAsUser != null)
                {
                    var LR = BotAsUser.LastResponder;
                    if (LR != null) return LR;
                }
                if (IsInteractiveUser(_lastUser)) return _lastUser;
                User LU = _lastResult != null ? _lastResult.Requester : null;
                if (IsInteractiveUser(LU)) return LU;
                return null;
            }
            set
            {
                if (BotAsUser != null)
                {
                    if (value == BotAsUser) return;
                    BotAsUser.LastResponder = value;
                }
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
                    if (IsInteractiveUser(_lastResult.Requester)) return _lastResult;
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
                LastUser = value.Requester;

                if (LR != null && LR.Requester != BotAsUser)
                {
                    if (value.Requester != LR.Requester)
                    {
                        _lastUser = value.Requester ?? _lastUser;
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
        private static GraphMaster TheUserListernerGraph;

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
        public IEnglishFactiodEngine LuceneIndexer;
        public ITripleStore TripleStore;


        #region Conversation methods

        public Result SaidUser(string input, string user, string[] otherName, OutputDelegate traceConsole, bool saveResults, bool saveResultsOnJustHeard)
        {
            return GlobalChatWithUser(input, user, otherName == null ? null : otherName[0], traceConsole, saveResults,
                                      saveResultsOnJustHeard);
        }

        private Result GlobalChatWithUser(string input, string user, string otherName, OutputDelegate traceConsole, bool saveResults, bool saveResultsOnJustHeard)
        {
            User targetUser = BotAsUser;
            string youser = input;
            int lastIndex = input.IndexOfAny("-,:".ToCharArray());
            User targ = null;
            if (lastIndex > 0)
            {
                youser = input.Substring(0, lastIndex);
                targ = FindUser(youser);
                if (targ != null) targetUser = targ;
            }
            if (otherName != null)
                if (targ == null)
                {
                    targ = FindUser(otherName);
                    if (targ != null) targetUser = targ;
                }

            traceConsole = traceConsole ?? writeDebugLine;
            User CurrentUser = null;
            if (user != null)
            {

                User lckleyUser = null;
                if (user == "null")
                {
                    CurrentUser = this.LastUser;
                }
                else
                {
                    CurrentUser = FindUser0(user);
                    CurrentUser = CurrentUser ?? FindOrCreateUser(user) ?? this.LastUser;
                }
            }
            else
            {
                CurrentUser = this.LastUser;
            }          
            var varMSM = this.pMSM;
            varMSM.clearEvidence();
            varMSM.clearNextStateValues();
            //  myUser.TopicSetting = "collectevidencepatterns";
            Result res = null;
            var request = CurrentUser.CreateRequest(input, targetUser);
            request.IsTraced = true;
            request.OriginalSalientRequest = request;
            request.SaveResultsOnJustHeard = saveResultsOnJustHeard;
            Result requestCurrentResult = request.FindOrCreateCurrentResult();
            ChatLabel label = request.PushScope;
            try
            {
                res = ChatWithRequest(request, requestCurrentResult);
            }
            catch (ChatSignal e)
            {
                if (label.IsSignal(e)) return e.result;
                throw;
            }
            catch (Exception exception)
            {
                traceConsole("" + exception);
            }
            finally
            {
                label.PopScope();
            }
            res = res ?? requestCurrentResult;
            string useOut = null;
            //string useOut = resOutput;
            if (!res.IsEmpty)
            {
                useOut = res.Output;
                CurrentUser = res.Requester;
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
                saveResults = false;
            }
            if (saveResults)
            {
                LastResult = res;
                this.LastUser = CurrentUser;
            }

            traceConsole(useOut);
            return res;
        }

        public MasterRequest MakeRequestToBot(Unifiable rawInput, string username)
        {
            return MakeRequestToBot(rawInput, FindOrCreateUser(username));
        }

        public MasterRequest MakeRequestToBot(Unifiable rawInput, User findOrCreateUser)
        {
            var rtarget = BotAsUser;
            AIMLbot.MasterRequest r = findOrCreateUser.CreateRequest(rawInput, rtarget);
            if (rtarget == null)
            {
                OnBotCreated(() => r.SetSpeakerAndResponder(findOrCreateUser, BotAsUser));
            }
            findOrCreateUser.CurrentRequest = r;
            r.depth = 0;
            r.IsTraced = findOrCreateUser.IsTraced;
            return r;
        }

        /// <summary> 
        /// Given some raw input string username/unique ID creates a response for the user
        /// </summary>
        /// <param name="rawInput">the raw input</param>
        /// <param name="UserGUID">a usersname</param>
        /// <returns>the result to be output to the user</returns>        
        public string ChatString(string rawInput, string UserGUID)
        {
            return Chat(rawInput, UserGUID).Output;
        }

        /// <summary>
        /// Given some raw input and a unique ID creates a response for a new user
        /// </summary>
        /// <param name="rawInput">the raw input</param>
        /// <param name="UserGUID">an ID for the new user (referenced in the result object)</param>
        /// <returns>the result to be output to the user</returns>
        public Result Chat(string rawInput, string UserGUID)
        {
            Request request = MakeRequestToBot(rawInput, UserGUID);
            request.IsTraced = this.IsTraced;
            return ChatWithRequest(request);
        }
        /// <summary>
        /// Given a request containing user input, produces a result from the Proccessor
        /// </summary>
        /// <param name="request">the request from the user</param>
        /// <returns>the result to be output to the user</returns>
        ///
        public Result ChatWithRequest(Request request)
        {
            Result requestCurrentResult = request.FindOrCreateCurrentResult();
            return ChatWithRequest(request, requestCurrentResult);
        }

        public Result ChatWithRequest(Request request, Result parentResultIn)
        {
            User target = request.Responder;
            User user = request.Requester;
            SettingsDictionary userPredicates = user.Predicates;
            bool isTraced = request.IsTraced | request.IsToplevelRequest;

            UndoStack undoStack = UndoStack.GetStackFor(request);
            Unifiable requestrawInput = request.rawInput;

            undoStack.pushValues(userPredicates, "i", user.UserName);
            undoStack.pushValues(userPredicates, "rawinput", requestrawInput);
            undoStack.pushValues(userPredicates, "input", requestrawInput);
            if (target != null && target.UserName != null)
            {
                undoStack.pushValues(userPredicates, "you", target.UserName);
            }
            //lock (user.QueryLock)
            {
                ChatLabel label = request.PushScope;
                try
                {
                    Result allResults = ChatWithToplevelResults(request, parentResultIn);
                    /*
                    // ReSharper disable ConditionIsAlwaysTrueOrFalse
                    if (res.OutputSentenceCount == 0 && false)
                    // ReSharper restore ConditionIsAlwaysTrueOrFalse
                    {
                        request.UndoAll();
                        request.IncreaseLimits(1);
                         res = ChatWithRequest4(request, user, target, G);
                    }
                     */
                    if (request.IsToplevelRequest)
                    {
                        AddSideEffectHook(request, user, parentResultIn);
                        user.JustSaid = requestrawInput;
                        if (target != null)
                        {
                            target.JustSaid = user.ResponderJustSaid; //.Output;
                        }
                    }
                    return allResults;
                }
                catch (ChatSignalOverBudget e)
                {
                    request.UndoAll();
                    writeToLog("ChatWithUser ChatSignalOverBudget: ( request.UndoAll() )" + request + " " + e);
                    return (Result) e.result;
                }
                catch (ChatSignal e)
                {
                    if (label.IsSignal(e)) return (Result) request.CurrentResult;
                    throw;
                }
                finally
                {
                    AddHeardPreds(parentResultIn.RawOutput, HeardPredicates);
                    request.CommitSideEffects(false);
                    label.PopScope();
                    undoStack.UndoAll();
                    request.UndoAll();
                    request.CommitSideEffects(true);
                    request.Exit();
                    request.SetSpeakerAndResponder(user, parentResultIn.Responder);
                }
            }
        }

        private void AddSideEffectHook(Request request, User originalRequestor, Result res)
        {
            request.AddSideEffect("Populate the Result object",
                                  () =>
                                  {
                                      PopulateUserWithResult(originalRequestor, request, res);
                                  });
        }

        public Result ChatWithToplevelResults(Request request, Result parentResult)
        {
            var originalRequestor = request.Requester;
            var originalTargetUser = request.Responder;
            ChatLabel label = request.PushScope;
            try
            {
                GraphMaster G = request.Graph;
                if (request.ParentMostRequest.DisallowedGraphs.Contains(G))
                {
                    writeToLog("ChatWithRequest4: DisallowedGraphs " + G);
                    return (Result) request.CurrentResult;
                }
                string why = request.WhyComplete;
                if (why != null)
                {
                    writeToLog("ChatWithRequest4: WhyComplete " + why);
                    return (Result) request.CurrentResult;
                }

                Result result = ChatFor1Result(request, parentResult);
                if (result.OutputSentences.Count != 0)
                {
                    result.RotateUsedTemplates();
                }
                return (Result)result;
            }
            catch (ChatSignalOverBudget e)
            {
                writeToLog("ChatWithRequest4 ChatSignalOverBudget: " + request + " " + e);
                return (Result)request.CurrentResult;
            }
            catch (ChatSignal e)
            {
                if (label.IsSignal(e)) return (Result)request.CurrentResult;
                throw;
            }
            finally
            {
                label.PopScope();
                request.SetSpeakerAndResponder(originalRequestor, originalTargetUser);
            }
        }

        public Result ChatFor1Result(Request request, Result parentResult)
        {
            //result = request.CreateResult(request);
            User originalRequestor = request.Requester;
            //LastUser = user; 
            Result childResult;
            GraphMaster G = request.Graph;
            bool isTraced = request.IsTraced || G == null;
            OutputDelegate writeToLog = this.writeToLog;
            string rr = request.rawInput;
            if (rr.StartsWith("@") || (rr.IndexOf("<") + 1 < rr.IndexOf(">")))
            {
                childResult = ChatWithNonGraphmaster(request, parentResult, G, isTraced, writeToLog);
            }
            else if (request.GraphsAcceptingUserInput)
            {
                childResult = ChatUsingGraphMaster(request, parentResult, G, isTraced, writeToLog);
            }
            else
            {
                childResult = request.CreateResult(request);
                string nai = NotAcceptingUserInputMessage;
                if (isTraced) this.writeToLog("ERROR {0} getting back {1}", request, nai);
                request.AddOutputSentences(null, nai, parentResult);
            }
            User popu = originalRequestor ?? request.Requester ?? parentResult.Requester;
            parentResult.IsComplete = true;
            childResult = childResult ?? parentResult;
            parentResult.SetOutput = childResult.RawOutput;
            popu.addRequestTemplates(request);
            if (streamDepth > 0) streamDepth--;
            return (Result) childResult;
        }

        private Result ChatWithNonGraphmaster(Request request, Result result, GraphMaster G, bool isTraced, OutputDelegate writeToLog)
        {
            writeToLog = writeToLog ?? DEVNULL;
            isTraced = request.IsTraced;
            //chatTrace = null;

            streamDepth++;

            string rawInputString = request.rawInput.AsString();

            if (rawInputString.StartsWith("@"))
            {
                result = request.CreateResult(request);
                if (chatTrace) result.IsTraced = isTraced;
                StringWriter sw = new StringWriter();

                bool myBotBotDirective = BotDirective(request, rawInputString, sw.WriteLine);
                string swToString = sw.ToString();
                if (writeToLog != null) writeToLog("ChatWithNonGraphmaster: " + swToString);
                else
                {
                    writeToLog = DEVNULL;
                }
                if (myBotBotDirective)
                {
                    Result requestCurrentResult = result ?? request.CurrentResult;
                    if (requestCurrentResult != null)
                    {
                        requestCurrentResult.SetOutput = swToString;
                    }
                    return result;
                }
                writeToLog("ERROR: cannot find command " + request.rawInput);
                return null;
            }
            ChatLabel label = request.PushScope;
            var orig = request.ResponderOutputs;
            if (AlwaysUseImmediateAimlInImput && ContainsAiml(rawInputString))
            {
                try
                {
                    result = ImmediateAiml(StaticAIMLUtils.getTemplateNode(rawInputString), request, Loader, null);
                    //request.rawInput = result.Output;
                    return result;
                }
                catch (ChatSignal e)
                {
                    if (label.IsSignal(e)) return e.result;
                    throw;
                }
                catch (Exception e)
                {
                    isTraced = true;
                    this.writeToLog(e);
                    writeToLog("ImmediateAiml: ERROR: " + e);
                    label.PopScope();
                    return null;
                }
            }
            return null;
        }

        private Result ChatUsingGraphMaster(Request request, Result result, GraphMaster G, bool isTraced, OutputDelegate writeToLog)
        {
            //writeToLog = writeToLog ?? DEVNULL;
            {
                ParsedSentences parsedSentences = ParsedSentences.GetParsedSentences(request, isTraced, writeToLog);

                bool printedSQs = false;
                G = G ?? GraphMaster;

                // grab the templates for the various sentences from the graphmaster
                request.IsTraced = isTraced;
                //result = request.CreateResult(request);

                // load the queries
                List<GraphQuery> AllQueries = new List<GraphQuery>();

                bool topleveRequest = request.IsToplevelRequest;

                int UNLIMITED = 10000;
                request.MaxOutputs = UNLIMITED;
                request.MaxPatterns = UNLIMITED;
                request.MaxTemplates = UNLIMITED;

                // Gathers the Pattern SubQueries!
                foreach (Unifiable userSentence in parsedSentences.NormalizedPaths)
                {
                    AllQueries.Add(G.gatherQueriesFromGraph(userSentence, request, MatchState.UserInput));
                }
                try
                {
                    // gather the templates and patterns
                    foreach (var ql in AllQueries)
                    {
                        if (topleveRequest)
                        {
                            ql.TheRequest.SuspendSearchLimits = true;
                            ql.NoMoreResults = false;
                            ql.MaxTemplates = UNLIMITED;
                            ql.MaxPatterns = UNLIMITED;
                            ql.MaxOutputs = UNLIMITED;
                        }
                        G.RunGraphQuery(ql);
                        //if (request.IsComplete(result)) return result;
                    }
                    foreach (var ql in AllQueries)
                    {
                        request.TopLevelQuery = ql;
                        if (chatTrace) result.IsTraced = isTraced;
                        if (ql.PatternCount > 0)
                        {
                            request.TopLevelQuery = ql;
                            // if (ql.TemplateCount > 0)
                            {
                                request.TopLevelQuery = ql;
                                result.AddSubqueries(ql);
                            }
                            var kept = ql.PreprocessSubQueries(request, result.SubQueries, isTraced, ref printedSQs,
                                                               writeToLog);
                        }
                        // give a 20 second blessing
                        if (false && result.SubQueries.Count > 0 && !request.SraiDepth.IsOverMax)
                        {
                            writeToLog("Extending time");
                            request.TimeOutFromNow = TimeSpan.FromSeconds(2);
                        }
                        //ProcessSubQueriesAndIncreasLimits(request, result, ref isTraced, printedSQs, writeToLog);
                    }
                    int solutions;
                    bool hasMoreSolutions;
                    CheckResult(request, result, out solutions, out hasMoreSolutions);
                }
                catch (ChatSignal exception)
                {
                    writeToLog("ChatSignalOverBudget: " + exception.Message);
                }
            }
            return result;
        }

        private void ProcessSubQueriesAndIncreasLimits(Request request, Result result, ref bool isTraced, bool printedSQs, OutputDelegate writeToLog)
        {
            writeToLog = writeToLog ?? DEVNULL;
            int sqc = result.SubQueries.Count;
            int solutions;
            bool hasMoreSolutions;
            CheckResult(request, result, out solutions, out hasMoreSolutions);
            return;
            if (result.OutputSentenceCount == 0 || sqc == 0)
            {
                return;
                //  string oldSets = QuerySettings.ToSettingsString(request.GetQuerySettings());
                isTraced = true;
                //todo pick and chose the queries
                int n = 0;
                while (result.OutputSentenceCount == 0 && n < 3 && request.depth < 3)
                {
                    n++;
                    request.UndoAll();
                    request.IncreaseLimits(1);
                    CheckResult(request, result, out solutions, out hasMoreSolutions);
                    if (result.OutputSentenceCount != 0 || sqc != 0)
                    {
                    }
                    // string newSets = QuerySettings.ToSettingsString(request.GetQuerySettings());
                    //writeToLog("AIMLTRACE: bumped up limits " + n + " times for " + request + "\n --from\n" + oldSets + "\n --to \n" +
                    //         newSets);
                }
            }
            // process the templates into appropriate output
            PostProcessSubqueries(request, result, isTraced, writeToLog);
        }
        private void PostProcessSubqueries(Request request, Result result, bool isTraced, OutputDelegate writeToLog)
        {
            writeToLog = writeToLog ?? DEVNULL;
            {
                if (isTraced)
                {
                    if (result.OutputSentenceCount != 1 && !request.Graph.UnTraced)
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
        }
        internal void PopulateUserWithResult(User user, Request request, Result result)
        {
            User popu = user ?? request.Requester ?? result.Requester;
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

        public string EnsureEnglish(string arg)
        {
            // ReSharper disable ConvertToConstant.Local
            bool DoOutputSubst = false;
            // ReSharper restore ConvertToConstant.Local
            // ReSharper disable ConditionIsAlwaysTrueOrFalse
            if (DoOutputSubst)
            // ReSharper restore ConditionIsAlwaysTrueOrFalse
            {
                string sentence = ApplySubstitutions.Substitute(OutputSubstitutions, arg);
                sentence = TextPatternUtils.ReTrimAndspace(sentence);
                if (TextPatternUtils.DifferentBesidesCase(arg, sentence))
                {
                    writeDebugLine("OutputSubst: " + arg + " -> " + sentence);
                    arg = sentence;
                }
            }
            return arg;

            return ToEnglish(arg);
        }

        public delegate void InputParser(Request request, IEnumerable<Unifiable> rawSentences);

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
            text = Trim(text);
            if (text == "")
            {
                writeToLog(" had white string ");
                return "";
            }
            if (TheCyc != null)
            {
                text = TheCyc.CleanupCyc(text);
            }
            return Trim(CleanNops(text.Replace("#$", " ").Replace("  ", " ")));
        }


        private void CheckResult(Request request, Result result, out int solutions, out bool hasMoreSolutions)
        {
            var isTraced = request.IsTraced || result.IsTraced;
            hasMoreSolutions = false;
            solutions = 0;
            // checks that we have nothing to do
            if (result == null || result.SubQueries == null)
            {
                if (isTraced)
                {
                    writeToLog("NO QUERIES FOR " + request);
                }
                return;
            }

            List<SubQuery> AllQueries = GetQueriesFromResults(request, result, isTraced);
            if (AllQueries == null || AllQueries.Count == 0)
            {
                if (isTraced)
                {
                    writeToLog("NO QUERIES FOR RESULTS " + request);
                }
                return;
            }
            List<SubQuery> sortMe = SortCandidateSolutions(AllQueries, isTraced);
            if (sortMe == null || sortMe.Count == 0)
            {
                //if (isTraced)
                {
                    writeToLog("NO SORTED QUERIES " + request);
                }
                return;
            }
            solutions = GetSolutions(request, result, sortMe, solutions, out hasMoreSolutions);
        }

        private List<SubQuery> GetQueriesFromResults(Request request, Result result, bool isTraced)
        {
            List<SubQuery> AllQueries = new List<SubQuery>();
            //int solutions;
            List<SubQuery> resultSubQueries = result.SubQueries;

            List<TemplateInfo> AllTemplates = new UList();
            bool found1 = false;
            lock (resultSubQueries)
            {
                foreach (SubQuery query in resultSubQueries)
                {
                    var queryTemplates = query.Templates;
                    if (queryTemplates == null || queryTemplates.Count == 0) continue;
                    AllQueries.Add(query);
                    if (!found1) found1 = true;
                    lock (queryTemplates)
                        AllTemplates.AddRange(query.Templates);
                }
            }

            if (!found1)
            {
                //solutions = 0;
                //hasMoreSolutions = false;
                result.TemplatesSucceeded = 0;
                result.OutputsCreated = 0;
                if (isTraced)
                {
                    writeToLog("NO TEMPLATES FOR " + request);
                }
                //return;
            }
            return AllQueries;
        }

        private List<SubQuery> SortCandidateSolutions( List<SubQuery> AllQueries, bool isTraced)
        {
            if (!BE_COMPLETE_NOT_FAST)
            {
                if (isTraced)
                {
                    PrintQueryList("-- ", AllQueries);
                } 
                return AllQueries;
            }
            List<SubQuery> sortMe = new List<SubQuery>(AllQueries);
            
            sortMe.Sort();

            bool countChanged = sortMe.Count != AllQueries.Count;
            for (int index = 0; index < sortMe.Count; index++)
            {
                var subQuery = sortMe[index];
                var allQ = AllQueries[index];
                if (subQuery.EqualsMeaning(allQ))
                {
                    continue;
                }
                countChanged = true;
            }
            if (isTraced || countChanged)
            {
                string cc = countChanged ? "QUERY SAME " : "QUERY SORT ";
                writeToLog("--------------------------------------------");
                if (countChanged)
                {

                    int sqNum = 0;
                    writeToLog("AllQueries.Count = " + sortMe.Count + " was " + AllQueries);
                    if (false) PrintQueryList("---BEFORE QUERY ", AllQueries);
                }
                if (isTraced)
                {
                    PrintQueryList("--- " + cc + " ", AllQueries);
                }
                writeToLog("--------------------------------------------");
            }
            return sortMe;
        }

        private void PrintQueryList(string cc, List<SubQuery> AllQueries)
        {
            int sqNum = 0;
            foreach (SubQuery query in AllQueries)
            {
                writeToLog(cc + sqNum + ": " + query.Pattern + " " + query.Pattern.Graph);
                sqNum++;
            }
        }

        private int GetSolutions(Request request, Result result, List<SubQuery> sortMe, int solutions, out bool hasMoreSolutions)
        {
            foreach (SubQuery query in sortMe)
            {
                if (result.IsComplete)
                {
                    hasMoreSolutions = false;
                    return solutions;
                }
                result.CurrentQuery = query;
                foreach (TemplateInfo s in query.Templates)
                {
                    hasMoreSolutions = true;
                    try
                    {
                        s.Query = query;
                        // Start each the same
                        var lastHandler = ProcessQueryTemplate(request, s.Query, s, result, request.LastHandler,
                                                               ref solutions,
                                                               out hasMoreSolutions);
                        if (result.IsComplete)
                        {
                            hasMoreSolutions = false;
                            return solutions;
                        }
                    }
                    catch (ChatSignal)
                    {
                        if (!request.IsToplevelRequest) throw;
                    }
                }
            }
            hasMoreSolutions = false;
            result.CurrentQuery = null;
            return solutions;
        }

        private AIMLTagHandler ProcessQueryTemplate(Request request, SubQuery query, TemplateInfo s, Result result, AIMLTagHandler lastHandler, ref int solutions, out bool hasMoreSolutions)
        {
            AIMLTagHandler childHandler = null;
            s.Rating = 1.0;
            hasMoreSolutions = false;
            try
            {
                request.CurrentQuery = query;
                s.Query = query;
                query.CurrentTemplate = s;
                bool createdOutput;
                bool templateSucceeded;
                XmlNode sOutput = s.ClonedOutput;
                childHandler = TagHandling.proccessResponse(query, request, result, sOutput, s.Guard, out createdOutput,
                                               out templateSucceeded, lastHandler, s, false, false);
                solutions++;
                query.CurrentTagHandler = childHandler;
                request.LastHandler = lastHandler;
                if (templateSucceeded)
                {
                    result.TemplatesSucceeded++;
                    s.OnTemplatesSucceeded(query, request);
                }
                else
                {
                    s.OnTemplatesFailed(query, request);
                }
                if (createdOutput)
                {
                    result.OutputsCreated++;
                    s.OnOutputsCreated(query, request);
                    hasMoreSolutions = true;
                    //break; // KHC: single vs. Multiple
                    if (((QuerySettingsReadOnly)request).ProcessMultipleTemplates == false)
                    {
                        if (request.IsComplete(result))
                        {
                            hasMoreSolutions = false;
                            return lastHandler;
                        }
                    }
                }
                return childHandler;
            }
            catch (ChatSignal e)
            {
                throw;
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
                hasMoreSolutions = false;
            }
            return childHandler;
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
                    message = SafeFormat(message, args);
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

        public Result ImmediateAiml(XmlNode templateNode, Request request0,
                                            AIMLLoader loader, AIMLTagHandler handler)
        {
            bool fastCall = handler == null;
            Result masterResult = request0.CreateResult(request0);
            bool prev = request0.GraphsAcceptingUserInput;
            try
            {
                request0.GraphsAcceptingUserInput = true;
                var mr = ImmediateAIML0(request0, templateNode, handler, fastCall);
                return mr;
            }
            catch (ChatSignal ex)
            {
                writeToLog(ex);
                return masterResult;
            }
            catch (Exception ex)
            {
                writeToLog(ex);
                return masterResult;
            }
            finally
            {
                request0.GraphsAcceptingUserInput = prev;
            }
        }

        private Result ImmediateAIML0(Request parentRequest, XmlNode templateNode, AIMLTagHandler handler, bool isFastAIML)
        {
            if (isFastAIML)
            {

            }

            string requestName = ToTemplateXML(templateNode);

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
                if (parentRequest != request) request.Graph = parentRequest.Graph;
                //request.depth = parentRequest.depth + 1;
            }

            Result result = request.CreateResult(request);
            //request.CurrentResult = result;
            if (request.CurrentResult != result)
            {
                writeToLog("ERROR did not set the result!");
            }
            if (request.Graph != result.Graph)
            {
                writeToLog("ERROR did not set the Graph!");
            }
            TemplateInfo templateInfo = null; //
            if (false)
            {
              //  templateInfo = new TemplateInfo(templateNode, null, null, null);
            }
            bool templateSucceeded;
            bool createdOutput;
            SubQuery query = request.CurrentQuery;
            bool doUndos = false;
            bool copyChild = templateNode.IsReadOnly;
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
            var lastHandler = TagHandling.proccessResponse(query, request, result, templateNode, sGuard, out createdOutput, out templateSucceeded,
                             handler, templateInfo, copyChild, false); //not sure if should copy parent
            if (doUndos) query.UndoAll();
            request.LastHandler = lastHandler;
            return result;
        }

        internal bool IsOutputSentence(string sentence, string outputSentence)
        {
            if (outputSentence != null)
            {
                if ((" " + outputSentence+" ").ToUpper().Contains(" BR "))
                {
                    return true;
                }
            }
            if (sentence == null) return false;
            string o = ToEnglish(sentence);
            if (o == null) return false;
            return o.Length > 0;
        }

        public string ToHeard(string message)
        {
            if (message == null) return null;
            if (Trim(message).Length == 0) return "";
            if (message.StartsWith("  "))
            {
                return message;
            }
            message = CleanNops(message);
            message = Trim(message);
            if (message == "") return "";
            if (false && message.Contains("<"))
            {
                string messageIn = message;
                message = ForInputTemplate(message);
                //if (messageIn != message) writeDebugLine("heardSelfSay - ForInputTemplate: " + messageIn + " -> " + message);
            }

            if (message == "") return "";
            //if (message.Contains("<"))
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
            return "  " + message;
        }

        public static string CleanNops(string message)
        {
            string testBR = (" " + message + " ");
            if (testBR.ToUpper().Contains(" BR "))
            {
                message =
                    ReTrimAndspace(ReplacePairs(message, " br ", " \n ", " BR ", " \n ", " NOP ", " ", " nop ", " "));
            }
            return message;
        }

        static char[] toCharArray = "@#$%^&*()_+<>,/{}[]\\\";'~~".ToCharArray();
        public string ToEnglish(string sentenceIn)
        {
            if (sentenceIn == null)
            {
                return null;
            }
            sentenceIn = CleanNops(sentenceIn);
 
            sentenceIn = ReTrimAndspace(sentenceIn);
            if (sentenceIn == "")
            {
                return "";
            }

            string sentence = VisibleRendering(StaticAIMLUtils.getTemplateNode(sentenceIn).ChildNodes,
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

            const bool DoInputSubsts = false;
            // ReSharper disable ConditionIsAlwaysTrueOrFalse
            if (DoInputSubsts)
            // ReSharper restore ConditionIsAlwaysTrueOrFalse
            {
                sentenceIn = ToInputSubsts(sentenceIn);
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

        public string ToInputSubsts(string sentenceIn)
        {
            string sentence;
            sentence = ApplySubstitutions.Substitute(InputSubstitutions, sentenceIn);
            //sentence = string.Join(" ", sentence.Split(toCharArray, StringSplitOptions.RemoveEmptyEntries));
            sentence = ReTrimAndspace(sentence);
            if (DifferentBesidesCase(sentenceIn, sentence))
            {
                writeToLog("InputSubst: " + sentenceIn + " -> " + sentence);
                sentenceIn = sentence;
            }
            return sentenceIn;
        }

        #endregion

        private void SetupConveration()
        {
            //HeardSelfSayQueue.Start();
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
            var myLuceneIndexer = new MyLuceneIndexer(PathToLucene, fieldName, this, wordNetEngine);
            this.LuceneIndexer = myLuceneIndexer;
            myLuceneIndexer.TheBot = this;
            TripleStore = myLuceneIndexer.TripleStoreProxy;
            myLuceneIndexer.wordNetEngine = wordNetEngine;
            Console.WriteLine("*** DONE Lucene ***");
        }

        private int napNum = 0;

        public static DateTime Now
        {
            get { return DateTime.Now; }
        }

        private void Sleep16Seconds(int secs)
        {
            napNum++;
            DateTime start = RTPBot.Now;
            var errOutput = DLRConsole.SYSTEM_ERR_WRITELINE;
            string thisTime = " #" + napNum;
            try
            {
                errOutput("START Sleep" + secs + "Seconds " + thisTime + " \ntime=" + RTPBot.Now.Subtract(start).TotalMilliseconds);
                Thread.Sleep(TimeSpan.FromSeconds(secs));
                errOutput("COMPLETE Sleep" + secs + "Seconds " + thisTime + " \ntime=" + RTPBot.Now.Subtract(start).TotalSeconds);
                Enqueue("ENQUE Sleep" + secs + "Seconds #" + thisTime, () => Sleep16Seconds(secs));

            }
            catch (ThreadAbortException e)
            {
                errOutput("ThreadAbortException Sleep" + secs + "Seconds " + e + " " + thisTime + " \ntime=" + RTPBot.Now.Subtract(start).TotalSeconds);
            }
            catch (ThreadInterruptedException e)
            {
                errOutput("ThreadInterruptedException Sleep" + secs + "Seconds " + e + " " + thisTime + " \ntime=" + RTPBot.Now.Subtract(start).TotalSeconds);
            }
            catch (Exception e)
            {
                errOutput("Exception Sleep" + secs + "Seconds " + e + " " + thisTime + " \ntime=" + RTPBot.Now.Subtract(start).TotalSeconds);
                throw;
            }
            finally
            {
                errOutput("Finanaly Sleep" + secs + "Seconds #" + thisTime + " \ntime=" + RTPBot.Now.Subtract(start).TotalSeconds);
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

        private object ChatWithThisBot(string cmd, Request request)
        {
            Request req = request.CreateSubRequest(cmd, request.Graph);
            req.SetSpeakerAndResponder(req.Requester, BotAsUser);
            req.IsToplevelRequest = request.IsToplevelRequest;
            return LightWeigthBotDirective(cmd, req);
        }
    }
}