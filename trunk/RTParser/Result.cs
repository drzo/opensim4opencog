using System;
using System.Collections.Generic;
using System.Text;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using RTParser.AIMLTagHandlers;
using RTParser.Normalize;
using RTParser.Utils;
using RTParser.Variables;
using UPath = RTParser.Unifiable;


namespace RTParser
{
    /// <summary>
    /// Encapsulates information about the result of a request to the bot
    /// </summary>
    public abstract class Result
    {
        public static int MaxPrintResults = 1;
        public SubQuery _CurrentQuery;
        private string AlreadyUsed = "xtxtxtxtxtxtxtxtxxt";

        /// <summary>
        /// The bot that is providing the answer
        /// </summary>
        public RTPBot TargetBot;

        /// The user that is providing the <that/> answer
        public User Responder;

        public void CollectRequest()
        {
            Request req = request;
            Responder = req.Responder;
            Requestor = req.Requester;
            //request = null;
        }

        /// <summary>
        /// The amount of time the request took to process
        /// </summary>
        public TimeSpan Duration = TimeSpan.MinValue;
        public string WhyComplete
        {
            get
            {
                lock (this)
                {
                    string s = null, t = null;
                    var graphQuery = this.TopLevel;
                    if (graphQuery != null) s = graphQuery.WhyComplete;
                    if (string.IsNullOrEmpty(s)) s = null;
                    var request1 = this.request;
                    if (request1 != null) t = request1.WhyComplete;
                    return s == null ? t : (s + " " + t);
                }
            }
        }
        public DateTime EndedOn = DateTime.MaxValue;

        private readonly ParsedSentences ChatInput;
        private readonly ParsedSentences ChatOutput;

        /// <summary>
        /// The individual sentences that constitute the raw input from the user
        /// </summary>
        public List<Unifiable> InputSentences
        {
            get { return ChatInput.InputSentences; }
        }

        /// <summary>
        /// The normalized sentence(s) (paths) fed into the graphmaster
        /// </summary>
        public List<Unifiable> NormalizedPaths
        {
            get { return ChatInput.NormalizedPaths; }
        }

        /// <summary>
        /// The individual sentences produced by the bot that form the complete response
        /// </summary>
        readonly public List<Unifiable> OutputSentences;

        public Result ParentResult;

        /// <summary>
        /// The request from the user
        /// </summary>
        public Request request;

        private bool Started = false;

        /// <summary>
        /// The subQueries processed by the bot's graphmaster that contain the templates that 
        /// are to be converted into the collection of Sentences
        /// </summary>
        public List<SubQuery> SubQueries = new List<SubQuery>();

        public TemplateInfo TemplateOfRating;
        public double TemplateRating = 0.0d;
        private readonly List<TemplateInfo> UsedTemplates1 = new List<TemplateInfo>();

        /// <summary>
        /// The user for whom this is a result
        /// </summary>
        public User Requestor;

        public OutputDelegate writeToLog = RTPBot.writeDebugLine;
        public int TemplatesSucceeded;
        public int OutputsCreated;

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="user">The user for whom this is a result</param>
        /// <param name="bot">The bot providing the result</param>
        /// <param name="request">The request that originated this result</param>
        public Result(User user, RTPBot bot, Request request, Result parent)
        {
            ChatInput = request.ChatInput;
            this.Requestor = user;
            this.TargetBot = bot;
            this.request = request;
            ParentResult = parent;
            ChatOutput = new ParsedSentences(bot.EnsureEnglish, MaxPrintResults);
            OutputSentences = ChatOutput.TemplateOutputs;
            if (ParentResult != null)
            {
                writeToLog = ParentResult.writeToLog;
            }
            else if (bot != null)
            {
                writeToLog = bot.writeToLog;
            }
            writeToLog = writeToLog ?? user.WriteLine;
            writeToLog = writeToLog ?? request.WriteLine;
            // this.request.CurrentResult = this;
        }

        public GraphMaster Graph
        {
            get { return request.Graph; }
            set { request.Graph = value; }
        }

        public GraphQuery TopLevel
        {
            get
            {
                Request request1 = request;
                if (request1 != null) return request1.TopLevel;
                SubQuery cc = CurrentQuery;
                if (cc != null) return cc.TopLevel;
                return null;
            }
            set { throw new NotImplementedException(); }
        }


        public double Score
        {
            get { return TemplateRating; }
        }

        /// <summary>
        /// If the query is being traced
        /// </summary>
        public virtual bool IsTraced { get; set; }

        public string SetOutput
        {
            set
            {
                lock (OutputSentences)
                {
                    AlreadyUsed = value;
                    OutputSentences.Clear();
                    OutputSentences.Add(value);
                    IsComplete = true;
                }
            }
        }

        /// <summary>
        /// The raw input from the user
        /// </summary>
        public Unifiable RawInput
        {
            get { return ChatInput.RawText; ; }
        }

        /// <summary>
        /// The result from the bot with logging and checking
        /// </summary>
        public Unifiable Output
        {
            get
            {
                lock (OutputSentences)
                    if (OutputSentences.Count > 0)
                    {
                        return RawOutput;
                    }
                    else
                    {
                        if (request.IsComplete(this))
                        {
                            writeToLog("ERROR: " + request.WhyComplete + " on " + RawInput +
                                       " from the user with an id: " + Requestor.UserID);
                            return Unifiable.Empty;
                            return TargetBot.TimeOutMessage;
                        }
                        else
                        {
                            var paths = Unifiable.CreateAppendable();
                            foreach (Unifiable pattern in NormalizedPaths)
                            {
                                //return pattern;
                                paths.Append(pattern.LegacyPath + Environment.NewLine);
                            }
                            writeToLog("The bot could not find any response for the input: " + RawInput +
                                       " with the path(s): " +
                                       Environment.NewLine + paths.ToString() + " from the user with an id: " +
                                       Requestor.UserID);
                            return Unifiable.Empty;
                        }
                    }
            }
        }

        public string EnglishSentences
        {
            get { return ChatOutput.RawText; }
        }

        /// <summary>
        /// Returns the raw sentences without any logging 
        /// </summary>
        public string RawOutput
        {
            get { return ChatOutput.RawText; }
        }

        public bool IsEmpty
        {
            get { return OutputSentenceCount == 0; }
        }

        public int OutputSentenceCount
        {
            get { lock (OutputSentences) return OutputSentences.Count; }
        }

        public ISettingsDictionary Predicates
        {
            get { return Requestor.Predicates; }
        }

        public SubQuery CurrentQuery
        {
            get
            {
                if (_CurrentQuery != null) return _CurrentQuery;
                Result r = ParentResult;
                while (r != null)
                {
                    SubQuery r_CurrentQuery = r._CurrentQuery;
                    if (r_CurrentQuery != null) return r_CurrentQuery;
                    r = r.ParentResult;
                }
                return null;
            }
        }

        public bool IsComplete
        {
            get { return EndedOn < DateTime.Now; }
            set { EndedOn = value ? DateTime.Now : DateTime.MaxValue; }
        }

        public bool IsSailent
        {
            get
            {
                if (OutputSentenceCount == 0) return false;
                if (RawOutput.Trim().Length == 0) return false;
                return true;
            }
        }

        public IList<TemplateInfo> UsedTemplates
        {
            get { return UsedTemplates1; }
        }

        public string _normalizedOutput;
        public ChatLabel CatchLabel;

        public string NormalizedOutput
        {
            get
            {
                return ChatOutput.TheMainSentence;
            }
        }

        public void AddSubqueries(GraphQuery queries)
        {
            if (queries.PatternCount == 0)
            {
                return;
            }
            var queriesGetBindings = queries.GetBindings();
            foreach (SubQuery query in queriesGetBindings)
            {
                if (IsTraced)
                {
                    writeToLog("AIMLTRACE SQ: " + this + " \n" + query.ToString().TrimStart());
                }
                SubQueries.Add(query);
            }
            DLRConsole.SystemFlush();
            Started = true;
        }

        public void AddOutputSentences(TemplateInfo ti, string unifiable)
        {
            AddOutputSentences0(ti, unifiable);
        }

        public void AddOutputSentences0(TemplateInfo ti, string unifiable)
        {
            if (null == unifiable)
            {
                writeToLog("ERROR assing null output " + ti);
                if (ti == null) return;
                return;
            }
            unifiable = unifiable.Trim();
            if (unifiable == "")
            {
                writeToLog("ERROR assing '' output " + ti);
                return;
            }
            unifiable = unifiable + " ";
            if (false && unifiable.Length > 2 && (unifiable.Contains("<br/>") || unifiable.Contains("&p;")))
            {
                string[] sents = unifiable.Split(new string[] {"<br/>", "&p;"}, StringSplitOptions.RemoveEmptyEntries);
                foreach (var s in sents)
                {
                    AddOutputSentences0(ti, s);
                }
                return;
            }
            if (false && unifiable.Length > 2 && (unifiable.Contains(". ") || unifiable.Contains("? ")))
            {
                string[] sents = unifiable.Split(new string[] {". ", "? "}, StringSplitOptions.RemoveEmptyEntries);
                foreach (var s in sents)
                {
                    AddOutputSentences0(ti, s);
                }
                return;
            }
            if (AlreadyUsed.Contains(unifiable)) return;

            if (ti != null)
            {
                lock (UsedTemplates1)
                {
                    double ThisRating = ti.Rating;
                    if (TemplateOfRating == null || TemplateRating < ThisRating)
                    {
                        TemplateOfRating = ti;
                        TemplateRating = ThisRating;
                        writeToLog("AIMLTRACE: OUTPUT RATING={0} {2} TI: {1} \n U: {3}", ThisRating, ti, ti.Graph,
                                   unifiable);
                    }
                    if (!Unifiable.IsNullOrEmpty(unifiable))
                    {
                        ti.TextSaved = unifiable;
                    }
                    else
                    {
                        ti.TextSaved = Unifiable.Empty;
                        return;
                    }

                    if (Unifiable.IsNullOrEmpty(unifiable))
                    {
                        throw new Exception("EmptyUnmif for " + ti);
                    }
                    if (!UsedTemplates1.Contains(ti)) UsedTemplates1.Add(ti);
                }
            }
            if (Unifiable.IsNullOrEmpty(unifiable))
            {
                return;
            }
            AlreadyUsed += unifiable;
            lock (OutputSentences)
            {
#if false
                if (unifiable == null || unifiable == "*" || unifiable == Unifiable.Empty)
                {
                    return;
                }
                int found = OutputSentences.IndexOf(unifiable);
                int c = OutputSentences.Count - 1;
                if (found == c)
                {
                    return;
                }
                if (found < 1)
                {
                    OutputSentences.Add(unifiable);
                    return;
                }
                OutputSentences.RemoveAt(found);
#endif
                if (unifiable.ToString().Contains("&"))
                {
                    OutputSentences.Remove(unifiable);
                }
                if (StaticXMLUtils.ContainsXml(unifiable))
                {
                    writeToLog("ERROR:  AddRssult: " + Requestor.UserID + " " + unifiable);
                }
                EndedOn = DateTime.Now;
                OutputSentences.Add(unifiable);
                return;
                bool isComplete = OutputSentences.Count >=
                                  ((QuerySettingsReadOnly) request.GetQuerySettings()).MinOutputs ||
                                  request.IsComplete(this);

                if (!isComplete) return;

                lock (ChatLabel.Labels)
                {
                    ChatLabel rd = this.CatchLabel;
                    if (rd == null) return;
                    if (!ChatLabel.IsFirst(rd)) return;
                    if (ti != null)
                    {
                        rd.TemplateInfo = ti;
                        rd.CreatedOutput = true;
                        rd.SubQuery = ti.Query;
                        rd.request = request;
                        // rd.KeepThrowing = true;
                        rd.TagHandler = ti.Query.LastTagHandler;
                        rd.result = (AIMLbot.Result) this;
                    }
                    throw rd;
                }
            }
        }

        public void WriteLine(string format, params object[] args)
        {
            lock (OutputSentences) OutputSentences.Add(string.Format(format, args));
        }

        /// <summary>
        /// Returns the raw output from the bot
        /// </summary>
        /// <returns>The raw output from the bot</returns>
        public override string ToString()
        {
            string whyComplete = WhyComplete;
            return ToString0() + (whyComplete != null ? " WhyComplete=" + whyComplete : "");
        }

        public string ToString0()
        {
            string msg = "";
            if (!Started)
            {
                msg = "!Started ";
            }            
            if (!IsComplete)
            {
                msg = "Incomplete ";

            }
            if (IsEmpty) return msg += "querycount=" + SubQueries.Count + " ";
            return msg + " \"" + Output + "\"";
        }

        public Unifiable GetOutputSentence(int sentence)
        {
            if (sentence == -1) return NormalizedOutput;
            lock (OutputSentences) return OutputSentences[sentence];
        }

        public void RotateUsedTemplates()
        {
            {
                var temps = UsedTemplates;
                if (temps != null)
                    foreach (TemplateInfo info in temps)
                    {
                        info.GraphmasterNode.RotateTemplate(info);
                    }
            }
        }
    }
}