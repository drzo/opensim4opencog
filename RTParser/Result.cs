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
            request = null;
        }

        /// <summary>
        /// The amount of time the request took to process
        /// </summary>
        public TimeSpan Duration;

        public DateTime EndedOn = DateTime.MaxValue;

        /// <summary>
        /// The individual sentences that constitute the raw input from the user
        /// </summary>
        public List<Unifiable> InputSentences = new List<Unifiable>();

        public int LinesToUse = 1;

        /// <summary>
        /// The normalized sentence(s) (paths) fed into the graphmaster
        /// </summary>
        public List<Unifiable> NormalizedPaths = new List<Unifiable>();

        /// <summary>
        /// The individual sentences produced by the bot that form the complete response
        /// </summary>
        public List<string> OutputSentences = new List<string>();

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
        private List<TemplateInfo> UsedTemplates1 = new List<TemplateInfo>();

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
            this.Requestor = user;
            this.TargetBot = bot;
            this.request = request;
            ParentResult = parent;
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

        public QueryList TopLevel
        {
            get
            {
                if (request != null) return request.TopLevel;
                return request.TopLevel;
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
            get { return request.rawInput; }
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
                        if (request.hasTimedOut)
                        {
                            writeToLog("ERROR: TIMEOUT on " + RawInput + " from the user with an id: " + Requestor.UserID);
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
                                       Requestor.UserID.AsString());
                            return Unifiable.Empty;
                        }
                    }
            }
        }

        public string EnglishSentences
        {
            get { return CollectString(MaxPrintResults, OutputSentences, OutputSentencesToEnglish, " "); }
        }


        /// <summary>
        /// Returns the raw sentences without any logging 
        /// </summary>
        public string RawOutput
        {
            get { return CollectString(MaxPrintResults, OutputSentences, OutputSentencesToEnglish, " "); }
        }

        private string OutputSentencesToEnglish(string arg)
        {
// ReSharper disable ConvertToConstant.Local
            bool DoOutputSubst = false;
// ReSharper restore ConvertToConstant.Local
// ReSharper disable ConditionIsAlwaysTrueOrFalse
            if (DoOutputSubst)
// ReSharper restore ConditionIsAlwaysTrueOrFalse
            {
                string sentence = ApplySubstitutions.Substitute(TargetBot.OutputSubstitutions, arg);
                sentence = TextPatternUtils.ReTrimAndspace(sentence);
                if (TextPatternUtils.DifferentBesidesCase(arg, sentence))
                {
                    writeToLog("OutputSubst: " + arg + " -> " + sentence);
                    arg = sentence;
                }
            }
            return arg;

            return TargetBot.ToEnglish(arg);            
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

        public void AddSubqueries(QueryList queries)
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
                    writeToLog("AIMLTRACE SQ: " + query.ToString().TrimStart());
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
                string[] sents = unifiable.Split(new string[] { "<br/>", "&p;" }, StringSplitOptions.RemoveEmptyEntries);
                foreach (var s in sents)
                {
                    AddOutputSentences0(ti, s);
                }
                return;
            }
            if (false && unifiable.Length > 2 && (unifiable.Contains(". ") || unifiable.Contains("? ")))
            {
                string[] sents = unifiable.Split(new string[] { ". ", "? " }, StringSplitOptions.RemoveEmptyEntries);
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
                        writeToLog("AIMLTRACE: OUTPUT RATING={0} {2} TI: {1} \n U: {3}", ThisRating, ti, ti.Graph, unifiable);
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
            }
        }

        public void WriteLine(string format, params object[] args)
        {
            lock (OutputSentences) OutputSentences.Add(string.Format(format, args));
        }

        private static string CollectString(int resultsLeft, IEnumerable<string> sentences, Func<string, string> english,
                                            string split)
        {
            var result = new StringBuilder();
            lock (sentences)
                foreach (string sentence in sentences)
                {
                    String sentenceForOutput = english(sentence);
                    if (string.IsNullOrEmpty(sentenceForOutput)) continue;
                    result.Append(sentenceForOutput + split);
                    resultsLeft--;
                    if (resultsLeft < 1) return result.ToString();
                }
            return result.ToString();
        }

        /// <summary>
        /// Returns the raw output from the bot
        /// </summary>
        /// <returns>The raw output from the bot</returns>
        public override string ToString()
        {
            if (!Started)
            {
                return "-no-started-subqueries=" + SubQueries.Count;
                if (IsEmpty) return "";
            }
            if (IsComplete)
            {
            }
            if (IsEmpty) return "";
            return Output;
        }

        public Unifiable GetOutputSentence(int sentence)
        {
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