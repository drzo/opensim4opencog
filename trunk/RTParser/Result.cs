using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using MushDLR223.Utilities;
using RTParser;
using RTParser.Utils;
using RTParser.Variables;
using UPath = RTParser.Unifiable;


namespace RTParser
{
    /// <summary>
    /// Encapsulates information about the result of a request to the bot
    /// </summary>
    abstract public class Result
    {

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

        public static int MaxPrintResults = 1;
        private List<TemplateInfo> UsedTemplates1 = new List<TemplateInfo>();

        /// <summary>
        /// The subQueries processed by the bot's graphmaster that contain the templates that 
        /// are to be converted into the collection of Sentences
        /// </summary>
        public List<Utils.SubQuery> SubQueries = new List<Utils.SubQuery>();


        public double Score
        {
            get
            {
                return TemplateRating;
            }
        }

        public double TemplateRating = 0.0d;
        public TemplateInfo TemplateOfRating;

        private bool Started = false;
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
                    bot.writeChatTrace("AIMLTRACE SQ: " + query);
                }
                SubQueries.Add(query);
            }
            DLRConsole.SystemFlush();
            Started = true;
        }

        /// <summary>
        /// If the query is being traced
        /// </summary>
        public virtual bool IsTraced { get; set; }

        private string AlreadyUsed = "xtxtxtxtxtxtxtxtxxt";
        public int LinesToUse = 1;

        public string SetOutput
        {
            set
            {
                lock (OutputSentences)
                {
                    AlreadyUsed = value;
                    OutputSentences.Clear();
                    OutputSentences.Add(value);                    
                }
            }
        }
        public void AddOutputSentences(TemplateInfo ti, string unifiable)
        {
            
            if (null == unifiable)
            {
                bot.writeToLog("ERROR assing null output " + ti);
                if (ti == null) return;
                return;
            }
            unifiable = unifiable.Trim();
            if (unifiable=="")
            {
                bot.writeToLog("ERROR assing '' output " + ti);
                return;
            }
            unifiable = unifiable + " ";
            if (false && unifiable.Length > 2 && (unifiable.Contains("<br/>") || unifiable.Contains("&p;")))
            {
                string[] sents = unifiable.Split(new string[] { "<br/>", "&p;" }, StringSplitOptions.RemoveEmptyEntries);
                foreach (var s in sents)
                {
                    AddOutputSentences(ti, s);
                }
                return;
            }
            if (false && unifiable.Length > 2 && (unifiable.Contains(". ") || unifiable.Contains("? ")))
            {
                string[] sents = unifiable.Split(new string[] { ". ", "? " }, StringSplitOptions.RemoveEmptyEntries);
                foreach (var s in sents)
                {
                    AddOutputSentences(ti, s);
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
                        bot.writeChatTrace("AIMLTRACE OUTPUT RATING={0} TI: {1} U: {2}", ThisRating, ti, unifiable);

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
                OutputSentences.Add(unifiable);
            }
        }

        public void WriteLine(string format, object[] args)
        {
            lock (OutputSentences) OutputSentences.Add(string.Format(format, args));
        }
/*
        public Result()
        {

        }*/
        /// <summary>
        /// The bot that is providing the answer
        /// </summary>
        public RTPBot bot;

        /// <summary>
        /// The user for whom this is a result
        /// </summary>
        public User user;

        /// <summary>
        /// The request from the user
        /// </summary>
        public Request request;

        /// <summary>
        /// The raw input from the user
        /// </summary>
        public Unifiable RawInput
        {
            get
            {
                return this.request.rawInput;
            }
        }

        /// <summary>
        /// The normalized sentence(s) (paths) fed into the graphmaster
        /// </summary>
        public List<UPath> NormalizedPaths = new List<UPath>();

        /// <summary>
        /// The amount of time the request took to process
        /// </summary>
        public TimeSpan Duration;

        /// <summary>
        /// The result from the bot with logging and checking
        /// </summary>
        public Unifiable Output
        {
            get
            {
                lock (OutputSentences) if (OutputSentences.Count > 0)
                    {
                        return this.RawOutput.Trim().Replace("  ", " ");
                    }
                    else
                    {
                        if (this.request.hasTimedOut)
                        {

                            this.bot.writeToLog("ERROR: TIMEOUT on " + this.RawInput + " from the user with an id: " + this.user.UserID);
                            return Unifiable.Empty;
                            return this.bot.TimeOutMessage;
                        }
                        else
                        {
                            Unifiable paths = Unifiable.CreateAppendable();
                            foreach (UPath pattern in this.NormalizedPaths)
                            {
                                //return pattern;
                                paths.Append(pattern.LegacyPath + Environment.NewLine);
                            }
                            this.bot.writeToLog("The bot could not find any response for the input: " + this.RawInput + " with the path(s): " +
                                Environment.NewLine + paths.AsNodeXML() + " from the user with an id: " + this.user.UserID.AsString());
                            return Unifiable.Empty;
                        }
                    }
            }
        }

        /// <summary>
        /// Returns the raw sentences without any logging 
        /// </summary>
        public string RawOutput
        {
            get
            {
                if (IsEmpty)
                {

                }
                int resultsLeft = MaxPrintResults;
                Unifiable result = Unifiable.CreateAppendable();
                lock (OutputSentences) foreach (string sentence in OutputSentences)
                    {
                        String sentenceForOutput = sentence.Replace("  ", " ").Trim();

                        if (!this.checkEndsAsSentence(sentenceForOutput))
                        {
                            sentenceForOutput += ".";
                        }
                        result.Append(sentenceForOutput + " ");
                        resultsLeft--;
                        if (resultsLeft < 1) return result;
                    }
                return result;//.Trim();
            }
        }

        public bool IsEmpty { get { return OutputSentenceCount == 0; } }

        public int OutputSentenceCount
        {
            get { lock (OutputSentences) return OutputSentences.Count; }
        }

        public ISettingsDictionary Predicates
        {
            get { return user.Predicates; }
        }

        /// <summary>
        /// The individual sentences produced by the bot that form the complete response
        /// </summary>
        public List<string> OutputSentences = new List<string>();

        /// <summary>
        /// The individual sentences that constitute the raw input from the user
        /// </summary>
        public List<Unifiable> InputSentences = new List<Unifiable>();

        public SubQuery _CurrentQuery;
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
        public Result ParentResult;
        public bool IsSailent
        {
            get
            {
                if (OutputSentenceCount == 0) return false;
                if (RawOutput.Trim().Length == 0) return false;
                return true;
            }
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="user">The user for whom this is a result</param>
        /// <param name="bot">The bot providing the result</param>
        /// <param name="request">The request that originated this result</param>
        public Result(User user, RTPBot bot, Request request, Result parent)
        {
            this.user = user;
            this.bot = bot;
            this.request = request;
            this.ParentResult = parent;
            // this.request.CurrentResult = this;
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
            if (IsEmpty) return "";
            return this.Output;
        }

        /// <summary>
        /// Checks that the provided sentence ends with a sentence splitter
        /// </summary>
        /// <param name="sentence">the sentence to check</param>
        /// <returns>True if ends with an appropriate sentence splitter</returns>
        private bool checkEndsAsSentence(string sentence)
        {
            sentence = sentence.Trim();
            if (sentence.EndsWith("?")) return true;
            foreach (Unifiable splitter in this.bot.Splitters)
            {
                if (sentence.EndsWith(splitter))
                {
                    return true;
                }
            }
            return false;
        }

        public Unifiable GetOutputSentence(int sentence)
        {
            lock (OutputSentences) return OutputSentences[sentence];
        }
        public IList<TemplateInfo> UsedTemplates
        {
            get
            {
                return UsedTemplates1;
            }
        }
    }
}
