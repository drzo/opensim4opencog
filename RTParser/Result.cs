using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using RTParser;
using RTParser.Utils;
using UPath = RTParser.Unifiable;


namespace RTParser
{
    /// <summary>
    /// Encapsulates information about the result of a request to the bot
    /// </summary>
    public class Result
    {

        public QueryList TopLevel
        {
           get
           {
               if (request != null) return request.TopLevel;
               return request.TopLevel;               
           }
        }

        public static int MaxPrintResults = 1;
        public List<TemplateInfo> UsedTemplates = new List<TemplateInfo>();

        /// <summary>
        /// The subQueries processed by the bot's graphmaster that contain the templates that 
        /// are to be converted into the collection of Sentences
        /// </summary>
        public List<Utils.SubQuery> SubQueries = new List<Utils.SubQuery>();

        private bool Started = false;
        public void AddSubqueries(QueryList queries)
        {
            if (queries.PatternCount == 0)
            {
                return;
            }
            var o = Console.Out;
            var queriesGetBindings = queries.GetBindings();
            foreach (SubQuery query in queriesGetBindings)
            {
                if (IsTraced)
                {
                    bot.writeChatTrace("AIMLTRACE SQ: " + query);
                    o.Flush();
                }
                SubQueries.Add(query);
            }
            Started = true;
        }

        public bool IsTraced = false;

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
        public void AddOutputSentences(TemplateInfo ti, Unifiable unifiable)
        {
            if (AlreadyUsed.Contains(unifiable)) return;
            if (ti != null)
                lock (UsedTemplates)
                {
                    if (unifiable.IsEmpty)
                    {
                        throw new Exception("EmptyUnmif for " + ti);
                    }
                    UsedTemplates.Add(ti);
                }
            if (unifiable.IsEmpty)
            {
                return;
            }
            AlreadyUsed += unifiable;
#if false
            if (unifiable==null || unifiable=="*" || unifiable==Unifiable.Empty)
            {
                return;
            }
            int found = OutputSentences.IndexOf(unifiable);
            int c = OutputSentences.Count - 1;
            if (found == c) return;
            if (found < 1)
            {
                OutputSentences.Add(unifiable);
                return;
            }
            OutputSentences.RemoveAt(found);
#endif
            lock (OutputSentences) OutputSentences.Add(unifiable);
        }

        public void WriteLine(string format, object[] args)
        {
            lock (OutputSentences) OutputSentences.Add(string.Format(format, args));
        }

        public Result()
        {

        }
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
                                Environment.NewLine + paths.AsNodeXML() + " from the user with an id: " + this.user.UserID.ToValue());
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
                lock (OutputSentences) foreach (var sentence in OutputSentences)
                    {
                        String sentenceForOutput = sentence.ToValue().Replace("  ", " ").Trim();

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

        public SettingsDictionary Predicates
        {
            get { return user.Predicates; }
        }

        /// <summary>
        /// The individual sentences produced by the bot that form the complete response
        /// </summary>
        public List<Unifiable> OutputSentences = new List<Unifiable>();

        /// <summary>
        /// The individual sentences that constitute the raw input from the user
        /// </summary>
        public List<Unifiable> InputSentences = new List<Unifiable>();

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="user">The user for whom this is a result</param>
        /// <param name="bot">The bot providing the result</param>
        /// <param name="request">The request that originated this result</param>
        public Result(User user, RTPBot bot, Request request)
        {
            this.user = user;
            this.bot = bot;
            this.request = request;
            this.request.result = this;
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
    }
}
