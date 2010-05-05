using System;
using System.Collections.Generic;
using System.Text;
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
        public void AddOutputSentences(Unifiable unifiable)
        {
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
            OutputSentences.Add(unifiable);
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
                if (OutputSentences.Count > 0)
                {
                    return this.RawOutput.AsString().Trim().Replace("  "," ");
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
        public Unifiable RawOutput
        {
            get
            {
                Unifiable result = Unifiable.CreateAppendable();
                foreach (string sentence in OutputSentences)
                {
                    return sentence;
                    Unifiable sentenceForOutput = sentence.Trim();

                    if (!this.checkEndsAsSentence(sentenceForOutput))
                    {
                        sentenceForOutput += ".";
                    }
                    result.Append(sentenceForOutput + " ");
                }
                return result;//.Trim();
            }
        }

        public decimal OutputSentenceCount
        {
            get { return OutputSentences.Count;  }
        }

        public SettingsDictionary Predicates
        {
            get { return user.Predicates; }
        }

        /// <summary>
        /// The subQueries processed by the bot's graphmaster that contain the templates that 
        /// are to be converted into the collection of Sentences
        /// </summary>
        public List<Utils.SubQuery> SubQueries = new List<Utils.SubQuery>();

        /// <summary>
        /// The individual sentences produced by the bot that form the complete response
        /// </summary>
        private List<Unifiable> OutputSentences = new List<Unifiable>();

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
            return this.Output;
        }

        /// <summary>
        /// Checks that the provided sentence ends with a sentence splitter
        /// </summary>
        /// <param name="sentence">the sentence to check</param>
        /// <returns>True if ends with an appropriate sentence splitter</returns>
        private bool checkEndsAsSentence(string sentence)
        {
            foreach (Unifiable splitter in this.bot.Splitters)
            {
                if (sentence.Trim().EndsWith(splitter))
                {
                    return true;
                }
            }
            return false;
        }

        public Unifiable GetOutputSentence(int sentence)
        {
            return OutputSentences[sentence];
        }
    }
}
