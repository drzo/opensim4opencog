using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using AltAIMLbot.Utils;

namespace AltAIMLbot
{
    /// <summary>
    /// Encapsulates information about the result of a request to the bot
    /// </summary>
    public  class Result
    {
        /// <summary>
        /// The bot that is providing the answer
        /// </summary>
        public AltBot bot;

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
        public string RawInput
        {
            get
            {
                return this.request.rawInput;
            }
        }

        /// <summary>
        /// The normalized sentence(s) (paths) fed into the graphmaster
        /// </summary>
        public List<string> NormalizedPaths = new List<string>();

        /// <summary>
        /// The amount of time the request took to process
        /// </summary>
        public TimeSpan Duration;

        /// <summary>
        /// The result from the bot with logging and checking
        /// </summary>
        public string Output
        {
            get
            {
                if (OutputSentences.Count > 0)
                {
                    return this.RawOutput;
                }
                else
                {
                    if (this.request.hasTimedOut)
                    {
                        return this.bot.TimeOutMessage;
                    }
                    else
                    {
                        StringBuilder paths = new StringBuilder();
                        foreach (string pattern in this.NormalizedPaths)
                        {
                            paths.Append(pattern + Environment.NewLine);
                        }
                        this.bot.writeToLog("The bot could not find any response for the input: " + this.RawInput + " with the path(s): " + Environment.NewLine + paths.ToString() + " from the user with an id: " + this.user.UserID);
                        return string.Empty;
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
                StringBuilder result = new StringBuilder();
                foreach (string sentence in OutputSentences)
                {
                    string sentenceForOutput = sentence.Trim();
                    if (!this.checkEndsAsSentence(sentenceForOutput))
                    {
                        sentenceForOutput += ".";
                    }
                    result.Append(sentenceForOutput + " ");
                }
                return result.ToString().Trim();
            }
        }

        /// <summary>
        /// The subQueries processed by the bot's graphmaster that contain the templates that 
        /// are to be converted into the collection of Sentences
        /// </summary>
        public List<Utils.SubQuery> SubQueries = new List<Utils.SubQuery>();

        /// <summary>
        /// The individual sentences produced by the bot that form the complete response
        /// </summary>
        public List<string> OutputSentences = new List<string>();

        /// <summary>
        /// The individual sentences that constitute the raw input from the user
        /// </summary>
        public List<string> InputSentences = new List<string>();

        public int MaxCanEvalResult { get; set; }
        private string matchable;
        public static int MaxPrintResults = 10;

        public readonly Utterance ChatInput;
        public User altResponder = null;
        private Utterance _chatOutput;
        public Utterance ChatOutput
        {
            get
            {
                if (_chatOutput == null)
                {
                    return null;
                }
                return _chatOutput;
            }
            private set { _chatOutput = value; }
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="user">The user for whom this is a result</param>
        /// <param name="bot">The bot providing the result</param>
        /// <param name="request">The request that originated this result</param>
        public Result(User user, AltBot bot, Request request)
        {
            this.user = user;
            this.bot = bot;
            this.request = request;
            this.request.result = this;
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="user">The user for whom this is a result</param>
        /// <param name="bot">The bot providing the result</param>
        /// <param name="request">The request that originated this result</param>
        public Result(string rawInput, User user, AltBot bot, Request parent, User targetUser)
        {
            this.request = parent;
            this.user = user;
            this.bot = bot;
            this.request.result = this;
            //ExitQueue = new CommitQueue();
            matchable = matchable ?? MakeMatchable(rawInput);
            SubQueries = new List<AltAIMLbot.Utils.SubQuery>();
            MaxCanEvalResult = 10;
            request = parent;
            ChatInput = parent.ChatInput;
            // //this.Requester = user;
            altResponder = targetUser;
            request.TargetBot = bot;
            ChatOutput = new Utterance(bot.EnsureEnglish, user, altResponder, null, MaxPrintResults)
                              {InResponse = ChatInput};
            // //OutputSentences = ChatOutput.SemanticSentences;
            //writeToLog = writeToLog ?? user.WriteToUserTrace;
            //writeToLog = writeToLog ?? request.WriteLine;
            // //this.request.TheCurrentResult = this;
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
            foreach (string splitter in this.bot.Splitters)
            {
                if (sentence.Trim().EndsWith(splitter))
                {
                    return true;
                }
            }
            return false;
        }

        // === PATCH AREA ===
        public TimeSpan Durration
        {
            get { return request.Durration; }
        }

        public  void FreeResult(){}
        
        public  object GetInputSentence(int sentence)
        { throw new NotImplementedException(); }

        public virtual Result result { get { return this; } }

        public object Requester
        {
            get { throw new NotImplementedException(); }
            // set { request.Requester = value; }
        }
        private string userSetResultComplete;
        public string WhyResultComplete
        {
            get
            {
                throw new NotImplementedException();
            }
            set { userSetResultComplete = value; }
        }

        #region StaticXMLUTils
        public static bool ContainsXml(string s)
        {
            if (s.Contains(">") && s.Contains("<")) return true;
            if (s.Contains("&") && s.Contains(";"))
            {
                return true;
            }
            return false;
        }
        public static string ReplaceMap(string strTrim, string[][] pairs)
        {
            foreach (string[] pair in pairs)
            {
                string pair1 = pair[0];
                string pair2 = pair[1];
                if (strTrim.Contains(pair1))
                {
                    strTrim = ReplaceAll(strTrim, pair1, pair2);
                    if (strTrim.Contains(pair1))
                    {
                        strTrim = strTrim.Replace(pair1, pair2);
                    }
                }
            }
            return strTrim;
        }
        public static string ReplacePairs(string strTrim, params string[] pairs)
        {
            int index = 0;
            int pairsLength = pairs.Length;
            while (index < pairsLength)
            {
                strTrim = ReplaceAll(strTrim, pairs[index++], pairs[index++]);
            }
            return strTrim;
        }
        /// <summary>
        /// Helps keep new strings garbage collectable by using the previous
        /// version if they are the same string
        /// </summary>
        /// <param name="before"></param>
        /// <param name="after"></param>
        /// <returns></returns>
        public static string OlderReference(string before, string after)
        {
            if (after == before)
            {
                return before;
            }
            return after;
        }

        public static string ReplaceAll(string source, string b, string a)
        {
            string sourceReplace = source.Replace(b, a);
            var result = OlderReference(source, sourceReplace);
            while (result.Contains(b))
            {
                sourceReplace = result.Replace(b, a);
                result = OlderReference(source, sourceReplace);
            }
            return result;
        }

        public static string ReTrimAndspace(string substitute)
        {
            if (substitute == null) return null;
            var s = OlderReference(substitute, substitute.Replace("> ", ">").Replace(" <", "<").Replace("  ", " "));
            if (s.Length == 1)
            {
                if (s != " ") return s;
                return s;
            }
            return Trim(substitute);
        }
        public static bool Unused(char arg1, char arg2)
        {
            throw new NotImplementedException();
            return false;
        }

        public static string Trim(string param1)
        {
            var outp = param1.Trim();
            return OlderReference(param1, outp);
        }
        public static string CleanWhitepaces(string xml2)
        {
            return CleanWhitepaces(xml2, null, Unused, Unused);
        }
        public static string CleanWhitepaces(string xml2, string padchars,
                                              Func<char, char, bool> ifBefore, Func<char, char, bool> ifAfter)
        {
            if (xml2 == null) return xml2;
            const long maxCleanSize = 2 << 14;
            int inlen = xml2.Length;
            if (inlen > maxCleanSize || inlen < 2)
            {
                return xml2;
            }

            if (padchars != null)
            {
                if (xml2.IndexOfAny(padchars.ToCharArray(), 0) == -1 || (xml2.IndexOfAny(new[] { '\\', ':', '/' }, 0) > 0))
                {
                    padchars = null;
                }
            }

            StringBuilder s = new StringBuilder(inlen);

            bool chgd = false;
            bool xmlFound = false;
            bool inwhite = true;
            bool pendingWhitespace = false;
            char lastChar = '\0';
            int charIndex = -1;
            foreach (char c0 in xml2)
            {
                charIndex++;
                if (c0 <= 32)
                {
                    if (inwhite)
                    {
                        chgd = true;
                        continue;
                    }
                    inwhite = true;
                    pendingWhitespace = true;
                    continue;
                }
                switch (c0)
                {
                    case '/':
                        if (lastChar == 'r')
                        {
                            xmlFound = true;
                        }
                        int nxtIndex = charIndex + 1;
                        if (nxtIndex < inlen && xml2[nxtIndex] == '>')
                        {
                            if (!pendingWhitespace)
                            {
                                chgd = true;
                                pendingWhitespace = true;
                            }
                        }
                        else
                        {
                            inwhite = true;
                            if (pendingWhitespace)
                            {
                                chgd = true;
                                pendingWhitespace = false;
                            }
                        }
                        break;
                    case '>':
                    case '<':
                    case '\\':
                        inwhite = true;
                        if (pendingWhitespace)
                        {
                            chgd = true;
                            pendingWhitespace = false;
                        }
                        break;
                    default:
                        if (padchars != null)
                        {
                            bool before = padchars.Contains("" + lastChar);
                            if (before && ifBefore(lastChar, c0))
                            {
                                if (!inwhite) pendingWhitespace = true;
                            }

                            bool after = padchars.Contains("" + c0);
                            if (after && ifAfter(lastChar, c0))
                            {
                                if (!inwhite) pendingWhitespace = true;
                            }
                        }
                        inwhite = false;
                        break;
                }
                if (pendingWhitespace)
                {
                    s.Append(' ');
                    pendingWhitespace = false;
                }
                s.Append(c0);
                lastChar = c0;
            }
            if (pendingWhitespace) chgd = true;
            int len = s.Length;
            if (xmlFound)
            {
                s = s.Replace("<sr />", "<srai><star /></srai>");
                //s = s.Replace("star/>", "star index=\"1\"/>");
                if (len != s.Length)
                {
                    chgd = true;
                }
            }
            if (!chgd)
            {
                if (len != inlen)
                {
                    return s.ToString();
                }
                return xml2;
            }
            //s = s.Replace("<star index=\"1\"", "<star");

            return s.ToString();
        }
        public  string MakeMatchable(string xml2)
        {
            if (xml2 == null) return xml2;
            if (!ContainsXml(xml2))
            {
                xml2 = ReplaceMap(xml2, new[]
                                     {
                                         new string[] {".", " "},
                                         new string[] {",", " "},
                                         new string[] {"?", " "},
                                         new string[] {"!", " "},
                                     });
                return ReTrimAndspace(xml2);
            }
            string xml22 = CleanWhitepaces(xml2);
            return OlderReference(xml2, xml22.ToUpper());
        }
        #endregion

        //public RTParser.Variables.ISettingsDictionary RequesterChanges { get { throw new NotImplementedException(); } }
        //public RTParser.Variables.ISettingsDictionary ResponderChanges { get { throw new NotImplementedException(); } }

    }
}
