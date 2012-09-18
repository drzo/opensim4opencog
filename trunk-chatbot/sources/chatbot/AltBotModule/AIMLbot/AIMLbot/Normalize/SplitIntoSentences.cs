using System;
using System.Text;
using System.Collections.Generic;
using RTParser;
using RTParser.Utils;

namespace AltAIMLbot.Normalize
{
    /// <summary>
    /// Splits the raw input into its constituent sentences. Split using the tokens found in 
    /// the bots Splitters string array.
    /// </summary>
    public class SplitIntoSentences
    {
        /// <summary>
        /// The bot this sentence splitter is associated with
        /// </summary>
        private AltBot bot;

        /// <summary>
        /// The raw input string
        /// </summary>
        private string inputString;

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot this sentence splitter is associated with</param>
        /// <param name="inputString">The raw input string to be processed</param>
        public SplitIntoSentences(AltBot bot, string inputString)
        {
            this.bot = bot;
            this.inputString = inputString;
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot this sentence splitter is associated with</param>
        public SplitIntoSentences(AltBot bot)
        {
            this.bot = bot;
        }

        /// <summary>
        /// Splits the supplied raw input into an array of strings according to the tokens found in
        /// the bot's Splitters List<>
        /// </summary>
        /// <param name="inputString">The raw input to split</param>
        /// <returns>An array of strings representing the constituent "sentences"</returns>
        public string[] Transform(string inputString)
        {
            this.inputString = inputString;
            return this.Transform();
        }

        /// <summary>
        /// Splits the raw input supplied via the ctor into an array of strings according to the tokens
        /// found in the bot's Splitters List<>
        /// </summary>
        /// <returns>An array of strings representing the constituent "sentences"</returns>
        public string[] Transform()
        {
            string[] tokens = (string[])AltBot.Splitters.ToArray();
            if (true)
            {
                return StaticAIMLUtils.SentenceBreaker(inputString, TrimEndTokens).ToArray();
            }
            string[] rawResult = this.inputString.Split(tokens, System.StringSplitOptions.RemoveEmptyEntries);
            List<string> tidyResult = new List<string>();
            foreach (string rawSentence in rawResult)
            {
                string tidySentence = rawSentence.Trim();
                if (tidySentence.Length > 0)
                {
                    tidyResult.Add(tidySentence);
                }
            }
            return (string[])tidyResult.ToArray();
        }

        private string TrimEndTokens(string s)
        {
            string[] tokens = (string[])AltBot.Splitters.ToArray();
            s = s.TrimEnd();
            foreach (string t0 in tokens)
            {
                string t = t0.TrimEnd();
                if (s.EndsWith(t))
                {
                    s = s.Substring(0, s.Length - t.Length);
                }
            }
            return s;
        }
    }
}
