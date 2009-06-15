using System;
using System.Text;
using System.Collections.Generic;

namespace RTParser.Normalize
{
    /// <summary>
    /// Splits the raw input into its constituent sentences. Split using the tokens found in 
    /// the bots Splitters Unifiable array.
    /// </summary>
    public class SplitIntoSentences
    {
        /// <summary>
        /// The bot this sentence splitter is associated with
        /// </summary>
        private RTParser.RTPBot bot;

        /// <summary>
        /// The raw input Unifiable
        /// </summary>
        private Unifiable inputString;

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot this sentence splitter is associated with</param>
        /// <param name="inputString">The raw input Unifiable to be processed</param>
        public SplitIntoSentences(RTParser.RTPBot bot, Unifiable inputString)
        {
            this.bot = bot;
            this.inputString = inputString;
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot this sentence splitter is associated with</param>
        public SplitIntoSentences(RTParser.RTPBot bot)
        {
            this.bot = bot;
        }

        /// <summary>
        /// Splits the supplied raw input into an array of strings according to the tokens found in
        /// the bot's Splitters List<>
        /// </summary>
        /// <param name="inputString">The raw input to split</param>
        /// <returns>An array of strings representing the constituent "sentences"</returns>
        public Unifiable[] Transform(Unifiable inputString0)
        {
            this.inputString = inputString0;
            return this.Transform();
        }

        /// <summary>
        /// Splits the raw input supplied via the ctor into an array of strings according to the tokens
        /// found in the bot's Splitters List<>
        /// </summary>
        /// <returns>An array of strings representing the constituent "sentences"</returns>
        public Unifiable[] Transform()
        {
            string[] tokens = (string[])this.bot.Splitters.ToArray();
            string[] rawResult = this.inputString.AsString().Split(tokens, System.StringSplitOptions.RemoveEmptyEntries);
            List<Unifiable> tidyResult = new List<Unifiable>();
            foreach (string rawSentence in rawResult)
            {
                string tidySentence = rawSentence.Trim();
                if (tidySentence.Length>0)
                {
                    tidyResult.Add(tidySentence);
                }
            }
            return (Unifiable[])tidyResult.ToArray();
        }
    }
}
