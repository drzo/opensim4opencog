using System;
using System.Text;
using System.Text.RegularExpressions;

namespace RTParser.Normalize
{
    /// <summary>
    /// Checks the text for any matches in the bot's substitutions dictionary and makes
    /// any appropriate changes.
    /// </summary>
    public class ApplySubstitutions : RTParser.Utils.TextTransformer
    {
        public ApplySubstitutions(RTParser.RTPBot bot, Unifiable inputString)
            : base(bot, inputString)
        { }

        public ApplySubstitutions(RTParser.RTPBot bot)
            : base(bot)
        { }

        /// <summary>
        /// Produces a random "marker" Unifiable that tags text that is already the result of a substitution
        /// </summary>
        /// <param name="len">The length of the marker</param>
        /// <returns>the resulting marker</returns>
        private static Unifiable getMarker(int len)
        {
            char[] chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".ToCharArray();
            StringBuilder result = new StringBuilder();
            Random r = new Random();
            for (int i = 0; i < len; i++)
            {
                result.Append(chars[r.Next(chars.Length)]);
            }
            return result.ToString();
        }

        protected override Unifiable ProcessChange()
        {
            return ApplySubstitutions.Substitute(this.Proc, this.Proc.Substitutions, this.inputString);
        }

        /// <summary>
        /// Static helper that applies replacements from the passed dictionary object to the 
        /// target Unifiable
        /// </summary>
        /// <param name="bot">The bot for whom this is being processed</param>
        /// <param name="dictionary">The dictionary containing the substitutions</param>
        /// <param name="target">the target Unifiable to which the substitutions are to be applied</param>
        /// <returns>The processed Unifiable</returns>
        public static Unifiable Substitute(RTParser.RTPBot bot, RTParser.Utils.SettingsDictionary dictionary, Unifiable target)
        {
            Unifiable marker = ApplySubstitutions.getMarker(5);
            string result = target.AsString();
            foreach (string pattern in dictionary.SettingNames)
            {
                string p2 = ApplySubstitutions.makeRegexSafe(pattern);
                //Unifiable match = "\\b"+@p2.Trim().Replace(" ","\\s*")+"\\b";
                Unifiable match = "\\b" + p2.TrimEnd().TrimStart() + "\\b";
                Unifiable replacement = marker+dictionary.grabSetting(pattern).Trim()+marker;
                result = Regex.Replace(result, match, replacement, RegexOptions.IgnoreCase);
            }

            return result.Replace(marker, "");
        }


        /// <summary>
        /// Given an input, escapes certain characters so they can be used as part of a regex
        /// </summary>
        /// <param name="input">The raw input</param>
        /// <returns>the safe version</returns>
        private static string makeRegexSafe(string input)
        {
            string result = input.Replace("\\","");
            result = result.Replace(")", "\\)");
            result = result.Replace("(", "\\(");
            result = result.Replace(".", "\\.");
            return result;
        }
    }
}
