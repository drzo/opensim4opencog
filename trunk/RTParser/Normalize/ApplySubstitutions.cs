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
            Random r = new Random(len);
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
        public static string Substitute(RTParser.RTPBot bot, RTParser.Utils.SettingsDictionary dictionary, string target)
        {
            string marker = ApplySubstitutions.getMarker(5);
            string markerSP = ApplySubstitutions.getMarker(3);
            string result = " " + Unifiable.ToVMString(target) + " ";
            foreach (string pattern in dictionary.SettingNames)
            {
                var vvalue = dictionary.grabSetting(pattern);
                var value = vvalue.AsString();
                if (value.Trim().Length == 0)
                {
                    Console.WriteLine("   VV: '{0}'", vvalue);
                }

                string p2 = ApplySubstitutions.makeRegexSafe(pattern);
                //Unifiable match = "\\b"+@p2.Trim().Replace(" ","\\s*")+"\\b";
                string match = "\\b" + p2.TrimEnd().TrimStart() + "\\b";
                string replacement = marker + value.Replace(" ", markerSP) + marker;
                if (Regex.IsMatch(result, match, RegexOptions.IgnoreCase))
                {
                    string testResult = Regex.Replace(result, match, replacement, RegexOptions.IgnoreCase);
                    Console.WriteLine("\n  SUBST :");
                    Console.WriteLine("   R: '{0}'", result);
                    Console.WriteLine("  PT: '{0}'", pattern);
                    Console.WriteLine("   V: '{0}'", value);
                    Console.WriteLine("  P2: '{0}'", p2);
                    Console.WriteLine("   M: '{0}'", match);
                    Console.WriteLine("  PR: '{0}'", replacement);
                    Console.WriteLine("  RS: '{0}'", testResult);
                    Console.WriteLine("  TS: '{0}'", testResult.Replace(marker, "").Replace(markerSP, " "));
                }
                result = Regex.Replace(result, match, replacement, RegexOptions.IgnoreCase);
            }
            return result.Replace(marker, "").Replace(markerSP, " ");
        }

        public static string SubstituteRecurse(RTParser.RTPBot bot, RTParser.Utils.SettingsDictionary dictionary, string target)
        {
            string result = Unifiable.ToVMString(target);
            String prev = "";
            while (prev != result)
            {
                prev = result;
                result = Substitute(bot, dictionary, target);
            }
            return  new StringUnifiable(result);
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
