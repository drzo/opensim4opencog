using System;
using System.Text;
using System.Text.RegularExpressions;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using RTParser.Variables;

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
            return ApplySubstitutions.Substitute(this.Proc.InputSubstitutions, this.inputString);
        }

        /// <summary>
        /// Static helper that applies replacements from the passed dictionary object to the 
        /// target Unifiable
        /// </summary>
        /// <param name="bot">The bot for whom this is being processed</param>
        /// <param name="dictionary">The dictionary containing the substitutions</param>
        /// <param name="target">the target Unifiable to which the substitutions are to be applied</param>
        /// <returns>The processed Unifiable</returns>
        public static string Substitute(ISettingsDictionary dictionary, string target)
        {
            string marker = ApplySubstitutions.getMarker(5);
            string markerSP = ApplySubstitutions.getMarker(3);
            string result = " " + Unifiable.ToVMString(target) + " ";
            System.Collections.Generic.IEnumerable<string> dictionarySettingNames = dictionary.SettingNames(0);
            foreach (string pattern in dictionarySettingNames)
            {
                if (string.IsNullOrEmpty(pattern)) continue;
                var vvalue = dictionary.grabSetting(pattern);
                var value = vvalue.AsString();
                value = value.Replace("\\b", " ");
                value = value.ToUpper();
                string replacement;
                if (value.Trim().Length == 0)
                {
                    value = " ";
                    replacement = " ";
                }
                else
                {
                    value = value.Trim();
                    if (value == pattern)
                    {
                        continue;
                    }
                    replacement = marker + value.Replace(" ", markerSP) + marker;
                }
                string p2 = ApplySubstitutions.makeRegexSafe(pattern);
                //Unifiable match = "\\b"+@p2.Trim().Replace(" ","\\s*")+"\\b";
                string match = "\\b" + p2.TrimEnd().TrimStart() + "\\b";
                if (Regex.IsMatch(result, match, RegexOptions.IgnoreCase))
                {
                    string testResult = Regex.Replace(result, match, replacement, RegexOptions.IgnoreCase);
                    if (false)
                    {
                        OutputDelegate to = DLRConsole.DebugWriteLine;
                        to("\n  SUBST :");
                        to("   R: '{0}'", result);
                        to("  PT: '{0}'", pattern);
                        to("   V: '{0}'", value);
                        to("  P2: '{0}'", p2);
                        to("   M: '{0}'", match);
                        to("  PR: '{0}'", replacement);
                        to("  RS: '{0}'", testResult);
                        to("  TS: '{0}'", testResult.Replace(marker, "").Replace(markerSP, " "));
                    }
                    result = testResult;
                }
                
            }
            return result.Replace(marker, "").Replace(markerSP, " ");
        }

        public static string SubstituteRecurse(RTParser.RTPBot bot, SettingsDictionary dictionary, string target)
        {
            string result = Unifiable.ToVMString(target);
            String prev = "";
            while (prev != result)
            {
                prev = result;
                result = Substitute(dictionary, target);
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
            return Regex.Escape(input);
            string result = input.Replace("\\","");
            result = result.Replace(")", "\\)");
            result = result.Replace("(", "\\(");
            result = result.Replace(".", "\\.");
            return result;
        }
    }
}
