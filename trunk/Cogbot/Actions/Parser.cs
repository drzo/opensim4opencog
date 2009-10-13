using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    public class Parser
    {
        public static string[] ParseArguments(string str)
        {
            List<string> list = new List<string>();
            string current = String.Empty;
            string trimmed = null;
            bool withinQuote = false;
            bool escaped = false;

            foreach (char c in str)
            {
                if (c == '"')
                {
                    if (escaped)
                    {
                        current += '"';
                        escaped = false;
                    }
                    else
                    {
                        current += '"';
                        withinQuote = !withinQuote;
                    }
                }
                else if (c == ' ' || c == '\t')
                {
                    if (escaped || withinQuote)
                    {
                        current += c;
                        escaped = false;
                    }
                    else
                    {
                        trimmed = current.Trim();
                        if (trimmed.StartsWith("\"") && trimmed.EndsWith("\""))
                        {
                            trimmed = trimmed.Remove(0, 1);
                            trimmed = trimmed.Remove(trimmed.Length - 1);
                            trimmed = trimmed.Trim();
                        }
                        if (trimmed.Length > 0)
                            list.Add(trimmed);
                        current = String.Empty;
                    }
                }
                else if (c == '\\')
                {
                    if (escaped)
                    {
                        current += '\\';
                        escaped = false;
                    }
                    else
                    {
                        escaped = true;
                    }
                }
                else
                {
                    if (escaped)
                        throw new FormatException(c.ToString() + " is not an escapable character.");
                    current += c;
                }
            }

            trimmed = current.Trim();

            if (trimmed.StartsWith("\"") && trimmed.EndsWith("\""))
            {
                trimmed = trimmed.Remove(0, 1);
                trimmed = trimmed.Remove(trimmed.Length - 1);
                trimmed = trimmed.Trim();
            }

            if (trimmed.Length > 0)
                list.Add(trimmed);

            return list.ToArray();
        }

        public static string[] SplitOff(string[] args, int p)
        {
            if (p >= args.Length) return new string[0];
            string[] newstring = new string[args.Length - p];
            int ci = 0;
            while (p < args.Length)
            {
                newstring[ci] = args[p];
                p++;
                ci++;
            }
            return newstring;
        }

        static string[] preps = { "of", "to", "in", "for", "with", "as", "by", "at", "from", "on", "is" };

        public Dictionary<string, string> prepPhrases;
        public string objectPhrase;
        public string str;
        public string[] tokens;

        public int Length
        {
            get
            {
                return tokens.Length;
            }
        }

        public string this[int i]
        {
            get { return tokens[i]; }
        }

        public string this[string i]
        {
            get { return prepPhrases[i]; }
        }

        public Parser(string _str)
        {
            str = _str;
            prepPhrases = new Dictionary<string, string>();
            foreach (string prep in preps)
                prepPhrases[prep] = string.Empty;
            objectPhrase = "";

            string currentPrep = "";
            // sometimes ParseArgumetns throw exception
            try
            {
                tokens = ParseArguments(str);
            }
            catch (Exception e)
            {
                tokens = str.Split(new char[] {' ', ','}, StringSplitOptions.RemoveEmptyEntries);
            }
            bool firstTok = true;

            for (int i = 0; i < tokens.Length; ++i)
            {
                if (prepPhrases.ContainsKey(tokens[i]))
                {
                    currentPrep = tokens[i];
                    firstTok = true;
                }
                else
                {
                    if (currentPrep == "")
                    {
                        if (!firstTok)
                            objectPhrase += " ";
                        objectPhrase += tokens[i];
                        firstTok = false;
                    }
                    else
                    {
                        if (!firstTok)
                            prepPhrases[currentPrep] += " ";
                        prepPhrases[currentPrep] += tokens[i];
                        firstTok = false;
                    }
                }
            }
        }

        public static Parser ParseArgs(string args)
        {
            try
            {
                return new Parser(args);
            } catch (Exception e)
            {
               Console.WriteLine(""+e);
                
                return null;
            }
        }
    }
}
