using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    public class Parser
    {
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
                tokens = Parsing.ParseArguments(str);
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

        internal static Parser ParseArgs(string args)
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
