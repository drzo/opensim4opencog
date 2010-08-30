using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Text;
using System.Text.RegularExpressions;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;

namespace MushDLR223.ScriptEngines
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
                else if (c == ' ' || c == '\t' || c == ',')
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
                        if (c == ',') list.Add(",");
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

        public static string Rejoin(string[] args, int p)
        {
            if (p >= args.Length) return string.Empty;
            if (p == args.Length) return args[p];
            string newstring = args[p];
            for (int i = p+1; i < args.Length; i++)
            {
                newstring += " ";
                newstring += EscapeIfNeeded(args[i]);
            }
            return newstring;
        }

        public static string EscapeIfNeeded(string s)
        {
            if (s.Length == 0) return "\"\"";
            bool needsQuotes = s.Contains(" ");
            bool needsSQuotes = s.Contains("\"");
            if (needsSQuotes)
            {
                return "'" + s + "'";
            }
            if (needsQuotes) return "\"" + s + "\"";
            return s;
        }

        static string[] preps = { "of", "to", "in", "for", "with", "as", "by", "at", "from", "on", "is" };

        public readonly NameValueCollection prepPhrases;
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

        public string this[string Param]
        {
            get
            {
                Param = ToKey(Param);
                return prepPhrases[Param];
            }
            set
            {
                Param = ToKey(Param);
                prepPhrases[Param] = value;
            }
        }

        public Parser(string[] Args)
            : this(string.Join(" ", Args??new string[0]))
        {

        }

        public Parser(string _str)
        {
            str = _str;
            prepPhrases = new NameValueCollection();
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
                string prep = tokens[i];
                int lenM1 = prep.Length - 1;
                string splitter = "=";
                if (prep.Contains("="))
                {
                    splitter = "=";
                }
                else if (prep.Contains(":"))
                {
                    splitter = ":";                    
                }
                else
                {
                    char lastChar = prep[lenM1];

                    if (lastChar == '-')
                    {
                        prep = prep.Substring(0, lastChar);
                        Add(ToKey(prep), "False");
                        continue;
                    }
                    if (lastChar == '+')
                    {
                        prep = prep.Substring(0, lastChar);
                        Add(ToKey(prep), "True");
                        continue;
                    }

                    if (prep.StartsWith("--"))
                    {
                        Add(ToKey(prep), "True");
                        continue;
                    }                    
                }
                

                if (prep.Contains(splitter))
                {
                    string[] args2 = prep.Split(splitter.ToCharArray(), 0, StringSplitOptions.None);
                    if (args2.Length > 1)
                    {
                        currentPrep = args2[0];
                        tokens[i] = string.Join(splitter, args2, 1, args2.Length - 1);
                        i--;
                        continue;
                    }
                }

                if (Array.IndexOf(preps, prep) >= 0 || prep.StartsWith("-"))
                {
                    currentPrep = ToKey(prep);
                    firstTok = true;
                }

                else
                {
                    if (currentPrep == "")
                    {
                        if (!firstTok)
                            objectPhrase += " ";
                        objectPhrase += prep;
                        firstTok = false;
                    }
                    else
                    {
                        if (!firstTok)
                            prepPhrases[currentPrep] += " ";
                        prepPhrases[currentPrep] += prep;
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
                DLRConsole.DebugWriteLine("" + e);
                
                return null;
            }
        }
        public static string[] Parse(string command)
        {
            return Parser.ParseArguments(command);	
        }
        private int Count
        {
            get
            {
                return prepPhrases.Count;
            }
        }
#if false
        // Variables
        // private readonly Dictionary<String,String> Parameters = new Dictionary<string, string>();
        private string[] tokens;
        private string[] Keys;
        private string[] Values;
        private bool[] Flags;
        private int[] OriginalPart;
        
        // Constructor
        public Parser(string[] Args)
        {
            new Parser(string.Join(" ", Args));
            Keys = new string[Args.Length];
            Values = new string[Args.Length];
            Flags = new bool[Args.Length];
            tokens = Args;
            //Parameters = new StringDictionary();
            Regex Splitter = new Regex(@"^-{1,2}|=",
                RegexOptions.IgnoreCase | RegexOptions.Compiled);

            Regex Remover = new Regex(@"^['""]?(.*?)['""]?$",
                RegexOptions.IgnoreCase | RegexOptions.Compiled);

            string Parameter = null;
            string[] Parts;

            // Valid parameters forms:
            // {-,/,--}param{ ,=,:}((",')value(",'))
            // Examples: 
            // -param1 value1 --param2
            //   /param4=happy -param5 '--=nice=--'
            foreach (string Txt in Args)
            {
                // Look for new parameters (-,/ or --) and a
                // possible enclosed value (=,:)
                Parts = Splitter.Split(Txt, 3);

                switch (Parts.Length)
                {
                    // Found a value (for the last parameter 
                    // found (space separator))
                    case 1:
                        if (Parameter != null)
                        {
                            if (!ContainsKey(Parameter))
                            {
                                Parts[0] =
                                    Remover.Replace(Parts[0], "$1");

                                Add(Parameter, Parts[0]);
                            }
                            Parameter = null;
                        }
                        // else Error: no parameter waiting for a value (skipped)
                        break;

                    // Found just a parameter
                    case 2:
                        // The last parameter is still waiting. 
                        // With no value, set it to true.
                        if (Parameter != null)
                        {
                            if (!ContainsKey(Parameter))
                                AddTrue(Parameter);
                        }
                        Parameter = Parts[1];
                        break;

                    // Parameter with enclosed value
                    case 3:
                        // The last parameter is still waiting. 
                        // With no value, set it to true.
                        if (Parameter != null)
                        {
                            if (!ContainsKey(Parameter))
                                AddTrue(Parameter);
                        }

                        Parameter = Parts[1];

                        // Remove possible enclosing characters (",')
                        if (!ContainsKey(Parameter))
                        {
                            Parts[2] = Remover.Replace(Parts[2], "$1");
                            Add(Parameter, Parts[2]);
                        }

                        Parameter = null;
                        break;
                }
            }
            // In case a parameter is still waiting
            if (Parameter != null)
            {
                if (!ContainsKey(Parameter))
                    AddTrue(Parameter);
            }
            checkKVs();
        }
#endif
        private void checkKVs()
        {
            return;
            string[] s = GetAfterIndex(0);
            OutputDelegate d = DLRConsole.DebugWriteLine;	
            foreach (var a in s)
            {
                d(a);
            }
            foreach (var a in tokens)
            {
                d(a);
            }
            DLRConsole.SystemFlush();
            if(s.Length == tokens.Length)
            {
                
            }
        }

        public bool TryGetValue(string name, out string value)
        {
            value = this[name];
            return value != null;
        }


        private bool ContainsKey(string k)
        {
            return IndexOf(k) != -1;
        }

        private void AddTrue(string ks)
        {
            Add(ks, "True");
        }

        private bool IsTrueString(string v)
        {
            if (String.IsNullOrEmpty(v)) return false;
            if (v.StartsWith("+")) return true;
            if (IsFalseString(v)) return false;
            return true;
        }
        private bool IsFalseString(string v)
        {
            if (String.IsNullOrEmpty(v)) return false;
            v = v.ToLower();
            if (v == "no" || v == "false" || v == "nil" || v.StartsWith("-")) return true;
            return false;
        }

        private void Add(string k, string v)
        {
            k = ToKey(k);
            prepPhrases[k] = v;
        }

        static string ToKey(string k)
        {
            k = k.Trim("-+= \"'".ToCharArray());
            return k;
        }

        public bool IsTrue(string key)
        {
            if (ContainsKey(key))
            {
                if (!IsTrueString(this[key])) return false;
                return true;
            }
            return false;
        }

        public bool ContainsFlag(string flag)
        {
            while (flag.StartsWith("-"))
            {
                flag = flag.Substring(1);
            }
            return ContainsKey(flag) || ContainsKey("--" + flag);
        }

        public string[] Without(string swipl)
        {
            int i = IndexOf(swipl);
            if (i == -1) return tokens;

            var p = new List<string>();

            return p.ToArray();
        }

        private int IndexOf(string flag)
        {
            int i = 0;
            flag = ToKey(flag);
            foreach(string key in prepPhrases.Keys)
            {
                if (key.Equals(flag)) return i;
                i++;
            }
            return -1;

        }

        private string[] GetWithoutIndex(int i)
        {
            if (i == -1) return tokens;
            var p = new List<string>();
            for (int j = 0; j < i; j++)
            {
                p.Add(prepPhrases[i]);
            }
            for (int j = i + 1; j < Count; j++)
            {
                p.Add(prepPhrases[i]);
            }
            return p.ToArray();
        }

        public bool GetWithout(string key, out string[] args)
        {
            int i = IndexOf(key);
            args = GetWithoutIndex(i);
            return i != -1;
        }

        public bool GetAfter(string key, out string[] args)
        {

            int i = IndexOf(key);
            args = tokens;
            if (i == -1)
            {
                return false;
            }
            args = GetAfterIndex(i);
            return true;

        }

        private string[] GetAfterIndex(int i)
        {
            var p = new List<string>();
            for (int j = i + 1; j < Count; j++)
            {
                p.Add(prepPhrases[i]);
            }
            return p.ToArray();
        }

        public bool GetBoolean(string key, ref bool value)
        {
            string v = this[key];
            if (v == null) return false;
            if (IsFalseString(v))
            {
                value = false;
                return true;
            }
            if (IsTrueString(v))
            {
                value = true;
                return true;                
            }
            return false;
        }

        public string[] ToArray()
        {
            return tokens;
        }

    }
}