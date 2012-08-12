using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Text;
using System.Text.RegularExpressions;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;

namespace MushDLR223.ScriptEngines
{
    public class CmdRequest : Parser, ParseInfo
    {
        public static implicit operator string[](CmdRequest request)
        {
            return request.tokens;
        }
        public object CallerAgent;
        public OutputDelegate Output;

        public CmdRequest(CmdRequest other, String[] args)
            : base(args)
        {
            CallerAgent = other.CallerAgent;
            Output = other.Output;
            SetCmdInfo(other);
        }


        public CmdRequest(string[] text, object callerIDORZero, OutputDelegate writeLine, ParseInfo command)
            : base(text)
        {
            CallerAgent = callerIDORZero;
            Output = writeLine;
            SetCmdInfo(command);
        }
        public CmdRequest AdvanceArgs(int used)
        {
            StartArg += used;
            tokens = SplitOff(tokens, used);
            ParseTokens();
            return this;
        }
    }

    public class Parser : ParseInfo
    {

        protected int StartArg;
        private IDictionary<string, object> ParamMap
        {
            get
            {
                return prepPhrases;
            }
        }
        public T GetValue<T>(string Param)
        {
            Param = ToKey(Param);
            return (T)ParamMap[Param];
        }
        public void SetValue<T>(string Param, T value)
        {
            Param = ToKey(Param);
            ParamMap[Param] = value;
        }

        public static string[] ParseArguments(string str)
        {
            str = PaddSpecialChars(str);


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

        public static char[] needSpacesArround = "+-[]!".ToCharArray();

        public static string PaddSpecialChars(string str)
        {
            if (str.Length < 2) return str;
            int startAt = 0;
            int ioa = str.IndexOfAny(needSpacesArround, startAt);
            if (ioa == -1) return str;
            while (ioa != -1)
            {
                if (ioa > 0 && str[ioa - 1] != ' ')
                {
                    str = str.Substring(0, ioa) + " " + str.Substring(ioa);
                    ioa = str.IndexOfAny(needSpacesArround, startAt);
                    continue;
                }
                if (ioa == str.Length - 1) break;
                if (str[ioa + 1] != ' ')
                {
                    str = str.Substring(0, ioa + 1) + " " + str.Substring(ioa + 1);
                    ioa = str.IndexOfAny(needSpacesArround, startAt);
                    continue;
                }
                startAt = ioa + 1;
                ioa = str.IndexOfAny(needSpacesArround, startAt);
            }
            return str.Trim();
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
            for (int i = p + 1; i < args.Length; i++)
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

        public readonly Dictionary<string, object>/*NameValueCollection*/ prepPhrases;
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

        virtual public string this[string Param]
        {
            get
            {
                Param = ToKey(Param);
                object obj;
                if (!prepPhrases.TryGetValue(Param, out obj) || ReferenceEquals(null, obj))
                {
                    
                    return null;
                }
                return obj.ToString();
            }
            set
            {
                Param = ToKey(Param);
                prepPhrases[Param] = value;
            }
        }

        public Parser(string[] Args)
            : this(Args, string.Join(" ", Args ?? new string[0]))
        {
        }
        public Parser(string Args)
            : this(SafeParseArgs(Args), Args)
        {
        }

        public static string[] SafeParseArgs(string str)
        {
            try
            {
                return ParseArguments(str);
            }
            catch (Exception e)
            {
                return str.Split(new char[] { ' ', ',' }, StringSplitOptions.RemoveEmptyEntries);
            }
        }

        public Parser(string[] tokes, string _str)
        {
            str = _str;
            tokens = tokes;
            prepPhrases = new Dictionary<string, object>();// new NameValueCollection();
            foreach (string prep in preps)
            {
                EnsurePrepKey(prep);
            }
            objectPhrase = "";

            string currentPrep = "";
            // sometimes ParseArgumetns throw exception

            bool firstTok = true;

            string lastPrep = null;
            for (int i = 0; i < tokens.Length; ++i)
            {
                string prep = tokens[i];
                if (string.IsNullOrEmpty(prep)) continue;
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
                        prep = prep.Substring(0, lenM1);
                        Add(ToKey(prep), "False");
                        continue;
                    }
                    if (lastChar == '+')
                    {
                        prep = prep.Substring(0, lenM1);
                        Add(ToKey(prep), "True");
                        continue;
                    }

                    if (prep.StartsWith("--"))
                    {
                        Add(ToKey(prep), "True");
                        lastPrep = ToKey(prep);
                        continue;
                    }
                    else
                    {
                        if (lastPrep != null)
                        {
                            Add(lastPrep, prep);
                        }
                    }
                    lastPrep = null;
                }


                if (prep.Contains(splitter))
                {
                    string[] args2 = prep.Split(splitter.ToCharArray(), StringSplitOptions.None);
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
                        EnsurePrepKey(currentPrep);
                        if (!firstTok)
                            prepPhrases[currentPrep] += " ";
                        prepPhrases[currentPrep] += prep;
                        firstTok = false;
                    }
                }
            }
        }

        private void EnsurePrepKey(string prep)
        {
            if (!prepPhrases.ContainsKey(prep)) prepPhrases[prep] = String.Empty;
        }

        public static Parser ParseArgs(string args)
        {
            try
            {
                return new Parser(args);
            }
            catch (Exception e)
            {
                DLRConsole.DebugWriteLine("" + e);

                return null;
            }
        }

        public static Parser ParseArgs(string[] args)
        {
            try
            {
                return new Parser(args);
            }
            catch (Exception e)
            {
                DLRConsole.DebugWriteLine("" + e);

                return null;
            }
        }

        public static string[] Parse(string command)
        {
            return Parser.ParseArguments(command);
        }
        private int CountNever
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
            if (s.Length == tokens.Length)
            {

            }
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

        public static string ToKey(string k)
        {
            k = k.Trim("-+= :\"'".ToCharArray()).Replace(" ", "_").ToLower();
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

        public int IndexOf(string flag)
        {
            int i = 0;
            flag = ToKey(flag);
            foreach (string key in tokens)
            {
                if (ToKey(key).Equals(flag)) return i;
                i++;
            }
            return -1;

        }

        private string[] GetWithoutIndex(int i, int len)
        {
            if (i == -1) return tokens;
            var p = new List<string>();
            for (int j = 0; j < i; j++)
            {
                p.Add(tokens[j]);
            }
            for (int j = i + 1 + len; j < tokens.Length; j++)
            {
                p.Add(tokens[j]);
            }
            return ModArgs = p.ToArray();
        }

        public bool GetWithout(string key, out string[] args)
        {
            int i = IndexOf(key);
            if (i < 0)
            {
                args = tokens;
                return false;
            }
            ModArgs = args = GetWithoutIndex(i, 0);
            return i != -1;
        }

        private string[] accume = null;
        public bool Destructive = false;
        protected string[] ModArgs
        {
            get
            {
                return accume;
            }
            set
            {
                if (Destructive) tokens = value;
                accume = value;
            }
        }

        public bool GetAfter(string key, out string[] args)
        {

            int i = IndexOf(key);
            args = tokens;
            if (i == -1)
            {
                return false;
            }
            ModArgs = args = GetAfterIndex(i);
            return true;

        }

        public string[] GetAfterIndex(int i)
        {
            var p = new List<string>();
            for (int j = i + 1; j < tokens.Length; j++)
            {
                p.Add(tokens[j]);
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

        public void ParseParams(object[] parameters)
        {
            // throw new NotImplementedException();
        }

        public T GetValue<T>(string key, T i)
        {
            string value;
            if (TryGetValue(key, out value))
            {
                return ChangeType<T>(value);
            }
            return i;
        }

        private T ChangeType<T>(object value)
        {
            return (T)ScriptManager.ChangeType(value, typeof(T));
        }

        public bool TryGetValue<T>(string name, out T value)
        {
            int len;
            return TryGetValueInt<T>(name, out value, out len) >= 0;
        }


        public bool TryGetValueWithout(string key, out string value, out string[] strings)
        {
            int len;
            int at = TryGetValueInt(key, out value, out len);
            if (at < 0)
            {
                strings = tokens;
                return false;
            }
            else
            {
                ModArgs = strings = GetWithoutIndex(at, len);
                return true;
            }
        }

        public int TryGetValueInt<T>(string name, out T value, out int len)
        {
            len = 1;
            int found = IndexOf(name);
            object ovalue;
            if (!ParamMap.TryGetValue(name, out ovalue))
            {
                if (found < 0)
                {
                    value = default(T);
                    len = 0;
                    return -1;
                }
                value = ChangeType<T>(GetAfterIndex(found)[0]);
                return found;
            }
            value = ChangeType<T>(ovalue);
            return found;
        }

        public void SetCmdInfo(ParseInfo info)
        {
            Parameters = info.Parameters;
            ParameterVersions = info.ParameterVersions;
            //ParamMap = new Dictionary<string, object>();
            SelectVersion();
            ParseTokens();
        }


        public NamedParam[][] ParameterVersions { get; set; }
        public NamedParam[] Parameters { get; set; }
        private ParseInfo CmdInfo { get { return this; } }

        protected NamedParam[] VersionSelected;
        private void SelectVersion()
        {
            if (VersionSelected != null) return;
            VersionSelected = Parameters;
            if (ParameterVersions == null || ParameterVersions.Length == 0) return;
            int tokenLen = tokens.Length;
            if (ParameterVersions.Length > 1)
            {
                foreach (var vchck in ParameterVersions)
                {
                    if (VersionSelected.Length == tokenLen)
                    {
                        VersionSelected = vchck;
                    }
                }
            }
            int skip = this.StartArg;
            int argCurrent = 0;
            foreach (NamedParam param in VersionSelected)
            {
                if (skip > 0)
                {
                    skip--;
                    continue;
                }
                string name = ToKey(param.Key);
                if (!ParamMap.ContainsKey(name))
                    ParamMap[name] = null;
            }
        }

        protected void ParseTokens()
        {
            int tokenLen = tokens.Length;
            int skip = this.StartArg;
            int argCurrent = 0;
            if (VersionSelected == null)
            {
                return;
            }
            foreach (NamedParam param in VersionSelected)
            {
                if (skip > 0)
                {
                    skip--;
                    continue;
                }
                if (argCurrent >= tokenLen) return;
                string name = ToKey(param.Key);

                if (param.IsOptional)
                {
                    bool wasBool = typeof(bool) == param.Type;
                    if (!KeyMatches(tokens[argCurrent], param))
                    {
                        if (wasBool)
                        {
                            ParamMap[name] = false;
                            continue;
                        }
                        ParamMap[name] = null;
                        continue;
                    }
                    if (wasBool)
                    {
                        ParamMap[name] = true;
                        argCurrent++;
                        continue;
                    }
                }
                int argsStart = argCurrent;
                int argsUsed;
                object value = ParseArg(param, param.Type, tokens, argsStart, out argsUsed);
                argCurrent += argsUsed;
                ParamMap[name] = value;
            }
        }

        static private bool KeyMatches(string token, NamedParam param)
        {
            var k1 = ToKey(token);
            var k2 = ToKey(param.Key);
            return k1 == k2;
        }

        private object ParseArg(NamedParam param, Type parseFor, string[] text, int start, out int argsUsed)
        {
            argsUsed = 1;
            return text[start];
        }

        public static string Join(string[] strings)
        {
            return string.Join(" ", strings);
        }
    }

    public class ParserFilterFormatException : FormatException
    {
        private string[] Args;
        private int Where;
        private string Why;

        public ParserFilterFormatException(string msg, string[] args, int where)
        {
            Why = msg;
            Args = args;
            Where = where;
        }
        public override string Message
        {
            get
            {
                return Why + ": arg= " + Where + " of " + string.Join(" ", Args);
            }
        }
    }

    public interface ParseInfo
    {
        NamedParam[][] ParameterVersions { get; }
        NamedParam[] Parameters { get; }
    }
}