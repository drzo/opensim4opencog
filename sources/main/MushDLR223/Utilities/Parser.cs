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
        public bool IsFFI = false;
        public bool WantsResult = true;

        public CmdRequest(CmdRequest other, String[] args)
            : base(args)
        {
            CallerAgent = other.CallerAgent;
            Output = other.Output;
            KeysRequired = false;
            SetCmdInfo(other);
        }


        public CmdRequest(string[] text, object callerIDORZero, OutputDelegate writeLine, ParseInfo command)
            : base(text)
        {
            CallerAgent = callerIDORZero;
            Output = writeLine;
            KeysRequired = false;
            SetCmdInfo(command);
        }
        public CmdRequest AdvanceArgs(int used)
        {
            StartArg += used;
            tokens = SplitOff(tokens, used);
            ParseTokens();
            return this;
        }

        public string[] GetProperty(string loc)
        {
            string[] args;
            if (GetAfter(loc, out args)) return args;
            return null;
        }
    }

    public class Parser : ParseInfo
    {
        const int MISSING = -1;
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

        public static char[] needSpacesArround = "+[]!".ToCharArray();

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
            string newstring = EscapeIfNeeded(args[p]);
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
            bool needsDoubleQuotes = s.Contains(" ");
            bool needsSQuotes = s.Contains("\"");
            if (needsSQuotes)
            {
                return "'" + s + "'";
            }
            if (needsDoubleQuotes) return "\"" + s + "\"";
            return s;
        }

        readonly string[] preps = { "of", "to", "in", "for", "with", "as", "by", "at", "from", "on", "is" };

        public readonly Dictionary<string, object> prepPhrases;
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
            : this(Args, Rejoin(Args ?? new string[0],0))
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

        private bool ContainsKey(string k)
        {
            bool atKey;
            if (ParamMap.ContainsKey(k)) return true;
            return IndexOf(k, false, out atKey) != MISSING;
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

        public bool ContainsFlag(string key)
        {
            EnsurePreParsed();
            if (ParamMap.ContainsKey(key)) return true;
            while (key.StartsWith("-"))
            {
                key = key.Substring(1);
            }
            bool atKey;
            return IndexOf(key, true, out atKey) != MISSING && atKey;
        }

        public int IndexOf(string key, bool requireKey, out bool atKey)
        {
            int i = 0;
            key = ToKey(key);
            foreach (string tok in tokens)
            {
                if (ToKey(tok).Equals(key))
                {
                    atKey = true;
                    return i;
                }
                i++;
            }
            if (requireKey)
            {
                atKey = true;
                return MISSING;
            }
            atKey = false;
            if (KeysRequired) return MISSING;
            int index = 0;
            EnsureVersionSelected();
            foreach (var param in VersionSelected.Parameters)
            {
                if (index >= tokens.Length) return MISSING;
                if (param.IsOptional)
                {
                    if (ToKey(tokens[index]) != key)
                    {
                        continue;
                    }
                }
                if (ToKey(param.Key) == key)
                {
                    return index;
                }
                index++;
            }
            return MISSING;

        }


        public bool HasAllRequiredKeys()
        {
            EnsureVersionSelected();
            foreach (NamedParam p in VersionSelected.Parameters)
            {
                if (p.IsOptional) continue;
                string key = ToKey(p.Key);
                bool fnd = false;
                foreach (var s in tokens)
                {
                    if (key == ToKey(s))
                    {
                        fnd = true;
                        break;
                    }
                }
                if (!fnd) return false;
            }
            return true;
        }
        private bool preparseStarted = false;
        private void EnsurePreParsed()
        {
            if (!EnsureVersionSelected()) return;
            if (preparseStarted) return;
            preparseStarted = true;
            ParseAndRemoveOptionalKeys();
            if (HasAllRequiredKeys())
            {
                KeysRequired = true;
            }
            else
            {
                KeysRequired = false;
            }
        }
        public int ParseAndRemoveOptionalKeys()
        {
            EnsureVersionSelected();
            int count = 0;            
            foreach (NamedParam p in VersionSelected.Parameters)
            {
                if (!p.IsOptional) continue;
                string key = ToKey(p.Key);
                bool atKey;
                int i = IndexOf(key, false, out atKey);
                if (i == MISSING)
                {
                    continue;
                }
                int startVal = i + (atKey ? 1 : 0);
                int len = ValueLen(key, null);
                if (len == 0 && atKey)
                {
                    ParamMap[key] = IsTrueString(tokens[i]);
                }
                else
                {
                    string[] arg = new List<string>(tokens).GetRange(startVal, len).ToArray();
                    ParamMap[key] = ChangeType(arg, p.Type);
                }
                count++;
                tokens = GetWithoutIndex(i, (atKey ? 1 : 0) + ValueLen(key, null));
            }
            return count;
        }

        private bool EnsureVersionSelected()
        {
            if (VersionSelected == null && ParameterVersions != null)
            {
                VersionSelected = ParameterVersions[0];
                if (ParameterVersions.Count != 1)
                {
                }
            }
            return VersionSelected != null;
        }

        private string[] GetWithoutIndex(int i, int len)
        {
            if (i == MISSING) return tokens;
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
            bool atKey;
            int i = IndexOf(key, false, out atKey);
            if (i == MISSING)
            {
                args = tokens;
                return false;
            }
            ModArgs = args = GetWithoutIndex(i, (atKey ? 1 : 0) + ValueLen(key, null));
            return i != MISSING;
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
            bool atKey;
            int i = IndexOf(key, false, out atKey);
            args = tokens;
            if (i == MISSING)
            {
                return false;
            }
            ModArgs = args = GetAfterIndex(i + (atKey ? 1 : 0) + ValueLen(key, null));
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

        public void ParseParams(ParseInfo parameters)
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
            return (T)ChangeType(value, typeof(T));
        }

        private static object ChangeType(object value, Type t)
        {
            return ScriptManager.ChangeType(value, t);
        }

        public bool TryGetValue<T>(string key, out T value)
        {
            EnsurePreParsed();
            object obj;
            if (ParamMap.TryGetValue(key, out obj))
            {
                value = (T) obj;
                return true;
            }
            int len;
            int at;
            bool atKey;
            if (TryGetValueInt<T>(key, out value, out at, out len, out atKey))
            {
                return true;
            }
            if (KeysRequired) return false;
            NamedParam parm = GetParm(key);
            if (parm.IsOptional)
            {
                return false;
            }
            value = ChangeType<T>(tokens[this.StartArg++]);
            return true;
        }

        public bool TryGetValueOr<T>(string key, int arg, out T value)
        {
            int len;
            int at;
            bool atKey;
            if (TryGetValueInt(key, out value, out at, out len, out atKey))
            {
                KeysRequired = true;
                return true;
            }
            int index = this.StartArg + arg;
            if (index >= tokens.Length)
            {
                value = default(T);
                return false;
            }
            value = ChangeType<T>(tokens[index]);
            return true;
        }

        public bool TryGetValueWithout<T>(string key, out T value, out string[] strings)
        {
            int len;
            int at;
            bool atKey;
            if (!TryGetValueInt(key, out value, out at, out len, out atKey))
            {
                strings = tokens;
                return false;
            }
            else
            {
                ModArgs = strings = GetWithoutIndex(at, len + (atKey ? 1 : 0));
                return true;
            }
        }

        public bool TryGetValueInt<T>(string key, out T value, out int found, out int len, out bool atKey)
        {
            EnsurePreParsed();
            key = ToKey(key);
            len = ValueLen(key, typeof (T));
            object ovalue;
            found = IndexOf(key, false, out atKey);
            if (ParamMap.TryGetValue(key, out ovalue))
            {
                value = ChangeType<T>(ovalue);
                return true;
            }
            if (found == MISSING)
            {
                value = default(T);
                len = 0;
                return false;
            }
            int startAt = found + (atKey ? 1 : 0);
            string[] after = new List<string>(tokens).GetRange(startAt, len).ToArray();
            value = ChangeType<T>(after);
            return true;
        }

        private int ValueLen(string key, Type t)
        {
            if (t != typeof(bool)) return 1;
            NamedParam kt = GetParm(key);
            if (kt.Type == typeof(bool)) return kt.IsOptional ? 0 : 1;
            return 1;
        }

        private NamedParam GetParm(string key)
        {
            key = ToKey(key);
            foreach (var param in VersionSelected.Parameters)
            {
                if (ToKey(param.Key) == key)
                {
                    return param;
                }
            }
            return default(NamedParam);
        }

        public void SetCmdInfo(ParseInfo info)
        {
            ParameterVersions = info.ParameterVersions;
        }


        public List<KeyParams> ParameterVersions { get; set; }

        protected KeyParams VersionSelected;
        public bool KeysRequired = true;

        static KeyParams SelectVersion(string[] tokens, List<KeyParams> ParameterVersions)
        {
            if (ParameterVersions == null || ParameterVersions.Count == 0)
            {
                return null;
            }
            KeyParams best = null;
            float bestScore = 99999999;
            foreach (var version in ParameterVersions)
            {
                float thisScore = TestSelectVersion(version.Parameters, tokens);

                if (thisScore <= bestScore)
                {
                    best = version;
                    bestScore = thisScore;
                }
            }
            return best;
        }

        public static float TestSelectVersion(NamedParam[] VersionSelected, string[] tokens)
        {
            int tokenLen = tokens.Length;
            int max;
            int requireds = 0;
            int optionals = 0;
            int requiredsG = 0;
            int optionalsG = 0;
            int argCurrent = 0;
            int used = 0;
            int len = 0;
            int argStart = used + len;
            foreach (NamedParam param in VersionSelected)
            {
                if (param.IsOptional) optionals += 1;
                else requireds += 1;
                object value;
                if (TryGetKey(param, argStart, tokens, out value, out used, out len))
                {

                    if (param.IsOptional)
                    {
                        optionalsG++;
                    }
                    else
                    {
                        requiredsG++;
                    }
                    argStart = used + len;
                }
            }
            if (requireds > tokenLen) return 99999;
            return requireds/(requiredsG + 1) - optionalsG/(optionals + 1);
        }

        private static bool TryGetKey(NamedParam param, int argStart, string[] args, out object o, out int used, out int len)
        {
            string key = ToKey(param.Key);
            used = 0;
            for (int i = argStart; i < args.Length; i++)
            {
                used = 1;
                string s = args[i];
                if (ToKey(s) != key) continue;
                if (param.Type == typeof (bool))
                {
                    o = true;
                    len = 1;
                    return true;
                }
                if (i + 1 < args.Length)
                {
                    o = args[i + 1];
                    len = 2;
                    return true;
                }
                len = MISSING;
                o = null;
                return true;
            }
            len = 0;
            o = null;
            return false;
        }

        protected void ParseTokens()
        {
            if (VersionSelected == null)
            {
                VersionSelected = SelectVersion(tokens, ParameterVersions);
                ParseTokens();
            }
            int tokenLen = tokens.Length;
            int skip = this.StartArg;
            int argCurrent = 0;
            if (VersionSelected == null)
            {
                return;
            }
            foreach (NamedParam param in VersionSelected.Parameters)
            {
                if (skip > 0)
                {
                    skip--;
                    continue;
                }
                if (argCurrent >= tokenLen) return;
                string key = ToKey(param.Key);

                if (param.IsOptional)
                {
                    bool wasBool = typeof(bool) == param.Type;
                    if (!KeyMatches(tokens[argCurrent], param))
                    {
                        if (wasBool)
                        {
                            ParamMap[key] = false;
                            continue;
                        }
                        ParamMap[key] = null;
                        continue;
                    }
                    if (wasBool)
                    {
                        ParamMap[key] = true;
                        argCurrent++;
                        continue;
                    }
                }
                int argsStart = argCurrent;
                int argsUsed;
                object value = ParseArg(param, param.Type, tokens, argsStart, out argsUsed);
                argCurrent += argsUsed;
                ParamMap[key] = value;
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

    public class KeyParams : IEquatable<KeyParams>
    {
        public NamedParam[] Parameters;

        public KeyParams(NamedParam[] namedParams)
        {
            Parameters = namedParams;
        }
        public static bool operator ==(KeyParams p1, KeyParams p2)
        {
            if (ReferenceEquals(null, p2)) return ReferenceEquals(null, p1);
            if (ReferenceEquals(null, p1)) return false;
            if (Equals(p1.Parameters, p2.Parameters)) return true;
            if (p1.Parameters.Length == p2.Parameters.Length)
            {
                return false;
            }
            return false;
        }

        public static bool operator !=(KeyParams p1, KeyParams p2)
        {
            return !(p1 == p2);
        }

        /// <summary>
        /// Indicates whether the current object is equal to another object of the same type.
        /// </summary>
        /// <returns>
        /// true if the current object is equal to the <paramref name="other"/> parameter; otherwise, false.
        /// </returns>
        /// <param name="other">An object to compare with this object.
        ///                 </param>
        public bool Equals(KeyParams other)
        {
            return this == other;
        }

        /// <summary>
        /// Determines whether the specified <see cref="T:System.Object"/> is equal to the current <see cref="T:System.Object"/>.
        /// </summary>
        /// <returns>
        /// true if the specified <see cref="T:System.Object"/> is equal to the current <see cref="T:System.Object"/>; otherwise, false.
        /// </returns>
        /// <param name="obj">The <see cref="T:System.Object"/> to compare with the current <see cref="T:System.Object"/>. 
        ///                 </param><exception cref="T:System.NullReferenceException">The <paramref name="obj"/> parameter is null.
        ///                 </exception><filterpriority>2</filterpriority>
        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != typeof (KeyParams)) return false;
            return Equals((KeyParams) obj);
        }

        /// <summary>
        /// Serves as a hash function for a particular type. 
        /// </summary>
        /// <returns>
        /// A hash code for the current <see cref="T:System.Object"/>.
        /// </returns>
        /// <filterpriority>2</filterpriority>
        public override int GetHashCode()
        {
            return (Parameters != null ? Parameters.GetHashCode() : 0);
        }
    }

    public interface ParseInfo
    {
        List<KeyParams> ParameterVersions { get; }
    }
}