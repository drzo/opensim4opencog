using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;

namespace MushDLR223.ScriptEngines
{
    public class Parser : ParseInfo
    {

        static public Dictionary<string, Type> ResultTypes = new Dictionary<string, Type>();

        public static bool GetBool(IDictionary<string, object> map, string key)
        {
            object res;
            return TryGetValue(map, key, out res) && (bool)res;
        }

        public static bool TryGetValue<T>(IDictionary<string, T> map, string key, out T res)
        {
            key = key.Trim("-+= :\"'".ToCharArray());
            if (map.TryGetValue(key, out res)) return true;
            string nameToLower = key.ToLower();
            if (nameToLower != key && map.TryGetValue(nameToLower, out res)) return true;
            var name2 = ToCamelCase(key);
            if (name2 != key && map.TryGetValue(name2, out res)) return true;
            return false;
        }
        public static object GetValue(IDictionary<string, object> map, string key)
        {
            object res;
            if (TryGetValue(map, key, out res)) return res;
            return null;
        }

        static readonly Dictionary<string, string> CamelCache = new Dictionary<string, string>();
        static readonly Dictionary<string, string> PrologCache = new Dictionary<string, string>();
        public static string ToCase(string key, Dictionary<string, string> cache, Func<string, string> toCase)
        {
            lock (cache)
            {
                string camel;
                if (cache.TryGetValue(key, out camel))
                {
                    return camel;
                }
                camel = toCase(key);
                cache[key] = camel;
                return camel;
            }
        }
        public static string ToMapKey(string key)
        {
            key = key.Trim("-+= :\"'".ToCharArray());
            return ToCamelCase(key);
        }
        public static string ToCamelCase(string key)
        {
            return ToCase(key, CamelCache, ToCamelCase0);
        }
        public static string ToCamelCase0(string key)
        {

            if (key.Contains("-"))
            {
                // LISPY
                key = key.ToLower();
            }
            char[] camelToCharArray = key.ToCharArray();
            var ch = camelToCharArray[0];
            if (Char.IsUpper(ch))
            {
                return key;
            }
            StringBuilder sb = new StringBuilder(camelToCharArray.Length);
            sb.Append(Char.ToUpper(ch));
            bool makeNextUpper = false;
            for (int i = 1; i < camelToCharArray.Length; i++)
            {
                ch = camelToCharArray[i];
                if (makeNextUpper)
                {
                    sb.Append(Char.ToUpper(ch));
                    makeNextUpper = false;
                    continue;
                }
                makeNextUpper = Char.IsSymbol(ch);
                if (makeNextUpper) continue;
                sb.Append(ch);
            }
            return sb.ToString();
        }
        public static string ToPrologCase(string key)
        {
            return ToCase(key, PrologCache, ToPrologCase0);
        }
        public static string ToPrologCase0(string pn)
        {
            bool cameCased = false;
            foreach (char c in pn)
            {
                if (Char.IsUpper(c) || c == '.' || c == '-')
                {
                    cameCased = true;
                    break;
                }
            }
            if (!cameCased) return pn;
            StringBuilder newname = new StringBuilder();
            bool lastCapped = true;
            bool lastUnderscored = true;
            foreach (char c in pn)
            {

                if (Char.IsUpper(c))
                {
                    if (lastCapped)
                    {
                        newname.Append(CharToLower(c));
                    }
                    else
                    {
                        if (!lastUnderscored) newname.Append('_');
                        newname.Append(CharToLower(c));
                        lastCapped = true;
                    }
                    lastUnderscored = false;
                }
                else
                {
                    if (c == '_' || c == '-')
                    {
                        lastCapped = false;
                        if (lastUnderscored) continue;
                        newname.Append('_');
                        lastUnderscored = true;
                        continue;
                    }
                    newname.Append(CharToLower(c));
                    lastCapped = false;
                    lastUnderscored = false;
                }
            }
            return newname.ToString();
        }

        private static char CharToLower(char c)
        {
            if (c == '-') return '_';
            return Char.ToLower(c);
        }

        public static IDictionary<string, object> CreateMap()
        {
            return new Dictionary<string, object>();
        }

        public string[] GetProperty(string key)
        {
            string[] args;
            object obj;
            object value = this[key];
            if (value is string[])
            {
                return (string[]) value;
            }
            if (value is string)
            {
                return ParseArguments((string) value);
            }
            int at;
            int len;
            int keyLen;
            Type gpt = null;
            int valueLen = ValueLen(key, gpt);
            if (TryGetValueInt(key, gpt, out value, out at, out len, out keyLen))
            {
                return GetRange(at + keyLen, valueLen);
            } 
            return new string[0];
            //return tokens;
        }

        const int MISSING = -1;
        protected int StartArg;

        public T GetValue<T>(string key)
        {
            return (T) ChangeType(GetValue(ParamMap, key), typeof (T));
        }

        public void SetValue<T>(string key, T value)
        {
            this[key] = value;
        }

        public static string[] ParseArguments(string str)
        {
            if (str == null) return null;
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

                        trimmed = RemoveQuotes(current);
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

            trimmed = RemoveQuotes(current);

            if (trimmed.Length > 0)
                list.Add(trimmed);

            return list.ToArray();
        }

        private static string RemoveQuotes(string trimmed)
        {
            trimmed = trimmed.Trim();
            if (trimmed.StartsWith("\"") && trimmed.EndsWith("\""))
            {
                return trimmed.Substring(1, trimmed.Length - 2);
            }
            if (trimmed.StartsWith("'") && trimmed.EndsWith("'"))
            {
                return trimmed.Substring(1, trimmed.Length - 2);
            }
            return trimmed;
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

        //readonly string[] preps = { "of", "to", "in", "for", "with", "as", "by", "at", "from", "on", "is" };

        public IDictionary<string, object> ParamMap;
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

        virtual public object SyncRoot
        {
            get
            {
                return (object)ParamMap ?? this;
            }
        }
        virtual public object this[string key]
        {
            get
            {
                lock (SyncRoot)
                {
                    return GetValue(ParamMap, key);
                }
            }
            set
            {
                lock (SyncRoot)
                {
                    ParamMap[ToMapKey(key)] = value;
                }
            }
        }
        virtual public string GetString(string key)
        {
            EnsurePreParsed();
            var v = this[key];
            if (v != null) return "" + v;
            return Rejoin(GetProperty(key), 0);
        }
        public Parser(string[] Args)
            : this(Args, Args == null ? null : Rejoin(Args, 0))
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

        protected Parser(string[] tokes, string _str)
        {
            str = _str;
            tokens = tokes;
            ParamMap = new Dictionary<string, object>(); // new NameValueCollection();

            if (tokens != null || tokes.Length > 0)
            {
                PrepTokens();
            }
        }
        bool prepedTokens;            
        private void PrepTokens()
        {
            if (tokens == null || tokens.Length == 0) return;
            if (prepedTokens) return;
            prepedTokens = true;            
            string currentPrep = "";
            // sometimes ParseArgumetns throw exception

            bool firstTok = true;

            string lastKey = null;
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
                        Add(prep, "False");
                        continue;
                    }
                    if (lastChar == '+')
                    {
                        prep = prep.Substring(0, lenM1);
                        Add(prep, "True");
                        continue;
                    }

                    if (prep.StartsWith("--"))
                    {
                        Add(prep, "True");
                        lastKey = prep;
                        continue;
                    }
                    else
                    {
                        if (lastKey != null)
                        {
                            Add(lastKey, prep);
                        }
                    }
                    lastKey = null;
                }


                if (prep.Contains(splitter) && prep.StartsWith("-"))
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

                if (prep.StartsWith("-"))
                {
                    currentPrep = prep;
                    firstTok = true;
                }

                else
                {
                    if (currentPrep == "")
                    {
                    }
                    else
                    {

                        EnsurePrepKeyAsString(currentPrep);
                        if (!firstTok) 
                            SetValue(currentPrep, GetString(currentPrep) + " " + prep);
                        else
                            SetValue(currentPrep, prep);
                        firstTok = false;
                    }
                }
            }       
        }

        private void EnsurePrepKeyAsString(string key)
        {
            if (!ParamMap.ContainsKey(ToMapKey(key))) ParamMap[ToMapKey(key)] = "";
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

        public bool ContainsKey(string key)
        {
            int keyLen;
            if (ParamMap.ContainsKey(ToMapKey(key))) return true;
            return IndexOf(key, false, out keyLen) != MISSING;
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
        private static bool IsFalseString(string v)
        {
            if (v == null) return false;
            if (v == "")
            {
                return true;
            }
            v = v.ToLower();
            if (v.StartsWith("no") || v == "false" || v == "nil" || v.StartsWith("-")) return true;
            return false;
        }

        private void Add(string key, string v)
        {
            this[key] = v;
        }

        public static string ToKey(string key)
        {
            key = key.Trim("-+= :\"'".ToCharArray()).Replace(" ", "_");
            return ToCamelCase(key);
            return key.ToLower();
        }

        public bool IsTrue(string key)
        {
            if (ContainsKey(key))
            {
                object value = this[key];
                if (value is bool) return (bool) value;
                if (!IsTrueString("" + value)) return false;
                return true;
            }
            return false;
        }

        public bool ContainsFlag(string key)
        {
            EnsurePreParsed();
            if (ParamMap.ContainsKey(ToMapKey(key))) return true;
            int keyLen;
            return IndexOf(key, true, out keyLen) != MISSING && keyLen > 0;
        }

        public int IndexOf(string key, bool requireKey, out int keyLen)
        {
            int i = 0;
            key = ToKey(key);
            foreach (string tok in tokens)
            {
                if (ToKey(tok).Equals(key))
                {
                    keyLen = 1;
                    return i;
                }
                i++;
            }
            if (requireKey)
            {
                keyLen = 1;
                return MISSING;
            }
            keyLen = 0;
            if (KeysRequired) return MISSING;
            return GetIndex(key, requireKey);
        }

        private int GetIndex(string key, bool requireKey)
        {
            int index = 0;
            int optionalOrdinal = 0;
            EnsureVersionSelected();
            foreach (var param in VersionSelected.Parameters)
            {
                if (index >= tokens.Length) return MISSING;
                if (param.IsOptional)
                {
                    if (!param.IsFlag) optionalOrdinal++;
                    if (requireKey && ToKey(tokens[index]) != key)
                    {
                        continue;
                    }
                    if (optionalOrdinal > 1)
                    {
                        return MISSING;
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
            PrepTokens();
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
                string key = p.Key;
                int keyLen;
                int i = IndexOf(key, false, out keyLen);
                if (i == MISSING)
                {
                    continue;
                }
                int startAt = i + keyLen;
                int len = ValueLen(key, null);
                len = LenCheck(startAt, len);
                if (len == 0 && keyLen > 0)
                {
                    this[key] = IsTrueString(tokens[i]);
                }
                else
                {
                    string[] arg = GetRange(startAt, len);
                    this[key] = ChangeType(arg, p.Type);
                }
                count++;
                tokens = GetWithoutIndex(i, keyLen + len);
            }
            return count;
        }

        private bool EnsureVersionSelected()
        {
            if (VersionSelected == null)
            {
                if (ParameterVersions == null || ParameterVersions.Count == 0)
                {
                    return false;
                }
                VersionSelected = SelectVersion(tokens, ParameterVersions);
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
            for (int j = i + len; j < tokens.Length; j++)
            {
                p.Add(tokens[j]);
            }
            return ModArgs = p.ToArray();
        }

        public bool GetWithoutFlag(string key, out string[] args)
        {
            return GetWithout(key, out args, typeof (bool));
        }
        public bool GetWithout(string key, out string[] args, Type suggest)
        {
            int keyLen;
            int i = IndexOf(key, false, out keyLen);
            if (i == MISSING)
            {
                args = tokens;
                return false;
            }
            int len = ValueLen(key, suggest);
            len = LenCheck(i + keyLen, len);
            ModArgs = args = GetWithoutIndex(i, keyLen + len);
            return i != MISSING;
        }

        private int LenCheck(int startAt, int len)
        {
            if (len >= 0) return len;
            len = tokens.Length - startAt;
            return len;
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
            int keyLen;
            int i = IndexOf(key, false, out keyLen);
            args = tokens;
            if (i == MISSING)
            {
                return false;
            }
            int len = ValueLen(key, null);
            len = LenCheck(i + keyLen, len);
            ModArgs = args = GetAfterIndex(i + keyLen + len);
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
            var v0 = this[key];
            if (v0 == null) return false;
            if (v0 is bool)
            {
                value = (bool) v0;
                return true;
            }
            var v = "" + v0;
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
            object obj;
            if (!TryGetValueOfType(key, typeof (T), out obj))
            {
                value = default(T);
                return false;
            }
            value = ChangeType<T>(obj);
            return true;
        }

        public bool TryGetValueOfType(string key, Type t, out object value)
        {
            EnsurePreParsed();
            object obj;
            if (ParamMap.TryGetValue(ToMapKey(key), out obj))
            {
                value = ChangeType(obj, t);
                return true;
            }
            int len;
            int at;
            int keyLen;
            if (TryGetValueInt(key, t, out value, out at, out len, out keyLen))
            {
                return true;
            }
            if (KeysRequired) return false;
            NamedParam parm = GetParm(key);
            if (parm.IsOptional)
            {
                return false;
            }
            if (this.StartArg >= tokens.Length)
            {
                return false;
            }
            len = LenCheck(StartArg, len);
            value = ChangeType(GetRange(StartArg, len), t);
            StartArg += len;
            return true;
        }

        private string[] GetRange(int start, int len)
        {
            if (len < 0) len = LenCheck(start, len);
            return new List<string>(tokens).GetRange(start, len).ToArray();
        }

        public bool TryGetValueOr<T>(string key, int arg, out T value)
        {
            int len;
            int at;
            int keyLen;
            object obj;
            if (TryGetValueInt(key, typeof(T), out obj, out at, out len, out keyLen))
            {
                value = ChangeType<T>(obj);
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

        public bool TryGetValueWithout<T>(string key, out T value, bool modify, out string[] argsWithout)
        {
            int len;
            int at;
            int keyLen;
            object obj;
            if (!TryGetValueInt(key, typeof(T), out obj, out at, out len, out keyLen))
            {
                argsWithout = tokens;
                value = default(T);
                return false;
            }
            else
            {
                value = ChangeType<T>(obj);
                argsWithout = GetWithoutIndex(at, len + keyLen);
                if (modify) tokens = argsWithout;
                return true;
            }
        }

        private bool TryGetValueInt(string key, Type t, out object value, out int found, out int len, out int keyLen)
        {
            EnsurePreParsed();
            key = ToKey(key);
            object ovalue;
            found = IndexOf(key, false, out keyLen);
            len = ValueLen(key, t);
            if (found == MISSING)
            {
                if (ParamMap.TryGetValue(key, out value))
                {
                    return true;
                }
                value = null;
                len = 0;
                return false;
            }
            int startAt = found + keyLen;
            len = LenCheck(startAt, len);
            value = GetRange(startAt, len);
            return true;
        }

        private int ValueLen(string key, Type t)
        {            
            NamedParam kt = GetParm(key);
            t = t ?? kt.Type;
            if (t == null)
            {
                return 0;
            }
            if (t == typeof(bool)) return kt.IsOptional ? 0 : 1;
            if (kt.IsRest || (t != null && t.IsArray))
            {
                return -1;
            }
            return 1;
        }

        private NamedParam GetParm(string key)
        {
            key = ToKey(key);
            if (VersionSelected == null) return default(NamedParam);
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

        static KeyParams SelectVersion(string[] tokens, IList<KeyParams> ParameterVersions)
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
            return best ?? ParameterVersions[0];
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
            EnsureVersionSelected();
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
                string key = param.Key;

                if (param.IsOptional)
                {
                    bool wasBool = typeof(bool) == param.Type;
                    if (!KeyMatches(tokens[argCurrent], param))
                    {
                        if (wasBool)
                        {
                            this[key] = false;
                            continue;
                        }
                        this[key] = null;
                        continue;
                    }
                    if (wasBool)
                    {
                        this[key] = true;
                        argCurrent++;
                        continue;
                    }
                }
                int argsStart = argCurrent;
                int argsUsed;
                object value = ParseArg(param, param.Type, tokens, argsStart, out argsUsed);
                argCurrent += argsUsed;
                this[key] = value;
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
        /// true if the current object is equal to the <paramref key="other"/> parameter; otherwise, false.
        /// </returns>
        /// <param key="other">An object to compare with this object.
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
        /// <param key="obj">The <see cref="T:System.Object"/> to compare with the current <see cref="T:System.Object"/>. 
        ///                 </param><exception cref="T:System.NullReferenceException">The <paramref key="obj"/> parameter is null.
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

    public class KeyStringComparer : IEqualityComparer<string>
    {
        #region Implementation of IEqualityComparer<string>

        /// <summary>
        /// Determines whether the specified objects are equal.
        /// </summary>
        /// <returns>
        /// true if the specified objects are equal; otherwise, false.
        /// </returns>
        /// <param name="x">The first object of type <paramref name="T"/> to compare.
        ///                 </param><param name="y">The second object of type <paramref name="T"/> to compare.
        ///                 </param>
        public bool Equals(string x, string y)
        {
            return Parser.ToMapKey(x) == Parser.ToMapKey(y);
        }

        /// <summary>
        /// Returns a hash code for the specified object.
        /// </summary>
        /// <returns>
        /// A hash code for the specified object.
        /// </returns>
        /// <param name="obj">The <see cref="T:System.Object"/> for which a hash code is to be returned.
        ///                 </param><exception cref="T:System.ArgumentNullException">The type of <paramref name="obj"/> is a reference type and <paramref name="obj"/> is null.
        ///                 </exception>
        public int GetHashCode(string obj)
        {
            return Parser.ToMapKey(obj).GetHashCode();
        }

        #endregion
    }
}