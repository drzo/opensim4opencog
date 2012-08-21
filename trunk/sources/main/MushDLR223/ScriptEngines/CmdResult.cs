using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;

namespace MushDLR223.ScriptEngines
{
    public interface CmdResult
    {
        bool Success { get; set; }
        bool IsCompleted { get; set; }
        bool InvalidArgs { get; set; }
        bool CompletedSynchronously { get; }
        string Message { get; set; }
        string ToPostExecString();
    }
    public class ACmdResult : IAsyncResult, CmdResult
    {
        static public Dictionary<string, Type> ResultTypes = new Dictionary<string, Type>();
        public IDictionary<string, object> Results;

        public event OutputDelegate CallbackEvents; 
        public String Message
        {
            get
            {
                lock (SyncRoot)
                {
                    object res = GetValue(Results, "Message");
                    if (res == null) return "no message";
                    return res.ToString();
                }
            }
            set
            {
                string before = "" + this["Message"];
                string newstring = before + "\n" + value;
                Results["Message"] = newstring.TrimStart();
            }
        }
        public bool InvalidArgs
        {
           get
           {
               return thisBool("InvalidArgs");
           } 
           set
           {
               this["InvalidArgs"] = value;
           }
        }
        public bool Success
        {
            get {
                return thisBool("Success");
            }
            set
            {
                this["Success"] = value;
            }
        }
        public bool IsCompleted
        {
            get
            {
                return thisBool("IsCompleted");
            }
            set
            {
                this["IsCompleted"] = value;
            }
        }
        /// <summary>
        /// true if the asynchronous operation completed synchronously; otherwise, false.
        /// </summary>
        public bool CompletedSynchronously
        {
            get
            {
                return thisBool("CompletedSynchronously");
            }
            set
            {
                this["CompletedSynchronously"] = value;
            }
        }

        private bool thisBool(string key)
        {
            lock (SyncRoot) return GetBool(Results, key);
        }

        public static bool GetBool(IDictionary<string, object> map, string key)
        {
            object res;
            return TryGetValue(map, key, out res) && (bool) res;
        }

        public static bool TryGetValue<T>(IDictionary<string, T> map, string name, out T res)
        {
            if (map.TryGetValue(name, out res)) return true;
            string nameToLower = name.ToLower();
            if (nameToLower != name && map.TryGetValue(nameToLower, out res)) return true;
            var name2 = ToCamelCase(name);
            if (name2 != name && map.TryGetValue(name2, out res)) return true;
            return false;
        }
        public static object GetValue(IDictionary<string, object> map, string name)
        {
            object res;
            if (TryGetValue(map, name, out res)) return res;
            return null;
        }

        protected object this[string name]
        {
            get {
                lock (SyncRoot) return GetValue(Results, name);
            }
            set
            {
                lock (SyncRoot)
                {
                    Results[ToCamelCase(name)] = value;
                }
            }
        }

        public object SyncRoot
        {
            get { return Results; }
        }

        public ACmdResult(string message, bool passFail)
        {
            Results = ACmdResult.CreateMap();
            Message = message;
            Success = passFail;
            IsCompleted = true;
            CompletedSynchronously = true;
            InvalidArgs = false;
        }
        public override string ToString()
        {
            if (!Success) return string.Format("ERROR: {0}", Message);
            return ToPostExecString();
        }

        /// <summary>
        /// Gets a System.Threading.WaitHandle that is used to wait for an asynchronous operation to complete.
        /// </summary>
        /// A System.Threading.WaitHandle that is used to wait for an asynchronous operation to complete.
        public WaitHandle AsyncWaitHandle
        {
            get { throw new NotImplementedException(); }
        }

        /// <summary>
        /// Gets a user-defined object that qualifies or contains information about an asynchronous operation.
        /// </summary>
        /// Returns: A user-defined object that qualifies or contains information about an asynchronous operation.
        public object AsyncState
        {
            get
            {
                return this;/* throw new NotImplementedException();*/
            }
        }

        static readonly Dictionary<string, string> CamelCache = new Dictionary<string, string>();
        static readonly Dictionary<string, string> PrologCache = new Dictionary<string, string>();
        public static string ToCase(string name, Dictionary<string, string> cache, Func<string, string> toCase)
        {
            lock (cache)
            {
                string camel;
                if (cache.TryGetValue(name, out camel))
                {
                    return camel;
                }
                camel = toCase(name);
                cache[name] = camel;
                return camel;
            }
        }
        public static string ToCamelCase(string name)
        {
            return ToCase(name, CamelCache, ToCamelCase0);
        }
        public static string ToCamelCase0(string name)
        {

            if (name.Contains("-"))
            {
                // LISPY
                name = name.ToLower();
            }
            char[] camelToCharArray = name.ToCharArray();
            var ch = camelToCharArray[0];
            if (Char.IsUpper(ch))
            {
                return name;
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
        public static string ToPrologCase(string name)
        {
            return ToCase(name, PrologCache, ToPrologCase0);
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

        public string ToPostExecString()
        {
            string SkipKey = Parser.ToKey("message");
            StringWriter tw = new StringWriter();
            foreach (KeyValuePair<string, object> kv in Results)
            {
                if (Parser.ToKey(kv.Key) == SkipKey) continue;
                tw.WriteLine("" + kv.Key + " = " + kv.Value);
            }
            return tw.ToString();
        }
    }
}