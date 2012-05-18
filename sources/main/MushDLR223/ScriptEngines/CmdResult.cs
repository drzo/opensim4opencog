using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;

namespace MushDLR223.ScriptEngines
{
    public class CmdResult : IAsyncResult
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
                    object res;
                    if (!Results.TryGetValue("Message", out res)) return "no message";
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
               lock (SyncRoot)
                {
                    object res;
                    return Results.TryGetValue("InvalidArgs", out res) && (bool)res;
                }
           } 
           set
           {
               this["InvalidArgs"] = value;
           }
        }
        public bool Success
        {
            get
            {
                lock (SyncRoot)
                {
                    object res;
                    return Results.TryGetValue("Success", out res) && (bool)res;
                }
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
                lock (SyncRoot)
                {
                    object res;
                    return Results.TryGetValue("IsCompleted", out res) && (bool)res;
                }
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
                lock (SyncRoot)
                {
                    object res;
                    return Results.TryGetValue("CompletedSynchronously", out res) && (bool)res;
                }
            }
            set
            {
                this["CompletedSynchronously"] = value;
            }
        }
        protected object this[string name]
        {
            get
            {
                lock (SyncRoot)
                {
                    object res;
                    if (Results.TryGetValue(name.ToLower(), out res)) return res;
                    var name2 = ToCamelCase(name);
                    if (name2 != name && Results.TryGetValue(name.ToLower(), out res)) return res;
                    return null;
                }
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
        public CmdResult(string usage, bool b, IDictionary<string,object> resholder)
        {
            Results = resholder;
            Message = usage;
            Success = b;
            IsCompleted = true;
            CompletedSynchronously = true;
            InvalidArgs = false;
        }
        public CmdResult(string param1, bool param2)
            : this(param1, param2, new Dictionary<string, object>())
        {
        }
        public override string ToString()
        {
            if (!Success) return string.Format("ERROR: {0}", Message);
            return Message;
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
    }
}