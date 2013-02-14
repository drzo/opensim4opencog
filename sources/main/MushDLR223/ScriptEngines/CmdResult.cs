using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using MushDLR223.Utilities;

namespace MushDLR223.ScriptEngines
{
    [Flags]
    public enum CMDFLAGS
    {
        None = 0,
        Inherit = 32,
        /// <summary>
        /// Command *must* to build a return
        /// </summary>
        ForceResult = 1,
        /// <summary>
        /// Command does not *have* to build a return
        /// </summary>
        NoResult = 2,
        /// <summary>
        /// Force the command to be ran outside of a TaskQueue 
        /// </summary>
        ForceAsync = 4,
        /// <summary>
        /// The command to be ran inside of a TaskQueue 
        /// </summary>
        SynchronousChannel = 64,
        /// <summary>
        /// Force the command to complete before returning 
        /// </summary>
        ForceCompletion = 8,
        /// <summary>
        /// Force the command to complete before returning 
        /// </summary>
        IsConsole = 16,

        Foregrounded = ForceCompletion | ForceResult,
        Backgrounded = ForceAsync | NoResult,
        Console = ForceResult | IsConsole,
    }

    public interface CmdResult
    {
        bool Success { get; set; }
        bool IsCompleted { get; set; }
        bool InvalidArgs { get; set; }
        bool CompletedSynchronously { get; }
        string Message { get; set; }
        IEnumerable<string> Keys { get; }
        object this[string key] { get; }
        CmdRequest Request { get; }
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

        public static bool TryGetValue<T>(IDictionary<string, T> map, string key, out T res)
        {
            if (map.TryGetValue(key, out res)) return true;
            string nameToLower = key.ToLower();
            if (nameToLower != key && map.TryGetValue(nameToLower, out res)) return true;
            var name2 = Parser.ToCamelCase(key);
            if (name2 != key && map.TryGetValue(name2, out res)) return true;
            return false;
        }
        public static object GetValue(IDictionary<string, object> map, string key)
        {
            object res;
            if (TryGetValue(map, key, out res)) return res;
            return null;
        }

        public IEnumerable<string> Keys
        {
            get { lock (SyncRoot) return LockInfo.CopyOf(Results.Keys); }
        }

        public object this[string key]
        {
            get {
                lock (SyncRoot) return GetValue(Results, key);
            }
            set
            {
                lock (SyncRoot)
                {
                    Results[Parser.ToMapKey(key)] = value;
                }
            }
        }

        public CmdRequest Request
        {
            get { return null; }
        }

        public string ToPostExecString()
        {
            return ToString();
        }

        public object SyncRoot
        {
            get { return Results; }
        }

        public static ACmdResult Complete(string verb, string message, bool passFail)
        {
            return new ACmdResult(verb,  message, passFail);
        }
        public ACmdResult(string verb, string message, bool passFail)
        {
            Results = Parser.CreateMap();
            Results["verb"] = verb;
            Message = message;
            Success = passFail;
            IsCompleted = true;
            CompletedSynchronously = true;
            InvalidArgs = false;
        }
        public override string ToString()
        {
            return CmdRequest.ToPostExecString(this);
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

    }

    public class CmdRequest : Parser, ParseInfo, CmdResult
    {
        public static string cmdnameProp = "verb";
        protected bool thisBool(string key)
        {
            lock (SyncRoot) return ACmdResult.GetBool(ParamMap, key);
        }

        public CMDFLAGS CmdFlags
        {
            get
            {
                return (CMDFLAGS)this["CmdFlags"];
            }
            set
            {
                this["CmdFlags"] = value;
            }
        }

        public bool IsFFI
        {
            get
            {
                return thisBool("IsFFI");
            }
            set
            {
                this["IsFFI"] = value;
            }
        }
        public bool RunSync
        {
            get
            {
                return (CmdFlags & CMDFLAGS.ForceCompletion) != 0;
            }
            set
            {
                CmdFlags |= (value ? CMDFLAGS.ForceCompletion : CMDFLAGS.ForceAsync);
            }
        }
        public string CmdName
        {
            get { return "" + this[cmdnameProp]; }
            set { this[cmdnameProp] = value; }
        }
        public bool WantsResults
        {
            get
            {
                return (CmdFlags & CMDFLAGS.ForceResult) != 0;
            }
            set
            {
                CmdFlags |= (value ? CMDFLAGS.ForceResult : CMDFLAGS.NoResult);
            }
        }

        public static CmdRequest MakeCmdRequest(IDictionary<string, object> request)
        {
            return MakeCmdRequest("" + request[cmdnameProp], request, null);
        }
        public static CmdRequest MakeCmdRequest(IDictionary<string, object> request, IDictionary<string, object> result)
        {
            return MakeCmdRequest("" + request[cmdnameProp], request, result);
        }
        public static CmdRequest MakeCmdRequest(string cmdname, IDictionary<string, object> request)
        {
            return MakeCmdRequest(cmdname, request, null);
        }
        public static CmdRequest MakeCmdRequest(string cmdname, IDictionary<string, object> request, IDictionary<string, object> result)
        {
            request[cmdnameProp] = cmdname;
            var cmd = new CmdRequest(request);
            cmd.IsFFI = true;
            if (result != null) cmd.Results = result;
            return cmd;
        }
        public static implicit operator string[](CmdRequest request)
        {
            return request.tokens;
        }
        public object CallerAgent;
        public OutputDelegate Output;

        protected void WriteLine(string s, params object[] args)
        {
            if (s == null) return;
            var message = DLRConsole.SafeFormat(s, args);

            if (!String.IsNullOrEmpty(WriteLineResultName))
            {
                AppendResults(WriteLineResultName, message);
            }
            if (Output != null && Output != WriteLine) Output(message);
        }
        public string WriteLineResultName = "message";
        protected void AppendResults(string key, string format)
        {
            lock (Results)
            {
                object obj;
                if (!Results.TryGetValue(key, out obj))
                {
                    Results[key] = format;
                }
                else
                {
                    string before = "" + obj;
                    string newstring = before + "\n" + format;
                    Results[key] = newstring.TrimStart();
                }
            }
        }

        private IDictionary<string, object> _results;
        public IDictionary<string, object> Results
        {
            get
            {
                if (_results == null)
                {
                    return ParamMap;
                }
                return _results;
            }
            set
            {
                if (_results == value) return;
                if (_results == null)
                {
                    _results = value;
                    return;
                }
                _results = value;

            }
        }

        public CmdRequest(string verb, string args, object callerIDORZero, OutputDelegate writeLine, ParseInfo command)
            : base(args)
        {
            CmdFlags = CMDFLAGS.Inherit;
            CmdName = verb;
            CallerAgent = callerIDORZero;
            Output = writeLine;
            IsFFI = false;
            KeysRequired = false;
            SetCmdInfo(command);
        }

        private CmdRequest(IDictionary<string, object> dictionary)
            : base((string[])null)
        {
            CmdFlags = CMDFLAGS.Inherit;
            ParamMap = dictionary;
        }

        public CmdRequest AdvanceArgs(int used)
        {
            StartArg += used;
            tokens = SplitOff(tokens, used);
            ParseTokens();
            return this;
        }

        public event OutputDelegate CallbackEvents;
        public String Message
        {
            get
            {
                lock (SyncRoot)
                {
                    object res = GetValue(Results, "Message");
                    if (res == null)
                    {
                        return "no message";
                    }
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
            get
            {
                return thisBool("Success");
            }
            set
            {
                IsCompleted = true;
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

        public IEnumerable<string> Keys
        {
            get { lock (SyncRoot) return LockInfo.CopyOf(Results.Keys); }
        }

        public CmdRequest Request
        {
            get { return this; }
        }

        public string ToPostExecString()
        {
            return ToString();
        }

        public override string ToString()
        {
            return ToPostExecString(this);
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

        public static List<String> SkipPostExecKeys = new List<string> ();
        public static string ToPostExecString(CmdResult res)
        {
            if (SkipPostExecKeys.Count==0)
            {
                SkipPostExecKeys.Add(ToMapKey("Message"));
            }
            StringWriter tw = new StringWriter();

            if (res.IsCompleted) if (!res.Success) tw.WriteLine("ERROR: {0}", res.Message);

            foreach (string key in res.Keys)
            {
                if (SkipPostExecKeys.Contains(ToMapKey(key))) continue;
                tw.WriteLine(" " + key + " = \"" + res[key] + "\"");
            }
            string toString = tw.ToString();
            CmdRequest req = res.Request;
            if (req != null)
            {

                if ((req.CmdFlags & CMDFLAGS.IsConsole) != 0)
                {
                    toString = toString.Replace("\r", " ").Replace("\n", " ");
                }
            }
            return toString;
        }

        public CmdResult Complete(string verb, string message, bool successOrFalse)
        {
            Success = successOrFalse;
            if (!successOrFalse) this["FailureMessage"] = message;
            string msg = "" + GetValue(Results, "Message");
            if (string.IsNullOrEmpty(msg))
            {
                Message = message;
            }
            else if (!msg.Contains(message))
            {
                Message += message;
            }
            IsCompleted = true;
            return this;
        }
    }
}