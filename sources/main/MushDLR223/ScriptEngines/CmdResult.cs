using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;

namespace MushDLR223.ScriptEngines
{
    [Flags]
    public enum CMDFLAGS
    {
        Inherit = 0,
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
            var name2 = Parser.ToCamelCase(name);
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
                    Results[Parser.ToCamelCase(name)] = value;
                }
            }
        }

        public object SyncRoot
        {
            get { return Results; }
        }

        public static ACmdResult Complete(string verb, string message, bool passFail)
        {
            return ACmdResult.Complete(verb,  message, passFail);
        }
        public ACmdResult(string verb, string message, bool passFail)
        {
            Results = CmdRequest.CreateMap();
            Results["verb"] = verb;
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

    public class CmdRequest : Parser, ParseInfo, CmdResult
    {
        public static string cmdnameProp = "verb";
        protected object this[string name]
        {
            get
            {
                lock (SyncRoot)
                {
                    return ACmdResult.GetValue(ParamMap, name);
                }
            }
            set
            {
                lock (SyncRoot)
                {
                    ParamMap[ToKey(name)] = value;
                }
            }
        }
        protected bool thisBool(string key)
        {
            lock (SyncRoot) return ACmdResult.GetBool(ParamMap, key);
        }

        protected object SyncRoot
        {
            get { return ParamMap; }
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