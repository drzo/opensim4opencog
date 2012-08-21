using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using Cogbot;
using Cogbot.World;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using System.Reflection;
using OpenMetaverse.StructuredData;
using Simulator = OpenMetaverse.Simulator;

namespace Cogbot.Actions
{

    public class PrimSpec : DenotingAnotherType
    {
        public Type ImplementationType
        {
            get { return typeof(List<SimObject>); }
        }
    }

    public class AgentSpec : DenotingAnotherType
    {
        public Type ImplementationType
        {
            get { return typeof(List<SimAvatar>); }
        }
    }

    static public class Htmlize
    {
        public static string NoEnts(string example)
        {
            if (example == null)
            {
                ///  return null;
            }
            return example.Replace("\"", "&qt;").Replace("<", "&lt;").Replace(">", "&gt;").Replace("\r\n", "<br>").Replace("\n", "<br>");
        }

        public static string WikiBC(string named)
        {
            return Wiki("BotCommands#" + named.Replace(" ", ""), named);
        }

        public static string Wiki(string page, string named)
        {
            return "<a href='wiki/" + page + "'>" + named + "</a>";
        }
    }

    public enum CommandCategory : int
    {
        Parcel,
        Appearance,
        Movement,
        Simulator,
        Communication,
        Inventory,
        Objects,
        Voice,
        BotClient,
        Friends,
        Groups,
        Other,
        Unknown,
        Search,
        Money,
        Security
    }

    /// <summary>
    /// Command is complete enough to be called by the foriegn function interface and from console
    /// </summary>
    public interface FFIComplete : FFIMarker
    {
    }
    /// <summary>
    /// Some design work must be done to decide how FFI will construct arguments and what results are returned
    /// Should be bugged if it is needed
    /// Command is still operational via console
    /// </summary>
    public interface FFITODO : FFIMarker
    {
    }
    /// <summary>
    /// THe FFI arleady has a better way to call and the command should not be used
    /// Command is still operational via console
    /// </summary>
    public interface FFINOUSE : FFIMarker
    {
    }
    /// <summary>
    /// Has some FFI state subclass
    /// </summary>
    public interface FFIMarker
    {
    }
    /// <summary>
    /// An interface for commands is only invoked on Region mastering bots
    /// Such as terrain uploads and simulator info (10 bots doing the command at once will create problems)
    /// Non region master bots are thinner clients and usually not fit for object tracking
    /// </summary>
    public interface RegionMasterCommand : BotCommand, SynchronousCommand
    {
    }
    /// <summary>
    /// An interface for commands is only invoked on Grid mastering bots
    /// Such as Directory info requests (10 bots doing the command at once will create problems)   
    /// </summary>
    public interface GridMasterCommand : BotCommand
    {
    }
    /// <summary>
    /// An interface for commands that do not target any specific bots
    ///  Such as pathsystem maintainance or application commands
    ///  The gridClient used though will be GridMaster
    /// </summary>
    public interface SystemApplicationCommand : BotCommand
    {
    }

    /// <summary>
    /// An interface for commands that do not require a connected grid client
    /// such as Login or settings but still targets each bot individually
    /// </summary>
    public interface BotSystemCommand : BotCommand
    {
    }

    /// <summary>
    /// An interface for commands that DO REQUIRE a connected grid client
    /// such as say,jump,movement
    /// </summary>    
    public interface BotPersonalCommand : BotCommand, SynchronousCommand
    {
    }
    /// <summary>
    /// An interface for commands that have to move thru a single TODO queue
    /// </summary>    
    public interface SynchronousCommand
    {
    }
    /// <summary>
    /// An interface for commands that are mainly informational
    /// </summary>    
    public interface AsynchronousCommand
    {
    }    
    /// <summary>
    /// An interface for commands that require a windowing interface
    /// </summary>    
    public interface GUICommand : AsynchronousCommand
    {
    }
    /// <summary>
    /// An interface for commands that are not recreated per call instance
    /// </summary>
    public interface BotStatefullCommand : BotCommand, IDisposable
    {
    }
    public interface BotCommand : MushDLR223.ScriptEngines.ScriptedCommand
    {
    }
    public class CommandInstance
    {
        public CommandInfo CmdInfo;
        public Command MakeInstance(BotClient client)
        {
            if (WithBotClient != null)
            {
                if (WithBotClient._mClient == client)
                {
                    return WithBotClient;
                }
            }
            var cmd = (Command)CmdInfo.CmdTypeConstructor.Invoke(new object[] { client });
            cmd.TheBotClient = client;
            WithBotClient = cmd;
            if (CmdInfo.IsStateFul) WithBotClient = cmd;
            return cmd;
        }
        public Command WithBotClient;
        public bool IsStateFul
        {
            get
            {
                return CmdInfo.IsStateFul;
            }
        }
        public Type CmdType
        {
            get
            {
                return CmdInfo.CmdType;
            }
        }

        public string Name { get; set; }

        public CommandInstance(Command describe)
        {
            WithBotClient = describe;
            CmdInfo = describe.GetCmdInfo();
            Name = CmdInfo.Name;
        }
    }
    public class CommandInfo: ParseInfo
    {
        public static Dictionary<Type, CommandInfo> MadeInfos = new Dictionary<Type, CommandInfo>();

        public bool IsStateFul;
        public string Category;
        public string Name { get; set; }
        public string helpString;  // overview
        public string usageString;   // after the colon of Usage:
        public bool IsGridClientCommand = false;

        public bool IsObsolete
        {
            get
            {
                if (CmdType.GetCustomAttributes(typeof(ObsoleteAttribute), false) != null) return true;
                return false;
            }
        }

        /// <summary>
        /// Introspective Parameters for calling command from code
        /// </summary>
        public List<KeyParams> ParameterVersions { get; set; }
        public KeyParams ResultMap { get; set; }

        public Type CmdType;
        public ConstructorInfo CmdTypeConstructor;
        public string Description
        {
            get
            {
                return helpString;
            }
        }
        public String ToPrologString()
        {
            return "command(" + ToPLAtomStr(Name) + "," + ToPLAtomStr(Description) + "," + ToPLAtomStr(usageString) +
                   "," + ToPLAtomStr(ParameterVersions.ToArray()) + "," + ToPLAtomStr(ResultMap) + ")";
        }

        private string ToPLAtomStr(Array ar)
        {
            if (ar == null) return "_";
            if (ar.Length == 0) return "[]";
            StringBuilder sb = new StringBuilder("[" + ToPLAtomStr(ar.GetValue(0)));
            for (int i = 1; i < ar.Length; i++)
            {
                sb.Append("," + ToPLAtomStr(ar.GetValue(i)));
            }
            return sb.ToString() + "]";
        }

        private string ToPLAtomStr(object name)
        {
            if (name == null) return "_";
            if (name is Array) return ToPLAtomStr((Array)name);
            if (name is String) return ToPLAtomStr((String)name);
            if (name is Type) return ToPLAtomStr(((Type)name).Name);
            if (name is NamedParam) return ToPLAtomStr(((NamedParam)name));
            return ToPLAtomStr("" + name);
        }
        private string ToPLAtomStr(String name)
        {
            if (name == null) return "_";
            return "\"" + name.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\"";
        }
        private string ToPLAtomStr(NamedParam nvc)
        {
            if (nvc == null) return "_";
            return (nvc.IsOptional ? "o" : "p") + "(" + ToPLAtomStr(nvc.Key) + "," + ToPLAtomStr(nvc.Type) + "," +
                   ToPLAtomStr(nvc.Comment) + ")";
        }
        public CommandInfo(Command live)
        {
            LoadFromCommand(live);
        }
        public void LoadFromCommand(Command live)
        {
            CmdType = live.GetType();
            lock (MadeInfos)
            {
                MadeInfos[CmdType] = this;
            }
            IsStateFul = live.IsStateFull || live is BotStatefullCommand;
            Name = live.Name;
            usageString = live.Details;
            if (ParameterVersions == null) ParameterVersions = live.ParameterVersions;
            else
            {
                foreach (var v in live.ParameterVersions)
                {
                    if (!ParameterVersions.Contains(v)) ParameterVersions.Add(v);
                }
            }
            helpString = live.Description;
            Category = "" + live.Category;
            ResultMap = live.ResultMap;
            CmdTypeConstructor = CmdType.GetConstructors()[0];
            IsGridClientCommand = CmdTypeConstructor.GetParameters()[0].ParameterType == typeof(GridClient);
        }


        public bool Matches(string str)
        {
            return ("" + Name + " " + Description).Contains(str);
        }

        public Command MakeInstanceCM(BotClient o)
        {
            var cmd = (Command)CmdTypeConstructor.Invoke(new object[] { o });
            cmd.TheBotClient = o;            
            return cmd;
        }


        public static string ToBNF(NamedParam[] parameters)
        {
            string usage = "";
            foreach (var p in parameters)
            {
                string argstring = p.Key;
                if (p.IsOptional)
                {
                    argstring = string.Format("[{0}]", argstring);
                }
                else
                {
                    argstring = string.Format("<{0}>", argstring);
                }
                usage += " " + argstring;
            }
            return usage;
        }
    }

    public abstract partial class Command : IComparable
    {
        private string _taskQueueNameOrNullSet;
        public virtual string TaskQueueNameOrNull
        {
            get { return _taskQueueNameOrNullSet ?? (ImpliesSync ? "OneAtATimeQueue" : null); }
            set { _taskQueueNameOrNullSet = value; }
        }
        public virtual void MakeInfo()
        {

        }
        public CommandInfo GetCmdInfo()
        {
            CommandInfo ci;
            lock (CommandInfo.MadeInfos)
            {
                if (!CommandInfo.MadeInfos.TryGetValue(GetType(), out ci))
                {
                    MakeInfo();
                    DefaultResultMap();
                    ci = CommandInfo.MadeInfos[GetType()] = new CommandInfo(this);
                }
            }
            return ci;
        }

        public bool IsStateFull;
        public CommandCategory Category;
        public string Name { get; set; }
        protected string helpString;
        protected string usageString;

        virtual public string Description
        {
            get
            {
                return helpString;
            }
            set
            {
                if (String.IsNullOrEmpty(value)) return;
                int half = value.ToLower().IndexOf("usage");
                if (half == -1)
                {
                    half = value.ToLower().IndexOf("use");
                }
                if (half == -1)
                {
                    helpString = value;
                    return;
                }
                helpString = value.Substring(0, half).TrimEnd();
                Details = value.Substring(half);

            }
        }

        public virtual string Details
        {
            get { return usageString; }
            set
            {
                value = value.Trim().Replace("Usage:", " ").Replace("usage:", " ").Replace("Use:", " ").Trim();
                if (string.IsNullOrEmpty(usageString))
                {
                    usageString = value + "<br/>";
                    return;
                }
                if (!usageString.Contains(value))
                {
                    usageString += value + "<br/>";
                }
            }
        }

        public List<KeyParams> _parameterVersions;
        /// <summary>
        /// Introspective Parameters for calling command from code
        /// </summary>
        public List<KeyParams> ParameterVersions
        {
            get
            {
                if (_parameterVersions == null) 
                {
                    _parameterVersions = new List<KeyParams>();
                }
                return _parameterVersions;
            }
            set
            {
                foreach (var paramse in value)
                {
                    AddVersion(paramse);
                }
            }
        }
        public KeyParams Parameters
        {
            get
            {
                if (VersionSelected != null) return VersionSelected;
                if (ParameterVersions == null || ParameterVersions.Count == 0) return null;
                return ParameterVersions[0];
            }
            set
            {
                AddVersion(value);
                AddUsage(value, Description);
            }
        }

        private void AddVersion(NamedParam[] value)
        {
            AddVersion(new KeyParams(value));

        }
        private void AddVersion(KeyParams value)
        {
            VersionSelected = value;
            var copy = ParameterVersions;
            if (!copy.Contains(VersionSelected))
            {
                copy.Add(VersionSelected);
            }
        }

        public KeyParams ResultMap;

        /// <summary>
        /// Show commandusage
        /// </summary>
        /// <returns>CmdResult Failure with a string containing the parameter usage instructions</returns>
        public virtual CmdResult ShowUsage()
        {
            return ShowUsage(Details);
        }
        public virtual CmdResult ShowUsage(string usg)
        {
            CmdResult res = Failure("Usage: //" + usg);
            res.InvalidArgs = true;
            return res;
        }

        public CmdRequest CurrentRequest
        {
            get
            {
                if (_currentRequest != null) return _currentRequest;
                return _lastRequest;
            }
            set
            {
                _currentRequest = value;
                _lastRequest = value;
            }
        }

        [ThreadStatic]
        private CmdRequest _currentRequest;
        private CmdRequest _lastRequest;


        private IDictionary<string, object> _results;
        public IDictionary<string, object> Results
        {
            get
            {
                if (_results == null)
                {
                    return CurrentRequest.Results;
                }
                return _results;
            }
            /*
            set
            {
                if (_results == value) return;
                if (_results == null)
                {
                    _results = value;
                    return;
                }
                _results = value;

            }*/
        }



        private OutputDelegate _writeLine;

        protected void WriteLine(string s, params object[] args)
        {
            if (s == null) return;
            var message = DLRConsole.SafeFormat(s, args);

            if (!String.IsNullOrEmpty(WriteLineResultName))
            {
                AppendResults(WriteLineResultName, message);
            }
            LocalWL(message);
        }
        public OutputDelegate WriteLineDelegate
        {
            private get
            {
                if (_writeLine == null)
                {
                    return StaticWriteLine;
                }
                return _writeLine;
            }
            set
            {
                if (value == null)
                {
                    _writeLine = StaticWriteLine;
                    return;
                }
                _writeLine = value;
            }
        }

        private int success = 0, failure = 0;

        public Command()
            : this(null)
        {
            if (this is BotStatefullCommand && !(this is SystemApplicationCommand))
            {
                // DLRConsole.DebugWriteLine("" + this + " is not a BotStatefullCommand?!");                
            }
        } // constructor

        private void StaticWriteLine(string s, params object[] args)
        {
            if (_mClient != null) _mClient.WriteLine(Name + ": " + s, args);
        }

        public Command(BotClient bc)
        {
            _mClient = bc;
            WriteLineDelegate = StaticWriteLine;
            Name = GetType().Name.Replace("Command", "");
            if (!(this is BotCommand))
            {
                DLRConsole.DebugWriteLine("" + this + " is not a BotCommand?!");
            }
            if (this is BotPersonalCommand)
            {
                //Parameters = CreateParams();
                Category = CommandCategory.Other;
            }
            if (this is BotSystemCommand)
            {
                //Parameters = CreateParams();
                Category = CommandCategory.Simulator;
            }
            if (this is RegionMasterCommand)
            {
                // Parameters = new[] { new NamedParam(typeof(Simulator), null) };
                Category = CommandCategory.Simulator;
            }
            if (this is SystemApplicationCommand)
            {
                // Parameters = CreateParams();
                Category = CommandCategory.BotClient;
            }
            if (this.GetType().Namespace.ToString() == "Cogbot.Actions.Movement")
            {
                Category = CommandCategory.Movement;
            }
        } // constructor



       /* /// <summary>
        /// 
        /// </summary>
        /// <param name="verb"></param>
        /// <param name="args"></param>
        public virtual CmdResult acceptInput(string verb, Parser args, OutputDelegate writeLine)
        {
            success = failure = 0;
            WriteLineDelegate = writeLine;
            try
            {
                Results.Clear();
                if (args == null) return Failure("No Args (Parse error)");
                CallerID = CogbotHelpers.NonZero(CallerID, UUID.Zero);
                return ExecuteRequestSyn(new CmdRequest(args.tokens, fromAgentID, writeLine, this.GetCmdInfo()));
            }
            catch (Exception e)
            {
                return Failure("" + e);
            }
        } // method: acceptInput
        */

        internal BotClient _mClient = null;

        public BotClient Client
        {
            get
            {
                return TheBotClient;
            }
            set
            {
                TheBotClient = value;
            }
        }


        public BotClient TheBotClient
        {
            get
            {
                if (_mClient == null)
                {
                    Failure("trying to access TheBotClient with an unknown client!?");
                    DLRConsole.DebugWriteLine("" + this + " has no TheBotClient?!");
                    return WorldObjects.GridMaster.client;
                }
                return _mClient;
            }
            set
            {
                _mClient = value;
            }
        }

        public ClientManager ClientManager
        {
            get
            {
                if (_mClient != null) return _mClient.ClientManager;
                return ClientManager.SingleInstance;
            }
        }

        public Cogbot.WorldObjects WorldSystem
        {
            get
            {
                if (_mClient == null)
                {
                    Failure("trying to access world with an unknown client!?");
                    return WorldObjects.GridMaster;
                }
                return _mClient.WorldSystem;
            }
        }

        public SimAvatarClient TheSimAvatar
        {
            get
            {
                if (_mClient == null)
                {
                    Failure("trying to access an avatar with an unknown client!?");
                    return WorldSystem.TheSimAvatar;
                }
                return _mClient.TheSimAvatar;
            }
        }

        public abstract CmdResult ExecuteRequest(CmdRequest args);
        public CmdResult ExecuteRequestSyn(CmdRequest args)
        {
            if (this is BotPersonalCommand)
            {
                if (!Client.IsLoggedInAndReady)
                {
                    return Failure("Not yet logged in!");
                }
            }
            CurrentRequest = args;
            success = failure = 0;
            var wlpre = this.WriteLineDelegate;
            this.WriteLineDelegate = args.Output;
            try
            {
                var cr = ExecuteRequest(args);
                cr.IsCompleted = true;
                return cr;
            }
            finally
            {
                WriteLineDelegate = wlpre;
            }
        }

        public int CompareTo(object obj)
        {
            if (obj is Command)
            {
                Command c2 = (Command)obj;
                return Category.CompareTo(c2.Category);
            }
            else
                throw new ArgumentException("Object is not of type Command.");
        }


        // Helpers

        protected Vector3 GetSimPosition()
        {
            return TheSimAvatar.SimPosition;
        }

        public UUID UUIDParse(string p)
        {
            UUID uuid = UUID.Zero;
            int argsUsed;
            if (UUIDTryParse(new[] { p }, 0, out uuid, out argsUsed)) return uuid;
            return UUID.Parse(p);
        }

        public bool UUIDTryParse(string[] args, int start, out UUID target, out int argsUsed)
        {
            return WorldSystem.UUIDTryParse(args, start, out target, out argsUsed);
        }

        public CmdResult Failure(string message)
        {
            var Name = "Failure";
            if (!message.ToLower().Contains(Name.ToLower()))
            {
                message = Name + ": " + message;
            }
            failure++;
            return Result(message, false);
        }

        public CmdResult Success(string message)
        {
            var Name = "Success";
            if (!message.ToLower().Contains(Name.ToLower()))
            {
                message = Name + ": " + message;
            }
            success++;
            return Result(message, true);
        }
        public void AddSuccess(string message)
        {
            var Name = "Success";
            if (!message.ToLower().Contains(Name.ToLower()))
            {
                message = Name + ": " + message;
            }
            success++;
            LocalWL(message);
        }
        public void LocalWL(string message)
        {
            try
            {
                message = message.Replace("<p>", "<br>").Replace("<br/>", "<br>").Replace("<br>", "\r\n").Replace(
                    "</p>", " ").
                    Replace("&lt;", "<").Replace("&gt;", ">");
                WriteLineDelegate(message);
            }
            catch (Exception e)
            {

            }
            try
            {
                if (DLRConsole.DebugWriteLine == WriteLineDelegate)
                {
                    return;
                }
                DLRConsole.DebugWriteLine(message);
            }
            catch (Exception e)
            {
                DLRConsole.DebugWriteLine(e);
            }
        }

        protected CmdResult Result(string message, bool tf)
        {
            if (!message.ToLower().Contains(Name.ToLower()))
            {
                message = Name + ": " + message;
            }
            var cr = CurrentRequest.Complete(Name, message, tf);
            LocalWL(message);
            return cr;
        }
        protected CmdResult SuccessOrFailure()
        {
            var cr = CurrentRequest.Complete(Name, Name + " " + failure + " failures and " + success + " successes", failure == 0);
            LocalWL(cr.ToPostExecString());
            return cr;
        }

        protected bool TryEnumParse(Type type, string[] names, int argStart, out int argsUsed, out object value)
        {
            return WorldObjects.TryEnumParse(type, names, argStart, out argsUsed, out value);
        }

        static public bool IsEmpty(ICollection enumerable)
        {
            return enumerable == null || enumerable.Count == 0;
        }

        protected Simulator TryGetSim(string[] args, out int argsUsed)
        {
            return WorldSystem.TryGetSim(args, out argsUsed);
        }

        public string WriteLineResultName = "message";
        private KeyParams VersionSelected;

        public virtual bool ImpliesSync
        {
            get { return this is SynchronousCommand && !(this is AsynchronousCommand); }
        }

        protected void SetWriteLine(string resultName)
        {
            WriteLineResultName = resultName;
        }

        protected static KeyParams CreateParams(params object[] paramz)
        {
            List<NamedParam> paramsz = new List<NamedParam>();
            int argNum = 1;
            for (int i = 0; i < paramz.Length; )
            {
                var o = paramz[i++];
                if (o is NamedParam)
                {
                    paramsz.Add((NamedParam)o);
                    continue;
                }
                if (o is string)
                {
                    string k = (string)o;
                    Type t = paramz[i++] as Type;
                    string comment = "" + paramz[i++];
                    NamedParam namedParam = new NamedParam(k, t);
                    namedParam.Comment = comment;
                    paramsz.Add(namedParam);
                }
                else
                {
                    throw new FormatException("CreateParams: " + o);
                }
            }
            return new KeyParams(paramsz.ToArray());
        }

        protected static List<KeyParams> CreateParamVersions(params NamedParam[][] paramz)
        {
            var list = new List<KeyParams>();
            foreach (var paramse in paramz)
            {
                list.Add(new KeyParams(paramse));
            }
            return list;
        }
        protected static List<KeyParams> CreateParamVersions(params KeyParams[] paramz)
        {
            var list = new List<KeyParams>();
            foreach (var paramse in paramz)
            {
                list.Add(paramse);
            }
            return list;
        }
        protected static NamedParam Optional(string name, Type type, string description)
        {
            NamedParam namedParam = new NamedParam(name, type);
            namedParam.Comment = description;
            namedParam.IsOptional = true;
            return namedParam;
        }

        protected static NamedParam Rest(string name, Type type, string description)
        {
            NamedParam namedParam = new NamedParam(name, type);
            namedParam.Comment = description;
            namedParam.IsRest = true;
            return namedParam;
        }

        protected static NamedParam Required(string name, Type type, string desc)
        {
            var o = Optional(name, type, desc);
            o.IsOptional = false;
            return o;
        }

        protected static NamedParam SequenceOf(string name, NamedParam param)
        {
            NamedParam namedParam = new NamedParam(name, param.Type.MakeArrayType(), param);
            namedParam.Comment = "list of " + param.Key;
            namedParam.IsSequence = true;
            return namedParam;
        }

        protected static NamedParam OneOf(params NamedParam[] oneof)
        {
            NamedParam namedParam = new NamedParam("oneOf", typeof (object), null, oneof);
            namedParam.IsOneOf = true;
            return namedParam;
        }

        protected string AddUsage(string example, string comment)
        {
            if (!example.ToLower().Contains(Name.ToLower()))
            {
                example = Name + " " + example;
            }
            string idea = "<p>" + Htmlize.NoEnts(example) + "  <i>" + Htmlize.NoEnts(comment) + "</i></p>";
            Details = idea;
            return idea;
        }
        protected string AddExample(string typed, string output)
        {
            string idea = "<p><pre>" + Htmlize.NoEnts(typed) + "</pre><br>Returns<br><pre>" + Htmlize.NoEnts(output) + "</pre></p>";
            Details = idea;
            return idea;
        }

        protected void AddVersion(KeyParams paramSpec, string comment)
        {
            AddUsage(paramSpec, comment);
        }
        protected string AddUsage(KeyParams parameters, string description)
        {
            AddVersion(parameters);
            if (string.IsNullOrEmpty(Name))
            {
                throw new NullReferenceException(GetType().Name);
            }
            string usage = Name + " " + CommandInfo.ToBNF(parameters.Parameters);
            return AddUsage(usage, description);
        }

        protected bool Reloading(BotClient client)
        {
            return false;
        }

        protected void DefaultResultMap()
        {
            ResultMap = ResultMap ??
                        CreateParams(
                            "message", typeof (string), "if success was false, the reason why",
                            "success", typeof (bool), "true if command was successful");
        }

        protected void AppendResults(string name, string format)
        {
            lock (Results)
            {
                object obj;
                if (!Results.TryGetValue(name, out obj))
                {
                    Results[name] = format;
                }
                else
                {
                    string before = "" + obj;
                    string newstring = before + "\n" + format;
                    Results[name] = newstring.TrimStart();
                }
            }
        }

        protected void SetResult(string propname, object item)
        {
            propname = Parser.ToKey(propname);
            success++;
            Results[propname] = item;
        }

        protected void AppendList(string propname, IEnumerable items)
        {
            propname = Parser.ToKey(propname);
            success++;
            foreach (var c in items)
            {
                AppendItem(propname, c);
            }
        }

        protected void AppendList<T>(IEnumerable<T> items)
        {
            string propname = Parser.ToKey(typeof (T).GetType().Name + "s");
            success++;
            foreach (var c in items)
            {
                AppendItem(propname, c);
            }
        }

        protected void AppendItem(object item)
        {
            AppendItem(item.GetType().Name + "s", item);
        }

        protected void AppendItem(string propname, object item)
        {
            propname = Parser.ToKey(propname);
            success++;
            var dictionary = Results;
            Type t = GetPropType(propname);
            object value;
            lock (dictionary)
                if (!dictionary.TryGetValue(propname, out value))
                {
                    dictionary[propname] =
                        value =
                        typeof (List<>).MakeGenericType(new[] {t}).GetConstructor(new Type[0]).Invoke(new object[0]);
                }
            ((IList) value).Add(item);
        }

        private Type GetPropType(string propname)
        {
            return typeof(object);
        }

        public static string GetTaskID(CmdRequest args, out bool createFresh)
        {
            createFresh = false;
            String id;
            args.TryGetValue("taskid", out id);
            if (id == "create")
            {
                createFresh = true;
                id = BotClient.UniqueThreadID();
            }
            return id;
        }

    }
}
