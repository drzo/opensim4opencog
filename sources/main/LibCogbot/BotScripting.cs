/******************************************************************************************
  Cogbot -- Copyright (c) 2008-2012, Douglas Miles, Kino Coursey, Daxtron Labs, Logicmoo
      and the Cogbot Development Team.
   
  Major contributions from (and special thanks to):
      Latif Kalif, Anne Ogborn and Openmeteverse Foundation

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**************************************************************************************************/
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Reflection;
using System.Windows.Forms;
using System.Xml;
using Cogbot.Actions.Land;
using Cogbot.Actions.Movement;
using Cogbot.Actions.Scripting;
using Cogbot.Actions.System;
using Cogbot.Actions.WebUtil;
using Cogbot.Library;
using Cogbot.Utilities;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Packets;
using OpenMetaverse.Utilities;
using Cogbot.Actions;
using System.Threading;
using System.Collections;
using Cogbot.ScriptEngines;
using System.IO;
using Cogbot;
using Cogbot.World;
using System.Drawing;
using Settings=OpenMetaverse.Settings;
using Cogbot.Actions.Agent;
using System.Text;
using Type=System.Type;
#if USE_SAFETHREADS
using Thread = MushDLR223.Utilities.SafeThread;
#endif
//using RadegastTab = Radegast.SleekTab;

// older LibOMV
//using TeleportFlags = OpenMetaverse.AgentManager.TeleportFlags;
//using TeleportStatus = OpenMetaverse.AgentManager.TeleportStatus;

namespace Cogbot
{
    public partial class BotClient : ScriptExecutor, ICollectionRequester
    {

        public static CommandInstance newCommandInfo(Command describe)
        {
            return new CommandInstance(describe);
        }

        public ScriptInterpreter _LispTaskInterperter;
        public ScriptInterpreter LispTaskInterperter
        {
            get
            {
                if (_LispTaskInterperter == null)
                {
                    LoadTaskInterpreter();
                }
                return _LispTaskInterperter;
            }
        }

        private MethodInvoker CatchUpInterns = () => { };

        private void DiscoverMoreSysvars()
        {
            foreach (var ms in LockInfo.CopyOf(Plugins).Values)
            {
                ConfigSettingAttribute.AddSingletonClass(ms.GetType());
            }
        }

        private void LoadTaskInterpreter()
        {
            lock (LispTaskInterperterLock)
                try
                {
                    if (_LispTaskInterperter != null) return;
                    //WriteLine("Start Loading TaskInterperter ... '" + TaskInterperterType + "' \n");
                    _LispTaskInterperter = ClientManager.SingleInstance.TaskInterperter.newInterpreter(this);
                    _LispTaskInterperter.LoadFile("cogbot.lisp", DebugWriteLine);
                    Intern("clientManager", ClientManager);
                    Intern("client", this);
                    if (scriptEventListener == null)
                    {
                        scriptEventListener = new ScriptEventListener(_LispTaskInterperter, this);
                        botPipeline.AddSubscriber(scriptEventListener);
                    }

                    //  WriteLine("Completed Loading TaskInterperter '" + TaskInterperterType + "'\n");
                    // load the initialization string
                    CatchUpInterns();
                }
                catch (Exception e)
                {
                    LogException("LoadTaskInterperter", e);
                }
        }

        public bool RunStartupClientLisp = true;
        public object RunStartupClientLisplock = new object();
        public void StartupClientLisp()
        {
            lock (RunStartupClientLisplock)
            {
                if (!RunStartupClientLisp) return;
                RunStartupClientLisp = false;
                DebugWriteLine("Running StartupClientLisp");
                string startupClientLisp = ClientManager.config.GetValue("startupClientLisp", String.Empty);
                if (startupClientLisp.Length > 1)
                {
                    try
                    {
                        LoadTaskInterpreter();
                        //InvokeJoin("Waiting on StartupClientLisp");
                        evalLispString("(progn " + startupClientLisp + ")");
                        DiscoverMoreSysvars();
                    }
                    catch (Exception ex)
                    {
                        LogException("StartupClientLisp", ex);
                    }
                }
                DebugWriteLine("Ran StartupClientLisp");
                ClientManager.MakeRunning(this);
                ClientManager.DoClientTodo(this);
            }
        }
        public void RunOnLogin()
        {
            //lock (RunStartupClientLisplock)
            {
                StartupClientLisp();
                if (!NeedRunOnLogin) return;
                InvokeJoin("Waiting on RunOnLogin");
                NeedRunOnLogin = false;
                string onLogin = ClientManager.config.GetValue("onLogin", String.Empty);
                if (onLogin.Length > 1)
                {
                    try
                    {
                        evalLispString("(progn \n" + onLogin + "\n)");
                    }
                    catch (Exception ex)
                    {
                        LogException("RunOnLogin: " + onLogin, ex);
                    }
                }
            }
        }

        public readonly object LispTaskInterperterLock = new object();
        readonly private List<Type> registeredTypes = new List<Type>();

        public void enqueueLispTask(object p)
        {
            scriptEventListener.enqueueLispTask(p);
        }

        public Object evalLispReader(TextReader stringCodeReader)
        {
            try
            {
                Object r = LispTaskInterperter.Read("evalLispString", stringCodeReader, WriteLine);
                if (LispTaskInterperter.Eof(r))
                    return r.ToString();
                return evalLispCode(r);
            }
            catch (Exception e)
            {
                LogException("evalLispInterp stringCodeReader", e);
                throw e;
            }
        }

        public string evalLispReaderString(TextReader reader)
        {
            return LispTaskInterperter.Str(evalLispReader(reader));
        }


        public string evalXMLString(TextReader reader)
        {
            return XmlInterp.evalXMLString(reader);
        }

        /// <summary>
        /// (thisClient.XML2Lisp2 "http://myserver/myservice/?q=" chatstring) 
        /// </summary>
        /// <param name="URL"></param>
        /// <param name="args"></param>
        /// <returns></returns>
        public string XML2Lisp2(string URL, string args)
        {
            return XmlInterp.XML2Lisp2(URL, args);
        } // method: XML2Lisp2


        public string XML2Lisp(string xcmd)
        {
            return XmlInterp.XML2Lisp(xcmd);
        }

        public string evalLispString(string lispCode)
        {
            try
            {
                if (lispCode == null || lispCode.Length == 0) return null;
                Object r = null;
                //lispCode = "(load-assembly \"libsecondlife\")\r\n" + lispCode;                
                StringReader stringCodeReader = new StringReader(lispCode);
                return evalLispReaderString(stringCodeReader);
            }
            catch (Exception e)
            {
                LogException("evalLispInterp lispCode=" + lispCode, e);
                throw e;
            }
        }

        public Object evalLispCode(Object lispCode)
        {
            try
            {
                if (lispCode == null) return null;
                if (lispCode is String)
                {
                    StringReader stringCodeReader = new StringReader(lispCode.ToString());
                    lispCode = LispTaskInterperter.Read("evalLispString", stringCodeReader, WriteLine);
                }
                WriteLine("Eval> " + lispCode);
                if (LispTaskInterperter.Eof(lispCode))
                    return lispCode.ToString();
                return LispTaskInterperter.Eval(lispCode);
            }
            catch (Exception e)
            {
                LogException("evalLispInterp lispCode=" + lispCode, e);
                throw e;
            }
        }



        readonly Dictionary<Assembly, List<Listener>> KnownAssembies = new Dictionary<Assembly, List<Listener>>();
        private readonly HashSet<Type> UsedAutoLoad = new HashSet<Type>();
        public void InvokeAssembly(Assembly assembly, string args, OutputDelegate output)
        {
            LoadListeners(assembly);
            InvokeNext("", () =>
                               {
                                   foreach (Listener item in LoadListeners(assembly))
                                   {
                                       item.InvokeCommand(args, output);
                                       RegisterListener(item);
                                   }
                               });
        }

        private bool ConstructType(Assembly assembly, Type type, string name, Predicate<Type> when, Action<Type> action)
        {
            bool found = false;
            foreach (Type t in assembly.GetTypes())
            {
                try
                {
                    if (t.IsSubclassOf(type) && when(t))
                    {
                        try
                        {
                            found = true;
                            Type type1 = t;
                            InvokeNext(name + " " + t, () => action(type1));
                        }
                        catch (Exception e)
                        {
                            e = ScriptManager.InnerMostException(e);
                            LogException("ERROR!  " + name + " " + t + " " + e + "\n In " + t.Name, e);
                        }
                    }
                }
                catch (Exception e)
                {
                    WriteLine(e.ToString());
                }
            }
            return found;
        }

        public List<Listener> LoadListeners(Assembly assembly)
        {
            ClientManager.RegisterAssembly(assembly);
            List<Listener> items = null;
            lock (KnownAssembies)
            {
                if (KnownAssembies.TryGetValue(assembly, out items))
                {
                    return items;
                }
                items = new List<Listener>();
                KnownAssembies.Add(assembly, items);
            }
            bool found = false;
            foreach (Type t in assembly.GetTypes()) RegisterType(t);
            foreach (Type t in assembly.GetTypes())
            {
                try
                {
                    if (t.IsSubclassOf(typeof(WorldObjectsModule)) && !typeof(NotAutoLoaded).IsAssignableFrom(t))
                    {
                        if (typeof(YesAutoLoad).IsAssignableFrom(t))
                        {
                            lock(UsedAutoLoad)
                            {
                                if (UsedAutoLoad.Contains(t)) continue;
                                UsedAutoLoad.Add(t);
                            }
                        }
                        ConstructorInfo info = t.GetConstructor(new Type[] { typeof(BotClient) });
                        try
                        {
                            found = true;
                            InvokeNext("LoadAssembly " + assembly, () =>
                            {
                                try
                                {
                                    Listener command = (Listener)info.Invoke(new object[] { this });
                                    items.Add(command);
                                }
                                catch (Exception e1)
                                {
                                    e1 = ScriptManager.InnerMostException(e1);
                                    LogException("ERROR! RegisterListener: " + e1 + "\n In " + Thread.CurrentThread.Name, e1);
                                }
                            });

                        }
                        catch (Exception e)
                        {
                            e = ScriptManager.InnerMostException(e);
                            LogException("ERROR! RegisterListener: " + e + "\n In " + t.Name, e);
                        }
                    }
                }
                catch (Exception e)
                {
                    WriteLine(e.ToString());
                }
            }
            if (!found)
            {
                // throw new Exception("missing entry point " + assembly);
            }
            return items;
        }

        /// <summary>
        /// Initialize everything that needs to be initialized once we're logged in.
        /// </summary>
        /// <param name="login">The status of the login</param>
        /// <param name="message">Error message on failure, MOTD on success.</param>
        public void RegisterCommand(string name, Cogbot.Actions.Command live)
        {
            string orginalName = name;
            name = name.Replace(" ", "").ToLower();
            while (name.EndsWith(".")) name = name.Substring(0, name.Length - 1);
            Monitor.Enter(Commands);
            live.TheBotClient = this;
            CommandInstance prev;
            if (!Commands.TryGetValue(name, out prev))
            {
                CommandInstance command = new CommandInstance(live);
                Commands.Add(name, command);
                command.Name = orginalName;
                if (command.IsStateFul)
                {
                    live.TheBotClient = this;
                    command.WithBotClient = live;
                }
            }
            else
            {
                if (prev.CmdType != live.GetType())
                {
                    RegisterCommand("!" + orginalName, live);
                }
            }
            Monitor.Exit(Commands);
        }

        public void RegisterCommand(Command command)
        {
            command.TheBotClient = this;
            RegisterCommand(command.Name, command);
        }

        internal void DoCommandAll(string line, UUID uUID, OutputDelegate outputDelegate)
        {
            ClientManager.DoCommandAll(line, uUID, outputDelegate);
        }

        public SortedList<string, CommandInfo> AllCommands()
        {
            var all = ClientManager.AllCommands();
            foreach (KeyValuePair<string, CommandInstance> pair in Commands)
            {
                all[pair.Key] = pair.Value.CmdInfo;
            }
            return all;
        }

        public CmdResult ExecuteCommand(string text, object session, OutputDelegate WriteLine, CMDFLAGS needResult)
        {
            text = ClientManager.GetCommandText(text);
            try
            {
                return ExecuteBotCommand(text, session, WriteLine, needResult);
            }
            catch (NoSuchCommand)
            {
            }
            try
            {
                return ClientManager.ExecuteSystemCommand(text, session, WriteLine, needResult);
            }
            catch (NoSuchCommand)
            {
            }
            string verb = Parser.ParseArguments(text)[0];
            Command act = GetCommand(verb, false);
            if (act != null)
            {
                if (act is GridMasterCommand)
                {
                    if (!WorldSystem.IsGridMaster)
                    {
                        throw new NoSuchCommand("I am not gridMaster " + text + ".");
                        return null;
                    }
                }
                if (act is RegionMasterCommand)
                {
                    if (!IsRegionMaster)
                    {
                        WriteLine("I am not regionMaster " + text + ".");
                    }
                }
                string pargs = (text.Length > verb.Length) ? text.Substring(verb.Length + 1) : "";
                return BotClient.DoCmdAct(act, verb, pargs, BotClient.SessionToCallerId(session),
                                          WriteLine, needResult);
            }
            throw new NoSuchCommand("I don't understand the ExecuteCommand " + text + ".");
        }


        public CmdResult ExecuteBotCommand(string text, object session, OutputDelegate WriteLine, CMDFLAGS needResult)
        {
            text = ClientManager.GetCommandText(text);
            try
            {

                if (text.StartsWith("("))
                {
                    InvokeJoin("ExecuteBotCommand " + text);
                    return new ACmdResult(evalLispString(text).ToString(), true);
                }

                text = text.Replace("\"", "").Replace("  ", " ");
                string verb = text.Split(' ')[0];
                verb = verb.ToLower();

                Command act = GetCommand(verb, false);
                if (act != null)
                {
                    if (act is GridMasterCommand)
                    {
                        if (!WorldSystem.IsGridMaster)
                        {
                            throw new NoSuchCommand("I am not gridMaster " + text + ".");
                            return null;
                        }
                    }
                    if (act is RegionMasterCommand)
                    {
                        if (!IsRegionMaster)
                        {
                            throw new NoSuchCommand("I am not regionMaster " + text + ".");
                            return null;
                        }
                    }
                    try
                    {
                        CmdResult res;
                        if (text.Length > verb.Length)
                            return DoCmdAct(act, verb, text.Substring(verb.Length + 1), session, WriteLine, needResult);
                        else
                            return DoCmdAct(act, verb, "", session, WriteLine, needResult);
                    }
                    catch (Exception e)
                    {
                        if (e is NoSuchCommand) throw e;
                        LogException("ExecuteBotCommand " + text, e);
                        return new ACmdResult("ExecuteBotCommand " + text + "cuased " + e, false);
                    }
                }
                else
                {
                    if (WorldSystem == null || WorldSystem.SimAssetSystem == null)
                        return new ACmdResult("no world yet for gesture", false);
                    UUID assetID = WorldSystem.SimAssetSystem.GetAssetUUID(text, AssetType.Gesture);
                    if (assetID != UUID.Zero) return ExecuteBotCommand("play " + assetID, session, WriteLine, needResult);
                    assetID = WorldSystem.SimAssetSystem.GetAssetUUID(text, AssetType.Unknown);
                    if (assetID != UUID.Zero) return ExecuteBotCommand("play " + assetID, session, WriteLine, needResult);
                    throw new NoSuchCommand(verb);
                }
            }
            catch (Exception e)
            {
                if (e is NoSuchCommand) throw e;
                LogException("ExecuteBotCommand " + text, e);
                return null;
            }
        }


        public CmdResult ExecuteCommand(CmdRequest request)
        {
            return ClientManager.ExecuteCommand(request, this);
        }

        static public CmdResult DoCmdAct(Command command, string verb, string args, 
            object callerSession, OutputDelegate del, CMDFLAGS needResult)
        {
            
            string cmdStr = "ExecuteActBotCommand " + verb + " " + args;
            callerSession = SessionToCallerId(callerSession);
            var cmdr = new CmdRequest(verb, args, callerSession, del, command.GetCmdInfo());
            return DoCmdAct(command, () => command.ExecuteRequestSyn(cmdr), cmdr, cmdStr, needResult) ?? cmdr;
        }

        static public CmdResult DoCmdAct(Command command, Func<CmdResult> task, CmdRequest req, string debugStr, CMDFLAGS flags)
        {
            BotClient robot = command._mClient;
            string sync = command.TaskQueueNameOrNull;
            bool needResult = (flags & CMDFLAGS.ForceResult) != 0;
            bool isConsole = (flags & CMDFLAGS.IsConsole) != 0;
            bool forceAsync = (flags & CMDFLAGS.ForceAsync) != 0;
            bool forceCompletion = (flags & CMDFLAGS.ForceCompletion) != 0;
            bool inherit = (flags & CMDFLAGS.Inherit) != 0;
            bool scriptMode = robot != null && robot.InScriptMode;
            bool cmdRequestsSync = command.ImpliesSync;
            bool invokeJoin = scriptMode || cmdRequestsSync;

            if (needResult)
            {
                invokeJoin = true;
            }
            if (forceCompletion)
            {
                forceAsync = false;
            }
            if (forceAsync)
            {
                sync = null;
                needResult = false;
                invokeJoin = false;
            }
            if (invokeJoin)
            {
                if (!needResult)
                {

                }
                else
                {
                    if (robot != null) robot.InvokeJoin(debugStr);
                }
            }
            if (sync != null)
            {
                CmdResult[] res = new CmdResult[1];
                ManualResetEvent mre = new ManualResetEvent(false);
                Abortable tq = null;
                if (robot == null)
                {
                    tq = Cogbot.ClientManager.OneAtATimeQueue;
                }
                else
                {
                    tq = robot.GetTaskQueueHandler(sync, true);
                }
                tq.Enqueue(() =>
                               {
                                   try
                                   {
                                       res[0] = task();
                                   }
                                   finally
                                   {
                                       if (needResult) mre.Set();
                                   }
                               });
                if (needResult) mre.WaitOne();
                return res[0];
            }
            else
            {
                return task();
            }
            //robot.OneAtATimeQueue.Enqueue(cmdStr, () => command.acceptInputWrapper(verb, args, callerID, del));
        }

        public bool IsValidCommand(string cmd)
        {
            return GetCommand(cmd, true) != null;
        }

        public Command GetCommand(string text, bool managerCmds)
        {
            if (string.IsNullOrEmpty(text)) return null;
            text = text.Trim();
            while (text.StartsWith("/"))
            {
                text = text.Substring(1).TrimStart();
            }
            if (string.IsNullOrEmpty(text)) return null;
            string[] pargs = Parser.ParseArguments(text);
            if (pargs==null || pargs.Length == 0) return null;
            text = pargs[0].ToLower();
            CommandInstance fnd;
            if (Commands == null || Commands.Count == 0)
            {
                WriteLine("No commands defined yet " + this);
                return null;
            }
            if (Commands.TryGetValue(text, out fnd)) return fnd.MakeInstance(this);
            if (managerCmds)
            {
                var cm = ClientManager;
                if (cm != null)
                {
                    CommandInfo fnd2;
                    if (cm.groupActions.TryGetValue(text, out fnd2)) return fnd2.MakeInstanceCM(null);
                }
            }
            if (text.EndsWith("s")) return GetCommand(text.Substring(0, text.Length - 1), managerCmds);
            return null;
        }
        public void Intern(string n, object v)
        {
            if (_LispTaskInterperter != null)
            {
                _LispTaskInterperter.Intern(n, v);
            }
            var PrevCode = CatchUpInterns;
            CatchUpInterns = () =>
            {
                PrevCode();
                _LispTaskInterperter.Intern(n, v);
            };
        }


        public void InternType(Type t)
        {
            //          LispTaskInterperter.InternType(t);
            ScriptManager.AddType(t);
        }
        internal void RegisterType(Type t)
        {
            //lock (GridClientNullLock)
            {
                RegisterType0(t);
            }
        }
        internal void RegisterType0(Type t)
        {
            ClientManager.RegisterType(t);
            if (registeredTypes.Contains(t)) return;
            registeredTypes.Add(t);
            try
            {
                if (t.IsSubclassOf(typeof(Command)))
                {
                    if (!typeof(SystemApplicationCommand).IsAssignableFrom(t) && !typeof(NotAutoLoaded).IsAssignableFrom(t))
                    {
                        string typename = t.Name;
                        bool useGridClient = false;
                        ConstructorInfo info = t.GetConstructor(new Type[] { typeof(BotClient) });
                        if (info == null)
                        {
                            useGridClient = true;
                            info = t.GetConstructor(new Type[] { typeof(GridClient) });
                        }
                        if (info == null)
                        {
                            WriteLine("Missing BotClient constructor in " + typename);
                            return;
                        }
                        var gc = gridClient;
                        try
                        {
                            Command command = null;
                            GridClientAccessed = false;
                            bool stateFullCmd = typeof(BotStatefullCommand).IsAssignableFrom(t);
                            if (!stateFullCmd)
                            {
                                //_gridClient = null;
                            }
                            else
                            {
                                _gridClient = gc;
                            }
                            if (useGridClient)
                            {
                                command = (Command)info.Invoke(new object[] { gridClient });
                            }
                            else
                            {
                                command = (Command)info.Invoke(new object[] { this });
                            }
                            if (GridClientAccessed)
                            {
                                command.IsStateFull = true;
                                GridClientAccessed = false;
                            }
                            if (stateFullCmd) command.IsStateFull = true;
                            RegisterCommand(command);
                        }
                        catch (Exception e)
                        {
                            LogException("RegisterCommand " + t.Name, e);
                        }
                        finally
                        {
                            _gridClient = gc;
                        }
                    }
                }
                try
                {
                    if (t.IsSubclassOf(typeof(WorldObjectsModule)) && typeof(YesAutoLoad).IsAssignableFrom(t))
                    {
                        ConstructorInfo info = t.GetConstructor(new Type[] { typeof(BotClient) });
                        try
                        {
                            InvokeNext("AutoLoad WorldObjectsModule " + t, () =>
                            {
                                try
                                {
                                    lock (UsedAutoLoad)
                                    {
                                        if (UsedAutoLoad.Contains(t))
                                        {
                                            return;
                                        }
                                        UsedAutoLoad.Add(t);
                                    }
                                    Listener item = (Listener)info.Invoke(new object[] { this });
                                    RegisterListener(item);
                                }
                                catch (Exception e1)
                                {
                                    e1 = ScriptManager.InnerMostException(e1);
                                    LogException("ERROR! RegisterListener: " + e1 + "\n In " + Thread.CurrentThread.Name, e1);
                                }
                            });

                        }
                        catch (Exception e)
                        {
                            e = ScriptManager.InnerMostException(e);
                            LogException("ERROR! RegisterListener: " + e + "\n In " + t.Name, e);
                        }
                    }
                }
                catch (Exception e)
                {
                    WriteLine(e.ToString());
                }
            }
            catch (Exception e)
            {
                LogException("RegisterType! " + t, e);
            }
        }
        public void FakeEvent(Object target, String infoName, params object[] parameters)
        {

            Type type = target.GetType();
            EventInfo eventInfo = type.GetEvent(infoName, BindingFlags.Instance | BindingFlags.Public |
                                                          BindingFlags.NonPublic | BindingFlags.IgnoreCase);
            MethodInfo m = null;
            if (eventInfo != null)
            {
                infoName = eventInfo.Name;
                m = eventInfo.GetRaiseMethod(true);
            }

            Exception lastException = null;
            if (m != null)
            {
                try
                {


                    m.Invoke(target, parameters);
                    return;
                }
                catch (Exception e)
                {
                    lastException = e;
                }
            }
            else
            {
                {
                    foreach (var o in new[] { "", "_", "m_" })
                    {


                        FieldInfo fieldInfo = type.GetField(o + infoName,
                                                            BindingFlags.Instance | BindingFlags.NonPublic) ??
                                              type.GetField(o + infoName,
                                                            BindingFlags.Instance | BindingFlags.Public)
                                              ?? type.GetField(o + infoName,
                                                               BindingFlags.Instance | BindingFlags.Public |
                                                               BindingFlags.NonPublic | BindingFlags.IgnoreCase);

                        if (fieldInfo != null)
                        {
                            Delegate del = fieldInfo.GetValue(target) as Delegate;

                            if (del != null)
                            {
                                del.DynamicInvoke(parameters);
                                return;
                            }
                        }
                    }
                }
                if (eventInfo != null)
                {
                    m = eventInfo.EventHandlerType.GetMethod("Invoke");

                    if (m != null)
                    {
                        Type dt = m.DeclaringType;
                        try
                        {


                            m.Invoke(target, parameters);
                            return;
                        }
                        catch (Exception e)
                        {
                            lastException = e;
                        }
                    }
                }
                var ms = eventInfo.GetOtherMethods(true);
                foreach (MethodInfo info in ms)
                {
                }
            }

            if (lastException != null) throw lastException;
            //MethodInfo m = eventInfo.GetOtherMethods(true);
            throw new NotSupportedException();
        }
        public CmdResult ExecuteTask(string scripttype, TextReader reader, OutputDelegate WriteLine)
        {
            var si = ScriptManager.LoadScriptInterpreter(scripttype, this, _LispTaskInterperter);
            object o = si.Read(scripttype, reader, WriteLine);
            if (o is CmdResult) return (CmdResult)o;
            if (o == null) return new ACmdResult("void", true);
            if (si.Eof(o)) return new ACmdResult("EOF " + o, true);
            o = si.Eval(o);
            if (o is CmdResult) return (CmdResult)o;
            if (o == null) return new ACmdResult("void", true);
            if (si.Eof(o)) return new ACmdResult("EOF " + o, true);
            return new ACmdResult("" + o, true);
        }

        public string DoHttpGet(string url)
        {
            return Encoding.UTF8.GetString((new System.Net.WebClient()).DownloadData(url)); ;
        }

        public string DoHttpPost(Object[] args)
        {
            NameValueCollection dict = new NameValueCollection();
            for (int i = 1; i < args.Length; i++)
            {
                dict.Add(args[i++].ToString(), args[i].ToString());
            }
            return HttpPost.DoHttpPost(args[0].ToString(), dict);
        }


        public CmdResult ExecuteXmlCommand(string cmd, object session, OutputDelegate line)
        {
            return XmlInterp.ExecuteXmlCommand(cmd, session, line);

        }
        public void ExecuteCommand(string text)
        {
            ExecuteCommand(text, CMDFLAGS.NoResult);
        }
        public CmdResult ExecuteCommand(string text, CMDFLAGS needResult)
        {
            // done inside the callee InvokeJoin("ExecuteCommand " + text);
            OutputDelegate WriteLine = DisplayNotificationInChat;
            return ExecuteCommand(text, this, WriteLine, needResult);
        }

        private bool InvokeJoin(string s)
        {
            return InvokeJoin(s, -1);
        }
        private bool InvokeJoin(string s, int millisecondsTimeout)
        {
            return OneAtATimeQueue.InvokeJoin(s, millisecondsTimeout);
        }
        internal bool InvokeJoin(string s, int millisecondsTimeout, ThreadStart task1, ThreadStart task2)
        {
            return OneAtATimeQueue.InvokeJoin(s, millisecondsTimeout, task1, task2);
        }

        public void InvokeNext(string s, ThreadStart e)
        {
            OneAtATimeQueue.Enqueue(s, () =>
            {
                e();
            });
        }

        #region Implementation of ICollectionRequester

        public object RequesterID
        {
            get { return this; }
        }

        private object _RequesterLock = new object();
        public object SessionLock
        {
            get { return _RequesterLock; }
        }
        public RequesterSession SessionMananger { get; set; }

        #endregion

        public Abortable GetTaskQueueHandler(string id, bool createIfMissing)
        {
            id = id.ToLower();
            IList<Abortable> BotTaskQueues = AllTaskQueues();
            lock (BotTaskQueues)
            {
                foreach (Abortable tq0 in BotTaskQueues)
                {
                    if (tq0.MatchesId(id)) return tq0;
                }
                if (!createIfMissing) return null;
                var tq = new TaskQueueHandler(this, new NamedPrefixThing(id, GetName), TimeSpan.FromMilliseconds(1),
                                              true);
                {
                    AddTaskQueue(id, tq);
                }
                return tq;
            }
        }

        public void AddTaskQueue(string id, TaskQueueHandler tq)
        {
            tq.Owner = this;
        }

        public ListAsSet<Abortable> AllTaskQueues()
        {
            ListAsSet<Abortable> all = ClientManager.AllTaskQueues();
            foreach (var tq in TaskQueueHandler.TaskQueueHandlers)
            {
                if (tq.Owner == this)
                {
                    all.Add(tq);
                }
            }
            all.AddRange(botCommandThreads);
            return all;
        }
        
        public string CreateTask(string id, ThreadStart task, string debugName0, bool createFresh, bool kill, EventWaitHandle mre, OutputDelegate WriteLine)
        {
            BotClient TheBotClient = this;
            string[] debugName = new[] { debugName0 };
            ThreadStart thread =
                () =>
                {
                    try
                    {
                        try
                        {
                            task();
                        }
                        catch (ThreadAbortException e)
                        {
                            WriteLine("Aborting " + debugName[0]);
                        }
                        catch (Exception e)
                        {
                            WriteLine("Problem with {0} {1}", debugName[0], e);
                        }
                    }
                    finally
                    {
                        try
                        {
                            if (mre != null) mre.Set();
                            if (createFresh)
                            {
                                TheBotClient.RemoveThread(Thread.CurrentThread);
                            }
                        }
                        catch (OutOfMemoryException)
                        {
                        }
                        catch (StackOverflowException)
                        {
                        }
                        catch (Exception)
                        {
                        }
                        WriteLine("done with " + debugName[0]);
                    }
                };
            String threadName = id;
            if (createFresh)
            {
                TheBotClient.InvokeThread(threadName, thread);
            }
            else
            {
                Abortable tq = TheBotClient.GetTaskQueueHandler(id, true);
                if (kill)
                {
                    tq.Abort();
                }
                if (task != null) tq.Enqueue(thread);
                debugName[0] += tq;
            }
            return debugName[0];
        }

        public static string UniqueThreadID()
        {
            return "" + UUID.Random();
        }
    }


}