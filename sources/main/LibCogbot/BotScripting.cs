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

        public static CommandInfo newCommandInfo(Command describe)
        {
            return new CommandInfo(describe);
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
                    }
                    catch (Exception ex)
                    {
                        LogException("StartupClientLisp", ex);
                    }
                }
                DebugWriteLine("Ran StartupClientLisp");
            }
        }
        public void RunOnLogin()
        {
            //lock (RunStartupClientLisplock)
            {
                StartupClientLisp();
                InvokeJoin("Waiting on RunOnLogin");
                if (!NeedRunOnLogin) return;
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
            CommandInfo prev;
            if (!Commands.TryGetValue(name, out prev))
            {
                CommandInfo command = new CommandInfo(live);
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



        public CmdResult ExecuteCommand(string text, object session, OutputDelegate WriteLine)
        {
            if (string.IsNullOrEmpty(text)) return null;
            text = text.Trim();
            while (text.StartsWith("/"))
            {
                text = text.Substring(1).TrimStart();
            }
            if (string.IsNullOrEmpty(text)) return null;
            CmdResult res = ExecuteBotCommand(text, session, WriteLine);
            if (res != null) return res;
            res = ClientManager.ExecuteSystemCommand(text, session, WriteLine);
            if (res != null) return res;
            string verb = Parser.ParseArguments(text)[0];
            Command act = GetCommand(verb, false);
            if (act != null)
            {
                if (act is GridMasterCommand)
                {
                    if (!WorldSystem.IsGridMaster)
                    {
                        WriteLine("I am not gridMaster " + text + ".");
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
                                          WriteLine);
            }
            WriteLine("I don't understand the ExecuteCommand " + text + ".");
            return null;
        }


        public CmdResult ExecuteBotCommand(string text, object session, OutputDelegate WriteLine)
        {
            if (text == null)
            {
                return null;
            }
            text = text.Trim();
            while (text.StartsWith("/")) text = text.Substring(1).TrimStart();
            if (text.Length == 0)
            {
                return null;
            }
            try
            {

                if (text.StartsWith("("))
                {
                    InvokeJoin("ExecuteBotCommand " + text);
                    return new CmdResult(evalLispString(text).ToString(), true);
                }
                //            Settings.LOG_LEVEL = Helpers.LogLevel.Debug;
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
                            return null;
                        }
                    }
                    if (act is RegionMasterCommand)
                    {
                        if (!IsRegionMaster)
                        {
                            return null;
                        }
                    }
                    try
                    {
                        CmdResult res;
                        if (text.Length > verb.Length)
                            return DoCmdAct(act, verb, text.Substring(verb.Length + 1), session, WriteLine);
                        else
                            return DoCmdAct(act, verb, "", session, WriteLine);
                    }
                    catch (Exception e)
                    {
                        LogException("ExecuteBotCommand " + text, e);
                        return new CmdResult("ExecuteBotCommand " + text + "cuased " + e, false);
                    }
                }
                else
                {
                    if (WorldSystem == null || WorldSystem.SimAssetSystem == null)
                        return new CmdResult("no world yet for gesture", false);
                    UUID assetID = WorldSystem.SimAssetSystem.GetAssetUUID(text, AssetType.Gesture);
                    if (assetID != UUID.Zero) return ExecuteBotCommand("gesture " + assetID, session, WriteLine);
                    assetID = WorldSystem.SimAssetSystem.GetAssetUUID(text, AssetType.Animation);
                    if (assetID != UUID.Zero) return ExecuteBotCommand("anim " + assetID, session, WriteLine);
                    return null;
                }
            }
            catch (Exception e)
            {
                LogException("ExecuteBotCommand " + text, e);
                return null;
            }
        }

        static public CmdResult DoCmdAct(Command command, string verb, string args, object callerSession, OutputDelegate del)
        {
            var callerID = SessionToCallerId(callerSession);
            string cmdStr = "ExecuteActBotCommand " + verb + " " + args;
            if (command is BotPersonalCommand)
            {
                BotClient robot = command.TheBotClient;
                robot.InvokeJoin(cmdStr);
            }
            return command.acceptInputWrapper(verb, args, callerID, del);
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
            text = Parser.ParseArguments(text)[0].ToLower();
            CommandInfo fnd;
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
                    if (cm.groupActions.TryGetValue(text, out fnd)) return fnd.MakeInstance(null);
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
            lock (GridClientNullLock)
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
            if (o == null) return new CmdResult("void", true);
            if (si.Eof(o)) return new CmdResult("EOF " + o, true);
            o = si.Eval(o);
            if (o is CmdResult) return (CmdResult)o;
            if (o == null) return new CmdResult("void", true);
            if (si.Eof(o)) return new CmdResult("EOF " + o, true);
            return new CmdResult("" + o, true);
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
        public CmdResult ExecuteCommand(string text)
        {
            // done inside the callee InvokeJoin("ExecuteCommand " + text);
            OutputDelegate WriteLine = DisplayNotificationInChat;
            return ExecuteCommand(text, this, WriteLine);
        }

        private bool InvokeJoin(string s)
        {
            // return InvokeJoin(s, -1);
            return true;
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
    }


}