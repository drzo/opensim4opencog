using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Threading;
using AIMLbot;
using cogbot;
using cogbot.Actions;
using cogbot.Utilities;
using CommandLine.Utility;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using Radegast;
using System.Windows.Forms;
using RTParser;
using SbsSW.SwiPlCs;
using User=RTParser.User;

namespace ABuildStartup
{
    internal static class NativeMethods
    {
        [DllImport("kernel32.dll")]
        internal static extern Boolean AllocConsole();
    }
    public class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        public static void Main()
        {
            string[] use = Environment.GetCommandLineArgs() ?? new string[0];

            if (ClientManager.MainThread == null)
            {
                ClientManager.MainThread = Thread.CurrentThread;
                NativeMethods.AllocConsole();
            }

            if (use.Length>0)
            {
                string arg0 = use[0].ToLower();
                if (arg0.EndsWith(".vshost.exe"))
                {
                    cogbot.ClientManager.IsVisualStudio = true;
                    arg0 = arg0.Replace(".vshost.exe", ".exe");
                }
                if (arg0.EndsWith(Application.ExecutablePath.ToLower()))
                {
                    var n = new List<string>();
                    foreach (var s in use)
                    {
                        n.Add(s);
                    }
                    n.RemoveAt(0);
                    use = n.ToArray();
                }
            }
            Main0(use);
        }

        static public Exception _appException = null;
        private static void DoAndExit(MethodInvoker o)
        {
            try
            {
                ExitCode = 0;
                o();
                _appException = null;
            }
            catch (Exception e)
            {
                if (_appException == null) _appException = e;
            }
            finally
            {
                FilteredWriteLine("ExitCode: " + ExitCode + " for " + Environment.CommandLine);
            }
            if (_appException != null)
            {
                FilteredWriteLine("" + _appException);
                if (ExitCode == 0) ExitCode = 1;
            }
            if (ExitCode != 0)
            {
                FilteredWriteLine("ExitCode: " + ExitCode + " for " + Environment.CommandLine);
            }
            Application.Exit();
            Environment.Exit(ExitCode);
            return;
        }

        static public int ExitCode
        {
            get
            {
                return Environment.ExitCode;
            }

            set
            {
                Environment.ExitCode = value;
            }
        }

        private static TextFilter filter = new TextFilter() {"+*"};
        static public void FilteredWriteLine(string str, params object[] args)
        {

            OutputDelegate del = new OutputDelegate(ClientManager.Real ?? Console.WriteLine);
            if (ClientManager.Filter == null)
            {
                ClientManager.Filter = new OutputDelegate(del);
            }
            filter.writeDebugLine(del, str, args);
        }

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        public static void Main0(string[] args)
        {
            args = args ?? new string[0];
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException);
            Application.ThreadException += HandleThreadException;
            Application.ApplicationExit += HandleAppExit;
            Application.EnterThreadModal += HandleEnterThreadModal;
            Application.LeaveThreadModal += HandleLeaveThreadModal;
            Application.ThreadExit += HandleThreadExit;
            AppDomain.CurrentDomain.UnhandledException += HandleUnhandledException;
            AppDomain.CurrentDomain.ProcessExit += HandleProcessExit;
            
            if (ClientManager.MainThread == null)
            {
                ClientManager.MainThread = Thread.CurrentThread;
            }
            Arguments arguments = new Arguments(args);
            string[] oArgs;
            if (arguments.GetAfter("--aiml", out oArgs))
            {
                string[] newArgs = oArgs;
                DoAndExit(() => RTPBotMain(oArgs));
            }
            if (arguments.GetAfter("--swipl", out oArgs))
            {
                string[] newArgs = oArgs;
                DoAndExit(() => PrologClient.Main(newArgs));
            }
            if (arguments.GetAfter("--main", out oArgs))
            {
                string[] newArgs = oArgs;
                string c = arguments["--main"];
                DoAndExit(() =>
                              {
                                  Type t = Type.GetType(c, false, false);
                                  if (t == null) t = Type.GetType(c, false, true);

                                  if (t != null)
                                  {
                                      RunType(t, newArgs);
                                  }
                                  else
                                  {
                                      throw new Exception(c);
                                  }
                              });
            }
            if (arguments.GetWithout("--noconfig", out oArgs))
            {
                arguments = new Arguments(oArgs);
                cogbot.ClientManager.NoLoadConfig = true;
                //cogbot.ClientManager.consoleDelegate = FilteredWriteLine;
            }

            bool noRadgast = arguments.GetWithout("--nogui", out oArgs);
            DoAndExit(() =>
                          {
                              string[] newArgs = oArgs;
                              cogbot.ClientManager.arguments = new Arguments(newArgs);
                              if (noRadgast)
                              {
                                  cogbot.Program.Main(newArgs);
                              }
                              else
                              {
                                  RadegastInstance instance = RadegastInstance.GlobalInstance;
                                  var mf = instance.MainForm;
                                  if (false) cogbot.ClientManager.SingleInstance.ProcessCommandArgs();
                                  Application.Run(mf);
                                  instance = null;

                              }
                          })
                ;
        }

        private static void RTPBotMain(string[] args)
        {
            OutputDelegate writeLine = Console.WriteLine;
            string[] oArgs;
            bool usedHttpd = true ;//|| (args.GetAfter("http", out oArgs));
            RTPBot myBot = new Bot();

            if (usedHttpd)
            {
                ScriptExecutorGetter geter = new ScriptExecutorGetterImpl(myBot);
                new ClientManagerHttpServer(geter, 5580);
            }
            Bot.Main(args, myBot, writeLine);
        }

        private static void HandleProcessExit(object sender, EventArgs e)
        {            
            DebugWrite("HandleProcessExit");
            var v = ClientManager.SingleInstance;
            if (v != null)
            {
                v.Quit();
            }
        }

        private static void HandleEnterThreadModal(object sender, EventArgs e)
        {
            DebugWrite("HandleEnterThreadModal");
        }
        private static void HandleLeaveThreadModal(object sender, EventArgs e)
        {
            DebugWrite("HandleLeaveThreadModal");
        }

        private static void HandleThreadExit(object sender, EventArgs e)
        {
            DebugWrite("HandleThreadExit");
        }

        private static void DebugWrite(string handlethreadexit)
        {
            FilteredWriteLine(handlethreadexit);
            bool wasMain = (Thread.CurrentThread == ClientManager.MainThread);
        }

        private static void HandleAppExit(object sender, EventArgs e)
        {
            DebugWrite("HandleAppExit");
            var v = ClientManager.SingleInstance;
            if (v != null)
            {
                v.Quit();
            } 
            Environment.Exit(0);
        }

        private static void RunType(Type t, string[] newArgs)
        {
            MethodInfo mi = t.GetMethod("Main", new Type[] { typeof(string[]) });
            if (mi != null)
            {
                mi.Invoke(null, new object[] { newArgs });
                return;
            }
            mi = t.GetMethod("Main", new Type[] { });
            if (mi != null)
            {

                // Environment.CommandLine = "foo";
                mi.Invoke(null, new object[] { });
                return;
            }
            foreach (var s in t.GetMethods(BindingFlags.Static | BindingFlags.NonPublic))
            {
                if (s.Name.ToLower() == "main")
                {
                    var ps = s.GetParameters();
                    if (ps.Length == 0)
                    {

                        s.Invoke(null, new object[] { });
                        return;
                    }
                    if (ps.Length == 1)
                    {
                        mi.Invoke(null, new object[] { newArgs });
                        return;
                    }
                }
            }
            throw new MethodAccessException("No main for " + t);
        }

        static void HandleThreadException(object sender, ThreadExceptionEventArgs e)
        {
            _appException = (Exception)e.Exception;
            FilteredWriteLine("!!HandleThreadException!! " + e.Exception);
        }

        static void HandleUnhandledException(object sender, UnhandledExceptionEventArgs e)
        {
            _appException = (Exception)e.ExceptionObject;
            FilteredWriteLine("!!HandleUnhandledException!! " + e.ExceptionObject);
        }
    }

    internal class ScriptExecutorGetterImpl : ScriptExecutorGetter, ScriptExecutor
    {
        #region Implementation of ScriptExecutorGetter

        private RTPBot TheBot;
        private User myUser;

        public ScriptExecutorGetterImpl(RTPBot bot)
        {
            TheBot = bot;
        }
        public ScriptExecutor GetScriptExecuter(object o)
        {
            return this;
        }

        public void WriteLine(string s, params object[] args)
        {
            //TheBot.writeChatTrace(s, args);
            Console.WriteLine(s,args);
        }

        #endregion

        #region Implementation of ScriptExecutor

        public CmdResult ExecuteCommand(string s, OutputDelegate outputDelegate)
        {
            StringWriter sw = new StringWriter();
            if (s.StartsWith("aiml"))
            {
                s = s.Substring(4).Trim();
                if (s.StartsWith("@ "))
                    s = "@withuser" + s.Substring(1);
            }
            if (s.StartsWith("say")) s = "@" + s;
            sw.WriteLine("AIMLTRACE " + s);
            myUser = TheBot.LastUser;
            bool r = TheBot.BotDirective(myUser, s, sw.WriteLine);
            s = sw.ToString();
            WriteLine(s);
            return new CmdResult(s, r);
        }

        public CmdResult ExecuteXmlCommand(string s, OutputDelegate outputDelegate)
        {
            return ExecuteCommand(s, outputDelegate);
        }

        public string GetName()
        {
            return TheBot.GlobalSettings.grabSettingNoDebug("NAME");
        }

        public object getPosterBoard(object slot)
        {
            string sslot = "" + slot;
            sslot = sslot.ToLower();
            var u = TheBot.GlobalSettings.grabSetting(sslot);
            if (Unifiable.IsNull(u)) return null;
            if (u.IsEmpty) return "";
            return u.ToValue(null);
        }

        #endregion
    }
}
