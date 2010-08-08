using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Threading;
using System.Windows.Forms;
using cogbot;
using CommandLine;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using Radegast;
using SbsSW.SwiPlCs;

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

            if (use.Length > 0)
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

        private static TextFilter filter = new TextFilter() { "+*" };
        static public void FilteredWriteLine(string str, params object[] args)
        {

            OutputDelegate del = new OutputDelegate(ClientManager.Real ?? DLRConsole.DebugWriteLine);
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
            ClientManager.arguments = new Parser(args);
            string[] oArgs;
            if (ClientManager.arguments.GetAfter("--aiml", out oArgs))
            {
                string[] newArgs = oArgs;
                AllocConsole();
                DoAndExit(() => RTParser.RTPBot.Main(args));
            }
            if (ClientManager.arguments.GetAfter("--swipl", out oArgs))
            {
                string[] newArgs = oArgs;
                AllocConsole();
                DoAndExit(() => PrologClient.Main(newArgs));
            }
            if (ClientManager.arguments.GetAfter("--main", out oArgs))
            {
                string[] newArgs = oArgs;
                AllocConsole();
                string c = ClientManager.arguments["--main"];
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
            if (ClientManager.arguments.GetWithout("--noconfig", out oArgs))
            {
                ClientManager.arguments = new Parser(oArgs);
                ClientManager.NoLoadConfig = true;
            }
            if (ClientManager.arguments.GetWithout("--console", out oArgs))
            {
                ClientManager.dosBox = true;
                ClientManager.arguments = new Parser(oArgs);
            }
            if (ClientManager.arguments.GetWithout("--nogui", out oArgs))
            {
                ClientManager.noGUI = true;
            }

            MainProgram.CommandLine = new Radegast.CommandLine();
            ClientManager.arguments = new Parser(oArgs);

            if (ClientManager.dosBox) AllocConsole();

            DoAndExit(() =>
                          {
                              if (ClientManager.noGUI)
                              {
                                  ClientManager.UsingRadgastFromCogbot = true;
                                  cogbot.Program.Main(oArgs);
                              }
                              else
                              {
                                  ClientManager.UsingCogbotFromRadgast = true;
                                  RadegastMain(oArgs);
                              }
                          });

        }

        public static void AllocConsole()
        {
            if (!ClientManager.AllocedConsole)
            {
                ClientManager.AllocedConsole = true;
                NativeMethods.AllocConsole();
            }
        }

        /// <summary>
        /// Parsed command line options
        /// </summary>
        //public static CommandLine CommandLine;

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]

        public static void RadegastMain(string[] args)
        {
            // Read command line options
            Radegast.CommandLine CommandLine = MainProgram.CommandLine = new Radegast.CommandLine();
            CommandLineParser parser = new CommandLineParser(new CommandLineParserSettings(DLRConsole.Out));
            if (!parser.ParseArguments(args, MainProgram.CommandLine))
            {
              //  Environment.Exit(1);
            }

            // Change current working directory to Radegast install dir
            Directory.SetCurrentDirectory(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location));

            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);

            // Create main Radegast instance

            RadegastInstance instance = RadegastInstance.GlobalInstance;
            // See if we only wanted to display list of grids
            if (CommandLine.ListGrids)
            {
                DLRConsole.SystemWriteLine(CommandLine.GetHeader());
                DLRConsole.SystemWriteLine();
                Radegast.GridManager grids = instance.GridManger;
                DLRConsole.SystemWriteLine("Use Grid ID as the parameter for --grid");
                DLRConsole.SystemWriteLine("{0,-25} - {1}", "Grid ID", "Grid Name");
                DLRConsole.SystemWriteLine("========================================================");

                for (int i = 0; i < grids.Count; i++)
                {
                    DLRConsole.SystemWriteLine("{0,-25} - {1}", grids[i].ID, grids[i].Name);
                }

                Environment.Exit(0);
            }

            var mf = instance.MainForm;
            if (false) cogbot.ClientManager.SingleInstance.ProcessCommandArgs();
            Application.Run(mf);
            instance = null;
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
}
