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
using CommandLine;
using CommandLine.Utility;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using Radegast;
using System.Windows.Forms;
using RTParser;
using SbsSW.SwiPlCs;
using User = RTParser.User;

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
                DoAndExit(() => RTParser.RTPBot.Main(args));
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
                                  RadegastMain(args);
                              }
                          });

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
            CommandLineParser parser = new CommandLineParser(new CommandLineParserSettings(Console.Error));
            if (!parser.ParseArguments(args, MainProgram.CommandLine))
            {
                Environment.Exit(1);
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
                Console.WriteLine(CommandLine.GetHeader());
                Console.WriteLine();
                Radegast.GridManager grids = instance.GridManger;
                Console.WriteLine("Use Grid ID as the parameter for --grid");
                Console.WriteLine("{0,-25} - {1}", "Grid ID", "Grid Name");
                Console.WriteLine("========================================================");

                for (int i = 0; i < grids.Count; i++)
                {
                    Console.WriteLine("{0,-25} - {1}", grids[i].ID, grids[i].Name);
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
