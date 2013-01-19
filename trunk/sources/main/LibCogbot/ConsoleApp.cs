using System;
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.InteropServices;
#if (COGBOT_LIBOMV || USE_STHREADS)
using ThreadPoolUtil;
using Thread = ThreadPoolUtil.Thread;
using ThreadPool = ThreadPoolUtil.ThreadPool;
using Monitor = ThreadPoolUtil.Monitor;
#endif
using System.Threading;

using System.Windows.Forms;
using CommandLine;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using System.IO;
using Radegast;
using Settings=OpenMetaverse.Settings;
using Console = MushDLR223.Utilities.DLRConsole;

namespace Cogbot
{
    public class ConsoleApp
    {
        public static void Usage()
        {
            DLRConsole.SystemWriteLine("Usage: " + Environment.NewLine +
                    "cogbot.exe --first firstname --last lastname --pass password [--loginuri=\"uri\"] [--startpos \"sim/x/y/z\"] [--master \"master name\"] [--masterkey \"master uuid\"] [--gettextures] [--scriptfile \"filename\"] [--nogui]");
        }

        public static DLRConsole consoleBase;


        [STAThread]
        public static void RunConsoleApp(string[] args)
        {
            ClientManagerConfig.UsingCogbotFromRadegast = false;
            ClientManagerConfig.UsingRadegastFromCogbot = true;
            ConsoleLoop(args);
        }

        public static void ConsoleLoop(string[] args)
        {
            SetAllCommandLineOptions(args);
            consoleBase = consoleBase ?? new DLRConsole("textform");
            ClientManager manager = ClientManager.SingleInstance;
            manager.outputDelegate = new OutputDelegate(WriteLine);
            manager.Run();
            //bool onlyConsole = !ClientManagerConfig.ShowRadegast;
            //RunInThread(Thread.CurrentThread.ApartmentState, manager.Run, onlyConsole);
        }


        public void WriteLine(ConsoleColor color, string format, params object[] args)
        {
            DLRConsole.WriteConsoleLine(color, format, args);
            /*
            try
            {
                if (color != ConsoleColor.White)
                    DLRConsole.SystemForegroundColor = color;
                DLRConsole.SystemWriteLine(format, args);
            }
            finally
            {
                Console.ResetColor();                
            }
             */
        }
        public string CmdPrompt(string p)
        {
            Console.SystemWrite(p);
            Console.SystemFlush();
            return Console.ReadLine();
        }

        public static void WriteLine(string str, params object[] args)
        {
            if (consoleBase == null)
            {
                DLRConsole.DebugWriteLine(str, args);
                return;
            }
            int index = str.IndexOf("]");
            if (index > 0 && str.StartsWith("["))
            {
                string sender = str.Substring(0, index).Trim();
                if (sender.StartsWith("[")) sender = sender.Substring(1);
                str = str.Substring(index + 1).Trim();
                consoleBase.Notice(sender, str, args);
            }
            else
            {
                consoleBase.Notice(str, args);
            }
        }

        public static void Notice(string sender, string str, params object[] args)
        {
            if (consoleBase == null)
            {
                try
                {
                    DLRConsole.SystemWriteLine("[" + sender + "] " + str, args);
                }
                catch (FormatException)
                {

                    DLRConsole.SystemWriteLine("[" + sender + "] " + str);
                }
                return;
            }
            consoleBase.Notice(sender, str, args);
        }

        /// <summary>
        /// ///////////MINAIN COGBOT PROGRAM //////////////////////////
        /// </summary>
        /// <param name="args"></param>
        [STAThread]
        public static void Main(string[] args)
        {
            args = SetAllCommandLineOptions(args);
            if (!ClientManagerConfig.UsingCogbotFromRadegast)
            {
                if (ChangeLCD) SetCurrentDirectory(typeof(Cogbot.ConsoleApp));
            }
            if (!ClientManagerConfig.UsingRadegastFromCogbot)
            {
                if (ClientManagerConfig.ShowRadegast || ClientManagerConfig.UsingCogbotFromRadegast)
                {
                    RadegastMain(args);
                }
            }
            RunConsoleApp(args);
        }

        private static string[] StartupPreparsed = null;
        public static string[] PreparseCommandArgs()
        {
            if (StartupPreparsed != null) return StartupPreparsed;
            string[] use = Environment.GetCommandLineArgs() ?? new string[0];
            if (use.Length > 0)
            {
                string arg0 = use[0].ToLower();
                if (arg0.EndsWith(".vshost.exe"))
                {
                    ClientManagerConfig.IsVisualStudio = true;
                    arg0 = arg0.Replace(".vshost.exe", ".exe");
                }
                ClientManagerConfig.StartLispThreadAtPluginInit = true;

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
            StartupPreparsed = use;
            return use;
        }

        static public Exception _appException = null;
        public static void DoAndExit(ThreadStart o)
        {
            try
            {
                ExitCode = 0;
                o();
                /*
                Thread t = new Thread(o, 0);
                t.SetApartmentState(ApartmentState.STA);
                t.Start();
                t.Join();
                */
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

            if (UseApplicationExit)
            {
                Application.Exit();
                Environment.Exit(ExitCode);
            }
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

        public static TextFilter filter = DLRConsole.TheGlobalLogFilter;// //new TextFilter() { "+*" };
        static public void FilteredWriteLine(string str, params object[] args)
        {

            OutputDelegate del = new OutputDelegate(ClientManager.Real ?? DLRConsole.DebugWriteLine);
            if (ClientManager.Filter == null)
            {
                ClientManager.Filter = new OutputDelegate(del);
            }
            filter.writeDebugLine(del, str, args);
        }


        public static string[] SetAllCommandLineOptions(string[] args)
        {
            ResizeThreadPools();
            args = SetCogbotCommandLineOptions(args);
            if (ClientManagerConfig.DosBox) AllocConsole();
            SetRadegastCommandLine(args);
            if (DLRConsole.HasWinforms)
            {
                Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);
            }
            else
            {
                //  NativeMethods.AllocConsole();
                // Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);
            }
            // MainProgram.CommandLine = new CommandLine {DisableSound = false};

            if (ClientManager.MainThread == null)
            {
                ClientManager.MainThread = Thread.CurrentThread;
            }
            return args;
        }

        public static bool SetttedCogbotOptions = false;
        /// <summary>
        /// runs in current thread
        /// </summary>
        /// <param name="args"></param>
        [STAThread]
        public static string[] SetCogbotCommandLineOptions(string[] args)
        {
            string[] use = PreparseCommandArgs();
            args = args ?? use ?? new string[0];
            if (SetttedCogbotOptions) return args;
            SetttedCogbotOptions = true;
            var DLRConsoleError = DLRConsole.Error;
            DLRConsole.DetectMainEnv(DLRConsoleError);
            
            if (ClientManager.MainThread == null)
            {
                ClientManager.MainThread = Thread.CurrentThread;
            }

            ClientManagerConfig.arguments = new Parser(args);
            ClientManagerConfig.arguments.Destructive = true;
            string[] oArgs;
            // Change current working directory to Program install dir?
            if (ClientManagerConfig.arguments.GetWithoutFlag("--lcd", out oArgs))
            {
                ChangeLCD = true;
            }
            string newDir;
            // Change current working directory to  --cd whatnot
            if (ClientManagerConfig.arguments.TryGetValueWithout("--cd", out newDir, true, out oArgs))
            {
                ChangeLCD = false;
                Directory.SetCurrentDirectory(newDir);
            }
            if (ClientManagerConfig.arguments.GetWithoutFlag("--noexcpt", out oArgs))
            {
                SetExceptionHandlers(false);
            }
            else SetExceptionHandlers(true);

            if (ClientManagerConfig.arguments.GetWithoutFlag("--console", out oArgs))
            {
                ClientManagerConfig.DosBox = true;
                ClientManagerConfig.CogbotREPL = true;
            }

            if (ClientManagerConfig.arguments.GetWithoutFlag("--noconfig", out oArgs))
            {
                ClientManagerConfig.arguments = new Parser(oArgs);
                ClientManagerConfig.NoLoadConfig = true;
            }
            if (ClientManagerConfig.arguments.GetWithoutFlag("--nogui", out oArgs))
            {
                ClientManagerConfig.ShowRadegast = false;
                ClientManagerConfig.DosBox = true;
            }
            else
            {
                try
                {
                    // probe for X windows
                    var f = System.Windows.Forms.SystemInformation.MenuAccessKeysUnderlined;
                }
                catch (Exception)
                {
                    // X windows missing
                    ClientManagerConfig.ShowRadegast = false;
                }
            }
            
            if (!ClientManagerConfig.ShowRadegast) ClientManagerConfig.CogbotREPL = true;
            if (ClientManagerConfig.arguments.GetWithoutFlag("--gui", out oArgs))
            {
                ClientManagerConfig.ShowRadegast = true;
            }
            if (ClientManagerConfig.arguments.GetWithoutFlag("--repl", out oArgs))
            {
                ClientManagerConfig.CogbotREPL = true;
            }
            if (ClientManagerConfig.arguments.GetWithoutFlag("--norepl", out oArgs))
            {
                ClientManagerConfig.CogbotREPL = false;
            }

            args = ClientManagerConfig.arguments.tokens;
            ClientManagerConfig.arguments = new Parser(args);
            return args;
        }

        public static void SetCurrentDirectory(Type type)
        {
            Directory.SetCurrentDirectory(Path.GetDirectoryName(type.Assembly.Location));
        }

        public static void AllocConsole()
        {
            if (!ClientManager.AllocedConsole)
            {
                ClientManager.AllocedConsole = DLRConsole.AllocConsole();
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
            if (!ClientManagerConfig.UsingRadegastFromCogbot)
            {
                ClientManagerConfig.UsingCogbotFromRadegast = true;
                ClientManagerConfig.UsingRadegastFromCogbot = false;
            }
            // Increase the number of IOCP threads available. Mono defaults to a tragically low number
            args = SetAllCommandLineOptions(args);
            // Change current working directory to Radegast install dir
            if (ChangeLCD) SetCurrentDirectory(typeof(RadegastInstance));

            Thread t = ClientManager.InSTAThread(StartRadegast, "StartExtraRadegast");
            if (ClientManagerConfig.UsingCogbotFromRadegast) t.Join();
            //StartRadegast();
        }

        private static bool HaveSetRadegastCmdLine = false;
        public static void SetRadegastCommandLine(string[] args)
        {
            args = SetCogbotCommandLineOptions(args);

            if (HaveSetRadegastCmdLine) return;
            HaveSetRadegastCmdLine = true;
            ResizeThreadPools();
            
            // Read command line options
            Radegast.CommandLine CommandLine = MainProgram.CommandLine = new Radegast.CommandLine();
            CommandLineParser parser = new CommandLineParser(new CommandLineParserSettings(DLRConsole.Out));
            TextWriter tw = new StringWriter();
            if (!parser.ParseArguments(args, CommandLine, tw))
            {
                //  Environment.Exit(1);
            }
        }

        // Increase the number of IOCP threads available. Mono defaults to a tragically low number
        private static void ResizeThreadPools()
        {
            int workerThreads, iocpThreads;
            ThreadPool.GetMaxThreads(out workerThreads, out iocpThreads);

            if (workerThreads < 500 || iocpThreads < 1000)
            {
                if (workerThreads < 500) workerThreads = 500;
                if (iocpThreads < 1000) iocpThreads = 1000;
                ThreadPool.SetMaxThreads(workerThreads, iocpThreads);
            }
        }

        public static void StartRadegast()
        {
            // Create main Radegast instance

            RadegastInstance instance = new RadegastInstance(new GridClient());
            // See if we only wanted to display list of grids

            var mf = instance.MainForm;
            Application.Run(mf);
            instance = null;
        }

        public static bool UsingExceptionHandlers = false;
        public static bool UseApplicationExit = true;
        public static bool ChangeLCD = true;

        public static void SetExceptionHandlers(bool use)
        {
            if (use == UsingExceptionHandlers) return;
            UsingExceptionHandlers = use;
            if (use)
            {
                Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException);
                Application.ThreadException += HandleThreadException;
                Application.ApplicationExit += HandleAppExit;
                Application.EnterThreadModal += HandleEnterThreadModal;
                Application.LeaveThreadModal += HandleLeaveThreadModal;
                Application.ThreadExit += HandleThreadExit;
                AppDomain.CurrentDomain.UnhandledException += HandleUnhandledException;
                AppDomain.CurrentDomain.ProcessExit += HandleProcessExit;
            }
            else
            {
                // or ThrowException?
                Application.SetUnhandledExceptionMode(UnhandledExceptionMode.Automatic);
                Application.ThreadException -= HandleThreadException;
                Application.ApplicationExit -= HandleAppExit;
                Application.EnterThreadModal -= HandleEnterThreadModal;
                Application.LeaveThreadModal -= HandleLeaveThreadModal;
                Application.ThreadExit -= HandleThreadExit;
                AppDomain.CurrentDomain.UnhandledException -= HandleUnhandledException;
                AppDomain.CurrentDomain.ProcessExit -= HandleProcessExit;
            }
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

        /// <summary>
        /// runs in a STA thread (by creating one)  Does not "join"
        /// </summary>
        /// <param name="args"></param>        
        public static void RunInThread(ApartmentState must, ThreadStart runMain, bool blocking)
        {
            if (!blocking || Thread.CurrentThread.ApartmentState != must)
            {
                Thread newThread = new Thread(runMain);
                newThread.SetApartmentState(must);
                newThread.Start();
                if (blocking) newThread.Join();
                return;
            }
            else
            {
                runMain();
            }
        }
    }
}