using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Net.Mail;
using System.Reflection;
using System.Runtime.Serialization;
using System.Text.RegularExpressions;
using System.Threading;
using System.Web;
using System.Windows.Forms;
using System.Xml;
using AIMLbot;
using AltAIMLbot;
using AltAIMLParser;
using LAIR.ResourceAPIs.WordNet;
using LogicalParticleFilter1;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using org.opencyc.api;
using RTParser.AIMLTagHandlers;
using RTParser.Database;
using RTParser.GUI;
using RTParser.Utils;
using RTParser.Variables;
using RTParser.Web;
using Console=System.Console;
using UPath = RTParser.Unifiable;
using UList = System.Collections.Generic.List<RTParser.Utils.TemplateInfo>;
using MasterRequest = AltAIMLParser.Request;
using Mono.CSharp;
using Action=System.Action;
using Attribute=System.Attribute;

#if (COGBOT_LIBOMV || USE_STHREADS)
using ThreadPoolUtil;
using Thread = ThreadPoolUtil.Thread;
using ThreadPool = ThreadPoolUtil.ThreadPool;
using Monitor = ThreadPoolUtil.Monitor;
#endif

namespace RTParser
{
    /// <summary>
    /// </summary>
    public partial class AltBot
    {
        internal readonly Dictionary<string, SystemExecHandler> ConsoleCommands = new Dictionary<string, SystemExecHandler>();
        int UseHttpd = -1;
        public static AltBot ConsoleRobot;
        public static string AIMLDEBUGSETTINGS =
            "clear -spam +user +bina +error +aimltrace +cyc -dictlog -tscore +loaded";

        //    "clear +*";
        public static string[] RUNTIMESETTINGS = { "-GRAPH", "-USERTRACE" };

        public static string[] RUNTIMESETTINGS_RADEGAST = {
                                                              "CLEAR",
                                                              "+*",
                                                              "+STARTUP",
                                                              "+ERROR",
                                                              "+EXCEPTION",
                                                              "+GRAPH",
                                                              "+AIMLFILE",
                                                            //  "-AIMLLOADER",
                                                              "-DEBUG9",
                                                            //  "-ASSET"
                                                          };

        public static readonly TextFilter LoggedWords = new TextFilter(RUNTIMESETTINGS_RADEGAST)
                                                            {
                                                            }; //maybe should be ERROR", "STARTUP


        #region Logging methods

        /// <summary>
        /// The last message to be entered into the log (for testing purposes)
        /// </summary>
        public string LastLogMessage = String.Empty;

        public OutputDelegate outputDelegate;

        public void writeToLog(Exception e)
        {
            writeDebugLine(writeException(e));
        }

        static public String writeException(Exception e)
        {
            if (e == null) return "-write no exception-";
            string s = "ERROR: " + e.Message + " " + e.StackTrace;
            Exception inner = e.InnerException;
            if (inner != null && inner != e)
                s = s + writeException(inner);
            return s;
        }

        /// <summary>
        /// Writes a (timestamped) message to the Processor's log.
        /// 
        /// Log files have the form of yyyyMMdd.log.
        /// </summary>
        /// <param name="message">The message to log</param>
        //public OutputDelegate writeToLog;
        private static string lastMessage = null;
        public void writeToLog(string message, params object[] args)
        {
            message = SafeFormat(message, args);
            if (String.IsNullOrEmpty(message)) return;
            if (lastMessage == message)
            {
                return;
            }
            lastMessage = message;
            bool writeToConsole = true; // outputDelegate == null;

            //message = message.Trim() + Environment.NewLine;
            if (outputDelegate != null)
            {
                try
                {
                    outputDelegate(message);
                }
                catch (Exception)
                {
                    writeToConsole = true;
                }
            }
            if (outputDelegate != writeDebugLine)
            {
                if (writeToConsole) writeDebugLine(message);
            }
            message = string.Format("[{0}]: {1}", DateTime.Now, Trim(message));
            writeToFileLog(message);
        }

        public Object LoggingLock = new object();
        public IDisposable HttpTextServer;
        public static int NextHttp = 5580;
        public static int NextHttpIncrement = 100;
        private TimeSpan MaxWaitTryEnter = TimeSpan.FromSeconds(10);
        internal AltBotCommands AltBotcommands;
        private static AIMLPadEditor GUIForm;
        private static Thread GUIFormThread;


        public void writeToFileLog(string message)
        {
            LastLogMessage = message;
            if (!IsLogging) return;
            lock (LoggingLock)
            {
                //  this.LogBuffer.Add(DateTime.Now.ToString() + ": " + message + Environment.NewLine);
                LogBuffer.Add(message);
                if (LogBuffer.Count > MaxLogBufferSize - 1)
                {
                    // Write out to log file
                    HostSystem.CreateDirectory(PathToLogs);

                    Unifiable logFileName = DateTime.Now.ToString("yyyyMMdd") + ".log";
                    FileInfo logFile = new FileInfo(HostSystem.Combine(PathToLogs, logFileName));
                    StreamWriter writer;
                    if (!logFile.Exists)
                    {
                        writer = logFile.CreateText();
                    }
                    else
                    {
                        writer = logFile.AppendText();
                    }

                    foreach (string msg in LogBuffer)
                    {
                        writer.WriteLine(msg);
                    }
                    writer.Close();
                    LogBuffer.Clear();
                }
            }
            if (!Equals(null, WrittenToLog))
            {
                WrittenToLog();
            }
        }

        #endregion

        private static void MainConsoleWriteLn(string fmt, params object[] ps)
        {
            writeDebugLine("--" + fmt, ps);
        }

        public static void Main(string[] args)
        {
            //Application.Run(new RTParser.GUI.AIMLPadEditor("layout check",new AltBot()));
            Run(args);
        }
        public static void Run(string[] args)
        {
            AltBot myBot = null;
            DLRConsole.DebugLevel = 6;
            TaskQueueHandler.TimeProcess("ROBOTCONSOLE: STARTUP", () => { myBot = Startup(args); });

            if (new List<string>(args).Contains("--gui"))
            {
                TaskQueueHandler.TimeProcess("ROBOTCONSOLE: RUN", () => RunGUI(args, myBot, MainConsoleWriteLn));
            } else
            {
                TaskQueueHandler.TimeProcess("ROBOTCONSOLE: RUN", () => Run(args, myBot, MainConsoleWriteLn));
            }
        }

        private static AltBot Startup(string[] args)
        {
            AltBot myBot;
            lock (typeof(AltBot))
            {
                TaskQueueHandler.TimeProcess("ROBOTCONSOLE: PREPARE", () => Prepare(args));
                myBot = ConsoleRobot;
            }
            TaskQueueHandler.TimeProcess("ROBOTCONSOLE: LOAD", () => Load(args, myBot, MainConsoleWriteLn));
            TaskQueueHandler.TimeProcess("ROBOTCONSOLE: NOP", () => { });
            return myBot;
        }

        public static void Prepare(string[] args)
        {
            AltBot myBot = ConsoleRobot = ConsoleRobot ?? new AltBot();
            OutputDelegate writeLine = MainConsoleWriteLn;
            for (int index = 0; index < args.Length; index++)
            {
                string s = args[index].ToLower();
                if (s == "--httpd")
                {
                    UseBreakpointOnError = false;
                    if (index + 1 < args.Length)
                    {
                        int portNum;
                        if (int.TryParse(args[index + 1], out portNum))
                        {
                            myBot.UseHttpd = portNum;
                            if (portNum == NextHttp)
                            {
                                NextHttp += NextHttpIncrement;
                            }
                        }
                        else
                        {
                            myBot.UseHttpd = NextHttp;
                            NextHttp += NextHttpIncrement;
                        }
                    }
                }
            }
        }

        public void StartHttpServer()
        {
            lock (initialSettingsLoadedLock) StartHttpServer_unlocked();
        }
        private void StartHttpServer_unlocked()
        {
            if (HttpTextServer != null) return;
            string[] oArgs;
            if (UseHttpd > 0 && this.HttpTextServer == null)
            {
                ScriptExecutorGetter geter = new WebScriptExecutor(this);
                HttpTextServer = MushDLR223.Utilities.HttpServerUtil.CreateHttpServer(geter, UseHttpd, BotUserID);
                RegisterObject("robot", HttpTextServer);
            }
        }

        public static void Load(string[] args, AltBot myBot, OutputDelegate writeLine)
        {
            myBot.outputDelegate = null; /// ?? Console.Out.WriteLine;

            // writeLine = MainConsoleWriteLn;
            bool gettingUsername = false;
            //myBot.loadGlobalBotSettings();
            string myName = "BinaBot Daxeline";
            //myName = "Test Suite";
            //myName = "Kotoko Irata";
            //myName = "Nephrael Rae";
            if (args != null)
            {
                string newName = "";
                for (int i = 0; i < args.Length; i++)
                {
                    string s = args[i];
                    if (s == "--breakpoints")
                    {
                        UseBreakpointOnError = true;
                        continue;
                    }
                    if (s == "--servitor")
                    {
                        //myBot.useServitor = true;
                        continue;
                    }
                    if (s == "--cycon")
                    {
                        myBot.CycEnabled = true;
                        continue;
                    }
                    if (s == "--noaiml")
                    {
                        myBot.needAimlFilesLoaded = false;
                        myBot.servitor.skiploadingAimlFiles = true;
                        continue;
                    }
                    if (s == "--cycoff")
                    {
                        myBot.CycEnabled = false;
                        continue;
                    }
                    if (s == "--nobreakpoints")
                    {
                        UseBreakpointOnError = false;
                        continue;
                    }
                    if (s == "--cmdprefix")
                    {
                        cmdPrefix = args[i + 1];
                        i++;
                        continue;
                    }
                    if (s == "--aiml" || s == "--botname" || s == "--name")
                    {
                        gettingUsername = true;
                        continue;
                    }
                    if (s.StartsWith("-"))
                    {
                        gettingUsername = false;
                        writeLine("passing option '" + s + "' to another program");
                        continue;
                    }
                    if (gettingUsername)
                    {
                        newName += " " + s;
                    }
                }
                newName = newName.Trim();
                if (newName.Length > 1)
                {
                    myName = newName;
                }
            }
            writeLine(Environment.NewLine);
            writeLine("Botname: " + myName);
            writeLine(Environment.NewLine);
            //myBot.isAcceptingUserInput = false;
            writeLine("-----------------------------------------------------------------");
            myBot.SetName(myName);
            myBot.sayProcessor = new sayProcessorDelegate((s) => Console.WriteLine("SP: " + s));
            myBot.servitor.loadComplete();
        }
        public static void RunGUI(string[] args, AltBot myBot, OutputDelegate writeLine)
        {
            GUIForm = new GUI.AIMLPadEditor(myBot.NameAsSet, myBot);
            Application.Run(GUIForm);
        }

        public static void Run(string[] args, AltBot myBot, OutputDelegate writeLine)
        {
            string evidenceCode = "<topic name=\"collectevidencepatterns\"> " +
                                  "<category><pattern>HOW ARE YOU</pattern><template>" +
                                  "<think><setevidence evidence=\"common-greeting\" prob=1.0 /></think>" +
                                  "</template></category></topic>" +
                                  "";
            //Added from AIML content now
            // myBot.AddAiml(evidenceCode);
            User myUser = myBot.LastUser;
            var myUsersname = myUser.UserName;
            Request request = myUser.CreateRequest("current user toplevel", Unifiable.EnglishNothing, true, RequestKind.EventProcessor);
            myUser.LastRequest = request;
            myBot.BotDirective(myUser, request, "@help", writeLine);
            writeLine("-----------------------------------------------------------------");
            AIMLDEBUGSETTINGS = "clear +*";
            myBot.BotDirective(myUser, request, "@log " + AIMLDEBUGSETTINGS, writeLine);
            writeLine("-----------------------------------------------------------------");
            DLRConsole.SystemFlush();

            //string userJustSaid = String.Empty;
            myBot.LastUser = myUser;
            while (true)
            {
                myUser = myBot.LastUser;
                writeLine("-----------------------------------------------------------------");
                string input = TextFilter.ReadLineFromInput(writeLine, myUser.UserName + "> ");
                if (input == null)
                {
                    Environment.Exit(0);
                }
                input = Trim(input);
                if (input.ToLower() == "@quit")
                {
                    return;
                }
                if (input.ToLower() == "@exit")
                {
                    Environment.Exit(Environment.ExitCode);
                }
                RequestResult requestAcceptInput;
                myBot.AcceptInput(writeLine, input, myUser, true, RequestKind.ChatRealTime, out requestAcceptInput);

            }
        }

        public void AcceptInput(OutputDelegate writeLine, string input, User myUser, bool isToplevel, RequestKind kind, out RequestResult acceptInputResult)
        {
            AltBot myBot = this;
            if (_botAsUser == null)
            {
                writeLine("This bot has nopt be constructed correctly!");
            }
            User BotAsAUser = myBot.BotAsUser;
            myUser = myUser ?? myBot.LastUser;
            string myName = BotAsAUser.UserName;
            {
                writeLine("-----------------------------------------------------------------");
                if (string.IsNullOrEmpty(input))
                {
                    writeLine("{0}: {1}", myUser.UserName, myUser.JustSaid);
                    if (!WaitUntilVerbalOutput)
                    {
                        writeLine("---------------------");
                        writeLine("{0}: {1}", myName, BotAsAUser.JustSaid);
                    }
                    writeLine("-----------------------------------------------------------------");
                    acceptInputResult = new RequestResult(myUser, input, BotAsUser, myUser.That);
                    acceptInputResult.chatOutputillBeInBackground = true;
                    return;
                }
                try
                {
                    Unifiable cmdprefix = cmdPrefix ?? myUser.Predicates.grabSetting("cmdprefix");
                    if (cmdprefix == null) cmdprefix = myBot.GlobalSettings.grabSetting("cmdprefix");
                    if (!input.Contains("@") && !IsNullOrEmpty(cmdprefix))
                    {
                        input = cmdprefix.AsString() + " " + input;
                    }
                    if (input == "@")
                    {
                        input = myUser.JustSaid;
                    }

                    bool myBotBotDirective = false;
                    if (useServitor && !input.StartsWith("@"))
                    {
                        // See what the servitor says
                        updateRTP2Sevitor(myUser);
                        writeLine(myName + "> " + servitor.respondToChat(input, myUser, isToplevel, kind, out acceptInputResult));
                        updateServitor2RTP(myUser);
                    }
                    else
                    {
                        throw servitor.curBot.RaiseError("Should not be here?!");
                        if (!input.StartsWith("@"))
                        {
                            //      string userJustSaid = input;
                            input = "@locally " + myUser.UserName + " - " + input;
                        }
                        User user = myUser;
                        TaskQueueHandler.TimeProcess(
                            "ROBOTCONSOLE: " + input,
                            () =>
                            {
                                myBotBotDirective = myBot.BotDirective(user, input, writeLine);
                            });
                        //if (!myBotBotDirective) continue;
                    }
                    writeLine("-----------------------------------------------------------------");
                    writeLine("{0}: {1}", myUser.UserName, myUser.JustSaid);
                    if (!WaitUntilVerbalOutput)
                    {
                        writeLine("---------------------");
                        writeLine("{0}: {1}", myName, BotAsAUser.JustSaid);
                    }
                    writeLine("-----------------------------------------------------------------");
                }
                catch (Exception e)
                {
                    throw servitor.curBot.RaiseError("Should not be here?!");
                    writeLine("Error: {0}", e);
                }
            }
        }


        public object LightWeigthBotDirective(string input, Request request)
        {
            StringWriter sw = new StringWriter();
            OutputDelegate all = new OutputDelegate((s, args) =>
                                                        {
                                                            request.WriteLine(s, args);
                                                            sw.WriteLine(s, args);
                                                            writeDebugLine(s, args);
                                                        });
            var ss = input.Split(new string[] {"@"},StringSplitOptions.RemoveEmptyEntries);
            bool b = false;
            foreach (string s in ss)
            {
                if (BotDirective(request.Requester, request, "@" + s, all)) b = true;
            }
            string sws = sw.ToString();
            if (!b) return Unifiable.FAIL_NIL;
            return sws;
        }

        public bool BotDirective(User user, string input, OutputDelegate console)
        {
            try
            {
                User requester = (user ?? LastUser ?? BotAsUser);
                //Request request = requester.CreateRequest(input, BotAsUser);
                return BotDirective(requester, (Request) null, input, console);
            }
            catch (Exception e)
            {
                DLRConsole.DebugWriteLine("ERROR in BotDirective: " + e);
                return false;
            }
        }

        public bool BotDirective(User user, Request request, string input, OutputDelegate console)
        {
            try
            {
                user.LastRequest = request ?? user.LastRequest;
                return BotDirective(user, input, console, null);
            }
            catch (Exception e)
            {
                DLRConsole.DebugWriteLine("ERROR in BotDirective: " + e);
                return false;
            }
        }
        public bool BotDirective(User user, string input, OutputDelegate console, ThreadControl control)
        {
            User targetBotUser = this.BotAsUser;
            if (input == null) return false;
            input = Trim(input);
            if (input == "") return false;
            if (input.StartsWith("@"))
            {
                input = input.TrimStart(new[] { ' ', '@' });
            }
            if (input == "gui")
            {
                if (GUIFormThread == null)
                {
                    GUIFormThread = new Thread(() => {
                        GUIForm = GUIForm ?? new GUI.AIMLPadEditor(NameAsSet, this);
                        Application.Run(GUIForm);
                    });
                    GUIFormThread.TrySetApartmentState(ApartmentState.STA);
                    GUIFormThread.Start();
                }
                if (GUIForm != null) GUIForm.Show();               
                return true;
            }
            User myUser = user ?? LastUser ?? FindUser(UNKNOWN_PARTNER) ?? ExemplarUser ?? FindOrCreateUser(UNKNOWN_PARTNER);
            int firstWhite = input.IndexOf(' ');
            if (firstWhite == -1) firstWhite = input.Length - 1;
            string cmd = Trim(ToLower(input.Substring(0, firstWhite + 1)));
            string args = Trim(input.Substring(firstWhite + 1));
            bool showHelp = false;
            if (cmd == "help")
            {
                showHelp = true;
                console("Commands are prefixed with @cmd");
                console("@help shows help -- command help comming soon!");
                console("@quit -- exits the aiml subsystem");
            }



            if (AltBotCommands.ExecAnyAtAll(this, input, myUser, cmd, console, showHelp, args, targetBotUser, control)) return true;
            if (cmd == "query" || showHelp) if (AltBotcommands.ExecQuery(myUser.LastRequest, cmd, console, showHelp, args, myUser))
                    return true;

            if (cmd == "servitor")
            {
                if (showHelp)
                {

                }
                else
                {
                    // See what the servitor says
                    updateRTP2Sevitor(myUser);
                    servitor.respondToChat(input, myUser);
                    updateServitor2RTP(myUser);
                }
            }

            if (showHelp) console("@user [var [value]] -- lists or changes the current users get/set vars.");
            if (cmd == "user")
            {
                return myUser.DoUserCommand(args, console);
            }
            GraphMaster G = myUser.StartGraph;
            Request request = myUser.LastRequest;

            if (G != null && G.DoGraphCommand(cmd, console, showHelp, args, request)) return true;

            if (AltBotCommands.ChGraphCmd(request, showHelp, console, cmd, myUser, args)) return true;

            if (DoLogCmd(console, showHelp, cmd, args)) return true;

            if (AltBotCommands.CallOrExecCmd(request, showHelp, console, cmd, myUser, args)) return true;

            bool uc = BotUserDirective(myUser, input, console);
            if (uc)
            {
                return true;
            }
            if (showHelp)
            {
                string help = "Exec handlers: ";
                lock (ExecuteHandlers)
                {
                    foreach (KeyValuePair<string, SystemExecHandler> systemExecHandler in ExecuteHandlers)
                    {
                        help += " @" + systemExecHandler.Key;
                    }
                }
                console(help);
                return true;
            }
            SystemExecHandler handler;
            if (SettingsDictionaryReal.TryGetValue(ExecuteHandlers, cmd, out handler))
            {
                object result = handler(args, request);
                console("" + result);
                return true;
            }

            if (AltBotCommands.TaskCommand(request, console, cmd, args)) return true;
            if (cmd == "anim") return true;
            console("unknown: @" + input);
            return false;
        }


        private static Evaluator _Evaluator = null;
        private static CompilerContext ctx;
        private static CompilerSettings compset;
        private static ReportPrinter rp;

        static public Mono.CSharp.Evaluator GetEvalInstance()
        {
            if (_Evaluator == null)
            {
                if (ctx == null)
                {
                    if (compset == null) compset = new CompilerSettings();
                    if (rp == null)
                    {
                        rp = new StreamReportPrinter(System.Console.Out);
                    }
                    ctx = new CompilerContext(compset, rp);
                }
                
                _Evaluator = new Evaluator(ctx);
                _Evaluator.ReferenceAssembly(typeof(AltBot).Assembly);
                _Evaluator.Run("using RTParser;");
                _Evaluator.Run("using AltAIMLParser;");
                _Evaluator.Run("using AltAIMLbot;");

            }
            return _Evaluator;
        }

        [ThreadStaticAttribute] public static Request currentRequest;
        private object CSharpExec(string cmd, Request requestornull)
        {
            currentRequest = requestornull;
            var ei = GetEvalInstance();
            object result;
            bool result_set;

            string r = ei.Evaluate(cmd, out result, out result_set);

            if (r != null)
                writeDebugLine("CSharpExec: error on input '{0}' left over was '{1}' " + cmd, r);

            if (result_set == false)
            {
                writeDebugLine("CSharpExec: The expression '{0}' did not set a result", cmd);
            }

            return result;
        }

        private object SIPrologExec(string cmd, Request requestornull)
        {
            string queryMT = "baseKB";
            if (requestornull != null)
            {
                currentRequest = requestornull;
            }
            if (currentRequest != null)
            {
                var rps = currentRequest.RequesterPredicates;
                queryMT = rps.NameSpace;
                queryMT = rps.grabSetting("querymt,kb,mt,behavourmt,behavour") ?? queryMT;
            }
            object retval = null;
            
            
            prologEngine.askQuery(prologEngine.ParseQuery(cmd, queryMT), queryMT, true,
                                  (env) =>
                                      {
                                          foreach (var varname in new[] {"VALUE", "X", "OUT", "RETURN", "RETVAL"})
                                          {
                                              SIProlog.Part returnThis;
                                              if (env.TryGetValue(varname, out returnThis))
                                              {
                                                  retval = returnThis;
                                                  return false;
                                              }
                                          }
                                          var htv = env.ht.Values;
                                          if (htv.Count > 0)
                                          {
                                              foreach (var v in htv)
                                              {
                                                  retval = v;
                                                  return false;
                                              }
                                          }
                                          retval = "True";
                                          return false;
                                      });
             
            if (retval != null) return retval;
            return "False";
        }
    }

    public class RequestResult:IDisposable
    {
        public Result result;
        public Request request;
        public User Speaker, Hearer;
        public string Input, That;
        public bool chatOutputillBeInBackground;

        public RequestResult(User curUser, string input, User targetUser, string that)
        {
            Speaker = curUser;
            this.Input = input;
            Hearer = targetUser;
            That = that;
        }

        public string Error;

        public string OutputText;

        #region IDisposable Members

        public void Dispose()
        {
            throw new NotImplementedException();
        }

        #endregion
    }

    public partial class AltBotCommands //: AltBot
    {
        private static bool SplitOff(string args, string s, out string user, out string said)
        {
            return TextPatternUtils.SplitOff(args, s, out user, out said);
        }

        public static AltBot robotIn;

        public AltBotCommands(AltBot bot)
        {
            robotIn = bot;
        }

        internal static bool ExecAnyAtAll(AltBot robot, string input, User myUser, string cmd, OutputDelegate console, bool showHelp, string args, User targetBotUser, ThreadControl control)
        {
            
            if (showHelp)
                console(
                    "@withuser <user> - <text>  -- (aka. simply @)  runs text/command intentionally setting LastUser");

            if (showHelp)
                console(
                    "@locally <user> - <text>  -- runs text/command not intentionally not setting LastUser");
            if (cmd == "locally" || cmd == "@" || cmd == "withuser")
            {
                string said;
                string user;
                if (!SplitOff(args, "-", out user, out said))
                {
                    user = myUser.UserName;
                    said = args;
                }
                bool waitUntilVerbalOutput = ((cmd == "@" || cmd == "withuser") || robot.WaitUntilVerbalOutput);               
                User wasUser = robot.FindUser(user);
                User targetUser = robot.GetTargetUser(null, said, robot.BotAsUser);
                string that = targetUser.JustSaid;
                robot.HeardSomeoneSay1Sentence(false, true, myUser, targetUser, said, myUser.LastResult, control);
                User CurrentUser = robot.GetCurrentUser(user);
                MasterRequest request = CurrentUser.CreateRequest(said, that, targetUser, true, RequestKind.ChatRealTime);
                request.IsTraced = true;
                request.OriginalSalientRequest = request;
                if (cmd == "locally")
                {
                    waitUntilVerbalOutput = false;
                }
                bool saveResultsOnJustHeard = !waitUntilVerbalOutput;
                request.SaveResultsOnJustHeard = saveResultsOnJustHeard;                
                request.ResponderSelfListens = saveResultsOnJustHeard;
                Result res = robot.GlobalChatWithUser(request, said, user, null, AltBot.writeDebugLine,
                                                      saveResultsOnJustHeard, saveResultsOnJustHeard, true,
                                                      RequestKind.ChatRealTime);
                request.ResponderSelfListens = false;
                // detect a user "rename"
                bool userChanged = robot.DetectUserChange(myUser, wasUser, user);
                User theResponder = (res.Responder ?? res.request.Responder).Value;
                if (userChanged)
                {
                    //myUser = FindUser(user);
                    request.SetSpeakerAndResponder(myUser, theResponder);
                    if (cmd == "withuser" || cmd == "@")
                    {
                        robot.LastUser = myUser;
                    }
                }
                var justsaid = robot.OutputResult(res, console, false);
                if (theResponder == null)
                {
                    theResponder = (myUser == targetBotUser) ? request.Requester : targetBotUser;
                    robot.writeToLog("Making the responder " + theResponder);
                }
                if (theResponder == null)
                {
                    return true;
                }
                myUser.LastResponder = theResponder;
                if (saveResultsOnJustHeard)
                {
                    theResponder.JustSaid = justsaid;
                    // ReSharper disable ConditionIsAlwaysTrueOrFalse
                    if (robot.ProcessHeardPreds && request.ResponderSelfListens)
                        // ReSharper restore ConditionIsAlwaysTrueOrFalse
                        robot.HeardSelfSayResponse(false, true, theResponder, myUser, justsaid, res, control);
                }
                return true;
            }
            if (showHelp)
                console(
                    "@aimladd [graphname] <aiml/> -- inserts aiml content into graph (default LastUser.ListeningGraph )");
            if (cmd == "aimladd" || cmd == "+")
            {
                int indexof = args.IndexOf("<");
                if (indexof < 0)
                {
                    console(
                        "@aimladd [graphname] <aiml/> -- inserts aiml content into graph (default LastUser.ListeningGraph )");
                    return true;
                }
                string gn = args.Substring(0, indexof);
                GraphMaster g = robot.GetGraph(gn, myUser.StartGraph);
                String aiml = AltBot.Trim(args.Substring(indexof));
                robot.AddAiml(g, aiml, myUser.LastRequest);
                console("Done with " + args);
                return true;
            }
            /*
            if (showHelp) console("@prolog <load.pl>");
            if (cmd == "prolog")
            {
                CSPrologMain.Main(args.Split(" \r\n\t".ToCharArray(), StringSplitOptions.RemoveEmptyEntries));
                return true;
            }

            if (showHelp) console("@pl text to say");
            if (cmd == "pl")
            {
                string callme = "alicebot2(['" + string.Join("','", args.ToUpper()
                                                                        .Split(" \r\n\t".ToCharArray(),
                                                                               StringSplitOptions.RemoveEmptyEntries)) +
                                "'],Out),writeq('----------------------------------------------'),writeq(Out),nl,halt.";
                CSPrologMain.Main(new[] {callme});
                return true;
            }*/

            if (showHelp) console("@reload -- reloads any changed files ");
            if (cmd == "reload")
            {
                robot.ReloadAll();
                return true;
                //                return;//Success("WorldSystemModule.MyBot.ReloadAll();");
            }
            if (cmd == "topic" || cmd == "that")
            {
                console("*JustSaid = " + myUser.JustSaid);
                console("*that = " + myUser.That);
                console("*topic = " + myUser.Topic);
                foreach (string c in new[] { "that", "topic", "question", "he", "it", "yours_question", "they", "them", "who", "what", "when", "where", "why", "how", "name", })
                {
                    console(c + " = " + myUser.grabSetting(c));
                }
                return true;
            }
            if (cmd == "echo")
            {
                console(args);
                return true;
            }
            if (cmd == "ping")
            {
                console("pong");
                return true;
            }
            if (cmd == "gload")
            {
                console("Loading graphmasters");
                robot.ScanAndLoadGraphs();
                return true;
            }
            if (cmd == "gsave")
            {
                console("Saving graphmasters");
                robot.SaveLoadedGraphs();
                return true;
            }
            if (cmd == "gc")
            {
                console("Running lowmemory hooks with " + System.GC.GetTotalMemory(false) + "/" +
                        System.GC.GetTotalMemory(true));
                robot.RunLowMemHooks();
                console("Finish lowmemory hooks with " + System.GC.GetTotalMemory(false) + "/" +
                        System.GC.GetTotalMemory(true));
                return true;
            }
            if (showHelp) console("@load <graph> - <uri>");
            if (cmd == "load")
            {
                Request request = myUser.LastRequest;
                string graphname;
                string files;
                if (!SplitOff(args, "-", out graphname, out files))
                {
                    graphname = "current";
                    files = args;
                }
                GraphMaster G = robot.GetGraph(graphname, myUser.StartGraph);
                AIMLLoaderU loader = robot.GetLoader(request);
                LoaderOptions reqLoadOptionsValue = request.LoadOptions.Value;
                var prev = request.Graph;
                try
                {
                    request.Graph = G;
                    reqLoadOptionsValue.CtxGraph = G;
                    loader.loadAIMLURI(files, reqLoadOptionsValue);
                    // maybe request.TargetBot.ReloadHooks.Add(() => request.Loader.loadAIMLURI(args, reqLoadOptionsValue));
                    console("Done with " + files);
                }
                finally
                {
                    request.Graph = prev;
                }
                return true;
            }
            if (showHelp) console("@say[1] [who -] <text> -- fakes that the 'who' (default bot) just said it");
            if (cmd == "say" || cmd == "say1")
            {
                console(cmd + "> " + args);
                string who, said;
                if (!SplitOff(args, "-", out who, out said))
                {
                    who = targetBotUser.UserID;
                    said = args;
                }
                User factSpeaker = robot.FindOrCreateUser(who);
                User factSpeakerLastResponder = factSpeaker.LastResponder;
                User factSpeakerLastResponderValue = null;
                if (factSpeakerLastResponder != null)
                {
                    factSpeakerLastResponderValue = factSpeakerLastResponder.Value;
                }
                if (cmd == "say1")
                {
                    robot.HeardSomeoneSay1Sentence(true, true, factSpeaker, factSpeakerLastResponderValue, said,
                                                   factSpeaker.LastResult, control);
                }
                else
                {
                    robot.HeardSelfSayVerbal(true, true, factSpeaker, factSpeakerLastResponderValue, said,
                                             factSpeaker.LastResult, control);
                }
                return true;
            }

            if (showHelp)
                console(
                    "@set [type] [name [value]] -- emulates get/set tag in AIML.  'type' defaults to =\"user\" therefore same as @user");
            if (cmd == "set")
            {
                console(robot.DefaultPredicates.ToDebugString());
                return myUser.DoUserCommand(args, console);
                return true;
            }
            //Request request = robot.LastRequest;
            if (ExecCmdSetVar(robot, showHelp, input, console, cmd, args, targetBotUser)) return true;
            if (ExecCmdBot(robot, showHelp, console, cmd, args, targetBotUser)) return true;

            if (showHelp || cmd == "proof" || cmd == "botproof")
            {
                lock (myUser.TemplatesLock)
                {
                if (AltBotCommands.ExecProof(robot, cmd, console, showHelp, args, myUser))
                    return true;
            }}
            return false;
        }

        [HelpText("@setvar dictname.name [value] -- get/sets a variable using a global namespace context")]
        [CommandText("setvar")]
        private static bool ExecCmdSetVar(AltBot robot, bool showHelp, string input, OutputDelegate console, string cmd, string args, User myUser)
        {
         //   AltBot robot = request.TargetBot;
            if (showHelp)
                console(
                    "@setvar dictname.name [value] -- get/sets a variable using a global namespace context");
            if (cmd == "setvar")
            {
                myUser.DoUserCommand(args, console);
                ((SettingsDictionaryReal)robot.GlobalSettings).DoSettingsCommand(input, console); ;
                return myUser.DoUserCommand(args, console);
            }
            return false;
        }

        [HelpText("@bot [var [value]] -- lists or changes the bot GlobalPredicates.\n  example: @bot ProcessHeardPreds True or @bot ProcessHeardPreds False")]
        [CommandText("bot")]
        private static bool ExecCmdBot(AltBot robot, bool showHelp, OutputDelegate console, string cmd, string args, User targetBotUser)
        {
            if (showHelp) console("@bot [var [value]] -- lists or changes the bot GlobalPredicates.\n  example: @bot ProcessHeardPreds True or @bot ProcessHeardPreds False");
            if (cmd == "bot")
            {
                console(robot.HeardPredicates.ToDebugString());
                console(((SettingsDictionaryReal) robot.RelationMetaProps).ToDebugString());
                return targetBotUser.DoUserCommand(args, console);
            }
            return false;
        }


        [HelpText("@[bot]proof [clear|enable|reset|disable|[save [filename.aiml]]] - clears or prints a content buffer being used")]
        [CommandText("proof", "prf", "botproof")]
        static internal bool ExecProof(AltBot robot, string cmd, OutputDelegate console, bool showHelp, string args, User myUser)
        {

            if (showHelp)
                console("@[bot]proof [clear|enable|reset|disable|[save [filename.aiml]]] - clears or prints a content buffer being used");
            if (cmd == "botproof")
            {
                myUser = robot.BotAsUser;
                cmd = "proof";
            }
            if (cmd == "proof")
            {
                PrintOptions printOptions = PrintOptions.CONSOLE_LISTING;
                Request request = myUser.LastRequest;
                if (request != null) printOptions = request.WriterOptions;
                printOptions.ClearHistory();
                console("-----------------------------------------------------------------");
                Request ur = robot.MakeRequestToBot(args, myUser, true, RequestKind.BotPropertyEval);
                int i;
                Result r = myUser.LastResult;
                if (args.StartsWith("save"))
                {
                    args = StaticXMLUtils.Trim(args.Substring(4));
                    string hide = StaticAIMLUtils.GetTemplateSource(myUser.VisitedTemplates, printOptions);
                    console(hide);
                    if (args.Length > 0) HostSystem.AppendAllText(args, hide + "\n");
                    return true;
                }
                if (int.TryParse(args, out i))
                {
                    r = myUser.GetResult(i);
                    console("-----------------------------------------------------------------");
                    if (r != null)
                        StaticAIMLUtils.PrintResult(r, console, printOptions);
                }
                else
                {
                    if (args.StartsWith("disable"))
                    {
                        lock (myUser.TemplatesLock)
                        {
                            DisableTemplates(myUser, myUser.ProofTemplates);
                            if (args.EndsWith("all"))
                            {
                                DisableTemplates(myUser, myUser.VisitedTemplates);
                                DisableTemplates(myUser, myUser.UsedChildTemplates);
                            }
                        }
                    }
                    if (args == "enable" || args == "reset")
                    {
                        foreach (TemplateInfo C in myUser.DisabledTemplates)
                        {
                            C.IsDisabled = false;
                            myUser.VisitedTemplates.Add(C);
                        }
                        myUser.DisabledTemplates.Clear();
                    }
                    TimeSpan sleepBetween = TimeSpan.FromMilliseconds(500);
                    console("-----------------------------------------------------------------");
                    console("-------DISABLED--------------------------------------");
                    StaticAIMLUtils.PrintTemplates(myUser.DisabledTemplates, console, printOptions, sleepBetween);
                    console("-----------------------------------------------------------------");
                    console("-------PROOF--------------------------------------");
                    StaticAIMLUtils.PrintTemplates(myUser.ProofTemplates, console, printOptions, sleepBetween);
                    console("-----------------------------------------------------------------");
                    console("-------CHILD--------------------------------------");
                    StaticAIMLUtils.PrintTemplates(myUser.UsedChildTemplates, console, printOptions, sleepBetween);
                    console("-----------------------------------------------------------------");
                    console("-------USED--------------------------------------");
                    StaticAIMLUtils.PrintTemplates(myUser.VisitedTemplates, console, printOptions, sleepBetween);
                    console("-----------------------------------------------------------------");

                    if (args == "clear" || args == "reset")
                    {
                        // dont revive disabled templates on "clear" // myUser.DisabledTemplates.Clear();
                        myUser.ProofTemplates.Clear();
                        myUser.UsedChildTemplates.Clear();
                        myUser.VisitedTemplates.Clear();
                        console("--------------------ALL CLEARED------------------------------------------");
                    }
                    // clear history so next call to @proof shows something
                    printOptions.ClearHistory();
                }

                return true;
            }
            return false;
        }

        private static void DisableTemplates(User myUser, ICollection<TemplateInfo> templateInfos)
        {
            foreach (TemplateInfo C in GraphMaster.CopyOf(templateInfos))
            {
                C.IsDisabled = true;
                myUser.DisabledTemplates.Add(C);
            }
            templateInfos.Clear();
        }

        internal bool ExecQuery(Request request, string cmd, OutputDelegate console, bool showHelp, string args, User myUser)
        {
            if (showHelp) console("@query <text> - conducts a findall using all tags");
            if (cmd == "query")
            {
                AltBot robot = request.TargetBot;
                PrintOptions printOptions = request.WriterOptions ?? PrintOptions.CONSOLE_LISTING;
                console("-----------------------------------------------------------------");
                if (args == "")
                {
                    QuerySettings ur0 = myUser.GetQuerySettings();
                    if (ur0.MinOutputs != QuerySettings.UNLIMITED)
                    {
                        console("- query mode on -");
                        QuerySettings.ApplySettings(QuerySettings.FindAll, ur0);
                    }
                    else
                    {
                        console("- query mode off -");
                        QuerySettings.ApplySettings(QuerySettings.CogbotDefaults, ur0);
                    }
                    return true;
                }

                Request ur = robot.MakeRequestToBot(args, myUser, true, RequestKind.ChatForString);

                // Adds findall to request
                QuerySettings.ApplySettings(QuerySettings.FindAll, ur);

                ur.IsTraced = myUser.IsTraced;
                console("-----------------------------------------------------------------");
                var result = robot.ChatWithToplevelResults(ur, request.CurrentResult, true, RequestKind.ChatForString);//, myUser, targetBotUser, myUser.ListeningGraph);
                console("-----------------------------------------------------------------");
                StaticAIMLUtils.PrintResult(result, console, printOptions);
                console("-----------------------------------------------------------------");
                return true;
            }
            return false;
        }

        internal static bool ChGraphCmd(Request request, bool showHelp, OutputDelegate console, string cmd, User myUser, string args)
        {
            
            if (showHelp) console("@chgraph <graph> - changes the users graph");
            if (cmd == "graph" || cmd == "chgraph" || cmd == "cd")
            {
                AltBot robot = request.TargetBot;
                PrintOptions printOptions = request.WriterOptions ?? PrintOptions.CONSOLE_LISTING;
                GraphMaster current = myUser.StartGraph;
                GraphMaster graph = robot.FindGraph(args, current);
                if (args != "" && graph != null && graph != current)
                {
                    console("Changing to graph " + graph);
                    myUser.StartGraph = graph;
                    console("-----------------------------------------------------------------");
                    return true;
                }
                if (args == "~")
                {
                    graph = robot.GetUserGraph(myUser.UserID);
                    console("Changing to user graph " + graph);
                    myUser.StartGraph = graph;
                    console("-----------------------------------------------------------------");
                    return true;
                }
                if (args == "")
                {
                    console("-----------------------------------------------------------------");
                    foreach (var ggg in robot.SetOfLocalGraphs)
                    {
                        console("-----------------------------------------------------------------");
                        console("local=" + ggg + " keys='" + AsString(ggg.GraphNames) + "'");
                        ggg.WriteMetaHeaders(console, printOptions);
                        console("-----------------------------------------------------------------");
                    }
                    console("-----------------------------------------------------------------");
                    foreach (var ggg in AltBot.SetOfGraphs)
                    {
                        console("-----------------------------------------------------------------");
                        console("global=" + ggg + " keys='" + AsString(ggg.GraphNames) + "'");
                        ggg.WriteMetaHeaders(console, printOptions);
                        console("-----------------------------------------------------------------");
                    }
                }
                console("-----------------------------------------------------------------");
                console("StartGraph=" + current);
                console("HearSelfSayGraph=" + myUser.HeardSelfSayGraph);
                console("HeardYouSayGraph=" + myUser.HeardYouSayGraph);
                console("-----------------------------------------------------------------");
                return true;
            }
            return false;
        }

        private static string AsString(IEnumerable names)
        {
            string s = "";
            bool comma = false;
            foreach (var name in names)
            {
                if (comma)
                {
                    s += ",";
                }
                else
                {
                    comma = true;
                }
                s += name;
            }
            return s;
        }

        internal static bool CallOrExecCmd(Request request, bool showHelp, OutputDelegate console, string cmd, User myUser, string args)
        {
            if (showHelp)
                console("@eval <source>  --- runs source based on users language setting interp='" +
                        myUser.Predicates.grabSetting("interp") + "'");
            if (cmd == "eval")
            {
                cmd = "call";
                args = "@" + (myUser.Predicates.grabSetting("interp") ?? "cloj") + " " + args;
            }

            AltBot robot = request.TargetBot;
            PrintOptions printOptions = request.WriterOptions ?? PrintOptions.CONSOLE_LISTING;

            SystemExecHandler seh;
            if (robot.ConsoleCommands.TryGetValue(cmd.ToLower(), out seh))
            {
                robot.writeToLog("@" + cmd + " = " + seh(args, myUser.CurrentRequest));
            }

            if (showHelp) console("@call <lang> <source>  --- runs script source");
            if (cmd == "call")
            {
                string source; // myUser ?? LastUser.ShortName ?? "";
                string slang;
                if (args.StartsWith("@"))
                {
                    args = args.Substring(1);
                    int lastIndex = args.IndexOf(" ");
                    if (lastIndex > 0)
                    {
                        source = AltBot.Trim(args.Substring(lastIndex + 1));
                        slang = AltBot.Trim(args.Substring(0, lastIndex));
                    }
                    else
                    {
                        source = args;
                        slang = null;
                    }
                }
                else
                {
                    source = args;
                    slang = null;
                }
                Request ur = robot.MakeRequestToBot(args, myUser, true, RequestKind.BotPropertyEval);
                if (source != null)
                {
                    try
                    {
                        console(robot.SystemExecute(source, slang, ur));
                    }
                    catch (Exception e)
                    {
                        console("SystemExecute " + source + " " + slang + " caused " + e);
                    }
                }
                return true;
            }
            return false;
        }

        [HelpText("@tasks/threads/kill - control tasks")]
        [CommandText("tasks", "thread", "threads", "kill")]
        internal static bool TaskCommand(Request request, OutputDelegate console, string cmd, string args)
        {
            AltBot robot = request.TargetBot;
            if (cmd == "tasks")
            {
                int n = 0;
                IList<Thread> botCommandThreads = robot.ThreadList;
                List<string> list = new List<string>();
                if (false) lock (botCommandThreads)
                    {
                        int num = botCommandThreads.Count;
                        foreach (Thread t in botCommandThreads)
                        {
                            n++;
                            num--;
                            //System.Threading.ThreadStateException: Thread is dead; state cannot be accessed.
                            //  at System.Threading.Thread.IsBackgroundNative()
                            if (!t.IsAlive)
                            {
                                list.Add(string.Format("{0}: {1} IsAlive={2}", num, t.Name, t.IsAlive));
                            }
                            else
                            {
                                list.Insert(0, string.Format("{0}: {1} IsAlive={2}", num, t.Name, t.IsAlive));
                            }
                        }
                    }
                int found = 0;
                lock (TaskQueueHandler.TaskQueueHandlers)
                {
                    foreach (var queueHandler in TaskQueueHandler.TaskQueueHandlers)
                    {
                        found++;
                        if (queueHandler.Busy)
                            list.Insert(0, queueHandler.ToDebugString(true));
                        else
                        {
                            list.Add(queueHandler.ToDebugString(true));
                        }

                    }
                }
                foreach (var s in list)
                {
                    console(s);
                }
                console("TaskQueueHandlers: " + found + ", threads: " + n);
                return true;
            }
            if (cmd.StartsWith("thread"))
            {
                if (args == "list" || args == "")
                {
                    int n = 0;
                    var botCommandThreads = robot.ThreadList;
                    List<string> list = new List<string>();
                    lock (botCommandThreads)
                    {
                        int num = botCommandThreads.Count;
                        foreach (Thread t in botCommandThreads)
                        {
                            n++;
                            num--;
                            //System.Threading.ThreadStateException: Thread is dead; state cannot be accessed.
                            //  at System.Threading.Thread.IsBackgroundNative()
                            if (!t.IsAlive)
                            {
                                list.Add(string.Format("{0}: {1} IsAlive={2}", num, t.Name, t.IsAlive));
                            }
                            else
                            {
                                list.Insert(0, string.Format("{0}: {1} IsAlive={2}", num, t.Name, t.IsAlive));
                            }
                        }
                    }
                    foreach (var s in list)
                    {
                        console(s);
                    }
                    console("Total threads: " + n);
                    return true;
                }
                else
                {
                    cmd = args;
                    ThreadStart thread = () =>
                                             {
                                                 try
                                                 {
                                                     try
                                                     {

                                                         robot.BotDirective(request.Requester, request, args, console);
                                                     }
                                                     catch (Exception e)
                                                     {
                                                         console("Problem with " + args + " " + e);
                                                     }
                                                 }
                                                 finally
                                                 {
                                                     try
                                                     {
                                                         robot.HeardSelfSayQueue.RemoveThread(Thread.CurrentThread);
                                                     }
                                                     catch (OutOfMemoryException) { }
                                                     catch (StackOverflowException) { }
                                                     catch (Exception) { }
                                                     console("done with " + cmd);
                                                 }
                                             };
                    String threadName = "ThreadCommnand for " + cmd;
                    robot.HeardSelfSayQueue.MakeSyncronousTask(thread, threadName, TimeSpan.FromSeconds(20));
                    console(threadName);
                    return true;
                }
                if (args.StartsWith("kill"))
                {
                    int n = 0;
                    var botCommandThreads = robot.ThreadList;
                    lock (botCommandThreads)
                    {
                        int num = botCommandThreads.Count;
                        foreach (Thread t in botCommandThreads)
                        {
                            n++;
                            num--;
                            //System.Threading.ThreadStateException: Thread is dead; state cannot be accessed.
                            //  at System.Threading.Thread.IsBackgroundNative()
                            if (!t.IsAlive)
                            {
                                console("Removing {0}: {1} IsAlive={2}", num, t.Name, t.IsAlive);
                            }
                            else
                            {
                                console("Killing/Removing {0}: {1} IsAlive={2}", num, t.Name, t.IsAlive);
                                try
                                {
                                    if (t.IsAlive)
                                    {
                                        //  aborted++;
                                        t.Abort();
                                    }
                                    t.Join();
                                }
                                catch (Exception) { }
                            }
                            robot.HeardSelfSayQueue.RemoveThread(t);
                        }
                    }
                }
                return true;
            }
            return false;
        }
    }

    internal class CommandTextAttribute : Attribute
    {
        public string[] Value;
        public CommandTextAttribute(params string[] cmdnames)
        {
            Value = cmdnames;
        }
    }

    internal class HelpTextAttribute : Attribute
    {
        public string Value;
        public HelpTextAttribute(string text)
        {
            Value = text;
        }
    }

    public partial class AltBot
    {
        private static string cmdPrefix;

        public EasyLogger Logger = new EasyLogger(writeDebugLine);

        private void AddBotCommand(string s, Action action)
        {
            if (action != null)
                ConsoleCommands.Add(s, delegate(string cmd, Request user)
                                           {
                                               action();
                                               return cmd;
                                           });
        }

        private static void UnseenWriteline(string real)
        {
            string message = real.ToUpper();
            if ((message.Contains("ERROR") && !message.Contains("TIMEOUTMESSAGE")) || message.Contains("EXCEPTION"))
            {
                DLRConsole.SYSTEM_ERR_WRITELINE("HIDDEN ERRORS: {0}", real);
                return;
            }
            if (false) DLRConsole.SYSTEM_ERR_WRITELINE("SPAMMY: {0}", real);
        }

        public void TraceTest(String s, Action action)
        {
           // return;
            writeChatTrace(s);
            return;
            action();
        }

        public static Exception RaiseErrorStatic(InvalidOperationException invalidOperationException)
        {
            writeDebugLine(writeException(invalidOperationException));
            return invalidOperationException;
        }

        public Exception RaiseError(Exception invalidOperationException)
        {
            Logger.Warn(writeException(invalidOperationException));
            if (invalidOperationException is InvalidOperationException)
            {
                invalidOperationException = new NullReferenceException(invalidOperationException.Message,
                                                                       invalidOperationException);
            } 
            return invalidOperationException;
        }

        public Exception RaiseError(string f, params object[] args)
        {
            return RaiseError(new InvalidOperationException(DLRConsole.SafeFormat(f, args)));
        }
    }

    public class EasyLogger
    {
        public OutputDelegate writeAsWell;
        public EasyLogger(OutputDelegate od)
        {
            writeAsWell = od;
        }

        public void Warn(string f, params object[] a)
        {
            writeAsWell(f, a);
        }
    }
}