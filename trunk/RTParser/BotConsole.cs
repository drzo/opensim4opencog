using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Net.Mail;
using System.Reflection;
using System.Runtime.Serialization;
using System.Text.RegularExpressions;
using System.Threading;
using System.Web;
using System.Xml;
using AIMLbot;
using LAIR.ResourceAPIs.WordNet;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using org.opencyc.api;
using RTParser.AIMLTagHandlers;
using RTParser.Database;
using RTParser.Prolog;
using RTParser.Utils;
using RTParser.Variables;
using RTParser.Web;
using Console=System.Console;
using UPath = RTParser.Unifiable;
using UList = System.Collections.Generic.List<RTParser.Utils.TemplateInfo>;

namespace RTParser
{
    /// <summary>
    /// </summary>
    public partial class RTPBot
    {
        private readonly Dictionary<string, SystemExecHandler> ConsoleCommands = new Dictionary<string, SystemExecHandler>();
        private static Bot ConsoleRobot;
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
        public void writeToLog(string message, params object[] args)
        {
            message = DLRConsole.SafeFormat(message, args);
            if (String.IsNullOrEmpty(message)) return;
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
            message = string.Format("[{0}]: {1}", DateTime.Now, message.Trim());
            writeToFileLog(message);
        }

        public Object LoggingLock = new object();
        private static ClientManagerHttpServer HttpTextServer;

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
            writeDebugLine("-" + fmt, ps);
        }

        public static void Main(string[] args)
        {
            Run(args);
        }
        public static void Run(string[] args)
        {
            RTPBot myBot = null;
            TaskQueueHandler.TimeProcess("ROBOTCONSOLE: STARTUP", () => { myBot = Startup(args); });
            TaskQueueHandler.TimeProcess("ROBOTCONSOLE: RUN", () => Run(args, myBot, MainConsoleWriteLn));}

        private static RTPBot Startup(string[] args)
        {
            RTPBot myBot;
            lock (typeof(Bot))
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
            RTPBot myBot = new Bot();
            ConsoleRobot = myBot as Bot;
            OutputDelegate writeLine = MainConsoleWriteLn;
            bool usedHttpd = false;
            foreach (string s in args)
            {
                if (s == "--httpd")
                {
                    UseBreakpointOnError = false;
                    usedHttpd = true;
                }
            }

            string[] oArgs;
            if (usedHttpd)
            {
                ScriptExecutorGetter geter = new WebScriptExecutor(myBot);
                HttpTextServer = new ClientManagerHttpServer(geter, 5580);
            }            
        }

        public static void Load(string[] args, RTPBot myBot, OutputDelegate writeLine)
        {
            myBot.outputDelegate = null; /// ?? Console.Out.WriteLine;

            // writeLine = MainConsoleWriteLn;
            bool gettingUsername = false;
            myBot.loadGlobalBotSettings();
            string myName = "BinaBot Daxeline";
            //myName = "Test Suite";
            myName = "Kotoko Irata";
            //myName = "Nephrael Rae";
            if (args != null)
            {
                string newName = "";
                foreach (string s in args)
                {
                    if (s == "--breakpoints")
                    {
                        UseBreakpointOnError = true;
                        continue;
                    }
                    if (s == "--nobreakpoints")
                    {
                        UseBreakpointOnError = false;
                        continue;
                    }
                    if (s == "--aiml" || s == "--botname")
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
            myBot.isAcceptingUserInput = false;
            writeLine("-----------------------------------------------------------------");
            myBot.SetName(myName);
            myBot.isAcceptingUserInput = true;
        }
        public static void Run(string[] args, RTPBot myBot, OutputDelegate writeLine)
        {
            string evidenceCode = "<topic name=\"collectevidencepatterns\"> " +
                                  "<category><pattern>HOW ARE YOU</pattern><template>" +
                                  "<think><setevidence evidence=\"common-greeting\" prob=1.0 /></think>" +
                                  "</template></category></topic>" +
                                  "";
            //Added from AIML content now
            // myBot.AddAiml(evidenceCode);
            User myUser = myBot.LastUser;
            Request request = myUser.CreateRequest("current user toplevel", myBot.BotAsUser);
            myBot.BotDirective(request, "@help", writeLine);
            writeLine("-----------------------------------------------------------------");
            AIMLDEBUGSETTINGS = "clear +*";
            myBot.BotDirective(request, "@log " + AIMLDEBUGSETTINGS, writeLine);
            writeLine("-----------------------------------------------------------------");
            DLRConsole.SystemFlush();

            //string userJustSaid = String.Empty;
            myBot.LastUser = myUser;
            while (true)
            {
                User BotAsAUser = myBot.BotAsUser;
                myUser = myBot.LastUser;
                string myName = myUser.UserName;
                writeLine("-----------------------------------------------------------------");
                string input = TextFilter.ReadLineFromInput(DLRConsole.SystemWrite, myUser.UserName + "> ");
                if (input == null)
                {
                    Environment.Exit(0);
                }
                input = input.Trim();
                if (input.ToLower() == "@quit")
                {
                    return;
                }
                if (input.ToLower() == "@exit")
                {
                    Environment.Exit(Environment.ExitCode);
                }
                writeLine("-----------------------------------------------------------------");
                if (String.IsNullOrEmpty(input))
                {
                    writeLine(myName + "> " + BotAsAUser.JustSaid);
                    continue;
                }
                try
                {
                    Unifiable cmdprefix = myUser.Predicates.grabSettingNoDebug("cmdprefix");
                    if (cmdprefix == null) cmdprefix = myBot.GlobalSettings.grabSettingNoDebug("cmdprefix");
                    if (!input.Contains("@") && !IsNullOrEmpty(cmdprefix))
                    {
                        input = cmdprefix.AsString() + " " + input;
                    }
                    if (input == "@")
                    {
                        input = myUser.JustSaid;
                    }

                    bool myBotBotDirective = false;
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
                    if (!myBotBotDirective) continue;
                    writeLine("-----------------------------------------------------------------");
                    writeLine("{0}: {1}", myUser.UserName, myUser.JustSaid);
                    writeLine("---------------------");
                    writeLine("{0}: {1}", myName, BotAsAUser.JustSaid);
                    writeLine("-----------------------------------------------------------------");
                }
                catch (Exception e)
                {
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
            bool b = BotDirective(request, input, all);
            string sws = sw.ToString();
            if (!b) return Unifiable.FAIL_NIL;
            return sws;
        }

        public bool BotDirective(User user, string input, OutputDelegate console)
        {
            try
            {
                Request request = (user ?? LastUser ?? BotAsUser).CreateRequest(input, BotAsUser);
                return BotDirective(request, input, console);
            }
            catch (Exception e)
            {
                DLRConsole.DebugWriteLine("ERROR in BotDirective: " + e);
                return false;
            }
        }

        public bool BotDirective(Request request, string input, OutputDelegate console)
        {
            try
            {
                return BotDirective(request, input, console, null);
            }
            catch (Exception e)
            {
                DLRConsole.DebugWriteLine("ERROR in BotDirective: " + e);
                return false;
            }
        }
        public bool BotDirective(Request request, string input, OutputDelegate console, ThreadControl control)
        {
            User targetBotUser = this.BotAsUser;
            if (request != null && request.CurrentResult != null)
            {
                writeChatTrace("CurrentResult: " + request.CurrentResult);
            }
            if (input == null) return false;
            input = input.Trim();
            if (input == "") return false;
            if (input.StartsWith("@"))
            {
                input = input.TrimStart(new[] { ' ', '@' });
            }
            User myUser = request.Requester ?? LastUser ?? FindOrCreateUser(UNKNOWN_PARTNER);
            int firstWhite = input.IndexOf(' ');
            if (firstWhite == -1) firstWhite = input.Length - 1;
            string cmd = input.Substring(0, firstWhite + 1).Trim().ToLower();
            string args = input.Substring(firstWhite + 1).Trim();
            bool showHelp = false;
            if (cmd == "help")
            {
                showHelp = true;
                console("Commands are prefixed with @cmd");
                console("@help shows help -- command help comming soon!");
                console("@quit -- exits the aiml subsystem");
            }


            if (showHelp)
                console(
                    "@withuser <user> - <text>  -- (aka. simply @)  runs text/command intentionally setting LastUser");
            if (cmd == "withuser" || cmd == "@")
            {
                string said;
                string user;
                if (!SplitOff(args, "-", out user, out said))
                {
                    user = myUser.UserName;
                    said = args;
                }
                Result res = GlobalChatWithUser(said, user, null, writeDebugLine, true, false);
                // detect a user "rename"
                DetectUserChange(myUser, user);
                OutputResult(res, console, false);                
                return true;
            }

            if (showHelp)
                console(
                    "@locally <user> - <text>  -- runs text/command not intentionally not setting LastUser");
            if (cmd == "locally" || cmd == "@")
            {
                string said;
                string user;
                if (!SplitOff(args, "-", out user, out said))
                {
                    user = myUser.UserName;
                    said = args;
                }
                Result res = GlobalChatWithUser(said, user, null, writeDebugLine, true, true);
                request = res.request;
                request.ResponderSelfListens = true;
                // detect a user "rename"
                bool userChanged = DetectUserChange(myUser, user);
                if (userChanged)
                {
                    //myUser = FindUser(user);
                    request.Requester = myUser;                    
                }
                var justsaid = OutputResult(res, console, false);
                User theResponder = res.Responder ?? res.request.Responder;
                if (theResponder == null)
                {
                    theResponder = (myUser == targetBotUser) ? request.Requester : targetBotUser;
                    writeToLog("Making the responder " + theResponder);
                }
                if (theResponder == null)
                {
                    return true;
                }
                myUser.LastResponder = theResponder;
                theResponder.JustSaid = justsaid;
                // ReSharper disable ConditionIsAlwaysTrueOrFalse
                if (ProcessHeardPreds && request.ResponderSelfListens)
                    // ReSharper restore ConditionIsAlwaysTrueOrFalse
                    HeardSelfSayResponse(theResponder, myUser, justsaid, res, control);

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
                GraphMaster g = GetGraph(gn, myUser.ListeningGraph);
                String aiml = args.Substring(indexof).Trim();
                AddAiml(g, aiml, request);
                console("Done with " + args);
                return true;
            }

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
                CSPrologMain.Main(new[] { callme });
                return true;
            }

            if (showHelp) console("@reload -- reloads any changed files ");
            if (cmd == "reload")
            {
                ReloadAll();
                return true;
                //                return;//Success("WorldSystemModule.MyBot.ReloadAll();");
            }

            if (cmd == "echo")
            {
                console(args);
                return true;
            }
            if (showHelp) console("@load <graph> - <uri>");
            if (cmd == "load")
            {
                string graphname;
                string files;
                if (!SplitOff(args, "-", out graphname, out files))
                {
                    graphname = "current";
                    files = args;
                }
                GraphMaster G = GetGraph(graphname, request.Graph);
                AIMLLoader loader = GetLoader(request);
                LoaderOptions reqLoadOptionsValue = request.LoadOptions.Value;
                var prev = request.Graph;
                try
                {
                    request.Graph = G;
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
            if (showHelp) console("@say [who -] <text> -- fakes that the 'who' (default bot) just said it");
            if (cmd == "say")
            {
                console("say> " + args);
                string who, said;
                if (!SplitOff(args, "-", out who, out said))
                {
                    who = targetBotUser.UserID;
                    said = args;
                }
                User factSpeaker = FindOrCreateUser(who);
                HeardSelfSayVerbal(factSpeaker, factSpeaker.LastResponder, args, LastResult, control);
                return true;
            }

            if (showHelp) console("@say1 [who -] <sentence> -- fakes that 'who' (default bot) just said it");
            if (cmd == "say1")
            {
                console("say1> " + args);
                string who, said;
                if (!SplitOff(args, "-", out who, out said))
                {
                    who = targetBotUser.UserID;
                    said = args;
                }
                User factSpeaker = FindOrCreateUser(who);
                HeardSelfSay1Sentence(factSpeaker, factSpeaker.LastResponder, said, LastResult, control);
                return true;
            }

            if (showHelp)
                console(
                    "@set [type] [name [value]] -- emulates get/set tag in AIML.  'type' defaults to =\"user\" therefore same as @user");
            if (cmd == "set")
            {
                console(DefaultPredicates.ToDebugString());
                return myUser.DoUserCommand(args, console);
                return true;
            }
            if (showHelp)
                console(
                    "@setvar dictname.name [value] -- get/sets a variable using a global namespace context");
            if (cmd == "setvar")
            {
                myUser.DoUserCommand(args, console);
                GlobalSettings.DoSettingsCommand(input, console);;                
                return targetBotUser.DoUserCommand(args, console);
            }
            if (showHelp) console("@bot [var [value]] -- lists or changes the bot GlobalPredicates.\n  example: @bot ProcessHeardPreds True or @bot ProcessHeardPreds False");
            if (cmd == "bot")
            {
                console(HeardPredicates.ToDebugString());
                console(RelationMetaProps.ToDebugString());
                return targetBotUser.DoUserCommand(args, console);
            }

            PrintOptions printOptions = request.WriterOptions ?? PrintOptions.CONSOLE_LISTING;
            printOptions.ClearHistory();

            if (showHelp)
                console("@proof [[clear]|[save [filename.aiml]]] - clears or prints a content buffer being used");
            if (cmd == "proof")
            {
                console("-----------------------------------------------------------------");
                RequestImpl ur = MakeRequestToBot(args, myUser);
                int i;
                Result r = myUser.LastResult;
                if (args.StartsWith("save"))
                {
                    args = args.Substring(4).Trim();
                    string hide = GetTemplateSource(myUser.UsedTemplates, printOptions);
                    console(hide);
                    if (args.Length > 0) HostSystem.AppendAllText(args, hide + "\n");
                    return true;
                }
                if (int.TryParse(args, out i))
                {
                    r = myUser.GetResult(i);
                    console("-----------------------------------------------------------------");
                    if (r != null)
                        PrintResult(r, console, printOptions);
                }
                else
                {
                    var CId = myUser.DisabledTemplates;
                    var CI = myUser.UsedTemplates;
                    if (args == "disable")
                    {
                        foreach (TemplateInfo C in CI)
                        {
                            C.IsDisabled = true;
                            myUser.DisabledTemplates.Add(C);
                        }
                        CI.Clear();
                    }
                    if (args == "enable" || args == "reset")
                    {
                        foreach (TemplateInfo C in CId)
                        {
                            C.IsDisabled = false;
                            myUser.UsedTemplates.Add(C);
                        }
                        CId.Clear();
                    }
                    console("-----------------------------------------------------------------");
                    console("-------DISABLED--------------------------------------");
                    PrintTemplates(CId, console, printOptions);
                    console("-----------------------------------------------------------------");
                    console("-------ENABLED--------------------------------------");
                    PrintTemplates(CI, console, printOptions);
                    console("-----------------------------------------------------------------");
                    if (args == "clear" || args == "reset") CI.Clear();
                }

                return true;
            }


            if (showHelp) console("@query <text> - conducts a findall using all tags");
            if (cmd == "query")
            {
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

                Request ur = MakeRequestToBot(args, myUser);

                // Adds findall to request
                QuerySettings.ApplySettings(QuerySettings.FindAll, ur);

                ur.IsTraced = myUser.IsTraced;
                console("-----------------------------------------------------------------");
                var result = ChatWithToplevelResults(ur, request.CurrentResult);//, myUser, targetBotUser, myUser.ListeningGraph);
                console("-----------------------------------------------------------------");
                PrintResult(result, console, printOptions);
                console("-----------------------------------------------------------------");
                return true;
            }

            if (showHelp) console("@user [var [value]] -- lists or changes the current users get/set vars.");
            if (cmd == "user")
            {
                return myUser.DoUserCommand(args, console);
            }

            if (request.Graph.DoGraphCommand(cmd, console, showHelp, args, request)) return true;

            if (showHelp) console("@chgraph <graph> - changes the users graph");
            if (cmd == "graph" || cmd == "chgraph" || cmd == "cd")
            {
                GraphMaster current = myUser.ListeningGraph;
                GraphMaster graph = FindGraph(args, current);
                if (args == "" && graph != null && graph != current)
                {
                    console("Changing to graph " + graph);
                    myUser.ListeningGraph = graph;
                    console("-----------------------------------------------------------------");
                    return true;
                }
                if (args == "~")
                {
                    graph = FindGraph(myUser.UserID, null);
                    console("Changing to user graph " + graph);
                    myUser.ListeningGraph = graph;
                    console("-----------------------------------------------------------------");
                    return true;
                }
                if (args == "")
                {
                    console("-----------------------------------------------------------------");
                    foreach (KeyValuePair<string, GraphMaster> ggg in GraphMaster.CopyOf(LocalGraphsByName))
                    {
                        console("-----------------------------------------------------------------");
                        string n = ggg.Key;
                        GraphMaster gm = ggg.Value;
                        console("local=" + gm + " key='" + n + "'");
                        gm.WriteMetaHeaders(console, printOptions);
                        console("-----------------------------------------------------------------");
                    }
                    console("-----------------------------------------------------------------");
                    foreach (KeyValuePair<string, GraphMaster> ggg in GraphMaster.CopyOf(GraphsByName))
                    {
                        console("-----------------------------------------------------------------");
                        string n = ggg.Key;
                        GraphMaster gm = ggg.Value;
                        console("value=" + gm + " key='" + n + "'");
                        gm.WriteMetaHeaders(console, printOptions);
                        console("-----------------------------------------------------------------");
                    }
                }
                console("-----------------------------------------------------------------");
                console("ListeningGraph=" + current);
                console("-----------------------------------------------------------------");
                return true;
            }

            if (showHelp) console("@log " + AIMLDEBUGSETTINGS);
            if (cmd.StartsWith("log"))
            {
                LoggedWords.UpateLogging(args, console);
                return true;
            }
            if (cmd == "on" || cmd == "off")
            {
                return true;
            }

            if (showHelp)
                console("@eval <source>  --- runs source based on users language setting interp='" +
                        myUser.Predicates.grabSetting("interp") + "'");
            if (cmd == "eval")
            {
                cmd = "call";
                args = "@cloj " + args;
            }

            SystemExecHandler seh;
            if (ConsoleCommands.TryGetValue(cmd.ToLower(), out seh))
            {
                writeToLog("@" + cmd + " = " + seh(args, myUser.CurrentRequest));
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
                        source = args.Substring(lastIndex + 1).Trim();
                        slang = args.Substring(0, lastIndex);
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
                Request ur = MakeRequestToBot(args, myUser);
                if (source != null)
                {
                    try
                    {
                        console(SystemExecute(source, slang, ur));
                    }
                    catch (Exception e)
                    {
                        console("SystemExecute " + source + " " + slang + " caused " + e);
                    }
                }
                return true;
            }

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
            if (SettingsDictionary.TryGetValue(ExecuteHandlers, cmd, out handler))
            {
                console("" + handler(args, request));
                return true;
            }

            if (cmd == "tasks")
            {
                int n = 0;
                IList<Thread> botCommandThreads = ThreadList;
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
                    var botCommandThreads = ThreadList;
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

                                BotDirective(request, args, console);
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
                                HeardSelfSayQueue.RemoveThread(Thread.CurrentThread);
                            }
                            catch (OutOfMemoryException) { }
                            catch (StackOverflowException) { }
                            catch (Exception) { }
                            console("done with " + cmd);
                        }
                    };
                    String threadName = "ThreadCommnand for " + cmd;
                    HeardSelfSayQueue.MakeSyncronousTask(thread, threadName, TimeSpan.FromSeconds(20));
                    console(threadName);
                    return true;
                }
                if (args.StartsWith("kill"))
                {
                    int n = 0;
                    var botCommandThreads = ThreadList;
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
                            HeardSelfSayQueue.RemoveThread(t);
                        }
                    }
                }
            }
            console("unknown: @" + input);
            return false;
        }


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
            DLRConsole.SYSTEM_ERR_WRITELINE("SPAMMY: {0}", real);
        }

        public void TraceTest(String s, Action action)
        {
            return;
            writeChatTrace(s);  
            action();
        }

        public static Exception RaiseErrorStatic(InvalidOperationException invalidOperationException)
        {
            writeDebugLine(writeException(invalidOperationException));
            return invalidOperationException;
        }

        public Exception RaiseError(Exception invalidOperationException)
        {
            writeDebugLine(writeException(invalidOperationException));
            return invalidOperationException;
        }
    }
}