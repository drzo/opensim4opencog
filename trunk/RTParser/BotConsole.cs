using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Net.Mail;
using System.Reflection;
using System.Text.RegularExpressions;
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
    public partial class RTPBot : QuerySettings
    {
        private readonly Dictionary<string, SystemExecHandler> ConsoleCommands = new Dictionary<string, SystemExecHandler>();

        private static void MainConsoleWriteLn(string fmt, params object[] ps)
        {
            writeDebugLine("-" + fmt, ps);
        }

        public static void Main(string[] args)
        {
            RTPBot myBot = new Bot();
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
                new ClientManagerHttpServer(geter, 5580);
            }
            Main(args, myBot, writeLine);
        }

        public static void Main(string[] args, RTPBot myBot, OutputDelegate writeLine)
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
                        //UseBreakpointOnError = true;
                    }
                    if (s == "--nobreakpoints")
                    {
                        UseBreakpointOnError = false;
                    }
                    if (s == "--aiml" || s == "--botname")
                    {
                        gettingUsername = true;
                        continue;
                    }
                    if (s.StartsWith("-"))
                    {
                        gettingUsername = false;
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

            string evidenceCode = "<topic name=\"collectevidencepatterns\"> " +
                                  "<category><pattern>HOW ARE YOU</pattern><template>" +
                                  "<think><setevidence evidence=\"common-greeting\" prob=1.0 /></think>" +
                                  "</template></category></topic>" +
                                  "";
            //Added from AIML content now
            // myBot.AddAiml(evidenceCode);
            User myUser = myBot.LastUser;
            Request request = myUser.CreateRequest("current user toplevel");
            myBot.BotDirective(request, "@log " + AIMLDEBUGSETTINGS, writeLine);
            writeLine("-----------------------------------------------------------------");
            myBot.BotDirective(request, "@help", writeLine);
            writeLine("-----------------------------------------------------------------");
            DLRConsole.SystemFlush();

            String botJustSaid = null;
            string meneValue = null;
            string userJustSaid = String.Empty;
            myBot.LastUser = myUser;
            while (true)
            {
                myUser = myBot.LastUser;
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
                    writeLine(myName + "> " + botJustSaid);
                    continue;
                }
                try
                {
                    Unifiable cmdprefix = myBot.GlobalSettings.grabSettingNoDebug("cmdprefix");
                    if (!input.Contains("@") && !IsNullOrEmpty(cmdprefix))
                    {
                        input = cmdprefix.AsString() + " " + input;
                    }

                    bool myBotBotDirective = false;
                    if (input.StartsWith("@"))
                    {
                        request.TimesOutAt = DateTime.Now + new TimeSpan(0, 5, 0);
                        myBotBotDirective = myBot.BotDirective(request, input, writeLine);
                        //if (!myBotBotDirective) 
                        continue;
                    }
                    if (!myBotBotDirective)
                    {
                        userJustSaid = input;
                        //  myUser.TopicSetting = "collectevidencepatterns";
                        myBot.pMSM.clearEvidence();
                        myBot.pMSM.clearNextStateValues();
                        Request r = new AIMLbot.Request(input, myUser, myBot, null);
                        r.IsTraced = true;
                        ///r.ProcessMultipleTemplates = false; stored in user settings
                        writeLine("-----------------------------------------------------------------");
                        Result res = myBot.Chat(r);
                        if (!res.IsEmpty)
                        {
                            botJustSaid = res.Output;
                            meneValue = "" + res.Score;
                            if (myBot.ProcessHeardPreds)
                            {
                                writeLine("-----------------------------------------------------------------");
                                myBot.HeardSelfSayNow(botJustSaid);
                                writeLine("-----------------------------------------------------------------");
                            }
                        }
                        else
                        {
                            botJustSaid = "NULL";
                        }
                    }
                    writeLine("-----------------------------------------------------------------");
                    writeLine("{0}: {1}", myUser.UserName, userJustSaid);
                    writeLine("---------------------");
                    writeLine("{0}: {1}   mene value={2}", myName, botJustSaid, meneValue);
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
            Request request = (user ?? LastUser ?? BotAsUser).CreateRequest(input);
            return BotDirective(request, input, console);
        }

        public bool BotDirective(Request request, string input, OutputDelegate console)
        {
            if (input == null) return false;
            input = input.Trim();
            if (input == "") return false;
            if (input.StartsWith("@"))
            {
                input = input.TrimStart(new[] {' ', '@'});
            }
            User myUser = request.user ?? LastUser ?? FindOrCreateUser(UNKNOWN_PARTNER);
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
                    "@withuser <user> - <text>  -- (aka. simply @)  runs text/command not intentionally setting LastUser");
            if (cmd == "withuser" || cmd == "@")
            {
                int lastIndex = args.IndexOf("-");
                if (lastIndex > -1)
                {
                    string user = args.Substring(0, lastIndex).Trim();
                    string value = args.Substring(lastIndex + 1).Trim();
                    RequestImpl r = GetRequest(value, user);
                    r.IsTraced = true;
                    AIMLbot.Result res = Chat0(r, r.Graph);
                    double scored = res.Score;
                    Unifiable resOutput = res.Output;
                    string useOut = "Interesting.";
                    string oTest = resOutput.AsString();
                    if (!string.IsNullOrEmpty(oTest))
                    {
                        useOut = MyBot.CleanupCyc(oTest).AsString();
                    }
                    if (string.IsNullOrEmpty(useOut))
                    {
                        useOut = "Interesting.";
                        scored = 0.5;
                    }
                    else useOut = useOut.Replace(" _", " ");
                    if (!useOut.Contains("mene value=")) useOut = useOut + " mene value=" + scored;
                    console(useOut);
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
                CSPrologMain.Main(new[] {callme});
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
            if (showHelp) console("@load <uri>");
            if (cmd == "load")
            {
                if (Loader == null)
                {
                    Loader = new AIMLLoader(this, request);
                }
                LoaderOptions reqLoadOptionsValue = request.LoadOptions.Value;
                Loader.loadAIMLURI(args, reqLoadOptionsValue);
                // maybe request.TargetBot.ReloadHooks.Add(() => request.Loader.loadAIMLURI(args, reqLoadOptionsValue));
                console("Done with " + args);
                return true;
            }
            if (showHelp) console("@say <text> -- fakes that the bot just said it");
            if (cmd == "say")
            {
                console("say> " + args);
                HeardSelfSayNow(args);
                myUser.SetOutputSentences(args);
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

            if (showHelp) console("@bot [var [value]] -- lists or changes the bot GlobalPredicates.");
            if (cmd == "bot")
            {
                console(HeardPredicates.ToDebugString());
                console(RelationMetaProps.ToDebugString());
                return BotAsUser.DoUserCommand(args, console);
            }

            PrintOptions printOptions = request.WriterOptions ?? PrintOptions.CONSOLE_LISTING;
            printOptions.ClearHistory();

            if (showHelp)
                console("@proof [[clear]|[save [filename.aiml]]] - clears or prints a content buffer being used");
            if (cmd == "proof")
            {
                console("-----------------------------------------------------------------");
                RequestImpl ur = GetRequest(args, myUser.UserID);
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
                    User ur0 = myUser;
                    if (ur0.MinOutputs != UNLIMITED)
                    {
                        console("- query mode on -");
                        ApplySettings(FindAll, myUser);
                    }
                    else
                    {
                        console("- query mode off -");
                        ApplySettings(CogbotDefaults, myUser);
                    }
                    return true;
                }

                RequestImpl ur = GetRequest(args, myUser.UserID);
                // Adds findall to request
                if (true)
                {
                    ApplySettings(FindAll, ur);
                }
                else
                {
                    ur.ProcessMultipleTemplates = true;
                    ur.MaxOutputs = 99;
                    ur.MaxPatterns = 99;
                    ur.MaxTemplates = 99;
                    ur.ProcessMultiplePatterns = true;
                }
                ur.IsTraced = true;
                console("-----------------------------------------------------------------");
                AIMLbot.Result result = Chat0(ur, myUser.ListeningGraph);
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

            if (showHelp)
                console(
                    "@ls <graph> - * --  lists all graph elements matching some elements \n Example that lists only efualt patterns: " +
                    @"@ls ^\<category\>\<pattern\>\*\</pattern\>\<te");

            if (cmd == "ls")
            {
                int lastIndex = args.IndexOf("-");
                string graphname = "current";
                string match = ".*";
                if (lastIndex > -1)
                {
                    graphname = args.Substring(0, lastIndex).Trim();
                    match = args.Substring(lastIndex + 1).Trim();
                }
                GraphMaster G = GetGraph(graphname, myUser.ListeningGraph);
                G.Listing(console, args, printOptions);
                return true;
            }

            if (showHelp) console("@chgraph <graph> - changes the users graph");
            if (cmd == "graph" || cmd == "chgraph" || cmd == "cd")
            {
                GraphMaster current = myUser.ListeningGraph;
                GraphMaster graph = FindGraph(args, current);
                if (graph != null && graph != current)
                {
                    console("Changing to graph " + graph);
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
                        console("" + gm + " key='" + n + "'");
                        gm.WriteMetaHeaders(console, printOptions);
                        console("-----------------------------------------------------------------");
                    }
                    console("-----------------------------------------------------------------");
                    foreach (KeyValuePair<string, GraphMaster> ggg in GraphMaster.CopyOf(GraphsByName))
                    {
                        console("-----------------------------------------------------------------");
                        string n = ggg.Key;
                        GraphMaster gm = ggg.Value;
                        console("" + gm + " key='" + n + "'");
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
            if (ConsoleCommands.TryGetValue(cmd.ToLower(),out seh))
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
                RequestImpl ur = GetRequest(args, myUser.UserID);
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
            if (showHelp || uc) return true;
            SystemExecHandler handler;
            if (ExecuteHandlers.TryGetValue(cmd, out handler))
            {
                console("" + handler(args, request));
                return true;
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
    }
}