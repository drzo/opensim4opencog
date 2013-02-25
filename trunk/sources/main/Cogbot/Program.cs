/*
 * This is the main for the actual headless cogbot processor.
 * Usually it's run by the Radegast cogbot plugin
 * 
 *      "you're going to find lots of things in the code that make little sense"
 *                                             - dmiles, 5/18/2012
 *                                             
 */

using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Threading;
using System.Windows.Forms;
using Cogbot;
using CommandLine;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using Radegast;
using CommandLine.Text;
#if USE_SAFETHREADS
using Thread = MushDLR223.Utilities.SafeThread;
#endif
using ProgramUtil = Cogbot.ConsoleApp;

namespace Cogbot
{
    public static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        public static void Main(string[] args)
        {
            var state = Parser.ParseArgs(args).GetWithoutFlag("--mta", out args) ? ApartmentState.MTA : ApartmentState.STA;
            ProgramUtil.RunInThread(state, () => Run(args), true);
        }

        /// <summary>
        /// Non Blocking
        /// </summary>
        /// <param name="args"></param>
        public static void Start(string[] args)
        {
            var state = Parser.ParseArgs(args).GetWithoutFlag("--mta", out args) ? ApartmentState.MTA : ApartmentState.STA;
            ProgramUtil.RunInThread(state, () => Run(args), false);
        }

        /// <summary>
        /// runs in current thread
        /// </summary>
        /// <param name="args"></param>
        public static void Run(string[] args)
        {
            //if (args.Length == 0) args = new string[] { /*"--httpd", "--aiml",*/ "Nephrael", "Rae" };
            //if (args.Length == 0) args = new string[] { "--httpd", "--aiml", "Zeno", "Aironaut" };
            //if (args.Length == 0) args = new string[] { "--httpd", "--aiml", "test", "suite" }; 
           //if (args.Length == 0) args = new string[] { "--aiml", "kotoko", "irata", "--servitor" };
            //if (args.Length == 0) args = new string[] { "--httpd", "--aiml", "BinaBot", "Daxeline" }; 

            //if (args.Length == 0) args = new string[] { "--swipl" };                  


            var orig = args;
            args = ProgramUtil.SetAllCommandLineOptions(args);

            string[] oArgs;
            if (ClientManagerConfig.arguments.GetAfter("--aiml", out oArgs))
            {
                string[] newArgs = oArgs;
                ProgramUtil.AllocConsole();
                RunType("AltAIMLbot:AltAIMLbot.AltBot", args);
                return;
            }
            if (ClientManagerConfig.arguments.GetAfter("--plwin", out oArgs))
            {
                ProgramUtil.DoAndExit(() =>
                {
                    var firstProc = new System.Diagnostics.Process();
                    firstProc.StartInfo.FileName = @"c:\Program Files\pl\bin\swipl-win.exe";
                    firstProc.StartInfo.Arguments = "-f prolog/cogbot.pl";// "-f StartCogbot.pl";

                    firstProc.EnableRaisingEvents = true;
                    ProgramUtil.AllocConsole();
                    firstProc.Start();
                    firstProc.WaitForExit();
                });
                return;
            }
            if (ClientManagerConfig.arguments.GetAfter("--swipl", out oArgs))
            {
                string[] newArgs = oArgs;
                if (newArgs.Length == 0) newArgs = new string[] { "-f", "cynd/cogbot.pl" };
                RunType("PrologBotModule:PrologScriptEngine.PrologScriptInterpreter", newArgs);
                return;
            }
            string newDir;
            if (ClientManagerConfig.arguments.TryGetValueWithout("--main", out newDir, true, out oArgs))
            {
                string[] newArgs = oArgs;
                RunType(newDir, newArgs);
                return;
            }

            DoAndExit(() => ConsoleApp.Main(args));

        }

        public static void DoAndExit(ThreadStart o)
        {
            ProgramUtil.DoAndExit(o);
        }


        public static void RunType(string c, string[] newArgs)
        {
            DoAndExit(() =>
            {
                Type t = FindTypeByName(c, Assembly.GetCallingAssembly());
                if (t == null && c.Contains(":"))
                {
                    var ds = c.Split(':');
                    c = ds[1];
                    Assembly asem = AppDomain.CurrentDomain.Load(ds[0]);
                    t = FindTypeByName(c, asem);
                }
                if (t == null)
                {
                    t = FindTypeByName(c, Assembly.GetEntryAssembly() ?? Assembly.GetExecutingAssembly());
                }
                if (t != null)
                {
                    RunType(t, newArgs);
                }
                else
                {
                    throw new Exception("Class not found: " + c);
                }
            });
        }

        private static Type FindTypeByName(string c, Assembly assem)
        {
            Type t = Type.GetType(c, false, false) ?? Type.GetType(c, false, true);
            if (t != null || assem == null) return t;
            t = assem.GetType(c, false, false) ?? assem.GetType(c, false, true);
            if (t != null)
            {
                return t;
            }
            var many = assem.GetReferencedAssemblies();
            foreach (var an in many)
            {
                var asem = Assembly.Load(an);
                t = asem.GetType(c, false, false) ?? asem.GetType(c, false, true);
                if (t != null)
                {
                    return t;
                }
            }
            return null;
        }

        private static void RunType(Type t, string[] newArgs)
        {
            if (ProgramUtil.ChangeLCD)
            {
                ProgramUtil.SetCurrentDirectory(t);
            }
            MethodInfo mi = t.GetMethod("Main", new Type[] { typeof(string[]) });
            if (mi != null)
            {
                DoAndExit(() => mi.Invoke(null, new object[] { newArgs }));
                return;
            }
            mi = t.GetMethod("Main", new Type[] { });
            if (mi != null)
            {
                DoAndExit(() => mi.Invoke(null, new object[] { }));
                return;
            }
            var mainOrRun = new List<string>(new[] {"main", "run", "start", "exec"});
            foreach (var s in t.GetMethods(BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public))
            {
                if (mainOrRun.Contains(s.Name.ToLower()))
                {
                    var ps = s.GetParameters();
                    if (ps.Length == 0)
                    {
                        DoAndExit(() => s.Invoke(null, new object[] {}));
                        return;
                    }
                    if (ps.Length == 1)
                    {
                        DoAndExit(() => s.Invoke(null, new object[] {newArgs}));
                        return;
                    }
                }
            }
            throw new MethodAccessException("No main for " + t);
        }

    }
}
