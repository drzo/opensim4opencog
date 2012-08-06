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
            RunInSTAThread(() => RunCurrent(args), true);
        }

        /// <summary>
        /// Non Blocking
        /// </summary>
        /// <param name="args"></param>
        public static void Run(string[] args)
        {
            RunInSTAThread(() => RunCurrent(args), false);
        }

        /// <summary>
        /// runs in a STA thread (by creating one)  Does not "join"
        /// </summary>
        /// <param name="args"></param>        
        public static void RunInSTAThread(ThreadStart runMain, bool blocking)
        {
            if (!blocking || Thread.CurrentThread.ApartmentState != ApartmentState.STA)
            {
                Thread newThread = new Thread(runMain);
                newThread.SetApartmentState(ApartmentState.STA);
                newThread.Start();
                if (blocking) newThread.Join();
                return;
            }
            else
            {
                runMain();
            }
        }

        /// <summary>
        /// runs in current thread
        /// </summary>
        /// <param name="args"></param>
        public static void RunCurrent(string[] args)
        {
            args = ProgramUtil.SetAllCommandLineOptions(args);

            string[] oArgs;
            if (ClientManagerConfig.arguments.GetAfter("--aiml", out oArgs))
            {
                string[] newArgs = oArgs;
                ProgramUtil.AllocConsole();
                RunType("AIMLbot:RTParser.RTPBot", args);
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
            if (ClientManagerConfig.arguments.TryGetValueWithout("--main", out newDir, out oArgs))
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
                Type t = Type.GetType(c, false, false);
                if (t == null) t = Type.GetType(c, false, true);
                if (t == null && c.Contains(":"))
                {
                    var ds = c.Split(':');
                    c = ds[1];
                    Assembly asem = AppDomain.CurrentDomain.Load(ds[0]);
                    t = asem.GetType(c, false, false);
                    if (t == null) t = asem.GetType(c, false, true);
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
            foreach (var s in t.GetMethods(BindingFlags.Static | BindingFlags.NonPublic))
            {
                if (s.Name.ToLower() == "main")
                {
                    var ps = s.GetParameters();
                    if (ps.Length == 0)
                    {

                        DoAndExit(() => s.Invoke(null, new object[] { }));
                        return;
                    }
                    if (ps.Length == 1)
                    {
                        DoAndExit(() => s.Invoke(null, new object[] { newArgs }));
                        return;
                    }
                }
            }
            throw new MethodAccessException("No main for " + t);
        }

    }
}
