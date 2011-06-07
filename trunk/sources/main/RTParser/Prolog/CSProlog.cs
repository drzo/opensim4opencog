#if (!STANDARD)
#define debugging
#define arg1index
#define mswindows
#define newor
#define partialengine
#endif

#undef ctrlc

#if (!VISUAL_STUDIO)
#undef mswindows
#endif

#if mswindows
#define ctrlc
#else
#undef ctrlc
#endif

/*-----------------------------------------------------------------------------------------

  C#Prolog -- Copyright (C) 2007 John Pool -- j.pool@ision.nl

  This library is free software; you can redistribute it and/or modify it under the terms of
  the GNU General Public License as published by the Free Software Foundation; either version
  2 of the License, or any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for details, or enter 'license.' at the command prompt.

-------------------------------------------------------------------------------------------*/

using System;
using System.Text;
using System.Threading;
#if mswindows
using System.Runtime.InteropServices;
#endif
using IO = RTParser.Prolog.PrologIO;

namespace RTParser.Prolog
{
    class CSPrologMain
    {
#if ctrlc
        public static void CtrlHandler(ConsoleCtrl.ConsoleEvent consoleEvent)
        {
            Console.WriteLine("Event: {0}", consoleEvent);

            throw new AbortQueryException(consoleEvent.ToString());
        }
#endif

        public static void Main(string[] args)
        {
#if ctrlc   // this part was a (failed) experiment to handle ^C and capture the ';' if you want another solution.
            // It migth work under VS2005
            ConsoleCtrl cc = new ConsoleCtrl();    // DOES NOT WORK !!!! (although the small test below does!!)
            cc.ControlEvent += new ConsoleCtrl.ControlEventHandler(CtrlHandler);

            //      while (true) // event handler test
            //      {
            //        string s = Console.ReadLine();
            //        if (s == "E")
            //          break;
            //      }
#endif

            PrologEngine e = new PrologEngine();

            try
            {
#if mswindows
                Console.WriteLine("| Welcome to C#Prolog MS-Windows version {0} release {1}", e.VERSION, e.RELEASE);
#else
                Console.WriteLine ("| Welcome to C#Prolog Mono version {0} release {1}", e.VERSION, e.RELEASE);
#endif
                Console.WriteLine("|");
                Console.WriteLine("| Copyright (C) 2007 John Pool");
                Console.WriteLine("|");
                Console.WriteLine("| C#Prolog comes with ABSOLUTELY NO WARRANTY. This is free software, licenced");
                Console.WriteLine("| under the GNU General Public License, and you are welcome to redistribute it");
                Console.WriteLine("| under certain conditions. Enter 'license.' at the command prompt for details.");

                Console.WriteLine("?-[aimlbotFromCS],[load],sentence(E,[what,rivers,are,there,?],[],[],[]).");
                /*

                string sr = "['cynd/logicmoo_module_aiml.pro'].";
                e.ExecuteQuery(ref sr);

                if (args == null || args.Length == 0)
                {
                    sr = "alicebot2(['HI'],X).";
                    e.ExecuteQuery(ref sr);
                }
                */
                foreach (var call in args)
                {
                    string str = call;
                    e.ExecuteQuery(ref str);
                }

                while (!e.Halted)
                    try
                    {
                        string query = null;

                        Console.Write("\n{0}{1} ?- ", (e.Debugging ? "[d]" : ""), e.CmdNo);

                        while (true)
                        {
                            query += Console.ReadLine();
                            if (query.Trim().EndsWith("/")) break; // TEMP - TODO - TO TAKE OUT?
                            if (query.Trim().StartsWith("!")) break;
                            if (query.Trim().EndsWith(".")) break;
                            Console.Write("|  ");
                        }
                        //            if (query.Trim ().EndsWith ("/")) break; // TEMP - TODO - TO TAKE OUT?

                        bool result = e.ExecuteQuery(ref query);

                        if (query == null) continue;

                        e.TryCloseCurrentOutput(); // may have been changed by tell-command

                        while (!e.Halted)
                        {
                            if (!result)
                            {
                                Console.WriteLine(PrologEngine.NO);

                                break;
                            }

                            Console.Write("{0} ({1:f3} sec) ", e.Answer, e.ProcessorTime().TotalSeconds, e.ElapsedTime());
                            //Console.Write ("{0} ({1}ms proc/{2}ms wall) ",
                            //  answer, Math.Floor (e.ProcessorTime ().TotalMilliseconds), e.ElapsedTime ());

                            if (e.CanBacktrack(false))
                            {
                                string response = Console.ReadLine().Trim();

                                if (response.Equals("y") | response.Equals(";"))
                                    result = e.More();
                                else
                                    break;
                            }
                            else if (e.Answer == PrologEngine.YES)
                            {
                                Console.WriteLine();

                                break;
                            }
                            else
                                break;
                        }
                    }
                    catch (Exception x)
                    {
                        e.SetStandardOutput();
                        e.TryCloseCurrentOutput();
                        IO.WriteLine(x.Message + Environment.NewLine + x.StackTrace);
                    }
            }
            catch (Exception x)
            {
                e.TryCloseCurrentOutput();
                Console.WriteLine(x.Message + Environment.NewLine + x.StackTrace);
            }
        }
    }

    /// <summary>
    /// Class to catch console control events (ie CTRL-C) in C#.
    /// Calls SetConsoleCtrlHandler() in Win32 API
    /// </summary>

    public class ConsoleCtrl : IDisposable
    {
#if ctrlc
        [DllImport("kernel32.dll")]
        static extern bool SetConsoleCtrlHandler(ControlEventHandler e, bool add);
#else 
        static bool SetConsoleCtrlHandler(ControlEventHandler e, bool add) {
            return true;
        }
#endif

        /// <summary>
        /// The event that occurred.
        /// </summary>
        public enum ConsoleEvent
        {
            CTRL_C = 0,   // From wincom.h
            CTRL_BREAK = 1,
            CTRL_CLOSE = 2,
            CTRL_LOGOFF = 5,
            CTRL_SHUTDOWN = 6
        }

        /// <summary>
        /// Handler to be called when a console event occurs.
        /// </summary>
        public delegate void ControlEventHandler(ConsoleEvent consoleEvent);

        /// <summary>
        /// Event fired when a console event occurs
        /// </summary>
        public event ControlEventHandler ControlEvent;

        ControlEventHandler eventHandler;

        /// <summary>
        /// Create a new instance.
        /// </summary>
        public ConsoleCtrl()
        {
            // save this to a private var so the GC doesn't collect it...
            eventHandler = new ControlEventHandler(Handler);
            SetConsoleCtrlHandler(eventHandler, true);
        }

        ~ConsoleCtrl()
        {
            Dispose(false);
        }

        public void Dispose()
        {
            Dispose(true);
        }

        void Dispose(bool disposing)
        {
            if (disposing)
            {
                GC.SuppressFinalize(this);
            }
            if (eventHandler != null)
            {
                SetConsoleCtrlHandler(eventHandler, false);
                eventHandler = null;
            }
        }

        private void Handler(ConsoleEvent consoleEvent)
        {
            if (ControlEvent != null) ControlEvent(consoleEvent);
        }
    }
}

