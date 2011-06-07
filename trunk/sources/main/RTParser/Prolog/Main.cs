#if (!STANDARD)
#define debugging
#define arg1index
#define mswindows
#define newor
#define partialengine
#endif

#if (!VISUAL_STUDIO)
#undef mswindows
#endif

/*-----------------------------------------------------------------------------------------

  C#Prolog -- Copyright (C) 2007-2009 John Pool -- j.pool@ision.nl
                   Contributions 2009 by Lars Iwer -- lars.iwer@inf.tu-dresden.de

  This library is free software; you can redistribute it and/or modify it under the terms of
  the GNU General Public License as published by the Free Software Foundation; either version
  2 of the License, or any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for details, or enter 'license.' at the command prompt.

-------------------------------------------------------------------------------------------*/

using System;
using System.Text;

namespace RTParser.Prolog
{
    class PrologMain
    {
        private static PrologEngine engine = new PrologEngine();

        public static void Main0(string[] args)
        {
            // Event handler catches CTRL+C and prevents it from ending the application
            Console.CancelKeyPress += new ConsoleCancelEventHandler(Console_CancelKeyPress);

#if BLA
      try
      {
#endif
#if true
            //mswindows
            Console.WriteLine("| Welcome to C#Prolog MS-Windows version {0}, parser {1}", engine.VERSION, engine.RELEASE);
#else
        Console.WriteLine ("| Welcome to C#Prolog Mono version {0}, parser release {1}", e.VERSION, e.RELEASE);
#endif
            Console.WriteLine("|");
            Console.WriteLine("| Copyright (C) 2007-2009 John Pool");
            Console.WriteLine("|               with contributions 2009 by Lars Iwer ");
            Console.WriteLine("|");
            Console.WriteLine("| C#Prolog comes with ABSOLUTELY NO WARRANTY. This is free software, licenced");
            Console.WriteLine("| under the GNU General Public License, and you are welcome to redistribute it");
            Console.WriteLine("| under certain conditions. Enter 'license.' at the command prompt for details.");

            while (!engine.Halted)
            {
#if !DEBUG
        try
        {
#endif
                StringBuilder queryBuilder = new StringBuilder();
                String query = "";
                String line;

                while (query.Length == 0)
                {
                    Console.Write("\n{0}{1} ?- ", (engine.Debugging ? "[d]" : ""), engine.CmdNo);

                    while (true)
                    {
                        line = Console.ReadLine();
                        if (line != null)
                        {
                            queryBuilder.Append(line);
                            line.Trim();
                            if (line.EndsWith("/")) break; // TEMP
                            if (line.StartsWith("!")) break;
                            if (line.EndsWith(".")) break;
                            Console.Write("|  ");
                        }
                        else
                        {
                            // CTRL+C caught during composing a query -> reset
                            queryBuilder = new StringBuilder();
                            break;
                        }
                    }

                    query = queryBuilder.ToString();
                }
                bool result = engine.ExecuteQuery(ref query);

                if (query == null) continue;

                engine.TryCloseCurrentOutput(); // may have been changed by tell-command

                while (!engine.Halted)
                {
                    if (!result)
                    {
                        Console.WriteLine(PrologEngine.NO);

                        break;
                    }

                    Console.Write("{0} ({1:f3} sec) ", engine.Answer, engine.ProcessorTime().TotalSeconds, engine.ElapsedTime());
                    //Console.Write ("{0} ({1}ms proc/{2}ms wall) ",
                    //  answer, Math.Floor (e.ProcessorTime ().TotalMilliseconds), e.ElapsedTime ());

                    if (engine.CanBacktrack(false))
                    {
                        Console.Write("more? (y/n) ");
                        char response = Console.ReadKey().KeyChar;

                        if (response.Equals('y') | response.Equals(';'))
                            result = engine.More();
                        else
                            break;
                    }
                    else if (engine.Answer == PrologEngine.YES)
                    {
                        Console.WriteLine();

                        break;
                    }
                    else
                        break;
                }
#if !DEBUG
        }
        catch (Exception x)
        {
          engine.SetStandardOutput();
          engine.TryCloseCurrentOutput();
          PrologIO.WriteLine(x.Message + Environment.NewLine + x.StackTrace);
        }
#endif
            }
#if BLA       
      }
      catch (Exception x)
      {
        e.TryCloseCurrentOutput ();
        Console.WriteLine (x.Message + Environment.NewLine + x.StackTrace);
      }
#endif
        }

        static void Console_CancelKeyPress(object sender, ConsoleCancelEventArgs e)
        {
            // Event called when CTRL-C or CTRL-BREAK hit
            while (Console.KeyAvailable)
            {
                Console.ReadKey(false);
            }

            if (!engine.Halted)
            {
                engine.SetStandardOutput();
                engine.TryCloseCurrentOutput();
            }
            if (e.SpecialKey == System.ConsoleSpecialKey.ControlC)
            {
                e.Cancel = true;
            }
        }
    }
}

