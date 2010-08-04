#if VISUAL_STUDIO
#define debugging
#define arg1index
#define mswindows
#define newor
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
using System.IO;

namespace RTParser.Prolog
{
    class PrologIO
    {
        public static bool Verbose = true; // message display

        public static bool Error(string msg, params Object[] o)
        {
            Console.SetOut(Globals.StdOut);

            if (Globals.LineNo == -1) // interactive
                throw new Exception("\n*** error: " + String.Format(msg, o));
            else if (Globals.LineNo != -1)
                throw new Exception(String.Format("\n*** error in line {0} at position {1}: {2}",
                                                    Globals.LineNo, Globals.ColNo, String.Format(msg, o)));

            return false;
        }

        // true = warn ... false = error
        public static bool WarnOrError(string msg, params object[] o)
        {
            Console.SetOut(Globals.StdOut);
            string s = SafeFormat(msg, o);
            if (Globals.LineNo != -1)
                s = SafeFormat("\n*** error in line {0} at position {1}: {2}",
                                   Globals.LineNo, Globals.ColNo, s);
            else s = ("\n*** error: " + s);
            Message(s);
            return true;
        }

        private static string SafeFormat(string msg, params object[] objects)
        {
            if (objects == null || objects.Length == 0) return msg;
            try
            {
                return string.Format(msg, objects);
            }
            catch (Exception e)
            {
                RTPBot.writeDebugLine("ERROR " + e);
            }
            return msg;
        }


        public static bool Error(string msg)
        {
            Console.SetOut(Globals.StdOut);

            if (Globals.LineNo == -1) // interactive
                throw new FormatException("\n*** error: " + msg);
            else if (Globals.LineNo != -1)
                throw new Exception(String.Format("\n*** error in line {0} at position {1}: {2}",
                                                    Globals.LineNo, Globals.ColNo, msg));

            return false;
        }


        public static void Warning(string msg, params Object[] o)
        {
            Globals.SetStandardOutput();
            Console.WriteLine("\n*** warning: " + msg, o);
            Globals.RevertToCurrentOutput();
        }


        public static void Warning(string msg)
        {
            Globals.SetStandardOutput();
            Console.WriteLine("\n*** warning: " + msg);
            Globals.RevertToCurrentOutput();
        }


        public static void Message(string msg, params Object[] o)
        {
            Globals.SetStandardOutput();
            Console.WriteLine("\n--- " + msg, o);
            Globals.RevertToCurrentOutput();
        }


        public static void Message(string msg)
        {
            Globals.SetStandardOutput();
            Console.WriteLine("\n--- " + msg);
            Globals.RevertToCurrentOutput();
        }


        public static void Fatal(string msg, params Object[] o)
        {
            Globals.SetStandardOutput();
            throw new Exception("\n*** fatal: " + String.Format(msg, o));
        }


        public static void Fatal(string msg)
        {
            Globals.SetStandardOutput();
            throw new Exception("\n*** fatal: " + msg);
        }


        public static void Write(String s, params Object[] o)
        {
            Console.Write(s, o);
        }


        public static void Write(String s)
        {
            Console.Write(s);
        }


        public static void WriteLine(String s, params Object[] o)
        {
            Console.WriteLine(s, o);
        }


        public static void WriteLine(String s)
        {
            Console.WriteLine(s);
        }


        public static void Trace(String s)
        {
            Console.WriteLine(s);
        }

        public static void PrologPrint(Term t)
        {
            string functor;

            if (t.IsString)
                Console.Write(t.Functor);
            else if (t.IsAtom && ((functor = t.Functor)[0]) == '\'') // remove single quotes
                Console.Write(functor.Substring(1, functor.Length - 2));
            else
                Console.Write(t.ToString());
        }

        public static void PrologPrint(String s)
        {
            Console.Write(s);
        }
    }
}
