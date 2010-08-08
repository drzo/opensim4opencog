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
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Collections;
using System.Collections.Specialized;
using System.Globalization;
#if mswindows
using System.Runtime.InteropServices;
using MushDLR223.Utilities;

#endif

namespace RTParser.Prolog
{
    public class Globals
    {
        #region static readonly properties
        public static readonly string DefaultExtension = ".pl";
        public static readonly TextReader StdIn = DLRConsole.In;
        public static readonly TextWriter StdOut = DLRConsole.Out;
        #endregion

        #region private properties
        private static Hashtable variables = new Hashtable();            // static -> SINGLE ADDRESS SPACE !!!!!!!!!
        private static Parser currentParser = null;
        private static TextWriter currentOut = StdOut; // static -> nested tell-constructions may lead to problems
        private static TextWriter saveCurrentOut;
        #endregion

        #region public fields
        public static Hashtable Variables { get { return variables; } }
        public static string SpecialAtomChars = @"+-*/\^<=>`~:.?@#$&";
        public static CultureInfo CI = CultureInfo.InvariantCulture;
        public static Hashtable ConsultedFiles = new Hashtable();
        public static string ConsultFileName = null;   // file being currently consulted
        public static string ConsultModuleName = null; // name of cutrrent module (if any) in file being consulted
        public static Parser CurrentParser { get { return currentParser; } set { currentParser = value; } }
        public static int LineNo { get { return (currentParser == null || currentParser.InQueryMode) ? -1 : currentParser.LineNo; } }
        public static int ColNo { get { return (currentParser == null) ? -1 : currentParser.ColNo; } }
        #endregion

        // named variables in queries

        public static Term GetVariable(string s)
        {
            return (Term)variables[s];
        }

        public static void SetVariable(Term t, string s)
        {
            variables[s] = t;  // overwrite if exists
        }

        public static void EraseVariables()
        {
            variables.Clear();
        }

        public static string VariableName(Term t)
        {
            t = t.LinkEnd;

            foreach (DictionaryEntry de in variables)
                if (de.Value == t) return de.Key.ToString();

            return "_";
        }

        public static string Answer
        {
            get
            {
                StringBuilder s = new StringBuilder();
                string ans;
                Term t;

                foreach (DictionaryEntry de in variables)
                    if ((t = ((Term)de.Value)).IsUnified)
                        s.Append(String.Format("{0}{1} = {2}", Environment.NewLine, de.Key.ToString(), t));

                return ((ans = s.ToString()) == "") ? PrologEngine.YES : ans;
            }
        }

        // output

        public static void SetStandardOutput()
        {
            saveCurrentOut = currentOut;
            currentOut = StdOut;
            Console.SetOut(currentOut);
        }


        public static bool SetCurrentOutput(string outFile)
        {
            TryCloseCurrentOutput(); // first close any open file

            try
            {
                currentOut = saveCurrentOut = new StreamWriter(outFile);
                Console.SetOut(currentOut);

                return true;
            }
            catch
            {
                throw;
            }
        }


        public static void RevertToCurrentOutput()
        {
            currentOut = saveCurrentOut;
            Console.SetOut(currentOut);
        }

        public static void TryCloseCurrentOutput()
        {
            if (currentOut != StdOut) try { currentOut.Close(); }
                catch { }

            SetStandardOutput();
        }
    }


    public struct Utils
    {
        private static readonly string logFileName = "PL" + DateTime.Now.ToString("yyyy-MM-dd") + ".log";
        private static StreamWriter logFile;
        private static bool showMode = true;

        public static string AtomFromVarChar(string s)
        {
            return MakeAtom(s.Replace("'", "''"));
        }


        public static string RepeatString(string pat, int n)
        {
            return (new String('*', n)).Replace("*", pat); // any char will do
        }


        public static string Dequote(string s, char c)
        {
            int len = s.Length;
            string cs = c.ToString();

            if (len < 2)
                return s;
            else if (s[0] == c)
                return s.Substring(1, len - 2).Replace(cs + cs, cs);
            else
                return s;
        }


        public static string FileNameFromTerm(Term t, string defExt)
        {
            return (t.Value == null) ? null : ExtendedFileName(Dequoted(t.Functor), defExt);
        }


        public static string FileNameFromSymbol(string s, string defExt)
        {
            return (s == null || s == "") ? null : ExtendedFileName(Dequoted(s), defExt);
        }


        public static string Dequoted(string s)
        {
            if (s == null || s == "") return s;

            return (s[0] == '"' || s[0] == '\'') ? Dequote(s, s[0]) : s;
        }


        public static string EnsureQuoted(string s)
        {
            string result = MakeAtom(s);

            if (s[0] == '\'') return s; else return '\'' + s + '\'';
        }


        public static string ExtendedFileName(string s, string defExt)
        {
            try
            {
                string fileName = Dequoted(s);

                if (!Path.HasExtension(fileName)) fileName = Path.ChangeExtension(fileName, defExt);
                //if (!Path.HasExtension (fileName)) fileName = Path.ChangeExtension (fileName, Globals.DefaultExtension);

                fileName = Path.GetFullPath(fileName);

                string dirName = Path.GetDirectoryName(fileName);

                //      if (dirName == "")
                //      {
                //        if (Globals.ConsultDirName != "")
                //          fileName = Globals.ConsultDirName + @"\" + fileName; // use ConsultDirName if it was set
                //      }
                //      else
                //        Globals.ConsultDirName = dirName;

                return fileName;
            }
            catch
            {
                return null;
            }
        }


        public static string UnquoteIfUnnecessary(string s)
        {
            if (s[0] == '\'') return MakeAtom(s.Substring(1, s.Length - 2)); else return s;
        }


        public static string MakeAtom_ic(string a, bool atomic, out FType fType) // don't quote numbers if atomic
        {
            fType = FType.atom;

            if (a == null) PrologIO.Error("MakeAtom_ic -- got null-argument");

            if (a == "") return "";

            if (Char.IsLower(a[0]))
            {
                foreach (char c in a.ToCharArray())
                    if (!(c == '_' || Char.IsLetterOrDigit(c))) return '\'' + a + '\'';
            }
            else if (Char.IsDigit(a[0]))
            {
                {
                    bool isNumber = true;
                    bool hasDot = false;

                    foreach (char c in a.ToCharArray())
                    {
                        if (c == '.')
                        {
                            if (hasDot) { isNumber = false; break; } else hasDot = true;
                        }
                        else if (!Char.IsDigit(c))
                        { isNumber = false; break; }
                    }

                    if (isNumber) fType = FType.number;

                    return (isNumber && atomic) ? a : '\'' + a + '\'';
                }
            }
            else
            {
                foreach (char d in a.ToCharArray())
                    if (!(Globals.SpecialAtomChars.IndexOf(d) >= 0)) return '\'' + a + '\'';
            }
            return a;
        }


        public static string MakeAtom_ic(string a, bool atomic)
        {
            FType fType;

            return MakeAtom_ic(a, atomic, out fType);
        }


        public static string MakeAtomic(string a)
        {
            return MakeAtom_ic(a, true);
        }


        public static bool HasAtomShape(string s)
        {
            return (s == MakeAtom(s));
        }


        public static string MakeAtom(string a)
        {
            return MakeAtom_ic(a, false);
        }


        public static Match[] FindRegexMatches(string source, string matchPattern, bool findAllUnique)
        {
            Match[] result = null;
            Regex re = new Regex(matchPattern); //, RegexOptions.Multiline);
            MatchCollection mc = re.Matches(source);

            if (findAllUnique)
            {
                SortedList uniqueMatches = new SortedList();

                for (int i = 0; i < mc.Count; i++)
                    if (!uniqueMatches.ContainsKey(mc[i].Value)) uniqueMatches.Add(mc[i].Value, mc[i]);

                result = new Match[uniqueMatches.Count];
                uniqueMatches.Values.CopyTo(result, 0);
            }
            else
            {
                result = new Match[mc.Count];
                mc.CopyTo(result, 0);
            }

            return (result);
        }


        public static string WrapWithMargin(string s, string margin, int lenMax)
        // Break up a string into pieces that are at most lenMax characters long, by
        // inserting spaces that are at most lenMax positions apart from each other.
        // If possible, spaces are inserted after 'separator characters'; otherwise
        // they are simply inserted at each lenMax position. Prefix a margin to the 2nd+ string.
        {
            const string separators = @" +-/*^!@():,.;=[]{}<>\";

            StringBuilder sb = new StringBuilder();
            bool first = true;
            int p = 0;
            int rem = s.Length - p;

            while (rem > lenMax)
            {
                // get the position < lenMax of the last separator character
                int i = s.Substring(p, lenMax).LastIndexOfAny(separators.ToCharArray(), lenMax - 1);
                int segLen;

                if (i == -1) segLen = lenMax; else segLen = i + 1;

                if (first) first = false; else sb.Append(margin);
                sb.Append(s.Substring(p, segLen));
                sb.Append(Environment.NewLine);

                p += segLen;
                rem -= segLen;
            }

            if (first) first = false; else sb.Append(margin);

            if (rem != 0)
            {
                sb.Append(s.Substring(p));
                sb.Append(Environment.NewLine);
            }

            return sb.ToString();
        }


        public static void Assert(bool b, string s)
        {
            if (!b) throw new Exception(s);
        }


        public static string ForceSpaces(string s, int lenMax)
        // Break up a string into pieces that are at most lenMax characters long, by
        // inserting spaces that are at most lenMax positions apart from each other.
        // If possible, spaces are inserted after 'separator characters'; otherwise
        // they are simply inserted at each lenMax position.
        {
            const string separators = " -/:,.;";

            if (lenMax < 2) PrologIO.Error("Second argument of wrap must be > 1");

            return ForceSpaces(s, lenMax - 1, separators, 0); // 0 is current pos in separators
        }


        private static string ForceSpaces(string s, int lenMax, string separators, int i)
        {
            int len = s.Length;
            StringBuilder sb = new StringBuilder();
            string blank = Environment.NewLine;

            // special cases
            if (len <= lenMax) return s; // nothing to do

            if (i == separators.Length) // done with all separators -- now simply insert spaces
            {
                int r = len; // rest

                while (r > 0)
                {
                    sb.Append(s.Substring(len - r, (r > lenMax) ? lenMax : r));
                    sb.Append(blank);
                    r -= lenMax;
                }
                return sb.ToString().Trim();
            }
            // end of special cases

            string[] words = s.Split(new Char[] { separators[i] }); // split, using the current separator

            for (int k = 0; k < words.Length; k++)
            {
                string t = ForceSpaces(words[k], lenMax, separators, i + 1); // apply the next separator to each word

                // do not re-place the separator after the last word
                sb.Append(t + (k == words.Length - 1 ? "" : (separators[i] + blank))); // recursively handle all seps
            }

            return sb.ToString();
        }


        // Return the ISO week number for a date. Week 1 of a year is the
        // first week of the year in which there are more than three days, i.e.
        // the week in which the first Thursday of the year lies.
        public static int WeekNo(DateTime date)
        {
            // special case: if the date is in a week that starts on December 29,
            // 30 or 31 (i.e. date day in [sun..wed]), then return 1
            if (date.Month == 12 && date.Day >= 29 && date.DayOfWeek <= DayOfWeek.Wednesday) return 1;

            DateTime jan1 = new DateTime(date.Year, 1, 1); // January 1st
            DayOfWeek jan1Day = jan1.DayOfWeek;
            // jan1 is in week 1 if jan1Day is in [sun..wed], since only in that case
            // there are > 3 days in the week. Calculate the start date of week 1.
            DateTime startWk1 = (jan1Day <= DayOfWeek.Wednesday)
              ? jan1.Subtract(new TimeSpan((int)jan1Day, 0, 0, 0))
              : jan1.Subtract(new TimeSpan((int)jan1Day - 7, 0, 0, 0));
            // Calculate the number of days between the given date and the start
            // date of week 1 and (integer) divide that by 7. This is the weekno-1.
            return 1 + (date - startWk1).Days / 7;
        }


#if mswindows
        //          Utils.SendNetBios (Environment.MachineName, Environment.UserName, "hoi");
        [DllImport("netapi32.dll")]
        private static extern short NetMessageBufferSend(IntPtr server, IntPtr recipient, IntPtr reserved, IntPtr message, int size);

        public static void SendNetBios(string server, string recipient, string text)
        {
            int err;
            IntPtr srv = IntPtr.Zero, rcp = IntPtr.Zero, txt = IntPtr.Zero, res = IntPtr.Zero;

            try
            {
                srv = Marshal.StringToBSTR(server);
                rcp = Marshal.StringToBSTR(recipient);
                txt = Marshal.StringToBSTR(text = string.Format("{0}/{1}: {2}", server, recipient, text));

                err = NetMessageBufferSend(srv, rcp, res, txt, (text.Length + 1) * 2);
            }
            catch (Exception /*e*/)
            {
                ;
            }
            finally
            {
                if (srv != IntPtr.Zero)
                    Marshal.FreeBSTR(srv);
                if (rcp != IntPtr.Zero)
                    Marshal.FreeBSTR(rcp);
                if (txt != IntPtr.Zero)
                    Marshal.FreeBSTR(txt);
            }
        }


        // Console

        private class Constants
        {
            // Standard input, output, and error
            internal const int STD_INPUT_HANDLE = -10;
            internal const int STD_OUTPUT_HANDLE = -11;
            internal const int STD_ERROR_HANDLE = -12;

            // Returned by GetStdHandle when an error occurs
            internal static readonly IntPtr INVALID_HANDLE_VALUE = new IntPtr(-1);
        }

        struct COORD
        {
            internal short X;
            internal short Y;

            public COORD(bool b) // constructor just to get rid of compiler warnings
            {
                X = 0;
                Y = 0;
            }
        }

        struct SMALL_RECT
        {
            internal short Left;
            internal short Top;
            internal short Right;
            internal short Bottom;

            public SMALL_RECT(bool b) // constructor just to get rid of compiler warnings
            {
                Left = 0;
                Top = 0;
                Right = 0;
                Bottom = 0;
            }
        }

        struct CONSOLE_SCREEN_BUFFER_INFO
        {
            internal COORD dwSize;
            internal COORD dwCursorPosition;
            internal ushort wAttributes;
            internal SMALL_RECT srWindow;
            internal COORD dwMaximumWindowSize;
        }

        [DllImport("kernel32.dll", SetLastError = true)]
        static extern bool GetConsoleScreenBufferInfo(
          IntPtr hConsoleOutput,
          out CONSOLE_SCREEN_BUFFER_INFO lpConsoleScreenBufferInfo
        );

        [DllImport("kernel32.dll", SetLastError = true)]
        static extern IntPtr GetStdHandle(
          int whichHandle
        );

        [DllImport("kernel32.dll", SetLastError = true)]
        static extern IntPtr GetConsoleWindow();

        static IntPtr GetHandle(int WhichHandle)
        {
            IntPtr h = GetStdHandle(WhichHandle);

            if (h == Constants.INVALID_HANDLE_VALUE)
            {
                switch (WhichHandle)
                {
                    case Constants.STD_INPUT_HANDLE:
                        throw new Exception("Can't get standard input handle");
                    //break;
                    case Constants.STD_OUTPUT_HANDLE:
                        throw new Exception("Can't get standard output handle");
                    //break;
                    case Constants.STD_ERROR_HANDLE:
                        throw new Exception("Can't get standard error handle");
                    //break;
                    default:
                        throw new Exception("Apparently invalid parameter to GetHandle");
                }
            }
            return h;
        }


        public static short NumCols
        {
            get
            {
                IntPtr h = GetHandle(Constants.STD_OUTPUT_HANDLE);
                CONSOLE_SCREEN_BUFFER_INFO csbi = new CONSOLE_SCREEN_BUFFER_INFO();

                if (!GetConsoleScreenBufferInfo(h, out csbi)) return 0;

                return csbi.dwSize.X;
            }
            //      set
            //      {
            //        IntPtr h = GetHandle (Constants.STD_OUTPUT_HANDLE);
            //        CONSOLE_SCREEN_BUFFER_INFO csbi = new CONSOLE_SCREEN_BUFFER_INFO();
            //
            //        if (!GetConsoleScreenBufferInfo (h, out csbi)) return;
            //
            //        COORD c = new COORD ();
            //        c.X = value;
            //        c.Y = csbi.dwSize.Y;
            //        SetConsoleScreenBufferSize (h,c);
            //
            //        return;
            //      }
        }
#endif


        // Log file

        public static void SetShow(bool mode)
        {
            showMode = mode;
        }


        public static void OpenLog()
        {
            logFile = new StreamWriter(logFileName);
        }


        public static void WriteLogLine(bool abort, string s, params object[] pa)
        {
            if (abort)
            {
                try
                {
                    logFile.WriteLine(s, pa);
                    throw new Exception(String.Format(s, pa));
                }
                finally
                {
                    CloseLog();
                }
            }
            else
            {
                Console.WriteLine(s, pa);
                logFile.WriteLine(s, pa);
            }
        }


        public static void WriteLogLine(string s, params object[] pa)
        {
            WriteLogLine(false, s, pa);
        }


        public static void WriteLine(string s, params object[] pa)
        {
            WriteLogLine(false, s, pa);
        }


        public static void WriteLine(string s)
        {
            WriteLogLine(false, s);
        }


        public static void Show(string s, params object[] pa)
        {
            if (showMode) WriteLogLine(false, s, pa);
        }


        public static void Show(string s)
        {
            if (showMode) WriteLogLine(false, s);
        }


        public static void CloseLog()
        {
            logFile.Flush();
            logFile.Close();
        }
    }
}
