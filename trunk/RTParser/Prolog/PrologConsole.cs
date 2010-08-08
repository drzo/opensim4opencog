using System;
using System.IO;
using MushDLR223.Utilities;

namespace RTParser.Prolog
{
    internal static class Console
    {
        public static void SetIn(TextReader reader)
        {
            DLRConsole.SetIn(reader);
        }

        public static void SetOut(TextWriter writer)
        {
            DLRConsole.SetOut(writer);
        }

        public static void WriteLine(string format, params object[] args)
        {
            DLRConsole.SystemWriteLine(format, args);
        }

        public static void WriteLine()
        {
            DLRConsole.SystemWriteLine();
        }

        public static void Write(string format, params object[] args)
        {
            DLRConsole.SystemWrite(format, args);
        }

        public static string ReadLine()
        {
            return DLRConsole.ReadLine();
        }

        internal static ConsoleKeyInfo ReadKey()
        {
            return DLRConsole.ReadKey();
        }

        public static ConsoleKeyInfo ReadKey(bool b)
        {
            return DLRConsole.ReadKey(b);
        }

        private static readonly object m_LogLock = new object();
        public static event ConsoleCancelEventHandler CancelKeyPress
        {
            add { lock (m_LogLock) { DLRConsole.CancelKeyPress += value; } }
            remove { lock (m_LogLock) { DLRConsole.CancelKeyPress -= value; } }
        }

        public static bool KeyAvailable
        {
            get { return DLRConsole.KeyAvailable; }
        }
    }
}