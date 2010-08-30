/*
 * Copyright (c) Contributors, http://opensimulator.org/
 * See CONTRIBUTORS.TXT for a full list of copyright holders.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the OpenSim Project nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE DEVELOPERS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using log4net;
using MushDLR223.ScriptEngines;

namespace MushDLR223.Utilities
{
    public delegate void CommandDelegate(string module, string[] cmd);

    public class Commands
    {
        /// <summary>
        /// Encapsulates a command that can be invoked from the console
        /// </summary>
        private class CommandInfo
        {
            /// <value>
            /// The module from which this command comes
            /// </value>
            public string module;

            /// <value>
            /// Whether the module is shared
            /// </value>
            public bool shared;

            /// <value>
            /// Very short BNF description
            /// </value>
            public string help_text;

            /// <value>
            /// Longer one line help text
            /// </value>
            public string long_help;

            /// <value>
            /// Full descriptive help for this command
            /// </value>
            public string descriptive_help;

            /// <value>
            /// The method to invoke for this command
            /// </value>
            public List<CommandDelegate> fn;
        }

        /// <value>
        /// Commands organized by keyword in a tree
        /// </value>
        private Dictionary<string, object> tree =
                new Dictionary<string, object>();

        /// <summary>
        /// Get help for the given help string
        /// </summary>
        /// <param name="helpParts">Parsed parts of the help string.  If empty then general help is returned.</param>
        /// <returns></returns>
        public List<string> GetHelp(string[] cmd)
        {
            List<string> help = new List<string>();
            List<string> helpParts = new List<string>(cmd);

            // Remove initial help keyword
            helpParts.RemoveAt(0);

            // General help
            if (helpParts.Count == 0)
            {
                help.AddRange(CollectHelp(tree));
                help.Sort();
            }
            else
            {
                help.AddRange(CollectHelp(helpParts));
            }

            return help;
        }

        /// <summary>
        /// See if we can find the requested command in order to display longer help
        /// </summary>
        /// <param name="helpParts"></param>
        /// <returns></returns>
        private List<string> CollectHelp(List<string> helpParts)
        {
            string originalHelpRequest = string.Join(" ", helpParts.ToArray());
            List<string> help = new List<string>();

            Dictionary<string, object> dict = tree;
            while (helpParts.Count > 0)
            {
                string helpPart = helpParts[0];

                if (!dict.ContainsKey(helpPart))
                    break;

                //m_log.Debug("Found {0}", helpParts[0]);

                if (dict[helpPart] is Dictionary<string, Object>)
                    dict = (Dictionary<string, object>)dict[helpPart];

                helpParts.RemoveAt(0);
            }

            // There was a command for the given help string
            if (dict.ContainsKey(String.Empty))
            {
                CommandInfo commandInfo = (CommandInfo)dict[String.Empty];
                help.Add(commandInfo.help_text);
                help.Add(commandInfo.long_help);
                help.Add(commandInfo.descriptive_help);
            }
            else
            {
                help.Add(string.Format("No help is available for {0}", originalHelpRequest));
            }

            return help;
        }

        private List<string> CollectHelp(Dictionary<string, object> dict)
        {
            List<string> result = new List<string>();

            foreach (KeyValuePair<string, object> kvp in dict)
            {
                if (kvp.Value is Dictionary<string, Object>)
                {
                    result.AddRange(CollectHelp((Dictionary<string, Object>)kvp.Value));
                }
                else
                {
                    if (((CommandInfo)kvp.Value).long_help != String.Empty)
                        result.Add(((CommandInfo)kvp.Value).help_text + " - " +
                                ((CommandInfo)kvp.Value).long_help);
                }
            }
            return result;
        }

        /// <summary>
        /// Add a command to those which can be invoked from the console.
        /// </summary>
        /// <param name="module"></param>
        /// <param name="command"></param>
        /// <param name="help"></param>
        /// <param name="longhelp"></param>
        /// <param name="fn"></param>
        public void AddCommand(string module, bool shared, string command,
                string help, string longhelp, CommandDelegate fn)
        {
            AddCommand(module, shared, command, help, longhelp,
                    String.Empty, fn);
        }

        /// <summary>
        /// Add a command to those which can be invoked from the console.
        /// </summary>
        /// <param name="module"></param>
        /// <param name="command"></param>
        /// <param name="help"></param>
        /// <param name="longhelp"></param>
        /// <param name="descriptivehelp"></param>
        /// <param name="fn"></param>
        public void AddCommand(string module, bool shared, string command,
                string help, string longhelp, string descriptivehelp,
                CommandDelegate fn)
        {
            string[] parts = Parser.Parse(command);

            Dictionary<string, Object> current = tree;

            foreach (string s in parts)
            {
                if (current.ContainsKey(s))
                {
                    if (current[s] is Dictionary<string, Object>)
                    {
                        current = (Dictionary<string, Object>)current[s];
                    }
                    else
                        return;
                }
                else
                {
                    current[s] = new Dictionary<string, Object>();
                    current = (Dictionary<string, Object>)current[s];
                }
            }

            CommandInfo info;

            if (current.ContainsKey(String.Empty))
            {
                info = (CommandInfo)current[String.Empty];
                if (!info.shared && !info.fn.Contains(fn))
                    info.fn.Add(fn);

                return;
            }

            info = new CommandInfo();
            info.module = module;
            info.shared = shared;
            info.help_text = help;
            info.long_help = longhelp;
            info.descriptive_help = descriptivehelp;
            info.fn = new List<CommandDelegate>();
            info.fn.Add(fn);
            current[String.Empty] = info;
        }

        public string[] FindNextOption(string[] cmd, bool term)
        {
            Dictionary<string, object> current = tree;

            int remaining = cmd.Length;

            foreach (string s in cmd)
            {
                remaining--;

                List<string> found = new List<string>();

                foreach (string opt in current.Keys)
                {
                    if (remaining > 0 && opt == s)
                    {
                        found.Clear();
                        found.Add(opt);
                        break;
                    }
                    if (opt.StartsWith(s))
                    {
                        found.Add(opt);
                    }
                }

                if (found.Count == 1 && (remaining != 0 || term))
                {
                    current = (Dictionary<string, object>)current[found[0]];
                }
                else if (found.Count > 0)
                {
                    return found.ToArray();
                }
                else
                {
                    break;
                    //                    return new string[] {"<cr>"};
                }
            }

            if (current.Count > 1)
            {
                List<string> choices = new List<string>();

                bool addcr = false;
                foreach (string s in current.Keys)
                {
                    if (s == String.Empty)
                    {
                        CommandInfo ci = (CommandInfo)current[String.Empty];
                        if (ci.fn.Count != 0)
                            addcr = true;
                    }
                    else
                        choices.Add(s);
                }
                if (addcr)
                    choices.Add("<cr>");
                return choices.ToArray();
            }

            if (current.ContainsKey(String.Empty))
                return new string[] { "Command help: " + ((CommandInfo)current[String.Empty]).help_text };

            return new string[] { new List<string>(current.Keys)[0] };
        }

        public string[] Resolve(string[] cmd)
        {
            string[] result = cmd;
            int index = -1;

            Dictionary<string, object> current = tree;

            foreach (string s in cmd)
            {
                index++;

                List<string> found = new List<string>();

                foreach (string opt in current.Keys)
                {
                    if (opt == s)
                    {
                        found.Clear();
                        found.Add(opt);
                        break;
                    }
                    if (opt.StartsWith(s))
                    {
                        found.Add(opt);
                    }
                }

                if (found.Count == 1)
                {
                    result[index] = found[0];
                    current = (Dictionary<string, object>)current[found[0]];
                }
                else if (found.Count > 0)
                {
                    return new string[0];
                }
                else
                {
                    break;
                }
            }

            if (current.ContainsKey(String.Empty))
            {
                CommandInfo ci = (CommandInfo)current[String.Empty];
                if (ci.fn.Count == 0)
                    return new string[0];
                foreach (CommandDelegate fn in ci.fn)
                {
                    if (fn != null)
                        fn(ci.module, result);
                    else
                        return new string[0];
                }
                return result;
            }

            return new string[0];
        }
    }

    public class DLRConsole
    {
        private static readonly ILog m_log = LogManager.GetLogger(MethodBase.GetCurrentMethod().DeclaringType);

        public static TextFilter TheGlobalLogFilter = new TextFilter()
                                                          {
                                                              "clear",
                                                              "+*",
                                                          };
                                 
        static public string ShouldPrint(string str, params object[] args)
        {
            string printStr = DLRConsole.SafeFormat(str, args);
            if (!TheGlobalLogFilter.ShouldPrint(printStr))
            {
                return null;
            }
            return printStr;
        }

        static private readonly object m_syncRoot = new object();

        static private int y = -1;
        private int cp = 0;
        private int h = 1;
        private string prompt = "# ";
        static private StringBuilder cmdline = new StringBuilder();
        public Commands Commands = new Commands();
        private bool echo = true;
        static private List<string> history = new List<string>();
        private bool gui = false;

        public object ConsoleScene = null;

        /// <summary>
        /// The default prompt text.
        /// </summary>
        public string DefaultPrompt
        {
            set { m_defaultPrompt = value + "# "; }
            get { return m_defaultPrompt; }
        }
        protected string m_defaultPrompt;

        private static readonly TextWriter IntitialConsoleOut = Console.Out;
        private static readonly TextWriter ConsoleOut = new OutputDelegateWriter(SystemWriteLine);

        static DLRConsole()
        {
            //Application.VisualStyleState
            AddOutput(IntitialConsoleOut);
        }

        public static ConsoleColor SystemForegroundColor
        {
            get { return System.Console.ForegroundColor; }
            set { System.Console.ForegroundColor = value; }
        }

        public static int BufferHeight
        {
            get { return Console.BufferHeight; }
            set { Console.BufferHeight = value; }
        }

        public static int BufferWidth
        {
            get { return Console.BufferWidth; }
            set { Console.BufferWidth = value; }
        }
        public static int CursorTop
        {
            get { return Console.CursorTop; }
            set { Console.CursorTop = value; }
        }
        public static int CursorLeft
        {
            get { return Console.CursorLeft; }
            set { Console.CursorLeft = value; }
        }

        public static TextWriter Out
        {
            get { return Console.Out ?? ConsoleOut; }
        }

        public static TextReader In
        {
            get { return Console.In; }
        }

        public static void SetIn(TextReader reader)
        {
            Console.SetIn(reader);
        }

        public static void SetOut(TextWriter writer)
        {
            //var old = Console.Out;
            //RemoveOutput(old);
            AddOutput(writer);
            Console.SetOut(ConsoleOut);
        }

        private static readonly object m_LogLock = new object();
        public static event ConsoleCancelEventHandler CancelKeyPress
        {
            add { lock (m_LogLock) { Console.CancelKeyPress += value; } }
            remove { lock (m_LogLock) { Console.CancelKeyPress -= value; } }
        }

        public static bool KeyAvailable
        {
            get { return Console.KeyAvailable; }
        }

        public DLRConsole(string defaultPrompt)
        {
            DefaultPrompt = defaultPrompt;

            Commands.AddCommand("console", false, "help", "help [<command>]",
                    "Get general command list or more detailed help on a specific command", Help);
        }

        public void SetGuiMode(bool mode)
        {
            gui = mode;
        }

        private void AddToHistory(string text)
        {
            if (text.Trim().Length == 0) return;
            while (history.Count >= 100)
                history.RemoveAt(0);

            history.Add(text);
        }

        /// <summary>
        /// derive an ansi color from a string, ignoring the darker colors.
        /// This is used to help automatically bin component tags with colors
        /// in various print functions.
        /// </summary>
        /// <param name="input">arbitrary string for input</param>
        /// <returns>an ansii color</returns>
        private static ConsoleColor DeriveColor(string input)
        {
            int colIdx = (input.ToUpper().GetHashCode() % 6) + 9;
            return (ConsoleColor)colIdx;
        }

        /// <summary>
        /// Sends a warning to the current console output
        /// </summary>
        /// <param name="format">The message to send</param>
        /// <param name="args">WriteLine-style message arguments</param>
        public void Warn(string format, params object[] args)
        {
            WriteNewLine(ConsoleColor.Yellow, format, args);
        }

        /// <summary>
        /// Sends a warning to the current console output
        /// </summary>
        /// <param name="sender">The module that sent this message</param>
        /// <param name="format">The message to send</param>
        /// <param name="args">WriteLine-style message arguments</param>
        public void Warn(string sender, string format, params object[] args)
        {
            WriteNewLine(DeriveColor(sender), sender, ConsoleColor.Yellow, format, args);
        }

        /// <summary>
        /// Sends a notice to the current console output
        /// </summary>
        /// <param name="format">The message to send</param>
        /// <param name="args">WriteLine-style message arguments</param>
        public void Notice(string format, params object[] args)
        {
            WriteNewLine(ConsoleColor.White, format, args);
        }

        /// <summary>
        /// Sends a notice to the current console output
        /// </summary>
        /// <param name="sender">The module that sent this message</param>
        /// <param name="format">The message to send</param>
        /// <param name="args">WriteLine-style message arguments</param>
        public void Notice(string sender, string format, params object[] args)
        {
            WriteNewLine(DeriveColor(sender), sender, ConsoleColor.White, format, args);
        }
        /// <summary>
        /// Sends an error to the current console output
        /// </summary>
        /// <param name="format">The message to send</param>
        /// <param name="args">WriteLine-style message arguments</param>
        public void Error(string format, params object[] args)
        {
            WriteNewLine(ConsoleColor.Red, format, args);
        }

        /// <summary>
        /// Sends an error to the current console output
        /// </summary>
        /// <param name="sender">The module that sent this message</param>
        /// <param name="format">The message to send</param>
        /// <param name="args">WriteLine-style message arguments</param>
        public void Error(string sender, string format, params object[] args)
        {
            WriteNewLine(DeriveColor(sender), sender, ConsoleColor.Red, format, args);
        }

        /// <summary>
        /// Sends a status message to the current console output
        /// </summary>
        /// <param name="format">The message to send</param>
        /// <param name="args">WriteLine-style message arguments</param>
        public void Status(string format, params object[] args)
        {
            WriteNewLine(ConsoleColor.Blue, format, args);
        }

        /// <summary>
        /// Sends a status message to the current console output
        /// </summary>
        /// <param name="sender">The module that sent this message</param>
        /// <param name="format">The message to send</param>
        /// <param name="args">WriteLine-style message arguments</param>
        public void Status(string sender, string format, params object[] args)
        {
            WriteNewLine(DeriveColor(sender), sender, ConsoleColor.Blue, format, args);
        }

        [Conditional("DEBUG")]
        public void Debug(string format, params object[] args)
        {
            WriteNewLine(ConsoleColor.Gray, format, args);
        }

        [Conditional("DEBUG")]
        public void Debug(string sender, string format, params object[] args)
        {
            WriteNewLine(DeriveColor(sender), sender, ConsoleColor.Gray, format, args);
        }

        static private int SetCursorTop(int top)
        {
            if (top >= 0 && top < BufferHeight)
            {
                CursorTop = top;
                return top;
            }
            else
            {
                return CursorTop;
            }
        }

        static private int SetCursorLeft(int left)
        {
            if (left >= 0 && left < BufferWidth)
            {
                CursorLeft = left;
                return left;
            }
            else
            {
                return CursorLeft;
            }
        }

        static public void WriteNewLine(ConsoleColor senderColor, string sender, ConsoleColor color, string format, params object[] args)
        {
            lock (cmdline)
            {
                if (y != -1)
                {
                    y = SetCursorTop(y);
                    CursorLeft = 0;

                    int count = cmdline.Length;

                    SystemWrite0("  ");
                    while (count-- > 0)
                        SystemWrite0(" ");

                    y = SetCursorTop(y);
                    CursorLeft = 0;
                }
                WritePrefixLine(senderColor, sender);
                WriteConsoleLine(color, format, args);
                if (y != -1)
                    y = CursorTop;
            }
        }

        static public void WriteNewLine(ConsoleColor color, string format, params object[] args)
        {
            lock (cmdline)
            {
                if (y != -1)
                {
                    y = SetCursorTop(y);
                    CursorLeft = 0;

                    int count = cmdline.Length;

                    SystemWrite0("  ");
                    while (count-- > 0)
                        SystemWrite0(" ");

                    y = SetCursorTop(y);
                    CursorLeft = 0;
                }
                WriteConsoleLine(color, format, args);
                if (y != -1)
                    y = CursorTop;
            }
        }

        static public void WriteConsoleLine(ConsoleColor color, string format, params object[] args)
        {
            try
            {
                lock (m_syncRoot)
                {
                    if (args == null || args.Length == 0)
                    {
                        args = new object[] { format };
                        format = "{0}";
                    }
                    try
                    {
                        if (color != ConsoleColor.White)
                            SystemForegroundColor = color;
                        if (args.Length == 0) SystemWriteLine0(format);
                        else SystemWriteLine0(format, args);
                        ResetColor();
                    }
                    catch (ArgumentNullException)
                    {
                        // Some older systems dont support coloured text.
                        SystemWriteLine0(format, args);
                    }
                    catch (FormatException e)
                    {
                        SystemWriteLine0("FormatException " + format + " ex=" + e);
                    }
                }
            }
            catch (ObjectDisposedException)
            {
            }
        }

        static private void WritePrefixLine(ConsoleColor color, string sender)
        {
            try
            {
                lock (m_syncRoot)
                {
                    sender = sender.ToUpper();

                    // SystemWriteLine("[" + sender + "] ");

                    SystemWrite0("[");

                    try
                    {
                        SystemForegroundColor = color;
                        SystemWrite0(sender);
                        ResetColor();
                    }
                    catch (ArgumentNullException)
                    {
                        // Some older systems dont support coloured text.
                        SystemWriteLine0(sender);
                    }

                    SystemWrite0("] \t");
                }
            }
            catch (ObjectDisposedException)
            {
            }
        }

        private void Help(string module, string[] cmd)
        {
            List<string> help = Commands.GetHelp(cmd);

            foreach (string s in help)
                Output(s);
        }

        private void Show()
        {
            lock (cmdline)
            {
                if (y == -1 || BufferWidth == 0)
                    return;

                int xc = prompt.Length + cp;
                int new_x = xc % BufferWidth;
                int new_y = y + xc / BufferWidth;
                int end_y = y + (cmdline.Length + prompt.Length) / BufferWidth;
                if (end_y / BufferWidth >= h)
                    h++;
                if (end_y >= BufferHeight) // wrap
                {
                    y--;
                    new_y--;
                    CursorLeft = 0;
                    CursorTop = BufferHeight - 1;
                    SystemWriteLine0(" ");
                }

                y = SetCursorTop(y);
                CursorLeft = 0;

                if (echo)
                    SystemWrite0("{0}{1}", prompt, cmdline);
                else
                    SystemWrite0("{0}", prompt);

                SetCursorLeft(new_x);
                SetCursorTop(new_y);
            }
        }

        public void LockOutput()
        {
            Monitor.Enter(cmdline);
            try
            {
                if (y != -1)
                {
                    y = SetCursorTop(y);
                    CursorLeft = 0;

                    int count = cmdline.Length + prompt.Length;

                    while (count-- > 0)
                        SystemWrite0(" ");

                    y = SetCursorTop(y);
                    CursorLeft = 0;

                }
            }
            catch (Exception)
            {
            }
        }

        public void UnlockOutput()
        {
            if (y != -1)
            {
                y = CursorTop;
                Show();
            }
            Monitor.Exit(cmdline);
        }

        public void Output(string text)
        {
            lock (cmdline)
            {
                if (y == -1)
                {
                    SystemWriteLine0(text);

                    return;
                }

                y = SetCursorTop(y);
                CursorLeft = 0;

                int count = cmdline.Length + prompt.Length;

                while (count-- > 0)
                    SystemWrite0(" ");

                y = SetCursorTop(y);
                CursorLeft = 0;

                SystemWriteLine0(text);

                y = CursorTop;

                Show();
            }
        }

        private bool ContextHelp()
        {
            string[] words = Parser.Parse(cmdline.ToString());

            bool trailingSpace = cmdline.ToString().EndsWith(" ");

            // Allow ? through while typing a URI
            //
            if (words.Length > 0 && words[words.Length - 1].StartsWith("http") && !trailingSpace)
                return false;

            string[] opts = Commands.FindNextOption(words, trailingSpace);

            if (opts[0].StartsWith("Command help:"))
                Output(opts[0]);
            else
                Output(String.Format("Options: {0}", String.Join(" ", opts)));

            return true;
        }

        public void Prompt()
        {
            string line = ReadLine(m_defaultPrompt, true, true);

            if (line != String.Empty)
            {
                m_log.Info("Invalid command");
            }
        }

        public string CmdPrompt(string p)
        {
            return ReadLine(String.Format("{0}: ", p), false, true);
        }

        public string CmdPrompt(string p, string def)
        {
            string ret = ReadLine(String.Format("{0} [{1}]: ", p, def), false, true);
            if (ret == String.Empty)
                ret = def;

            return ret;
        }

        // Displays a command prompt and returns a default value, user may only enter 1 of 2 options
        public string CmdPrompt(string prompt, string defaultresponse, string OptionA, string OptionB)
        {
            bool itisdone = false;
            string temp = CmdPrompt(prompt, defaultresponse);
            while (itisdone == false)
            {
                if ((temp == OptionA) || (temp == OptionB))
                {
                    itisdone = true;
                }
                else
                {
                    SystemWriteLine0("Valid options are " + OptionA + " or " + OptionB);
                    temp = CmdPrompt(prompt, defaultresponse);
                }
            }
            return temp;
        }

        // Displays a prompt and waits for the user to enter a string, then returns that string
        // (Done with no echo and suitable for passwords)
        public string PasswdPrompt(string p)
        {
            return ReadLine(p, false, false);
        }

        public void RunCommand(string cmd)
        {
            string[] parts = Parser.Parse(cmd);
            Commands.Resolve(parts);
        }

        public string ReadLine(string p, bool isCommand, bool e)
        {
            h = 1;
            cp = 0;
            prompt = p;
            echo = e;
            int historyLine = history.Count;

            if (gui)
            {
                SystemWrite0("{0}", prompt);
                string cmdinput = DLRConsole.ReadLine();

                if (isCommand)
                {
                    string[] cmd = Commands.Resolve(Parser.Parse(cmdinput));

                    if (cmd.Length != 0)
                    {
                        int i;

                        for (i = 0; i < cmd.Length; i++)
                        {
                            if (cmd[i].Contains(" "))
                                cmd[i] = "\"" + cmd[i] + "\"";
                        }
                        return String.Empty;
                    }
                }
                return cmdinput;
            }

            CursorLeft = 0; // Needed for mono
            SystemWrite0(" "); // Needed for mono

            lock (cmdline)
            {
                y = CursorTop;
                cmdline.Remove(0, cmdline.Length);
            }

            while (true)
            {
                Show();

                ConsoleKeyInfo key = DLRConsole.ReadKey(true);
                char c = key.KeyChar;

                if (!Char.IsControl(c))
                {
                    if (cp >= 318)
                        continue;

                    if (c == '?' && isCommand)
                    {
                        if (ContextHelp())
                            continue;
                    }

                    cmdline.Insert(cp, c);
                    cp++;
                }
                else
                {
                    switch (key.Key)
                    {
                        case ConsoleKey.Backspace:
                            if (cp == 0)
                                break;
                            cmdline.Remove(cp - 1, 1);
                            cp--;

                            CursorLeft = 0;
                            y = SetCursorTop(y);

                            SystemWrite0("{0}{1} ", prompt, cmdline);

                            break;
                        case ConsoleKey.End:
                            cp = cmdline.Length;
                            break;
                        case ConsoleKey.Home:
                            cp = 0;
                            break;
                        case ConsoleKey.UpArrow:
                            if (historyLine < 1)
                                break;
                            historyLine--;
                            LockOutput();
                            cmdline.Remove(0, cmdline.Length);
                            cmdline.Append(history[historyLine]);
                            cp = cmdline.Length;
                            UnlockOutput();
                            break;
                        case ConsoleKey.DownArrow:
                            if (historyLine >= history.Count)
                                break;
                            historyLine++;
                            LockOutput();
                            if (historyLine == history.Count)
                            {
                                cmdline.Remove(0, cmdline.Length);
                            }
                            else
                            {
                                cmdline.Remove(0, cmdline.Length);
                                cmdline.Append(history[historyLine]);
                            }
                            cp = cmdline.Length;
                            UnlockOutput();
                            break;
                        case ConsoleKey.LeftArrow:
                            if (cp > 0)
                                cp--;
                            break;
                        case ConsoleKey.RightArrow:
                            if (cp < cmdline.Length)
                                cp++;
                            break;
                        case ConsoleKey.Enter:
                            CursorLeft = 0;
                            y = SetCursorTop(y);

                            SystemWriteLine0("{0}{1}", prompt, cmdline);

                            lock (cmdline)
                            {
                                y = -1;
                            }

                            if (isCommand)
                            {
                                string[] cmd = Commands.Resolve(Parser.Parse(cmdline.ToString()));

                                if (cmd.Length != 0)
                                {
                                    int i;

                                    for (i = 0; i < cmd.Length; i++)
                                    {
                                        if (cmd[i].Contains(" "))
                                            cmd[i] = "\"" + cmd[i] + "\"";
                                    }
                                    AddToHistory(String.Join(" ", cmd));
                                    return String.Empty;
                                }
                            }

                            AddToHistory(cmdline.ToString());
                            return cmdline.ToString();
                        default:
                            break;
                    }
                }
            }
        }

        public static void DebugWriteLine(string format, params object[] args)
        {
            format = ShouldPrint(format, args);
            if (format == null) return;
            string prefix;
            string getCallerFormat = GetCallerFormat(format, out prefix);
            format = "[" + prefix + "] " + getCallerFormat;
            SystemWriteLine0(format);
        }
        private static void SystemWriteLine0(string format, params object[] args)
        {
            foreach (TextWriter o in Outputs)
            {
                try
                {
                    if (args == null || args.Length == 0)
                    {
                        o.WriteLine(format);
                    }
                    else
                    {
                        o.WriteLine(format, args);
                    }
                }
                catch (Exception e)
                {
                    System.Console.Error.WriteLine("" + e);
                    o.WriteLine(SafeFormat(format, args));
                }
            }
        }
        public static void SystemWriteLine(string format, params object[] args)
        {
            format = ShouldPrint(format, args);
            if (format == null) return;
            SystemWriteLine0(format);
        }
        public static string GetCallerFormat(string format, out string prefix)
        {
// ReSharper disable ConditionIsAlwaysTrueOrFalse
            if (false) return format;
// ReSharper restore ConditionIsAlwaysTrueOrFalse
            if (!format.StartsWith("["))
            {
                int fc = format.IndexOf(":");
                if (fc>1)
                {
                    prefix = format.Substring(0, fc);
                    if (prefix.Contains(" "))
                    {
                        prefix = CurrentCaller;
                        fc = -1;
                    } else
                    {
                        prefix = prefix.ToUpper();
                    }
                    format = format.Substring(fc + 1);
                    return format;
                } else
                {
                    prefix = CurrentCaller;
                    return format;
                }
            } else
            {
                int fc = format.IndexOf("]");
                if (fc == -1)
                {
                    prefix = CurrentCaller;
                    return format;
                } else
                {
                    prefix = format.Substring(1, fc - 1);
                    return format.Substring(fc + 1);
                }
            }
            return format;
        }
        internal static void SystemWriteLine0(string arg)
        {
            SystemWriteLine0("{0}", arg);
        }
        public static void DebugWriteLine(object arg)
        {
            string prefix;
            string getCallerFormat = GetCallerFormat("" + arg, out prefix);
            getCallerFormat = "[" + prefix + "] " + getCallerFormat;
            SystemWriteLine0(getCallerFormat);
        }
        public static void DebugWrite(string arg)
        {
            SystemWrite0("" + arg);
        }
        public static void SystemWrite(string format, params object[] args)
        {
            SystemWrite0(format, args);
        }
        static public string CurrentCaller
        {
            get
            {
                var st = new System.Diagnostics.StackTrace(true).GetFrames();
                if (st == null) return "NULL";
                {
                    for (int i = 0; i < st.Length; i++)
                    {
                        StackFrame s = st[i];
                        var m = s.GetMethod();
                        if (m != null)
                        {
                            Type caller = m.ReflectedType ?? m.DeclaringType;
                            if (caller == null) continue;
                            lock (TransparentCallers) if (TransparentCallers.Contains(caller)) continue;
                            lock (OpacheCallers) if (OpacheCallers.Contains(caller)) return CallerName(s);
                        }
                        return CallerName(s);
                    }
                }
                return CallerName(st[st.Length - 1]);
            }
        }

        private static string CallerName(StackFrame frame)
        {
            string str = null;
            if (frame == null) return str;            
            var m = frame.GetMethod();
            if (m != null)
            {
                //str =frame.GetMethod().Name;
                //if (str.StartsWith("get_")) str = str.Substring(4);
                //else if (str.StartsWith("set_")) str = str.Substring(4);

                str = frame.GetMethod().DeclaringType.Name;// +"_" + str;

                if (!string.IsNullOrEmpty(str)) return str.ToUpper();
            }
            var mo = frame.GetFileName() +"_"+ frame.GetFileLineNumber();
            return str;
        }

        internal static void SystemWrite0(string format, params object[] args)
        {
            foreach (TextWriter o in Outputs)
            {
                try
                {
                    if (args == null || args.Length == 0)
                    {
                        o.Write(format);
                    }
                    else
                    {
                        o.Write(format, args);
                    }
                }
                catch (Exception e)
                {
                    System.Console.Error.WriteLine("" + e);
                    o.Write(SafeFormat(format, args));
                }
            }
        }

        static readonly HashSet<TextWriter> _outputs = new HashSet<TextWriter>();

        public static readonly HashSet<Type> TransparentCallers = new HashSet<Type>()
                                                                      {
                                                                          typeof (OpenSimAppender),
                                                                          typeof (DLRConsole),
                                                                          typeof (System.Console),
                                                                          typeof (OutputDelegateWriter),
                                                                          typeof (TextFilter),
                                                                      };

        public static readonly HashSet<Type> OpacheCallers = new HashSet<Type>()
                                                              {
                                                              };

        protected static IEnumerable Outputs
        {
            get
            {
                lock (_outputs)
                {
                    if (_outputs.Count == 0)
                    {
                        AddOutput(Console.Out);
                    }
                    return new List<TextWriter>(_outputs);
                }
            }
        }

        public static void AddOutput(TextWriter writer)
        {
            if (writer != ConsoleOut && writer != null)
            {
                lock (_outputs) _outputs.Add(writer);
            }
        }

        public static void RemoveOutput(TextWriter writer)
        {
            if (writer != ConsoleOut && writer != null)
            {
                lock (_outputs) _outputs.Remove(writer);
            }
        }

        public static void SystemWriteLine()
        {
            foreach (TextWriter o in Outputs)
            {
                try
                {
                    o.WriteLine();
                }
                catch (Exception e)
                {
                    System.Console.Error.WriteLine("" + e + " in " + o);
                }
            }
        }
        public static ConsoleKeyInfo ReadKey(bool b)
        {
            return System.Console.ReadKey(b);
        }

        public static ConsoleKeyInfo ReadKey()
        {
            return System.Console.ReadKey();
        }

        public static string ReadLine()
        {
            return System.Console.ReadLine();
        }

        public static void SystemResetColor()
        {
            System.Console.ResetColor();
        }

        public static void ResetColor()
        {
            System.Console.ResetColor();
        }

        public static void SystemFlush()
        {
            Console.Out.Flush();
        }

        public static string SafeFormat(string fmt, params object[] args)
        {
            string str = fmt;
            if (args != null && args.Length > 0)
            {
                try
                {
                    str = string.Format(fmt, args);
                }
                catch (Exception e)
                {
                    System.Console.Error.WriteLine("SafeFormat: " + e);
                    System.Console.Error.WriteLine("f: " + fmt);
                    int ii = 0;
                    foreach (var o in args)
                    {                        
                        ii++;
                        string arg = " " + ii + ": " + o;
                        System.Console.Error.WriteLine(arg);
                        str += arg;
                    }
                }
            }
            return str;
        }
    }
}