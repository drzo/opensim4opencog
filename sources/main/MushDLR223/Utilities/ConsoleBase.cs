// #define USING_log4net
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
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using log4net;
using System.Text.RegularExpressions;
#if USING_log4net
using log4net.Appender;
#endif
using log4net.Core;
using TheConsole = MushDLR223.Utilities.DLRConsole;
using SystemConsole = System.Console;
using MushDLR223.ScriptEngines;
using System.Runtime.InteropServices;

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

    internal static class NativeMethodsKernel32
    {
        [DllImport("kernel32.dll")]
        internal static extern Boolean AllocConsole();        
    }

    public class DLRConsole
#if USING_log4net
        : AnsiColorTerminalAppender
#endif
    {
        private static readonly ILog m_log = LogManager.GetLogger(MethodBase.GetCurrentMethod().DeclaringType);
        public static bool AllocedConsole = false;
        public static bool HasWinforms = false;
        public static bool IsOnMonoUnix = false;
        public static bool SafelyRun(MethodInvoker call)
        {
            return SafelyRun(call, Error);
        }
        public static bool SafelyRun(MethodInvoker call, TextWriter showException)
        {
            try
            {
                call();
                return true;
            }
            catch (Exception e)
            {
                if (showException != null)
                {
                    SafelyRun(() =>
                    {
                        showException.WriteLine("SafelyRun: " + e);
                        showException.Flush();

                    }, null);
                }
                return false;
            }
        }

        public static void DetectMainEnv(TextWriter Console)
        {
            var osv = Environment.OSVersion;
            if (Console != null)
            {
                Console.WriteLine("Current Directory={0}", Environment.CurrentDirectory);      // Current working directory of the program
                Console.WriteLine("CommandLine={0}", Environment.CommandLine);                 // Command line used to execute the program
                Console.WriteLine("MachineName={0}", Environment.MachineName);                 // Name of the current machine
                Console.WriteLine("NewLine={0}", Environment.NewLine);                         // Newline character used by OS, \n for Unix, \n\r for Windows
                Console.WriteLine("Environment.OSVersion = " + osv);
                Console.WriteLine("Environment.OSVersion.Platform = " + osv.Platform);
                Console.WriteLine("Environment.OSVersion.VersionString = " + osv.VersionString);
                Console.WriteLine("Environment.OSVersion.ServicePack = " + osv.ServicePack);
                Console.WriteLine("ProcessorCount={0}", Environment.ProcessorCount);           // Number of CPU's in the machine
                Console.WriteLine("StackTrace={0}", Environment.StackTrace);                   // Prints all functions called in order
                Console.WriteLine("SystemDirectory={0}", Environment.SystemDirectory);         // Returns the "system" directory of the OS, not valid on Unix
                Console.WriteLine("TickCount={0}", Environment.TickCount);                     // Number of milliseconds since the system started
                Console.WriteLine("UserDomainName={0}", Environment.UserDomainName);           // Windows domain, Machine name on Unix
                Console.WriteLine("UserInteractive={0}", Environment.UserInteractive);         //
                Console.WriteLine("UserName={0}", Environment.UserName);                       // Current username
                Console.WriteLine("Version={0}", Environment.Version);                         // C# engine version
                Console.WriteLine("WorkingSet={0}", Environment.WorkingSet);                   // Memory allocated to the process

                // ExpandEnviromentalVariables expands any named variable between %%
                Console.WriteLine("ExpandEnvironentVariables={0}", Environment.ExpandEnvironmentVariables("This system has the following path: %PATH%"));

                String[] arguments = Environment.GetCommandLineArgs();
                Console.WriteLine("CommandLineArgs={0}", String.Join(", ", arguments));

                String[] drives = Environment.GetLogicalDrives();
                Console.WriteLine("GetLogicalDrives: {0}", String.Join(", ", drives));
            }
            IsOnMonoUnix = osv.Platform == PlatformID.Unix;
            HasWinforms = osv.Platform != PlatformID.Unix;
            MakeWindowsOnly("Mono.Security.dll");
            MakeWindowsOnly("XML.dll");
            MakeWindowsOnly("GraphvizDot.dll");
        }

        private static void MakeWindowsOnly(string p)
        {

            if (IsOnMonoUnix)
            {
                MoveIf(p, p + ".WindowsOnly");
            }
            else
            {
                string wo = p + ".WindowsOnly";
                if (File.Exists(p))
                {
                    if (File.Exists(wo))
                    {
                        File.Delete(wo);
                        File.Copy(p, wo);
                    }
                }
                MoveIf(wo, p);
            }
        }

        private static void MoveIf(string src, string dest)
        {
            if (File.Exists(src) && !File.Exists(dest))
                SafelyRun(() =>
                              {
                                  File.Move(src, dest);
                              });
        }

        public static bool AllocConsole()
        {
            DetectMainEnv(null);
            if (!AllocedConsole)
            {
                var ConsoleError = Console.Error;

                AllocedConsole = true;
                try
                {
                    NativeMethodsKernel32.AllocConsole();
                    //write nothing to rtest for error
                    ConsoleError.WriteLine("");
                    ConsoleError.Flush();
                    Console.Out.WriteLine("");
                    Console.Out.Flush();
                }
                catch (Exception e)
                {
                    AllocedConsole = false;
                    ConsoleError.WriteLine("AllocedConsole: " + e);
                    ConsoleError.Flush();
                }
            }
            return AllocedConsole;
        }

        public static TextFilter TheGlobalLogFilter = new TextFilter()
                                                          {
                                                              "clear",
                                                              "+*",
                                                          };

        static private readonly object m_syncRoot = new object();
        public static bool PrintToSystemConsole = true;
        static private int y = -1;
        private int cp = 0;
        private int h = 1;
        private string prompt = "# ";
        static private StringBuilder cmdline = new StringBuilder();
        private static readonly TextWriter InitialConsoleOut = SystemConsole.Out;
        private static readonly TextWriter InitialConsoleERR = SystemConsole.Error;
        public static readonly OutputDelegate SYSTEM_ERR_WRITELINE_REAL = InitialConsoleERR.WriteLine;
        public static readonly OutputDelegate SYSTEM_ERR_WRITELINE = CALL_SYSTEM_ERR_WRITELINE;
        private static readonly TextWriter ConsoleOut = new OutputDelegateWriter(SystemWriteLine);
        private static readonly TextWriter ConsoleError = new OutputDelegateWriter(SYSTEM_ERR_WRITELINE);

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

        static DLRConsole()
        {
            //Application.VisualStyleState
            var v0 = InitialConsoleOut;
            AddOutput(v0);
            ///SystemConsole.SetOut(ConsoleOut);
        }


        public static ConsoleColor ForegroundColor
        {
            get { return SystemConsole.ForegroundColor; }
            set { try { SystemConsole.ForegroundColor = value; } catch { } }
        }

        public static int BufferHeight
        {
            get { return SystemConsole.BufferHeight; }
            set { SystemConsole.BufferHeight = value; }
        }

        public static int BufferWidth
        {
            get { return SystemConsole.BufferWidth; }
            set { SystemConsole.BufferWidth = value; }
        }
        public static int CursorTop
        {
            get { return SystemConsole.CursorTop; }
            set { SystemConsole.CursorTop = value; }
        }
        public static int CursorLeft
        {
            get { return SystemConsole.CursorLeft; }
            set { SystemConsole.CursorLeft = value; }
        }

        public static TextWriter Out
        {
            get
            {
                TextWriter ret = SystemConsole.Out ?? ConsoleOut ?? InitialConsoleOut ?? InitialConsoleERR;
                return ret;
            }
        }
        public static TextWriter Error
        {
            get
            {
                TextWriter ret = SystemConsole.Error ?? ConsoleError ?? InitialConsoleERR ?? InitialConsoleOut;
                return ret;
            }
        }

        public static TextReader In
        {
            get { return SystemConsole.In; }
        }

        public static void SetIn(TextReader reader)
        {
            SystemConsole.SetIn(reader);
        }

        public static void SetOut(TextWriter writer)
        {
            //var old = Console.Out;
            //RemoveOutput(old);
            AddOutput(writer);
            SystemConsole.SetOut(ConsoleOut);
        }

        public static void SetError(TextWriter writer)
        {
            //var old = Console.Out;
            //RemoveOutput(old);
            //AddOutput(writer);
            SystemConsole.SetError(ConsoleOut);
        }

        private static readonly object m_LogLock = new object();
        public static event ConsoleCancelEventHandler CancelKeyPress
        {
            add { lock (m_LogLock) { SystemConsole.CancelKeyPress += value; } }
            remove { lock (m_LogLock) { SystemConsole.CancelKeyPress -= value; } }
        }

        public static bool KeyAvailable
        {
            get { return SystemConsole.KeyAvailable; }
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

        private DLRConsole m_console = null;

        public DLRConsole MConsole
        {
            get { return m_console; }
            set { m_console = value; }
        }

        /// <summary>
        /// derive an ansi color from a string, ignoring the darker colors.
        /// This is used to help automatically bin component tags with colors
        /// in various print functions.
        /// </summary>
        /// <param name="input">arbitrary string for input</param>
        /// <returns>an ansii color</returns>        
        private static readonly ConsoleColor[] Colors = {
            ConsoleColor.Gray,
            // the dark colors don't seem to be visible on some black background terminals like putty :(
            //ConsoleColor.DarkBlue,
            //ConsoleColor.DarkGreen,
            //ConsoleColor.DarkCyan,
            //ConsoleColor.DarkMagenta,
            //ConsoleColor.DarkGray,
            ConsoleColor.Blue,
            ConsoleColor.DarkRed,
            ConsoleColor.Green,
            ConsoleColor.Red,
            ConsoleColor.Cyan,
            ConsoleColor.Yellow,
            ConsoleColor.Magenta,
            ConsoleColor.DarkYellow
        };

#if USING_log4net
        override
#endif
        protected void Append(LoggingEvent le)
        {
            if (m_console != null)
                m_console.LockOutput();

            try
            {


#if USING_log4net
                string loggingMessage = RenderLoggingEvent(le);
#else
                string loggingMessage = le.ToString();
#endif
                string regex = @"^(?<Front>.*?)\[(?<Category>[^\]]+)\]:?(?<End>.*)";

                Regex RE = new Regex(regex, RegexOptions.Multiline);
                MatchCollection matches = RE.Matches(loggingMessage);

                // Get some direct matches $1 $4 is a
                if (matches.Count == 1)
                {
                    SystemWrite(matches[0].Groups["Front"].Value);
                    SystemWrite("[");

                    WriteColorText(DeriveColor(matches[0].Groups["Category"].Value), matches[0].Groups["Category"].Value);
                    SystemWrite("]:");

                    if (le.Level == Level.Error)
                    {
                        WriteColorText(ConsoleColor.Red, matches[0].Groups["End"].Value);
                    }
                    else if (le.Level == Level.Warn)
                    {
                        WriteColorText(ConsoleColor.Yellow, matches[0].Groups["End"].Value);
                    }
                    else
                    {
                        SystemWrite(matches[0].Groups["End"].Value);
                    }
                    SystemWriteLine();
                }
                else
                {
                    SystemWrite(loggingMessage);
                }
            }
            catch (Exception e)
            {
                TheConsole.DebugWriteLine("Couldn't write out log message: {0}", e.ToString());
            }
            finally
            {
                if (m_console != null)
                    m_console.UnlockOutput();
            }
        }

        private static int ColorIndex = 0;
        readonly static Dictionary<string, ConsoleColor> Name2Color = new Dictionary<string, ConsoleColor>();
        public static ConsoleColor DeriveColor(string input)
        {
            input = input.ToUpper();
            lock (Name2Color)
            {
                if (Name2Color.Count == 0)
                {
                    // switch from Gray to White if needbe
                    if (Colors[0] == ForegroundColor)
                    {
                        Colors[0] = ConsoleColor.White;
                    }
                }
                ConsoleColor color;
                if (!Name2Color.TryGetValue(input, out color))
                {
                    ColorIndex++;
                    if (ColorIndex >= Colors.Length) ColorIndex = 0;
                    color = Name2Color[input] = Colors[ColorIndex];
                    // Console.Out.WriteLine("ConsoleColor is " + color + " for " + input);
                }
                return color;
            }
            // it is important to do Abs, hash values can be negative
            return Colors[(Math.Abs(input.ToUpper().GetHashCode()) % Colors.Length)];
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
        public void WError(string format, params object[] args)
        {
            WriteNewLine(ConsoleColor.Red, format, args);
        }

        /// <summary>
        /// Sends an error to the current console output
        /// </summary>
        /// <param name="sender">The module that sent this message</param>
        /// <param name="format">The message to send</param>
        /// <param name="args">WriteLine-style message arguments</param>
        public void WError(string sender, string format, params object[] args)
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
            lock (cmdline) lock (m_syncRoot)
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
                    // dont trim off spaces
                    char[] trim = "\n\r".ToCharArray();
                    string safeFormat = SafeFormat(format, args);
                    safeFormat = OmitPrefix(sender, safeFormat);
                    string[] safeFormatSplit = safeFormat.Split(new[] { "\r\n", "\n", "\r" }, StringSplitOptions.None);
                    foreach (var argsFmt in safeFormatSplit)
                    {
                        WriteEachLine(senderColor, sender, color, argsFmt.Trim(trim));
                        if (y != -1)
                            y = CursorTop;
                    }
                }
        }

        static public string OmitPrefix(string sender, string trimmed)
        {
            string tt = trimmed.Trim().ToUpper();
            int spaced = trimmed.Length - tt.Length;

            string omittedPrefix = sender.ToUpper() + ":";
            if (tt.StartsWith(omittedPrefix))
            {
                trimmed = trimmed.Substring(spaced + omittedPrefix.Length);//.TrimStart();
                return trimmed;
            }
            omittedPrefix = "[" + sender.ToUpper() + "]";
            if (tt.StartsWith(omittedPrefix))
            {
                trimmed = trimmed.Substring(0, spaced) +
                          trimmed.Substring(spaced + omittedPrefix.Length + 1).TrimStart();
                return trimmed;
            }
            return trimmed;
        }

        static void WriteEachLine(ConsoleColor senderColor, string sender, ConsoleColor color, string trimmed)
        {
            if (y != -1)
                y = CursorTop;
            WritePrefixLine(senderColor, sender);
            if (IsOnMonoUnix) SystemConsole.WriteLine(trimmed);
            else WriteConsoleLine(color, "{0}", trimmed);
            omittedPrefix = "";
            if (y != -1)
                y = CursorTop;
        }

        static public void WriteNewLine(ConsoleColor color, string format, params object[] args)
        {
            lock (cmdline) lock (m_syncRoot)
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
                    format = SafeFormat(format, args);
                    try
                    {
                        if (color != ForegroundColor)  // ConsoleColor.White
                            ForegroundColor = color;
                        SystemWriteLine0(format);
                        ResetColor();
                    }
                    catch (ArgumentNullException)
                    {
                        // Some older systems dont support coloured text.
                        SystemWriteLine0(format);
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

        private static string omittedPrefix = "";
        static private void WritePrefixLine(ConsoleColor color, string sender)
        {
            try
            {
                lock (m_syncRoot)
                {
                    sender = sender.ToUpper();

                    // SystemWriteLine("[" + sender + "] ");

                    SystemWrite0("[");

                    WriteColorText(color, sender);
                    PauseIfTraced("WritePrefixLine: " + sender);
                    SystemWrite0("] \t");
                }
            }
            catch (ObjectDisposedException)
            {
            }
        }

        public static void WriteColorText(ConsoleColor color, string text)
        {
            lock (m_syncRoot)
            {
                try
                {
                    ForegroundColor = color;
                    SystemWrite0(text);
                    ResetColor();
                }
                catch (ArgumentNullException)
                {
                    // Some older systems dont support coloured text.
                    SystemWrite0(" (ANE) " + text);
                }
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
                string cmdinput = TheConsole.ReadLine();

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

                ConsoleKeyInfo key = TheConsole.ReadKey(true);
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
            string printStr = TheConsole.SafeFormat(format, args);
            if (!TheGlobalLogFilter.ShouldPrint(printStr))
            {
                CALL_SYSTEM_ERR_WRITELINE("!shouldprint- " + printStr);
                return;
            }
            if (printStr == null) return;
            string sender;
            string getCallerFormat = GetCallerFormat(printStr, out sender);
            WriteNewLine(DeriveColor(sender), sender, ConsoleColor.Gray, "{0}", printStr);
            return;
        }

        private static void CALL_SYSTEM_ERR_WRITELINE(string format, params object[] args)
        {
            ExecWithMaxTime(() => CALL_SYSTEM_ERR_WRITELINE_REAL(format, args), 1000);
        }

        private static Thread MainThread = Thread.CurrentThread;
        private static void ExecWithMaxTime(Action action, int i)
        {
            AutoResetEvent are = new AutoResetEvent(false);
            Thread orig = Thread.CurrentThread;
            Thread doIt = new Thread(() =>
                                         {
                                             try
                                             {
                                                 action();
                                                 are.Set();
                                             }
                                             catch (Exception abouirt)
                                             {
                                                 return;
                                             }
                                         });
            try
            {
                doIt.Start();
                if (are.WaitOne(i))
                {
                    doIt.Join();
                    return;
                }
                if (orig != MainThread)
                {
                    doIt.Abort();
                }
                else
                {
                    doIt.Abort();
                }
            }
            finally
            {
            }

        }

        private static void CALL_SYSTEM_ERR_WRITELINE_REAL(string format, params object[] args)
        {
            SystemFlush();
            format = SafeFormat(format, args);
            //%%%PauseIfTraced(format);

            //if (IsOnMonoUnix) WriteColorText(ConsoleColor.White, "CALL_SYSTEM_ERR_WRITELINE-001 " + format);
            SYSTEM_ERR_WRITELINE_REAL(format);
            SystemFlush();
        }

        private static void SystemWriteLine0(string format, params object[] args)
        {
            var Outputs = DLRConsole.Outputs;
            if (Outputs.Count == 0)
            {
                var sformat = SafeFormat(format, args);
                if (String.IsNullOrEmpty(sformat)) return;
                SystemWriteLine00(sformat.Trim());
                return;
            }

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
                    CALL_SYSTEM_ERR_WRITELINE("" + e);
                    o.WriteLine(SafeFormat(format, args));
                }
            }
        }
        private static void SystemWriteLine00(string format)
        {
            if (IsOnMonoUnix) CALL_SYSTEM_ERR_WRITELINE("SystemWriteLine00: " + format);
            format = GetFormat(format).TrimEnd();
            PauseIfTraced(format);
            foreach (TextWriter o in Outputs)
            {
                try
                {
                    o.WriteLine(format);
                }
                catch (Exception e)
                {
                    CALL_SYSTEM_ERR_WRITELINE("\n" + e + "\n while writeline-ing '" + format + "'");
                }
            }
        }

        public static void SystemWriteLine(string format, params object[] args)
        {
            format = SafeFormat(format, args);
            if (!TheGlobalLogFilter.ShouldPrint(format))
            {
                CALL_SYSTEM_ERR_WRITELINE(format);
            }
            if (String.IsNullOrEmpty(format)) return;
            SystemWriteLine00(format);
        }
        public static string GetCallerFormat(string format, out string prefix)
        {
            if (!format.StartsWith("["))
            {
                int fc = format.IndexOf(":");
                if (fc > 1)
                {
                    prefix = format.Substring(0, fc);
                    if (prefix.Contains(" "))
                    {
                        prefix = CurrentCaller;
                        fc = -1;
                    }
                    else
                    {
                        prefix = prefix.ToUpper();
                    }
                    format = format.Substring(fc + 1);
                    return format;
                }
                else
                {
                    prefix = CurrentCaller;
                    return format;
                }
            }
            else
            {
                int fc = format.IndexOf("]");
                if (fc == -1)
                {
                    prefix = CurrentCaller;
                    return format;
                }
                else
                {
                    prefix = format.Substring(1, fc - 1);
                    return format.Substring(fc + 1);
                }
            }
            return format;
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
            get { return FindCallerInStack(TransparentCallers, OpacheCallers, false); }
        }

        public static string FindCallerInStack(HashSet<MemberInfo> transparentCallers, HashSet<MemberInfo> opacheCallers, bool useMethodName)
        {
            if (SkipStackTraces) return "FindCallerInStack";
            var st = new System.Diagnostics.StackTrace(true).GetFrames();
            if (st == null) return "NULL";
            {
                for (int i = 0; i < st.Length; i++)
                {
                    StackFrame s = st[i];
                    var m = s.GetMethod();
                    if (m != null)
                    {
                        MemberInfo caller = m.ReflectedType ?? m.DeclaringType;
                        if (caller == null) continue;
                        caller = ResolveType(m);

                        if (transparentCallers == null)
                            transparentCallers = TransparentCallers;
                        lock (transparentCallers) if (transparentCallers.Contains(caller)) continue;
                        if (opacheCallers != null)
                            lock (opacheCallers) if (opacheCallers.Contains(caller))
                                    return CallerName(s, useMethodName);
                    }
                    return CallerName(s, useMethodName);
                }
            }
            return CallerName(st[st.Length - 1], useMethodName);
        }

        private static string CallerName(StackFrame frame, bool useMethodName)
        {
            string str = null;
            if (frame == null) return str;
            var m = frame.GetMethod();
            string suffix = "";
            if (m != null)
            {
                MemberInfo type = m; // +"_" + str;                   
                type = ResolveType(type);
                str = type.Name;
                if (type is Type)
                {
                    if (TransparentCallers.Contains(type))
                    {
                        //todo decide below to comment
                        useMethodName = true;
                        str = "";
                    }
                }
                if (useMethodName)
                {
                    suffix = m.Name;
                    if (suffix.StartsWith("get_") || suffix.StartsWith("set_")) suffix = suffix.Substring(4);
                    suffix = "." + suffix;
                }
                if (!string.IsNullOrEmpty(str)) return str.ToUpper() + suffix;
            }
            //var mo = frame.GetFileName() + "_" + frame.GetFileLineNumber() + suffix;
            return str + suffix;
        }

        private static MemberInfo ResolveType(MemberInfo type)
        {
            while (type.DeclaringType != null && type.DeclaringType != type)
            {
                type = type.DeclaringType;
            }
            while (type.ReflectedType != null && type.ReflectedType != type)
            {
                type = type.ReflectedType;
            }
            return type;
        }

        internal static void SystemWrite0(string format, params object[] args)
        {
            format = SafeFormat(format, args);
            SystemWrite00(format);
        }

        internal static void SystemWrite00(string format)
        {
            ExecWithMaxTime(() => SystemWrite000(format), 2000);            
        }

        internal static void SystemWrite000(string format)
        {
            format = GetFormat(format);
            PauseIfTraced("SystemWrite00- " + format); // keep
            foreach (TextWriter o in Outputs)
            {
                try
                {
                    o.Write(format);
                }
                catch (Exception e)
                {
                    CALL_SYSTEM_ERR_WRITELINE("\n" + e + "\n while writing '" + format + "'");
                }
            }
        }

        static bool lastWasTraced = false;
        private static void PauseIfTraced(string format)
        {
            return;
            bool wasTraced = format.ToUpper().Contains("VERIFYASSEMBLYLOADABLE");
            if (wasTraced)
            {
                SystemConsole.Out.WriteLine();
                SystemConsole.Out.Flush();
                SystemConsole.Error.Write("--s--------" + format + "|: ");
                SystemConsole.Error.Flush();
                string s = null;
                if (!lastWasTraced)
                {
                    s = SystemConsole.In.ReadLine();
                    if (!string.IsNullOrEmpty(s))
                    {
                        var ex = new Exception(s);
                        CALL_SYSTEM_ERR_WRITELINE(" " + s + " " + ex.StackTrace);
                    }
                }
            }
            SystemFlush();
            lastWasTraced = wasTraced;
        }


        private static string GetFormat(string format)
        {
            if (string.IsNullOrEmpty(omittedPrefix)) return format;
            string fupper = format.ToUpper();
            if (fupper.StartsWith(omittedPrefix))
            {
                return format.Substring(omittedPrefix.Length).TrimStart();
            }
            if (fupper.TrimStart().StartsWith(omittedPrefix.TrimStart()))
            {
                int getFrom = fupper.IndexOf(omittedPrefix);
                if (getFrom > 0)
                {
                    getFrom += omittedPrefix.Length;
                    return format.Substring(getFrom).TrimStart();
                }
            }
            return format;
        }

        static readonly HashSet<TextWriter> _outputs = new HashSet<TextWriter>();

        public static readonly HashSet<MemberInfo> TransparentCallers = new HashSet<MemberInfo>()
                                                                      {
                                                                          typeof (OpenSimAppender),
                                                                          typeof (DLRConsole),
                                                                          typeof (SystemConsole),
                                                                          typeof (OutputDelegateWriter),
                                                                          typeof (TextFilter),
                                                                          typeof (TaskQueueHandler),
                                                                      };

        public static readonly HashSet<MemberInfo> OpacheCallers = new HashSet<MemberInfo>()
                                                              {
                                                              };

        public static bool SkipStackTraces = true;

        protected static List<TextWriter> Outputs
        {
            get
            {
                var list = new List<TextWriter>();
                lock (_outputs)
                {
                    if (_outputs.Count == 0)
                    {
                        var use = InitialConsoleOut;
                        if (use != null) list.Add(use);
                    }
                    else
                    {
                        list.AddRange(_outputs);
                    }
                }
                return list;
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
                    CALL_SYSTEM_ERR_WRITELINE("" + e + " in " + o);
                }
            }
        }
        public static ConsoleKeyInfo ReadKey(bool b)
        {
            return SystemConsole.ReadKey(b);
        }

        public static ConsoleKeyInfo ReadKey()
        {
            return SystemConsole.ReadKey();
        }

        public static string ReadLine()
        {
            return SystemConsole.ReadLine();
        }

        public static void SystemResetColor()
        {
            SystemConsole.ResetColor();
        }

        public static void ResetColor()
        {
            SystemConsole.ResetColor();
        }

        public static void SystemFlush()
        {
            ExecWithMaxTime(() =>
                                {
                                    SystemConsole.Error.Flush();
                                    SystemConsole.Out.Flush();
                                }, 1000);
        }

        public static string SafeFormat(string fmt, params object[] args)
        {
            if (fmt == null)
            {
                return ExplainFormatError(fmt, args, SYSTEM_ERR_WRITELINE, new Exception());
            }

            string str = fmt;
            if (args != null && args.Length > 0)
            {
                if (args[0] is object[])
                {
                    args = (object[]) args[0];
                }
                try
                {
                    str = string.Format(fmt, args);
                }
                catch (Exception e)
                {
                    str = ExplainFormatError(fmt, args, SYSTEM_ERR_WRITELINE, e);
                }
            }
            return str ?? "<!--NULL-->";
        }
        private static string ExplainFormatError(string fmt, object[] args, OutputDelegate del, Exception exception)
        {
            del = del ?? TextFilter.DEVNULL;
            var str = new StringBuilder();
            str.AppendFormat(" SafeFormat: '{0}' ", exception);
            str.AppendFormat("f: {0} ", fmt ?? "FRMT-NULL");
            if (args == null)
            {
                str.AppendLine(" args = ARGS-NULL ");
                string s = str.ToString();
                del(s);
                return s;
            }
            else
            {
                string s = str.ToString();
                del(s);
                int ii = 0;
                foreach (var o in args)
                {
                    ii++;
                    string arg = string.Format(" arg{0}='{1}'", ii, o ?? "NULL");

                    del(arg);
                    str.AppendLine(arg);
                }
            }
            return str.ToString();
        }

        public static bool TooDeep()
        {
            return IsTooDeep();
        }
        public static bool IsTooDeep()
        {
            if (DLRConsole.SkipStackTraces) return false;
            StackFrame[] st = new StackTrace(false).GetFrames();
            int newStackTraceGetFramesLength = st == null ? 501 : st.Length;
            if (newStackTraceGetFramesLength > 300)
            {
                if (newStackTraceGetFramesLength > 500)
                {
                    DebugWriteLine("Stack overflow comming!? " + st);
                    return true;
                }
                ///DebugWriteLine("DepthCheck");
                //throw new Exception("Stack overflow comming!");
                return true;
            }
            return false;
        }

        public static void InvokeControl(Component rtb, MethodInvoker invoke)
        {
            invoke();
        }
        public static void InvokeControl(Control rtb, MethodInvoker invoke)
        {

            bool isReady = true;
            lock (rtb) isReady = rtb.IsHandleCreated;
            if (isReady)
            {
                InvokeControlAfterCreated(rtb, invoke);
                return;
            }
            // on uunix try this workarraund
            if (InvokeWithNoErrors(() => InvokeControlAfterCreated(rtb, invoke))) return;
            Console.WriteLine("WARN: InvokeControl on " + rtb + " before IsHandleCreated from " + invoke.Method.ReflectedType + ":" + invoke.Method);
            // make the handler object
            var once = new OnEventOnce(() => InvokeControlAfterCreated(rtb, invoke));
            // add the post invoke call
            once.AfterInvoke = () => { lock (rtb) rtb.HandleCreated -= once.OnEvent; };
            // add the OnHandleCreated
            lock (rtb) rtb.HandleCreated += once.OnEvent;
        }

        public class OnEventOnce
        {
            MethodInvoker invoker;
            public MethodInvoker AfterInvoke;
            public OnEventOnce(MethodInvoker invoke)
            {
                invoker = invoke;
            }
            public void OnEvent(object sender, EventArgs e)
            {
                invoker();
            }

        }
        public static bool InvokeControlAfterCreated(Control rtb, MethodInvoker invoke)
        {

            if (rtb.InvokeRequired)
            {
                rtb.Invoke(invoke);
                return true;
            }
            else
            {
                invoke();
                return false;
            }
        }
        public static bool InvokeWithNoErrors(MethodInvoker invoke)
        {
            try
            {
                invoke();
                return true;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
               /// Logger.Log("Failure in event handler: " + ex.Message, Helpers.LogLevel.Warning, ex);
                return false;
            }
        }

        public static string NoFormatDirectives(string prefix)
        {
            return prefix.Replace("{", "(").Replace("}", ")");

        }
    }
}
