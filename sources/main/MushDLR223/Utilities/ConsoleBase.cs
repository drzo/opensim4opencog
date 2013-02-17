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

#if (COGBOT_LIBOMV || USE_STHREADS || true)
using ThreadPoolUtil;
using Thread = System.Threading.Thread;
using ThreadPool = ThreadPoolUtil.ThreadPool;
using Monitor = ThreadPoolUtil.Monitor;
#endif
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
        public static bool NoConsoleVisible = false;
        public static HashSet<string> IgnoredSenders = new HashSet<string>();
        [ConfigSetting(Description="if false, Print method name with error messages. Printing them is expensive.")]
        public static bool SkipStackTraces = false;
        private static readonly object[] NOARGS = new object[0];
        //public static bool PrintToSystemConsole = true;
        public static DLRConsole SingleInstance = new DLRConsole();
        public static bool HasWinforms = false;
        public static bool IsOnMonoUnix = true;
        public static bool AlwaysWrite = false;
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
        static DLRConsole()
        {
            SafelyRun(() => DetectMainEnv(null));
            if (!IsOnMonoUnix)OverrideConsoleOutput();
        }

        private static void OverrideConsoleOutput()
        {
            var co = SystemConsole.Out;
            if (co is OutputDelegateWriter)
            {
                return;
            }
            SystemConsole.SetOut(new OutputDelegateWriter(DebugWriteLine));
        }

        private static bool detectedMainEnv = false;
        public static void DetectMainEnv(TextWriter Console)
        {
            if (detectedMainEnv) return;
            detectedMainEnv = true;
            var osv = Environment.OSVersion;
            Console = Console ?? InitialConsoleOut ?? InitialConsoleERR;
            if (Console != null)
            {
                Console.WriteLine("Current Directory={0}", SafeCall(() => Environment.CurrentDirectory));      // Current working directory of the program
                Console.WriteLine("CommandLine={0}", SafeCall(() => Environment.CommandLine));                 // Command line used to execute the program
                Console.WriteLine("MachineName={0}", SafeCall(() => Environment.MachineName));                 // Name of the current machine
                Console.WriteLine("NewLine={0}", SafeCall(() => Environment.NewLine));                         // Newline character used by OS, \n for Unix, \n\r for Windows
                if (osv != null)
                {
                    Console.WriteLine("Environment.OSVersion = " + osv);
                    Console.WriteLine("Environment.OSVersion.Platform = " + osv.Platform);
                    Console.WriteLine("Environment.OSVersion.VersionString = " + osv.VersionString);
                    Console.WriteLine("Environment.OSVersion.ServicePack = " + osv.ServicePack);
                }
                Console.WriteLine("ProcessorCount={0}", SafeCall(() => Environment.ProcessorCount));           // Number of CPU's in the machine
                //Console.WriteLine("StackTrace={0}", SafeCall(() => Environment.StackTrace));                   // Prints all functions called in order
                Console.WriteLine("SystemDirectory={0}", SafeCall(() => Environment.SystemDirectory));         // Returns the "system" directory of the OS, not valid on Unix
                Console.WriteLine("TickCount={0}", SafeCall(() => Environment.TickCount));                     // Number of milliseconds since the system started
                Console.WriteLine("UserDomainName={0}", SafeCall(() => Environment.UserDomainName));           // Windows domain, Machine name on Unix
                Console.WriteLine("UserInteractive={0}", SafeCall(() => Environment.UserInteractive));         //
                Console.WriteLine("UserName={0}", SafeCall(() => Environment.UserName));                       // Current username
                Console.WriteLine("Version={0}", SafeCall(() => Environment.Version));                         // C# engine version
                Console.WriteLine("WorkingSet={0}", SafeCall(() => Environment.WorkingSet));                   // Memory allocated to the process

                // ExpandEnviromentalVariables expands any named variable between %%
                Console.WriteLine("ExpandEnvironentVariables={0}", SafeCall(() => Environment.ExpandEnvironmentVariables("This system has the following path: %PATH%")));
                Console.WriteLine("CommandLineArgs={0}", SafeCall(() => String.Join(", ", Environment.GetCommandLineArgs())));
                Console.WriteLine("GetLogicalDrives: {0}", SafeCall(() => String.Join(", ", Environment.GetLogicalDrives())));
            }
            if (osv != null)
            {
                IsOnMonoUnix = osv.Platform == PlatformID.Unix;
                HasWinforms = osv.Platform != PlatformID.Unix;
            }
            MakeWindowsOnly("Mono.Security.dll");
            MakeWindowsOnly("XML.dll");
            MakeWindowsOnly("GraphvizDot.dll");
        }

        private static object SafeCall<T>(Func<T> func)
        {
            try
            {
                return func();
            }
            catch (Exception e)
            {
                return "" + e;
            }
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
            SafelyRun(() => DetectMainEnv(null));
            if (!AllocedConsole)
            {
                var ConsoleError = Console.Error;
                if (NoConsoleVisible) return false;
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
        static private int m_cursorYPosition = -1;
        //private int cp = 0;
        private int h = 1;
        private string prompt = "# ";
        static private StringBuilder cmdline = new StringBuilder();
        public static readonly TextWriter InitialConsoleOut = SystemConsole.Out;
        public static readonly TextWriter InitialConsoleERR = SystemConsole.Error;
        public static readonly OutputDelegate SYSTEM_ERR_WRITELINE_REAL = CALL_SYSTEM_ERR_WRITELINE;
        public static readonly OutputDelegate SYSTEM_ERR_WRITELINE = CALL_SYSTEM_ERR_WRITELINE;
        public static readonly TextWriter ConsoleOut = new OutputDelegateWriter(DebugWriteLine);
        public static readonly TextWriter ConsoleError = new OutputDelegateWriter(SYSTEM_ERR_WRITELINE);

        public Commands Commands = new Commands();
        ///private bool echo = true;
        ///static private List<string> history = new List<string>();
        private bool gui = false;

        public object ConsoleScene = null;

        /// <summary>
        /// The default prompt text.
        /// </summary>
        public string DefaultPrompt
        {
            set { m_defaultPrompt = value; }
            get { return m_defaultPrompt; }
        }
        protected string m_defaultPrompt;
        public DLRConsole()
        {
            SingleInstance = this;
            IsOnMonoUnix = Type.GetType("Mono.Runtime") != null;
            //Application.VisualStyleState
            //var v0 = InitialConsoleOut;
            //AddOutput(v0);
           // SystemConsole.SetOut(NULL_OUTPUT);
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
                TextWriter first = null;
                TextWriter ret = /*SystemConsole.Out ??*/ ConsoleOut ?? InitialConsoleOut ?? InitialConsoleERR;
                return ret;
            }
        }
        public static TextWriter Error
        {
            get
            {
                TextWriter ret = /*SystemConsole.Error ??*/ ConsoleError ?? InitialConsoleERR ?? InitialConsoleOut;
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
            while (m_history.Count >= 100)
                m_history.RemoveAt(0);

            m_history.Add(text);
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
            bool mustPrint = false;
            if (ContainsSubscribedBreakpointWords(format))
            {
                mustPrint = true;
            }
            WriteNewLinePhysical(senderColor, sender, color, format, args, mustPrint);
        }

        static public void WriteNewLinePhysical(ConsoleColor senderColor, string sender, ConsoleColor color, string format, object[] args, bool mustPrint)
        {
            if (NoConsoleVisible) return;
            if (AvoidPrefix(sender))
            {
                var c = CurrentCallerTrace;
                if (c != sender)
                {
                    if (!AvoidPrefix(c))
                    {
                        format = sender + " " + format;
                        sender = c;
                    }
                }
                format = sender + ": " + format;
            }
            if (IgnoreSender(sender))
            {
                return;
            }
            lock (cmdline) lock (m_syncRoot)
                {
                    if (m_cursorYPosition != -1)
                    {
                        m_cursorYPosition = SetCursorTop(m_cursorYPosition);
                        CursorLeft = 0;

                        int count = cmdline.Length;

                        SystemWrite0("  ");
                        while (count-- > 0)
                            SystemWrite0(" ");

                        m_cursorYPosition = SetCursorTop(m_cursorYPosition);
                        CursorLeft = 0;
                    }
                    // dont trim off spaces
                    char[] trim = "\n\r".ToCharArray();
                    string safeFormat = SafeFormat(format, args);
                    safeFormat = OmitPrefix(sender, safeFormat);
                    string[] safeFormatSplit = safeFormat.Split(new[] { "\r\n", "\n", "\r" }, StringSplitOptions.None);
                    foreach (var argsFmt in safeFormatSplit)
                    {
                        string fmt = argsFmt;
                        ExecWithMaxTime(() =>
                                            {
                                                WriteNewLine_Helper(senderColor, sender, color, fmt.Trim(trim));
                                                if (m_cursorYPosition != -1)
                                                    m_cursorYPosition = CursorTop;
                                            }, 2000);
                    }
                }
        }

        public static bool IsCallerNoDebug
        {
            get
            {
                string ctn = CurrentCallerTrace;
                if (IgnoreSender(ctn))
                {
                    return true;
                }
                return false;
            }
        }

        public static bool IgnoreSender(string sender)
        {
            if (DebugLevel > 8) return false;
            lock (IgnoredSenders) return (IgnoredSenders.Contains(sender));
        }
        public static void SetIgnoreSender(string sender, bool tf)
        {
            lock (IgnoredSenders)
            {
                if (sender == null)
                {
                    if (tf == false)
                    {
                        IgnoredSenders.Clear();
                    }
                    return;
                }
                sender = sender.ToUpper();
                if (tf)
                {
                    IgnoredSenders.Add(sender);
                }
                else
                {
                    IgnoredSenders.Remove(sender);
                }
            }
        }

        static public string OmitPrefix(string sender, string trimmed)
        {
            string tt = trimmed.TrimStart().ToUpper();
            int spaced = trimmed.Length - tt.Length;

            string omittedPrefix = sender.ToUpper() + ":";
            if (tt.StartsWith(omittedPrefix))
            {
                string before = trimmed;
                trimmed = trimmed.Substring(0, spaced) +
                          trimmed.Substring(spaced + omittedPrefix.Length).TrimStart();
                return trimmed;
            }
            omittedPrefix = "[" + sender.ToUpper() + "]";
            if (tt.StartsWith(omittedPrefix))
            {
                string before = trimmed;
                trimmed = trimmed.Substring(0, spaced) +
                          trimmed.Substring(spaced + omittedPrefix.Length).TrimStart();
                return trimmed;
            }
            return trimmed;
        }

        static void WriteNewLine_Helper(ConsoleColor senderColor, string sender, ConsoleColor color, string trimmed)
        {
            if (IgnoreSender(sender)) return;
            if (m_cursorYPosition != -1)
                m_cursorYPosition = CursorTop;
            WritePrefixLine(senderColor, sender);
            WriteConsoleLine(color, " {0}", trimmed);
            omittedPrefix = "";
            if (m_cursorYPosition != -1)
                m_cursorYPosition = CursorTop;
        }

        static public void WriteNewLine(ConsoleColor color, string format, params object[] args)
        {
            if (NoConsoleVisible) return;
            lock (cmdline) lock (m_syncRoot)
                {
                    if (m_cursorYPosition != -1)
                    {
                        m_cursorYPosition = SetCursorTop(m_cursorYPosition);
                        CursorLeft = 0;

                        int count = cmdline.Length;

                        SystemWrite0("wnl  ");
                        while (count-- > 0)
                            SystemWrite0(" ");

                        m_cursorYPosition = SetCursorTop(m_cursorYPosition);
                        CursorLeft = 0;
                    }
                    WriteConsoleLine(color, format, args);
                    if (m_cursorYPosition != -1)
                        m_cursorYPosition = CursorTop;
                }
        }

        static public void WriteConsoleLine(ConsoleColor color, string format, params object[] args)
        {
            if (NoConsoleVisible) return;
            try
            {
                lock (m_syncRoot)
                {
                    try
                    {
                        if (color != ForegroundColor)  // ConsoleColor.White
                            ForegroundColor = color;
                        SystemWriteLine0(format, args);
                        ResetColor();
                    }
                    catch (ArgumentNullException)
                    {
                        // Some older systems dont support coloured text.
                        SystemWriteLine0(format, args);
                    }
                    catch (FormatException e)
                    {
                        format = SafeFormat(format, args);
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
            if (NoConsoleVisible || tl_justWrotePrompt) return; 
            try
            {
                lock (m_syncRoot)
                {
                    sender = sender.ToUpper();

                    // SystemWriteLine("[" + sender + "] ");

                    SystemWrite0("[");

                    WriteColorText(color, sender);
                    PauseIfTraced("WritePrefixLine: " + sender);
                    SystemWrite0("] ");
                    Flush();
                }
            }
            catch (ObjectDisposedException)
            {
            }
        }

        private static void Flush()
        {
            try
            {
                InitialConsoleOut.Flush();
                InitialConsoleERR.Flush();
            }
            catch
            {
            }
        }

        public static void WriteColorText(ConsoleColor color, string text)
        {
            if (NoConsoleVisible) return;
            lock (m_syncRoot)
            {
                try
                {
                    ForegroundColor = color;
                    InitialConsoleOut.Write(text);
                    Flush();
                    ResetColor();
                }
                catch (ArgumentNullException)
                {
                    // Some older systems dont support coloured text.
                    SystemWrite0(" (ANE) " + text);
                    Flush();
                }
            }
        }

        private void Help(string module, string[] cmd)
        {
            List<string> help = Commands.GetHelp(cmd);

            foreach (string s in help)
                Output(s);
        }


        private const string LOGLEVEL_NONE = "(none)"; 

        private int m_cursorXPosition = 0;
        //private StringBuilder m_commandLine = new StringBuilder();
        private bool m_echo = true;
        private List<string> m_history = new List<string>();

        private void Show()
        {
            lock (cmdline)
            {
                if (m_cursorYPosition == -1 || System.Console.BufferWidth == 0)
                    return;

                int xc = prompt.Length + m_cursorXPosition;
                int new_x = xc % System.Console.BufferWidth;
                int new_y = m_cursorYPosition + xc / System.Console.BufferWidth;
                int end_y = m_cursorYPosition + (cmdline.Length + prompt.Length) / System.Console.BufferWidth;

                if (end_y >= System.Console.BufferHeight) // wrap
                {
                    m_cursorYPosition--;
                    new_y--;
                    SetCursorLeft(0);
                    SetCursorTop(System.Console.BufferHeight - 1);
                    System.Console.WriteLine(" ");
                }

                m_cursorYPosition = SetCursorTop(m_cursorYPosition);
                SetCursorLeft(0);

                if (m_echo)
                    System.Console.Write("{0}{1}", prompt, cmdline);
                else
                    System.Console.Write("{0}", prompt);

                SetCursorTop(new_y);
                SetCursorLeft(new_x);
            }
        }


        public void LockOutput()
        {
            Monitor.Enter(cmdline);
            try
            {
                if (m_cursorYPosition != -1)
                {
                    m_cursorYPosition = SetCursorTop(m_cursorYPosition);
                    System.Console.CursorLeft = 0;

                    int count = cmdline.Length + prompt.Length;

                    while (count-- > 0)
                        System.Console.Write(" ");

                    m_cursorYPosition = SetCursorTop(m_cursorYPosition);
                    SetCursorLeft(0);
                }
            }
            catch (Exception)
            {
            }
        }

        public void UnlockOutput()
        {
            if (m_cursorYPosition != -1)
            {
                m_cursorYPosition = System.Console.CursorTop;
                Show();
            }
            Monitor.Exit(cmdline);
        }

        static public void WriteColorTextNEW(ConsoleColor color, string sender)
        {
            try
            {
                lock (m_syncRoot)
                {
                    try
                    {
                        System.Console.ForegroundColor = color;
                        System.Console.Write(sender);
                        System.Console.ResetColor();
                    }
                    catch (ArgumentNullException)
                    {
                        // Some older systems dont support coloured text.
                        System.Console.WriteLine(sender);
                    }
                }
            }
            catch (ObjectDisposedException)
            {
            }
        }

        private void WriteLocalText(string text, string level)
        {
            string outText = text;

            bool needsPrefix = true;

            if (level != LOGLEVEL_NONE)
            {
                string regex = @"^(?<Front>.*?)\[(?<Category>[^\]]+)\]:?(?<End>.*)";

                Regex RE = new Regex(regex, RegexOptions.Multiline);
                MatchCollection matches = RE.Matches(text);

                if (matches.Count == 1)
                {
                    outText = matches[0].Groups["End"].Value;
                    System.Console.Write(matches[0].Groups["Front"].Value);

                    System.Console.Write("[");
                    WriteColorText(DeriveColor(matches[0].Groups["Category"].Value),
                            matches[0].Groups["Category"].Value);
                    System.Console.Write("]:");
                    needsPrefix = false;
                }
            }

            if (level == "error")
                WriteColorText(ConsoleColor.Red, outText);
            else if (level == "warn")
                WriteColorText(ConsoleColor.Yellow, outText);
            else if (needsPrefix)
            {
                string sender;
                string getCallerFormat = GetCallerFormat(outText, out sender);
                WriteNewLine(DeriveColor(sender), sender, ConsoleColor.Gray, "{0}", outText);
            }
            else
                System.Console.Write(outText);

            System.Console.WriteLine();
        }

        public void Output(string text)
        {
            Output(text, LOGLEVEL_NONE);
        }

        public void Output(string text, string level)
        {
            lock (cmdline)
            {
                if (m_cursorYPosition == -1)
                {
                    WriteLocalText(text, level);

                    return;
                }

                m_cursorYPosition = SetCursorTop(m_cursorYPosition);
                SetCursorLeft(0);

                int count = cmdline.Length + prompt.Length;

                while (count-- > 0)
                    System.Console.Write(" ");

                m_cursorYPosition = SetCursorTop(m_cursorYPosition);
                SetCursorLeft(0);

                WriteLocalText(text, level);

                m_cursorYPosition = System.Console.CursorTop;

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

        public string CmdPrompt(Func<string> p)
        {
            return ReadLine(String.Format("{0}: ", p()), false, true);
        }

        public string CmdPrompt(Func<string> p, string def)
        {
            string ret = ReadLine(String.Format("{0} [{1}]: ", p(), def), false, true);
            if (ret == String.Empty)
                ret = def;

            return ret;
        }

        // Displays a command prompt and returns a default value, user may only enter 1 of 2 options
        public string CmdPrompt(Func<string> prompt, string defaultresponse, string OptionA, string OptionB)
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

        public string ReadLine(string p, bool isCommand, bool echo)
        {
            m_cursorXPosition = 0;
            prompt = p;
            m_echo = echo;
            int historyLine = m_history.Count;

            SetCursorLeft(0); // Needed for mono
            System.Console.Write(" "); // Needed for mono

            lock (cmdline)
            {
                m_cursorYPosition = System.Console.CursorTop;
                cmdline.Remove(0, cmdline.Length);
            }

            while (true)
            {
                Show();

                ConsoleKeyInfo key = System.Console.ReadKey(true);
                char enteredChar = key.KeyChar;

                if (!Char.IsControl(enteredChar))
                {
                    if (m_cursorXPosition >= 318)
                        continue;

                    if (enteredChar == '?' && isCommand)
                    {
                        if (ContextHelp())
                            continue;
                    }

                    cmdline.Insert(m_cursorXPosition, enteredChar);
                    m_cursorXPosition++;
                }
                else
                {
                    switch (key.Key)
                    {
                        case ConsoleKey.Backspace:
                            if (m_cursorXPosition == 0)
                                break;
                            cmdline.Remove(m_cursorXPosition - 1, 1);
                            m_cursorXPosition--;

                            SetCursorLeft(0);
                            m_cursorYPosition = SetCursorTop(m_cursorYPosition);

                            if (m_echo)
                                System.Console.Write("{0}{1} ", prompt, cmdline);
                            else
                                System.Console.Write("{0}", prompt);

                            break;
                        case ConsoleKey.End:
                            m_cursorXPosition = cmdline.Length;
                            break;
                        case ConsoleKey.Home:
                            m_cursorXPosition = 0;
                            break;
                        case ConsoleKey.UpArrow:
                            if (historyLine < 1)
                                break;
                            historyLine--;
                            LockOutput();
                            cmdline.Remove(0, cmdline.Length);
                            cmdline.Append(m_history[historyLine]);
                            m_cursorXPosition = cmdline.Length;
                            UnlockOutput();
                            break;
                        case ConsoleKey.DownArrow:
                            if (historyLine >= m_history.Count)
                                break;
                            historyLine++;
                            LockOutput();
                            if (historyLine == m_history.Count)
                            {
                                cmdline.Remove(0, cmdline.Length);
                            }
                            else
                            {
                                cmdline.Remove(0, cmdline.Length);
                                cmdline.Append(m_history[historyLine]);
                            }
                            m_cursorXPosition = cmdline.Length;
                            UnlockOutput();
                            break;
                        case ConsoleKey.LeftArrow:
                            if (m_cursorXPosition > 0)
                                m_cursorXPosition--;
                            break;
                        case ConsoleKey.RightArrow:
                            if (m_cursorXPosition < cmdline.Length)
                                m_cursorXPosition++;
                            break;
                        case ConsoleKey.Enter:
                            SetCursorLeft(0);
                            m_cursorYPosition = SetCursorTop(m_cursorYPosition);

                            System.Console.WriteLine();
                            //Show();

                            lock (cmdline)
                            {
                                m_cursorYPosition = -1;
                            }

                            string commandLine = cmdline.ToString();

                            if (isCommand)
                            {
                                string[] cmd = Commands.Resolve(Parser.Parse(commandLine));

                                if (cmd.Length != 0)
                                {
                                    int index;

                                    for (index = 0; index < cmd.Length; index++)
                                    {
                                        if (cmd[index].Contains(" "))
                                            cmd[index] = "\"" + cmd[index] + "\"";
                                    }
                                    AddToHistory(String.Join(" ", cmd));
                                    return String.Empty;
                                }
                            }

                            // If we're not echoing to screen (e.g. a password) then we probably don't want it in history
                            if (m_echo && commandLine != "")
                                AddToHistory(commandLine);

                            return commandLine;
                        default:
                            break;
                    }
                }
            }
        }

        [ThreadStatic] private static bool SkipPrintingThisThread = false;
        public static HashSet<Thread> Printers = new HashSet<Thread>();
        public static HashSet<Thread> Skippers = new HashSet<Thread>();
        public static int DebugLevel = 0;
        public static void DebugWriteLine(string format, params object[] args)
        {
            if (DebugLevel == 0)
            {
                return;
            }
            string printStr = TheConsole.SafeFormat(format, args);
            if (!TheGlobalLogFilter.ShouldPrint(printStr))
            {
                if (DebugLevel > 5)
                    CALL_SYSTEM_ERR_WRITELINE("!shouldprint- " + printStr);
                return;
            }
            if (printStr == null) return;
            Thread ct = Thread.CurrentThread;
            Printers.Add(ct);
            if (PrintOnlyThisThread != null && PrintOnlyThisThread != ct)
            {
                return;
            }
            if (SkipPrintingThisThread) return;
            if (Skippers.Contains(ct)) return;

            string sender;
            string getCallerFormat = GetCallerFormat(printStr, out sender);
            ExecWithMaxTime(() => WriteNewLine(DeriveColor(sender), sender, ConsoleColor.Gray, "{0}", printStr), 2000);
            return;
        }

        private static void CALL_SYSTEM_ERR_WRITELINE(string format, params object[] args)
        {
            if (NoConsoleVisible) return;
            ExecWithMaxTime(() => CALL_SYSTEM_ERR_WRITELINE_REAL(format, args), 1000);
        }

        private static System.Threading.Thread MainThread = System.Threading.Thread.CurrentThread;
        public static void ExecWithMaxTime(Action action, int i)
        {
            try
            {
                action();
            }
            catch
            {
            }
            return;
            AutoResetEvent are = new AutoResetEvent(false);
            Thread orig = Thread.CurrentThread;
            Thread doIt = new Thread(() =>
                                         {
                                             try
                                             {
                                                 action();
                                                 are.Set();
                                             }
                                             catch (ThreadAbortException abouirt)
                                             {
                                                 return;
                                             }
                                             catch (Exception abouirt)
                                             {
                                                 SystemConsole.WriteLine(abouirt);
                                                 return;
                                             }
                                         });
            try
            {
                doIt.Start();
                if (are.WaitOne(i))
                {
                    if (false) doIt.Join();
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
            if (NoConsoleVisible) return;
            SystemFlush();
            format = SafeFormat(format, args);
            //%%%PauseIfTraced(format);

            //if (IsOnMonoUnix) WriteColorText(ConsoleColor.White, "CALL_SYSTEM_ERR_WRITELINE-001 " + format);
            InitialConsoleERR.WriteLine(format, NOARGS);
            SystemFlush();
        }

        [ThreadStatic]
        public static bool tl_justWrotePrompt = false;
        private static void SystemWriteLine0(string format, params object[] args)
        {
            if (ThreadDelegateWriter != null)
            {
                foreach (var outputDelegate in ThreadDelegateWriter)
                {
                    try
                    {
                        outputDelegate(format, args);
                    }
                    catch (Exception)
                    {
                    }
                }
            }
            var Outputs = DLRConsole.Outputs;
            bool writeNewLine = true;
            bool useSWL00 = false;
            if (Outputs.Count == 0 || IsOnMonoUnix || AlwaysWrite)
            {

                if (NoConsoleVisible) return;
                useSWL00 = true;
            }

            string defaultPrompt = SingleInstance.DefaultPrompt;
            if (defaultPrompt != null)
            {
                string formatTrim = format.Trim();
                string dpt = defaultPrompt.Trim();
                if (!tl_justWrotePrompt)
                {
                    if (formatTrim == dpt)
                    {
                        writeNewLine = false;
                        tl_justWrotePrompt = true;
                    }
                    else
                    {
                        tl_justWrotePrompt = false;
                    }
                }
                else
                {
                    if (formatTrim == "")
                    {
                        return;
                    }
                    tl_justWrotePrompt = false;
                }
            }
            else
            {
                tl_justWrotePrompt = false;
            }

            if (useSWL00)
            {
                var sformat = SafeFormat(format, args);
                if (String.IsNullOrEmpty(sformat)) return;
                if (writeNewLine)
                {
                    SystemWriteLine00(sformat.Trim());
                }
                else
                {
                    SystemWrite00(sformat.Trim());
                }
                return;
            }

            foreach (TextWriter o in Outputs)
            {
                try
                {
                    OutputDelegate ow = writeNewLine ? o.WriteLine : (OutputDelegate) o.Write;
                    if (args == null || args.Length == 0)
                    {
                        ow(format, args);
                    }
                    else
                    {
                        ow(format, args);
                    }
                }
                catch (Exception e)
                {
                    CALL_SYSTEM_ERR_WRITELINE("" + e);
                    try
                    {
                        o.WriteLine(SafeFormat(format, args));
                    }
                    catch (Exception e2)
                    {
                        CALL_SYSTEM_ERR_WRITELINE("" + e2);
                    }
                }

            }
        }
        private static void SystemWriteLine00(string format)
        {
            var Outputs = DLRConsole.Outputs;
            if (IsOnMonoUnix || Outputs.Count == 0)
            {
                CALL_SYSTEM_ERR_WRITELINE(format);
                return;
            }
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
                return;
            }
            if (String.IsNullOrEmpty(format)) return;
            SystemWriteLine00(format);
        }
        public static string GetCallerFormat(string format, out string prefix)
        {
            string testit = format.Trim();
            if (testit.EndsWith(":") || testit.Length < 4 || testit.StartsWith(":"))
            {
                prefix = CurrentCallerTrace;
                return format;
            }
            if (testit.StartsWith("["))
            {
                format = format.TrimStart();
                int fi = format.IndexOf("]", 0);
                if (fi < 4)
                {
                    prefix = CurrentCallerTrace;
                    return format;
                }
                prefix = format.Substring(1, fi - 1);
                if (AvoidPrefix(prefix))
                {
                    prefix = CurrentCallerTrace;
                    return format;
                }
                return format.Substring(prefix.Length + 2);
            }
            int fc = format.IndexOf(":");
            if (fc > 4 && fc < 32)
            {
                string canformat = format.TrimStart();
                fc = canformat.IndexOf(":");
                string canprefix = canformat.Substring(0, fc);
                if (!AvoidPrefix(canprefix))
                {
                    prefix = canprefix;
                    return canformat.Substring(fc + 1);
                }
            }
            prefix = CurrentCallerTrace;
            return format;

        }

        private static bool AvoidPrefix(string canprefix)
        {
            if (ContainsReportName("" + canprefix))
            {
                return true;
            }
            int len = canprefix.Length;
            if (len < 4) return true;
            if (canprefix.Contains("(") || canprefix.Contains(" ")) return true;
            canprefix = canprefix.ToUpper();
            if (canprefix.Contains("WRITE") || canprefix.Contains("PRINT")) return true;
            if (!char.IsLetter(canprefix[0])) return true;
            if (!char.IsLetter(canprefix[len - 1])) return true;
            if (canprefix == "HOSTPROC" || canprefix == "PROGRAM" || canprefix == "APPDOMAIN" || canprefix == "COMMAND" || canprefix == "THREADHELPER" || canprefix == "SUCCESS" || canprefix == "FAILURE") return true;
            return false; 
        }

        public static void DebugWriteLine(object arg)
        {
            string prefix;
            string getCallerFormat = GetCallerFormat("" + arg, out prefix);
            getCallerFormat = "[" + prefix.ToUpper() + "] " + getCallerFormat;
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
        static public string CurrentCallerTrace
        {
            get
            {
                string cc = FindCallerInStackTrace(TransparentCallers, OpacheCallers, false);
                if (cc == null || AvoidPrefix(cc))
                {
                    cc = FindCallerInStackTrace(TransparentCallers, OpacheCallers, true);
                }
                return cc;
            }
        }

        private static bool SkipStackTracesBusy = false;
        public static string FindCallerInStackTrace(HashSet<MemberInfo> transparentCallers, HashSet<MemberInfo> opacheCallers, bool useMethodName)
        {
            if (SkipStackTraces) return "FCISSkip";
            if (SkipStackTracesBusy) return "FCISBusy";
            SkipStackTracesBusy = true;
            var st = new System.Diagnostics.StackTrace(true).GetFrames();
            if (st == null)
            {
                SkipStackTracesBusy = false;
                return "FCISNoST";
            }
            try
            {
                int startAt = transparentCallers == null ? 1 : 3;
                int stLast = st.Length - 1;
                if (transparentCallers == null) transparentCallers = TransparentCallers;
                return FindCallerInStackTrace0(st, startAt, stLast,
                                          transparentCallers, opacheCallers ?? OpacheCallers, useMethodName);
            }
            finally
            {
                SkipStackTracesBusy = false;
            }
        }

        public static string FindCallerInStackTrace0(StackFrame[] st, int startAt, int stLast, 
            HashSet<MemberInfo> transparentCallers, HashSet<MemberInfo> opacheCallers, bool useMethodName) {
            string fallBackName = null;
            string cn = null;
            bool lastWasAvoidPrefix = false;
            StackFrame afterAvoidPrefix = null;
            {                
                MemberInfo typeSkipped = typeof(DLRConsole);
                StackFrame s2 = null;
                MemberInfo caller2 = null;
                for (int i = startAt; i < stLast; i++)
                {
                    StackFrame s = s2 ?? st[i];
                    s2 = st[i + 1];
                    if (lastWasAvoidPrefix)
                    {
                        lastWasAvoidPrefix = false;
                        afterAvoidPrefix = s;
                    }
                    var m = s.GetMethod();
                    var m2 = s2.GetMethod();
                    cn = CallerName(s, useMethodName);
                    string cmn = null;
                    if (m != null)
                    {
                        var caller = caller2 ?? ResolveType(m);
                        if (caller == null) continue;
                        caller2 = ResolveType(m2);
                        if (typeSkipped == caller && caller == caller2)
                        {
                            continue;
                        }
                        if (caller == typeof(TextFilter)) continue;
                        //typeSkipped = caller;
                        lock (UltraTransparentCallers)
                        {
                            if (IsTypeOf(caller, UltraTransparentCallers)) continue;
                        }
                        lock (opacheCallers)
                        {
                            if (IsTypeOf(caller, opacheCallers))
                            {
                                if (ContainsReportName("" + m))
                                {
                                    cmn = cmn ?? CallerName(s, !useMethodName);
                                    string fbn = cmn;
                                    if (fallBackName == null)
                                    {
                                        fallBackName = fbn;
                                    }
                                    lastWasAvoidPrefix = true;
                                    continue;
                                }
                                return cn;// CallerName(s, useMethodName);
                            }
                        }
                        lock (transparentCallers)
                        {
                            if (IsTypeOf(caller, transparentCallers))
                            {
                                cmn = cmn ?? CallerName(s, !useMethodName);
                                string fbn = cmn;
                                if (!AvoidPrefix(fbn))
                                {
                                    if (fallBackName == null)
                                    {
                                        fallBackName = fbn;
                                    }
                                }
                                lastWasAvoidPrefix = true;
                                continue;
                            }
                        }
                    }
                    if (AvoidPrefix(cn))
                    {
                        lastWasAvoidPrefix = true;
                        continue;
                    }
                    return cn;
                }
            }
            SkipStackTracesBusy = false;
            if (afterAvoidPrefix != null)
            {
                return CallerName(afterAvoidPrefix, useMethodName);
            }
            if (fallBackName != null)
            {
                return fallBackName;
            }
            return cn;// CallerName(st[stLast], useMethodName);
        }

        private static bool ContainsReportName(string callerString)
        {
            callerString = callerString.ToLower();
            foreach (
                var variable in
                    new string[]
                        {
                            "write", "trace", "print", "debug", "warn", "error", "info",
                            "exception", "flush", "break", "format"
                        })
            {
                if (callerString.Contains(variable))
                {
                    return true;
                }
            }
            return false;
        }

        private static bool IsTypeOf(MemberInfo caller, HashSet<MemberInfo> transparentCallers)
        {
            if (transparentCallers.Contains(caller)) return true;
            Type ct = caller as Type;
            if (ct != null)
            {
                bool cont = false;
                foreach (MemberInfo set in transparentCallers)
                {
                    Type t = set as Type;
                    if (t == null) continue;
                    if (t.IsAssignableFrom(ct))
                    {
                        return true;
                    }
                }          
            }
            return false;
        }

        private static string CallerName(StackFrame frame, bool useMethodName)
        {
            string str = "";
            if (frame == null) return str;
            var m = frame.GetMethod();
            string suffix = "";
            if (m != null)
            {
                MemberInfo type = m; // +"_" + str;                   
                type = ResolveType(type);
                str = type.Name ?? "" + type;
                if (type is Type)
                {
                    lock (TransparentCallers)
                    {
                        if (TransparentCallers.Contains(type) && !UltraTransparentCallers.Contains(type))
                        {
                            //todo decide below to comment
                            useMethodName = true;
                            str = "";
                        }
                    }
                }
                if (useMethodName)
                {
                    suffix = m.Name;
                    if (suffix.StartsWith("get_") || suffix.StartsWith("set_")) suffix = suffix.Substring(4);
                }
            }
            if (!string.IsNullOrEmpty(str) && !string.IsNullOrEmpty(suffix))
            {
                return str.ToUpper() + "." + suffix;
            }
            //var mo = frame.GetFileName() + "_" + frame.GetFileLineNumber() + suffix;
            return str.ToUpper() + suffix;
        }

        private static MemberInfo ResolveType(MemberInfo type)
        {
            bool foundType = false;
            while (type.DeclaringType != null && type.DeclaringType != type)
            {
                foundType = true;
                type = type.DeclaringType;
            }
            if (foundType) return type;
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

        private static bool InSystemWrite00 = false;
        internal static void SystemWrite00(string format)
        {
            if (InSystemWrite00)
            {
                return;
            }
            InSystemWrite00 = true;
            ExecWithMaxTime(() => SystemWrite000(format), 2000);
            InSystemWrite00 = false;
        }

        internal static void SystemWrite000(string format)
        {
            format = GetFormat(format);
            if (IsOnMonoUnix || AlwaysWrite)
            {
                SystemConsole.Write(format);
            }
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

        public static readonly HashSet<MemberInfo> UltraTransparentCallers = new HashSet<MemberInfo>()
                                                                      {
                                                                          typeof (OpenSimAppender),
                                                                          typeof (DLRConsole),
                                                                          typeof (SystemConsole),
                                                                          typeof (OutputDelegateWriter),
                                                                          typeof (TextWriter),
                                                                          typeof (TextFilter),
                                                                          typeof (RuntimeMethodHandle),
                                                                          typeof (MethodInfo),
                                                                          typeof (MethodBase),
                                                                          typeof (TaskQueueHandler),
                                                                          typeof (ExecutionContext),
                                                                          typeof (EventHandler),
                                                                          typeof (EventHandler<>)
                                                                      };

        public static readonly HashSet<MemberInfo> TransparentCallers = new HashSet<MemberInfo>(UltraTransparentCallers)
                                                                            {
                                                                                typeof (XmlDocumentLineInfo)
                                                                            };

        public static readonly HashSet<MemberInfo> OpacheCallers = new HashSet<MemberInfo>()
                                                              {
                                                                  typeof (ScriptedCommand),
                                                              };

        private TextWriter NULL_OUTPUT = new NULL_OUTPUT_TW();
        public static Thread PrintOnlyThisThread;

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
                        if (use != null && !NoConsoleVisible) list.Add(use);
                    }
                    else
                    {
                        list.AddRange(_outputs);
                    }
                }
                if (AllocedConsole && !NoConsoleVisible)
                {
                    var use = InitialConsoleOut;
                    if (!list.Contains(use))
                    {
                        list.Add(use);
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
            if (args != null && args.Length == 1 && args[0] is object[])
            {
                args = (object[]) args[0];
            }

            if (fmt == null)
            {
                return ExplainFormatError(null, args, SYSTEM_ERR_WRITELINE, new Exception());
            }

            string str = fmt;
            if (args != null && args.Length > 0)
            {
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
        private static string ExplainFormatError(string fmt, IEnumerable<object> args, OutputDelegate del, Exception exception)
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
            DebugWriteLine("WARN: InvokeControl on " + rtb + " before IsHandleCreated from " + invoke.Method.ReflectedType + ":" + invoke.Method);
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
            if (rtb.IsDisposed) return true;
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
                DebugWriteLine(ex);
               /// Logger.Log("Failure in event handler: " + ex.Message, Helpers.LogLevel.Warning, ex);
                return false;
            }
        }

        public static string NoFormatDirectives(string prefix)
        {
            return prefix.Replace("{", "(").Replace("}", ")");

        }

        [ThreadStatic]
        static List<OutputDelegate> ThreadDelegateWriter;
        public static void EnterThreadWriteLine(OutputDelegate writeLine)
        {
            if (ThreadDelegateWriter == null) ThreadDelegateWriter = new List<OutputDelegate>();
            ThreadDelegateWriter.Insert(0, writeLine);
        }

        public static void ExitThreadWriteLine(OutputDelegate writeLine)
        {
            if (ThreadDelegateWriter != null)
            {
                int count = ThreadDelegateWriter.Count;
                
                if (ThreadDelegateWriter[0]!=writeLine) return;
                ThreadDelegateWriter.RemoveAt(0);
                if (count == 1) ThreadDelegateWriter = null;
            }            
        }

        public static bool ContainsSubscribedBreakpointWords(string toWrite)
        {
            if (string.IsNullOrEmpty(toWrite)) return false;
            string l = toWrite.ToLower();
            if (l.Contains("warn") || l.Contains("error") || l.Contains("bad") || l.Contains("exce") || l.Contains("== null"))
            {
                if (IsCallerNoDebug) return false;
                return true;
            }
            return false;
        }
    }

    public class NULL_OUTPUT_TW : TextWriter
    {
        public override void Write(char[] buffer, int index, int count)
        {
            //base.Write(buffer, index, count);
        }
        public NULL_OUTPUT_TW()
        {

        }
        protected NULL_OUTPUT_TW(IFormatProvider formatProvider)
            : base(formatProvider)
        {

        }
        public override Encoding Encoding
        {
            get { return Encoding.Default; }
        }
    }
}
