using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using CommandLine.Utility;
using OpenMetaverse;
using System.IO;
using OpenSim.Framework.Console;
using Radegast;
using Settings=OpenMetaverse.Settings;


namespace cogbot
{
    public class Program
    {
        private static void Usage()
        {
            Console.WriteLine("Usage: " + Environment.NewLine +
                    "cogbot.exe --first firstname --last lastname --pass password [--loginuri=\"uri\"] [--startpos \"sim/x/y/z\"] [--master \"master name\"] [--masterkey \"master uuid\"] [--gettextures] [--scriptfile \"filename\"]");
        }

        internal static class NativeMethods
        {
            [DllImport("kernel32.dll")]
            internal static extern Boolean AllocConsole();
        }

        public static ConsoleBase consoleBase;


        [STAThread]
        public static void Main(string[] args)
        {
          //  NativeMethods.AllocConsole();
           // Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);

            ClientManager.UsingCogbotFromRadgast = false;
            ClientManager.UsingRadgastFromCogbot = true;
            ClientManager.arguments = new Arguments(args);
            consoleBase = new ConsoleBase("textform");
            ClientManager manager = new ClientManager();
            manager.outputDelegate = new OutputDelegate(WriteLine);
            if (!manager.ProcessCommandArgs())
            {
                Usage();
                return;
            }
            manager.Run();
        }


        public void WriteLine(ConsoleColor color, string format, params object[] args)
        {
            throw new NotImplementedException();
        }
        public string CmdPrompt(string p)
        {
            throw new NotImplementedException();
        }

        public static void WriteLine(string str, object[] args)
        {
            if (consoleBase==null)
            {
                try
                {
                    if (args == null || args.Length == 0)
                    {
                        args = new object[] { str };
                        str = "{0}";
                    } 
                    Console.WriteLine(str, args);
                }
                catch (FormatException)
                {

                    Console.WriteLine(str);
                }      
                return;
            }
            int index = str.IndexOf("]");
            if (index > 0 && str.StartsWith("["))
            {
                string sender = str.Substring(0, index).Trim();
                if (sender.StartsWith("[")) sender = sender.Substring(1);
                str = str.Substring(index + 1).Trim();
                consoleBase.Notice(sender, str, args);
            }
            else
            {
               consoleBase.Notice(str, args);
            }
        }

        public static void Notice(string sender, string str, object[] args)
        {
            if (consoleBase == null)
            {
                try
                {
                    Console.WriteLine("[" + sender + "] " + str, args);
                }
                catch (FormatException)
                {

                    Console.WriteLine("[" + sender + "] " + str);
                }
                return;
            }
            consoleBase.Notice(sender, str, args);
        }
    }
}