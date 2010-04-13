using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Net;
//using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions.External
{
    public class JarExec : Command, SystemApplicationCommand
    {
        //shellexec c:\\windows\\system32\\cmd.exe /k dir
        public static string DoShellExec(string cmd, string args)
        {
            Process proc = new Process();
            //proc.EnableRaisingEvents = false;
            proc.StartInfo.FileName = cmd;
            proc.StartInfo.Arguments = args;
            proc.StartInfo.UseShellExecute = false;
            proc.StartInfo.CreateNoWindow = true;
            proc.StartInfo.RedirectStandardOutput = true;
            proc.Start();
            StreamReader stdOut = proc.StandardOutput;
            proc.WaitForExit();
            string output = String.Empty;
            while (!stdOut.EndOfStream) output += "" + stdOut.ReadLine();
            return output;
        }

        public JarExec(BotClient Client)
            : base(Client)
        {
            Name = "JarExec";
            Description = "Do an java exec. Usage: java dir";
        }
        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            return Success(DoShellExec("java.exe",args.str));
        }
    }
}
