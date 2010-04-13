using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Net;
//using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions.External
{
    public class ShellExec : Command, SystemApplicationCommand
    {
        public static string DoShellExec(string cmd, string args)
        {
            Process proc = new Process();
            proc.EnableRaisingEvents = false;
            proc.StartInfo.FileName = cmd;
            proc.StartInfo.Arguments = args;
            proc.Start();
            var output = proc.StandardOutput;
            proc.WaitForExit();
            return output.ReadToEnd();
        }

        public ShellExec(BotClient Client)
            : base(Client)
        {
            Name = "ShellExec";
            Description = "Do an shell exec. Usage: shellexec dir";
        }
        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            return Success(DoShellExec(args[0],Parser.Rejoin(args.tokens,1)));
        }
    }
}
