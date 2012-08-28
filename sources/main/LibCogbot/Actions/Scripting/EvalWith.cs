using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
//using OpenMetaverse; //using libsecondlife;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Scripting
{
    internal class EvalWith : Command, BotSystemCommand
    {
        public EvalWith(BotClient Client)
            : base(Client)
        {
            Name = "evalwith";
        }

        public override void MakeInfo()
        {
            Description = "Evals a command with a scripting interpretor.";
            AddVersion(CreateParams(
                "interp", typeof (string), "the interp langauge",
                Rest("code", typeof (string), "what to eval")));
            Category = CommandCategory.Scripting;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 2) return ShowUsage();
            string interp = args[0];
            return TheBotClient.ExecuteTask(args.GetString("interp"), new StringReader(args.GetString("code")), WriteLine);
        }
    }
}