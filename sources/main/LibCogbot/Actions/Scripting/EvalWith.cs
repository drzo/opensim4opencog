using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
//using OpenMetaverse; //using libsecondlife;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Scripting
{
    class EvalWith : Command, BotSystemCommand
    {
        public EvalWith(BotClient Client)
            : base(Client)
        {
            Name = "evalwith";
            Description = "Evals a command with a scripting interpretor. Usage: evalwith <Interp> <Expression>";
        }
        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 2) return ShowUsage();
            string interp = args[0];
            return TheBotClient.ExecuteTask(interp, new StringReader(Parser.Rejoin(args.tokens, 1)),WriteLine);
        }
    }
}
