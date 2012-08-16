using System;
using System.Collections.Generic;
using System.Text;
//using OpenMetaverse; //using libsecondlife;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Scripting
{
    class EvalSys : Command, SystemApplicationCommand
    {
        public EvalSys(BotClient Client)
            : base(Client)
        {
            Name = "evalsys";
            Description = "Enqueue a lisp task in ClientMananger. Usage: EvalSys <lisp expression>";
        }
        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            //base.acceptInput(verb, args);
            return Success(ClientManager.evalLispString(args.str));
        }
    }
}
