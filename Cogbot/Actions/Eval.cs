using System;
using System.Collections.Generic;
using System.Text;
//using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Eval : Command, BotSystemCommand
    {
       public Eval(BotClient Client)
            : base(Client)
        {
            Name = "eval";
            Description = "Enqueue a lisp task on a bot. Usage: Eval <lisp expression>";
            Category = CommandCategory.BotClient;
        }
       public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
       {
           //base.acceptInput(verb, args);
           return Success(TheBotClient.evalLispString(args.str));
       }
    }
}
