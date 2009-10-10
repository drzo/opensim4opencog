using System;
using System.Collections.Generic;
using System.Text;
//using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Eval : Command
    {
       public Eval(BotClient Client)
            : base(Client)
        {
            Name = "eval";
            Description = " eval: Enqueue a lisp task. Eval <lisp expression>";
        }
       public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
       {
           //base.acceptInput(verb, args);
           return Success(TheBotClient.evalLispString(args.str));
       }
    }
}
