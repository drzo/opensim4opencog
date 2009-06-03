using System;
using System.Collections.Generic;
using System.Text;
//using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Eval : Action
    {
       public Eval(BotClient Client)
            : base(Client)
        {
            Name = "eval";
            helpString = " eval: Enqueue a lisp task. Eval <lisp expression>";
        }
       public override string acceptInput(string verb, Parser args, OutputDelegate WriteLine)
       {
           //base.acceptInput(verb, args);
           return (TheBotClient.evalLispString(args.str));
       }
    }
}
