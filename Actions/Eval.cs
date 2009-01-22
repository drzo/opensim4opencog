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
            helpString = " eval: Enqueue a lisp task. Eval <lisp expression>";
        }
       public override void acceptInput(string verb, Parser args)
       {
           //base.acceptInput(verb, args);
           WriteLine(Client.evalLispString(args.str));
       }
    }
}
