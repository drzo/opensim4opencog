using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Use : Action
    {
       public Use(TextForm parent)
            : base(parent)
        {
            helpString = "Use an item from inventory.";
        }
       public override void acceptInput(string verb, Parser args)
       {
        //   base.acceptInput(verb, args);
           string to_op = "";
           string objname = "";
           if (args.prepPhrases["to"].Length > 0)
           {
               to_op = args.prepPhrases["to"];
           }
           objname = args.objectPhrase;
           if (objname == "") { parent.output("I don't know what object to use."); return; }
           if (to_op == "") { parent.output("I don't know what to do with "+objname); return; }
           parent.output("Trying to (" + to_op + ") with (" + objname + ")");
           parent.UseInventoryItem(to_op, objname);
           
       }
    }
}
