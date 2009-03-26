using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheOpenSims; //using libsecondlife;

namespace cogbot.Actions
{
    class Use : Action
    {
       public Use(BotClient Client)
            : base(Client)
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
           if (objname == "") { WriteLine("I don't know what object to use."); return; }
           if (to_op == "") {
               Primitive prim;
               if (WorldSystem.tryGetPrim(objname, out prim))
               {
                   SimObject objToUse = WorldSystem.GetSimObject(prim);
                   WorldSystem.TheSimAvatar.UseAspect(objToUse);
                   return;
               }
               WriteLine("I don't know what to do with "+objname); return; 
           }
           WriteLine("Trying to (" + to_op + ") with (" + objname + ")");
           TheBotClient.UseInventoryItem(to_op, objname);
           
       }
    }
}
