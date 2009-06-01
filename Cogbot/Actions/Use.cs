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
       public override string acceptInput(string verb, Parser args)
       {
        //   base.acceptInput(verb, args);
           string to_op = "";
           string objname = "";
           if (args.prepPhrases["to"].Length > 0)
           {
               to_op = args.prepPhrases["to"];
           }
           objname = args.objectPhrase;
           if (objname == "") {
               return ("I don't know what object to use."); }
           if (to_op == "") {
               Primitive prim;
               if (WorldSystem.tryGetPrim(objname, out prim))
               {
                   BotMentalAspect objToUse = WorldSystem.GetSimObject(prim);
                   WorldSystem.TheSimAvatar.UseAspect(objToUse);
                   return "used " + objToUse;
               }
               return "I don't know what to do with "+objname; 
           }
           WriteLine("Trying to (" + to_op + ") with (" + objname + ")");
           TheBotClient.UseInventoryItem(to_op, objname);
           return "completed to (" + to_op + ") with (" + objname + ")";
           
       }
    }
}
