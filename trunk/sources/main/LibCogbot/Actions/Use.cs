using System;
using OpenMetaverse;
using cogbot.TheOpenSims;
using PathSystem3D.Navigation;

//using libsecondlife;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Agent
{
    class Use : Command, BotPersonalCommand
    {
        public Use(BotClient Client)
            : base(Client)
        {
            Description = "Use an item from inventory or world.";
            Category = CommandCategory.Objects;
            Parameters = new[] { new NamedParam(typeof(SimPosition), typeof(UUID)) };
            Name = "Use..";
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
       {
        //   base.acceptInput(verb, args);
           string to_op = "";
           string objname = "";
           if (args["to"].Length > 0)
           {
               to_op = args["to"];
           }
           objname = args.objectPhrase;
           if (objname == "") {
               return Failure("$bot don't know what object to use.");
           }
           if (to_op == "") {
               SimObject objToUse;
               if (WorldSystem.tryGetPrim(objname, out objToUse))
               {
                   if ((BotNeeds)TheSimAvatar["CurrentNeeds"] == null)
                   {
                       TheSimAvatar["CurrentNeeds"] = new BotNeeds(90.0f);
                   }
                   SimTypeUsage usage = objToUse.Affordances.GetBestUse((BotNeeds)TheSimAvatar["CurrentNeeds"]);
                   if (usage==null)
                   {
                       //usage = new MoveToLocation(TheSimAvatar, objToUse);
                       return Failure( "$bot don't have a use for " + objToUse + " yet.");
                   }
                   TheSimAvatar.Do(usage,objToUse);
                   return Success("used " + objToUse);
               }
               return Failure( "$bot don't know what to do with " + objname); 
           }
           WriteLine("Trying to (" + to_op + ") with (" + objname + ")");
           TheBotClient.UseInventoryItem(to_op, objname);
           return Success("completed to (" + to_op + ") with (" + objname + ")");
           
       }
    }
}
