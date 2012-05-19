using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheOpenSims;
using System.Threading;
using cogbot.Listeners; //using libsecondlife;

using MushDLR223.ScriptEngines;
using PathSystem3D.Navigation;

namespace cogbot.Actions
{
    class Use : Command, BotPersonalCommand
    {
        public Use(BotClient Client)
            : base(Client)
        {
            Description = "Interface to the OpenSims module. <a href='wiki/TheOpenSims'>Documentation Here</a>";
            Usage = "DMILES TODO";
            Category = CommandCategory.Objects;
            Parameters = new[] { new NamedParam(typeof(SimPosition), typeof(UUID)) };  //DMILES TODO
            // DMILE
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
            if (objname == "")
            {
                return Failure("$bot don't know what object to use.");
            }
            if (to_op == "")
            {
                SimObject objToUse;
                if (WorldSystem.tryGetPrim(objname, out objToUse))
                {
                    if ((BotNeeds)TheSimAvatar["CurrentNeeds"] == null)
                    {
                        TheSimAvatar["CurrentNeeds"] = new BotNeeds(90.0f);
                    }
                    SimTypeUsage usage = objToUse.Affordances.GetBestUse((BotNeeds)TheSimAvatar["CurrentNeeds"]);
                    if (usage == null)
                    {
                        //usage = new MoveToLocation(TheSimAvatar, objToUse);
                        return Failure("$bot don't have a use for " + objToUse + " yet.");
                    }
                    TheSimAvatar.Do(usage, objToUse);
                    return Success("used " + objToUse);
                }
                return Failure("$bot don't know what to do with " + objname);
            }
            WriteLine("Trying to (" + to_op + ") with (" + objname + ")");
            TheBotClient.UseInventoryItem(to_op, objname);
            return Success("completed to (" + to_op + ") with (" + objname + ")");

        }
    }

    class DoCommand : Command, BotPersonalCommand
    {
        public DoCommand(BotClient Client)
        {
            Name = GetType().Name.ToLower().Replace("command", "");
            Description = "Tell a bot to do an action on an object";
            Usage = "Usage: " + Name + " [UseTypeName] [object]";
        }
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 2) return ShowUsage();
            SimTypeUsage use = SimTypeSystem.FindObjectUse(args[0]);
            if (use == null) return Failure("Unknown use: " + args[0]);
            args = Parser.SplitOff(args, 1);
            int argsUsed;
            SimObject O = WorldSystem.GetSimObjectS(args, out argsUsed);
            if (O == null) return Failure("Cant find simobject " + string.Join(" ", args));
            WriteLine("Doing " + use + " for " + O);
            WorldSystem.TheSimAvatar.Do(use, O);
            return Success("Did " + use + " for " + O);
        }
    }

    class SimTypeCommand : Command, SystemApplicationCommand
    {
        public SimTypeCommand(BotClient Client)
        {
            Name = GetType().Name.ToLower().Replace("command","");
            Description = "Manipulates the SimType typesystem";
            Usage = "Usage: " + Name + " [ini|list|objects|uses|instances|load]";
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length > 0)
            {
                if (args[0] == "ini")
                {
                    SimTypeSystem.LoadDefaultTypes0();
                    WorldSystem.RescanTypes();
                    return Success("ReLoaded  ini");
                }
                if (args[0] == "list")
                {
                    return Success(SimTypeSystem.ListTypes(false, true, true, false));
                }
                if (args[0] == "load")
                {
                    if (args.Length > 1)
                    {
                        SimTypeSystem.LoadConfig(args[1]);
                    }
                    WorldSystem.RescanTypes();
                    return Success("(Re)Loaded " + args[1]);
                }
                if (args[0] == "uses")
                {
                    return Success(SimTypeSystem.ListTypes(true, true, false, false));
                }
                if (args[0] == "objects")
                {
                    return Success(SimTypeSystem.ListTypes(true, false, true, false));
                }
                if (args[0] == "instances")
                {
                    return Success(SimTypeSystem.ListTypes(true, false, false, true));
                }
            }
            return ShowUsage();
        }
    }
}

