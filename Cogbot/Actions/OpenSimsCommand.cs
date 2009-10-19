using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheOpenSims;
using System.Threading;
using cogbot.Listeners; //using libsecondlife;

namespace cogbot.Actions
{

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

