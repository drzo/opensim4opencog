using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheOpenSims;
using System.Threading;
using cogbot.Listeners; //using libsecondlife;

namespace cogbot.Actions
{

    class DoCommand : Command
    {
        public DoCommand(BotClient Client)
        {
            Name = GetType().Name.ToLower().Replace("command", "");
            helpString = "Tell a bot to do an action on an object";
            usageString = "Usage: " + Name + " [UseTypeName] [object]";
        }
        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 2) return usageString;
            SimTypeUsage use = SimTypeSystem.FindObjectUse(args[0]);
            if (use == null) return "Unknown use: " + args[0];
            args = Parsing.SplitOff(args,1);
            int argsUsed;
            Primitive p = WorldSystem.GetPrimitive(args, out argsUsed);
            if (argsUsed == 0) argsUsed = args.Length;
            string objname = String.Join("", args,0, argsUsed);
            if (p == null) return "cant find primitive " + objname;
            SimObject O = WorldSystem.GetSimObject(p, null);
            if (O == null) return "cant find simobject " + objname + " for " + p;
            WriteLine("Doing " + use + " for " + O);
            WorldSystem.TheSimAvatar.Do(use, O);
            return "Did " + use + " for " + O;
        }
    }

    class SimTypeCommand : Command, SystemApplicationCommand
    {
        public SimTypeCommand(BotClient Client)
        {
            Name = GetType().Name.ToLower().Replace("command","");
            helpString = "Manipulates the SimType typesystem";
            usageString = "Usage: " + Name + " [ini|list|objects|uses|instances|load]";
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length > 0)
            {
                if (args[0] == "ini")
                {
                    SimTypeSystem.LoadDefaultTypes0();
                    WorldSystem.RescanTypes();
                    return "ReLoaded  ini";
                }
                if (args[0] == "list")
                {
                    return SimTypeSystem.ListTypes(false, true, true, false);
                }
                if (args[0] == "load")
                {
                    if (args.Length > 1)
                    {
                        SimTypeSystem.LoadConfig(args[1]);
                    }
                    WorldSystem.RescanTypes();
                    return "(Re)Loaded " + args[1];
                }
                if (args[0] == "uses")
                {
                    return SimTypeSystem.ListTypes(true, true, false, false);
                }
                if (args[0] == "objects")
                {
                    return SimTypeSystem.ListTypes(true, false, true, false);
                }
                if (args[0] == "instances")
                {
                    return SimTypeSystem.ListTypes(true, false, false, true);
                }
            }
            return usageString;
        }
    }
    class TheSims : Command
    {
        //BotRegionModel BRM;
        public TheSims(BotClient Client)           
        {
            Name = "simbot";
            helpString = "Start theOpenSims type AI.";
            usageString = "simbot [on|start|stop|off|think|needs|info|load]";           
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length > 0)
            {
                if (args[0] == "on")
                {
                    SimActor avatar = GetSimAvatar();
                    return "Turned on " + avatar;
                }

                if (args[0] == "start")
                {
                    SimActor avatar = GetSimAvatar();
                    avatar.StartThinking();
                    return "Started Thinking " + avatar;
                }

                if (args[0] == "needs")
                {
                    SimAvatar avatar = GetSimAvatar();
                    return avatar.CurrentNeeds.ToString();
                }

                if (args[0] == "think")
                {
                    SimActor avatar = GetSimAvatar();
                    if (avatar.IsThinking())
                    {
                        avatar.PauseThinking();
                    }
                    avatar.ThinkOnce();
                    return "Think once " + avatar;
                }

                if (args[0] == "info")
                {
                    SimAvatar avatar = GetSimAvatar();
                    return "List " + avatar.DebugInfo();
                }


                //  if (BRM == null) return "the bot is off";

                if (args[0] == "stop")
                {
                    SimActor avatar = GetSimAvatar();
                    avatar.PauseThinking();
                    return "Stopped " + avatar;
                }
                else if (args[0] == "off")
                {
                    //    if (BRM == null) return "the bot was off";
                    SimActor avatar = GetSimAvatar();
                    avatar.PauseThinking();
                    //   BRM = null;
                    return "Stopped " + avatar;
                }

                if (args[0] == "load")
                {
                    SimTypeSystem.LoadConfig(args[1]);
                    WorldSystem.RescanTypes();
                    return "(Re)Loaded " + args[1];
                }
            }
            return usageString;
        }

        private SimActor GetSimAvatar()
        {
            return WorldSystem.TheSimAvatar;

        }

    }
}

