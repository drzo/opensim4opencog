using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheOpenSims;
using System.Threading;
using cogbot.Listeners; //using libsecondlife;

namespace cogbot.Actions
{

    class SimType : Command, BotSystemCommand
    {
        //BotRegionModel BRM;
        public SimType(BotClient Client)
        {
            Name = GetType().Name.ToLower();
            helpString = "Manipulates the SimType typesystem";
            usageString = "Usage: " + Name + " [ini|list|load]";
        }

        public override string Execute(string[] args, UUID fromAgentID)
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
                    return SimTypeSystem.ListTypes();
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

        public override string Execute(string[] args, UUID fromAgentID)
        {
            if (args.Length > 0)
            {
                if (args[0] == "on")
                {
                    SimAvatar avatar = GetSimAvatar();
                    return "Turned on " + avatar;
                }

                if (args[0] == "start")
                {
                    SimAvatar avatar = GetSimAvatar();
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
                    SimAvatar avatar = GetSimAvatar();
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
                    SimAvatar avatar = GetSimAvatar();
                    avatar.PauseThinking();
                    return "Stopped " + avatar;
                }
                else if (args[0] == "off")
                {
                    //    if (BRM == null) return "the bot was off";
                    SimAvatar avatar = GetSimAvatar();
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

        private SimAvatar GetSimAvatar()
        {
            return WorldSystem.TheSimAvatar;

        }

    }
}

