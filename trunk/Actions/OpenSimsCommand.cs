using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheOpenSims;
using System.Threading; //using libsecondlife;

namespace cogbot.Actions
{

    class TheSims : Command
    {
        BotRegionModel BRM;
        public TheSims(BotClient Client)           
        {
            Name = "simbot";
            helpString = "Start theOpenSims type AI.";
            usageString = "simbot [on|start|stop|off|think|ini|list]";
           
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {

            if (args.Length == 0) return usageString;

            if (args[0] == "ini")
            {
                SimObjectType.LoadDefaultTypes0();
                return "ReLoaded  ini";
            }
            if (args[0] == "types")
            {
                SimObjectType.ListTypes();
                return "Listed types";
            }
            if (args[0] == "load")
            {
                SimObjectType.LoadConfig(args[1]);
                return "ReLoaded  ini";
            }

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

            if (args[0] == "list")
            {
                SimAvatar avatar = GetSimAvatar();
                return "List " + avatar.DebugInfo();
            }

            
            if (BRM == null) return "the bot is off";

            if (args[0] == "stop")
            {
                SimAvatar avatar = GetSimAvatar();
                avatar.PauseThinking();
                return "Stopped " + avatar;
            }
            else if (args[0] == "off")
            {
                if (BRM == null) return "the bot was off";
                SimAvatar avatar = GetSimAvatar();
                avatar.PauseThinking();
                BRM = null;
                return "Stopped " + avatar;
            }
            else
            {
                return usageString;
            }

            return "";
        }

        private SimAvatar GetSimAvatar()
        {
            if (Client.Network.CurrentSim == null)
            {
                Client.Network.CurrentSim = Client.Network.Simulators[0];
            }
            if (BRM == null) BRM = new BotRegionModel(Client);
            Avatar self = Client.WorldSystem.GetAvatar(Client.Self.AgentID);
            SimAvatar avatar = BRM.GetSimAvatar(self);
            avatar.SetClient(Client);
            return avatar;

        }

    }
}

