using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheSims;
using System.Threading; //using libsecondlife;

namespace cogbot.Actions
{

    class TheSims : Command
    {
        BotRegionModel BRM;
        public TheSims(BotClient Client)           
        {
            Name = "thesims";
            helpString = "Start theSims type AI.";
            usageString = "thesims [on|off|status|think|stats]";
           
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {

            if (args.Length == 0) return usageString;

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

            else if (args[0] == "think")
            {
                if (BRM == null) return "the bot was off";
                SimAvatar avatar = GetSimAvatar();
                if (avatar.IsThinking())
                {
                    avatar.PauseThinking();
                }
                avatar.ThinkOnce();
                return "Think once " + avatar;
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
            if (BRM == null) BRM = new BotRegionModel(Client);
            Avatar self = Client.WorldSystem.GetAvatar(Client.Self.AgentID);
            return BRM.GetSimAvatar(self);
        }

    }
}

