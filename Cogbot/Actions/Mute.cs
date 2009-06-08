using System;
using System.Collections.Generic;
using System.Text;

using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Mute : Action
    {
        public Mute(BotClient Client)
            : base(Client)
        {
            helpString = "Toggle Mute or unmute a user";
            usageString = "To Mute an avatar, type \"Mute <avatar name>\"; to Mute all, type \"mute all\" \r\n" +
                          "To Unmute an avatar, type \"Mute <avatar name>\" again; to Unmute all, type \"mute all\" again";
        }

        public override string acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
           // base.acceptInput(verb, args);

            Avatar avatar;

            BotClient chat = TheBotClient;//.WorldSystem;
			
			if (args.str=="all") {
                chat.muted = !chat.muted;  // inverse mute
				if (chat.muted) return("All conversation muted");
				else return("All conversation unmuted");
			} 
			else if ((WorldSystem).tryGetAvatar(args.str, out avatar))
            {
               // Listeners.Chat chat = (Listeners.Chat)Client.registrationTypes["chat"];
                if (chat.muteList.Contains(avatar.Name))
                {
                    chat.muteList.Remove(avatar.Name);
                    return("Unmuted " + avatar.Name + ".");
                }
                else
                {
                    chat.muteList.Add(avatar.Name);
                    return("Muted " + avatar.Name + ".");
                }
            }
            else
            {
                return("I don't know who " + args.str + " is.");
            }
        }
    }
}
