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

        public override void acceptInput(string verb, Parser args)
        {
           // base.acceptInput(verb, args);

            Avatar avatar;

            BotClient chat = TheBotClient;//.WorldSystem;
			
			if (args.str=="all") {
                chat.muted = !chat.muted;  // inverse mute
				if (chat.muted) WriteLine("All conversation muted");
				else WriteLine("All conversation unmuted");
			} 
			else if ((WorldSystem).tryGetAvatar(args.str, out avatar))
            {
               // Listeners.Chat chat = (Listeners.Chat)Client.listeners["chat"];
                if (chat.muteList.Contains(avatar.Name))
                {
                    WriteLine("Unmuted " + avatar.Name + ".");
                    chat.muteList.Remove(avatar.Name);
                }
                else
                {
                    WriteLine("Muted " + avatar.Name + ".");
                    chat.muteList.Add(avatar.Name);
                }
            }
            else
            {
                WriteLine("I don't know who " + args.str + " is.");
            }
        }
    }
}
