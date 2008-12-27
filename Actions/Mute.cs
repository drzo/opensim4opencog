using System;
using System.Collections.Generic;
using System.Text;

using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Mute : Action
    {
        public Mute(TextForm parent)
            : base(parent)
        {
            helpString = "Toggle Mute or unmute a user";
            usageString = "To Mute an avatar, type \"Mute <avatar name>\"; to Mute all, type \"mute all\" \r\n" +
                          "To Unmute an avatar, type \"Mute <avatar name>\" again; to Unmute all, type \"mute all\" again";
        }

        public override void acceptInput(string verb, Parser args)
        {
           // base.acceptInput(verb, args);

            Avatar avatar;
			Listeners.Chat chat = (Listeners.Chat)parent.listeners["chat"];
			if (args.str=="all") {
			    chat.muted = !chat.muted;  // inverse mute
				if (chat.muted) parent.output("All conversation muted");
				else parent.output("All conversation unmuted");
			} 
			else if (((Listeners.Avatars)parent.listeners["avatars"]).tryGetAvatar(args.str, out avatar))
            {
               // Listeners.Chat chat = (Listeners.Chat)parent.listeners["chat"];
                if (chat.muteList.Contains(avatar.Name))
                {
                    parent.output("Unmuted " + avatar.Name + ".");
                    chat.muteList.Remove(avatar.Name);
                }
                else
                {
                    parent.output("Muted " + avatar.Name + ".");
                    chat.muteList.Add(avatar.Name);
                }
            }
            else
            {
                parent.output("I don't know who " + args.str + " is.");
            }
        }
    }
}
