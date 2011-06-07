using System;
using System.Collections.Generic;
using System.Text;
using cogbot.TheOpenSims;
using OpenMetaverse; //using libsecondlife;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions
{
    class Mute : Command, BotPersonalCommand
    {
        public Mute(BotClient Client)
            : base(Client)
        {
            Description = "Toggle Mute or unmute a user";
            Usage = "To Mute an avatar, type \"Mute <avatar name>\"; to Mute all, type \"mute all\" \r\n" +
                          "To Unmute an avatar, type \"Mute <avatar name>\" again; to Unmute all, type \"mute all\" again";
            Parameters = new[] { new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            // base.acceptInput(verb, args);

            SimAvatar avatar;

            BotClient chat = TheBotClient;//.WorldSystem;

            if (args.str == "all")
            {
                chat.muted = !chat.muted;  // inverse mute
                if (chat.muted) return Success("All conversation muted");
                else return Success("All conversation unmuted");
            }
            else if ((WorldSystem).tryGetAvatar(args.str, out avatar))
            {
                // Listeners.Chat chat = (Listeners.Chat)Client.registrationTypes["chat"];
                if (chat.muteList.Contains(avatar.GetName()))
                {
                    chat.muteList.Remove(avatar.GetName());
                    return Success("Unmuted " + avatar.GetName() + ".");
                }
                else
                {
                    chat.muteList.Add(avatar.GetName());
                    return Success("Muted " + avatar.GetName() + ".");
                }
            }
            else
            {
                return Failure("I don't know who " + args.str + " is.");
            }
        }
    }
}
