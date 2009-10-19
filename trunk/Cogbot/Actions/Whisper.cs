using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheOpenSims; //using libsecondlife;

namespace cogbot.Actions
{
    class Whisper : Command, BotPersonalCommand
    {
        public UUID currentAvatar;
        public UUID currentSession;

        public Whisper(BotClient Client)
            : base(Client)
        {
            Description = "Whisper a message to a user.";
            Usage = "To whisper a message to an avatar, type \"whisper to <avatar name>\"";

            currentAvatar = UUID.Zero;
            currentSession = UUID.Zero;
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            //base.acceptInput(verb, args);

            string to = args.prepPhrases["to"];

            if (to.Length > 0) {
                SimAvatar avatar;
                if (!WorldSystem.tryGetAvatar(to, out avatar))
                {
                    return Failure("I don't know who " + to + "is.");
                }
                currentAvatar = avatar.ID;
            }
            else if (currentAvatar == UUID.Zero)
            {
                return Failure("Please provide a name to whisper to.");
            }

            if (currentSession != UUID.Zero)
                Client.Self.InstantMessage(currentAvatar, args.objectPhrase, currentSession);
            else
                Client.Self.InstantMessage(currentAvatar, args.objectPhrase);
            return Success("sent message");
        }
    }
}
