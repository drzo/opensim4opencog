using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Whisper : Action
    {
        public UUID currentAvatar;
        public UUID currentSession;

        public Whisper(BotClient Client)
            : base(Client)
        {
            helpString = "Whisper a message to a user.";
            usageString = "To whisper a message to an avatar, type \"whisper to <avatar name>\"";

            currentAvatar = UUID.Zero;
            currentSession = UUID.Zero;
        }

        public override string acceptInput(string verb, Parser args)
        {
            //base.acceptInput(verb, args);

            string to = args.prepPhrases["to"];

            if (to.Length > 0) {
                Avatar avatar;
                if (!WorldSystem.tryGetAvatar(to, out avatar))
                {
                    return ("I don't know who " + to + "is.");
                }
                currentAvatar = avatar.ID;
            }
            else if (currentAvatar == UUID.Zero)
            {
                return ("Please provide a name to whisper to.");
            }

            if (currentSession != UUID.Zero)
                Client.Self.InstantMessage(currentAvatar, args.objectPhrase, currentSession);
            else
                Client.Self.InstantMessage(currentAvatar, args.objectPhrase);
            return "sent message";
        }
    }
}
