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

        public Whisper(TextForm parent)
            : base(parent)
        {
            helpString = "Whisper a message to a user.";
            usageString = "To whisper a message to an avatar, type \"whisper to <avatar name>\"";

            currentAvatar = UUID.Zero;
            currentSession = UUID.Zero;
        }

        public override void acceptInput(string verb, Parser args)
        {
            base.acceptInput(verb, args);

            string to = args.prepPhrases["to"];

            if (to.Length > 0) {
                Avatar avatar;
                Listeners.Avatars avatars = (Listeners.Avatars)parent.listeners["avatars"];
                if (!avatars.tryGetAvatar(to, out avatar))
                {
                    parent.output("I don't know who " + to + "is.");
                    return;
                }
                currentAvatar = avatar.ID;
            }
            else if (currentAvatar == UUID.Zero)
            {
                parent.output("Please provide a name to whisper to.");
                return;
            }

            if (currentSession != UUID.Zero)
                client.Self.InstantMessage(currentAvatar, args.objectPhrase, currentSession);
            else
                client.Self.InstantMessage(currentAvatar, args.objectPhrase);
        }
    }
}
