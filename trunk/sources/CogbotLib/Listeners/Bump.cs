using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Listeners
{
    class Bump : Listener
    {
        public Bump(BotClient parent)
            : base(parent)
        {
            client.Self.OnMeanCollision += new AgentManager.MeanCollisionCallback(Self_OnMeanCollision);
        }

        void Self_OnMeanCollision(MeanCollisionType type, UUID perp, UUID victim, float magnitude, DateTime time)
        {
            BotClient parent = client;
            Avatar perpAv, victimAv;
            Listeners.Avatars avatars = (Listeners.Avatars)parent.listeners["avatars"];
            if (avatars.tryGetAvatarById(perp, out perpAv) && avatars.tryGetAvatarById(victim, out victimAv))
            {
                if (victimAv.Name == client.Self.Name)
                    parent.WriteLine(perpAv.Name + " bumped into you.");
                else if (perpAv.Name == client.Self.Name)
                    parent.WriteLine("$bot bumped into " + victimAv.Name + ".");

                parent.enqueueLispTask("(on-meanCollision '(" + perpAv.Name + ") '(" + victimAv.Name + ") )");

            }
        }
    }
}
