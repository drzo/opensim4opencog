using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Listeners
{
    public class Teleport : Listener
    {
        public Teleport(BotClient parent)
            : base(parent)
        {
            client.Self.OnTeleport += new AgentManager.TeleportCallback(Self_OnTeleport);
        }

        void Self_OnTeleport(string message, TeleportStatus status, TeleportFlags flags)
        {
            BotClient parent = client;
            if (status == TeleportStatus.Failed)
            {
                parent.WriteLine("Teleport failed.");
                parent.describeSituation();
            }
            else if (status == TeleportStatus.Finished)
            {
                parent.WriteLine("Teleport succeeded.");
                parent.describeSituation();
            }
        }

    }
}
