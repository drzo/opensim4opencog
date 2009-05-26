using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Listeners
{
    public class Teleport : Listener
    {
        public Teleport(TextForm parent)
            : base(parent)
        {
            client.Self.OnTeleport += new AgentManager.TeleportCallback(Self_OnTeleport);
        }

        void Self_OnTeleport(string message, AgentManager.TeleportStatus status, AgentManager.TeleportFlags flags)
        {
            if (status == AgentManager.TeleportStatus.Failed)
            {
                parent.output("Teleport failed.");
                parent.describeSituation();
            }
            else if (status == AgentManager.TeleportStatus.Finished)
            {
                parent.output("Teleport succeeded.");
                parent.describeSituation();
            }
        }

    }
}
