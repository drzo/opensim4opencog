using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class SetMasterKeyCommand : Command
    {
        public DateTime Created = DateTime.Now;

        public SetMasterKeyCommand(cogbot.TextForm testClient)
        {
            Name = "setMasterKey";
            Description = "Sets the key of the master user.  The master user can IM to run commands.";
            Category = CommandCategory.TestClient;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            parent.MasterKey = UUID.Parse(args[0]);

            lock (client.Network.Simulators)
            {
                for (int i = 0; i < client.Network.Simulators.Count; i++)
                {
                    Avatar master = client.Network.Simulators[i].ObjectsAvatars.Find(
                        delegate(Avatar avatar)
                        {
                            return avatar.ID == parent.MasterKey;
                        }
                    );

                    if (master != null)
                    {
                        client.Self.InstantMessage(master.ID,
                            "You are now my master. IM me with \"help\" for a command list.");
                        break;
                    }
                }
            }

            return "Master set to " + parent.MasterKey.ToString();
        }
    }
}
