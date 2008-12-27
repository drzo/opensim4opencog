using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class WhoCommand: Command
    {
        public WhoCommand(cogbot.TextForm testClient)
		{
			Name = "who";
			Description = "Lists seen avatars.";
            Category = CommandCategory.Other;
		}

        public override string Execute(string[] args, UUID fromAgentID)
		{
			StringBuilder result = new StringBuilder();

            lock (client.Network.Simulators)
            {
                for (int i = 0; i < client.Network.Simulators.Count; i++)
                {
                    client.Network.Simulators[i].ObjectsAvatars.ForEach(
                        delegate(Avatar av)
                        {
                            result.AppendLine();
                            result.AppendFormat("{0} (Group: {1}, Location: {2}, UUID: {3})",
                                av.Name, av.GroupName, av.Position, av.ID.ToString());
                        }
                    );
                }
            }

            return result.ToString();
		}
    }
}
