using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class WhoCommand: Command, RegionMasterCommand
    {
        public WhoCommand(BotClient testClient)
		{
			Name = "who";
			Description = "Lists seen avatars.";
            Category = CommandCategory.Other;
            Parameters = new [] {  new NamedParam(typeof(GridClient), null) };
		}

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
		{
			StringBuilder result = new StringBuilder();

            lock (Client.Network.Simulators)
            {
                for (int i = 0; i < Client.Network.Simulators.Count; i++)
                {
                    if (Client.Network.Simulators[i].ObjectsAvatars.Count==0) continue;
                    result.AppendLine();
                    result.Append("Region: " + Client.Network.Simulators[i]);
                    Client.Network.Simulators[i].ObjectsAvatars.ForEach(
                        delegate(Avatar av)
                        {
                            if (string.IsNullOrEmpty(av.Name))
                            {
                                Client.Objects.SelectObjects(Client.Network.Simulators[i], new uint[] { av.LocalID });
                            }
                            result.AppendLine();
                            result.AppendFormat(" {0} (Group: {1}, Location: {2}, UUID: {3})",
                                av.Name, av.GroupName, av.Position, av.ID.ToString());
                        }
                    );
                }
            }

            return Success(result.ToString());;
		}
    }
}
