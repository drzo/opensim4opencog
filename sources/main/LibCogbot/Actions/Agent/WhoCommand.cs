using System;
using System.Collections.Generic;
using System.Text;
using cogbot.Listeners;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Agent
{
    public class WhoCommand: Command, RegionMasterCommand
    {
        public WhoCommand(BotClient testClient)
		{
			Name = "Who";
			Description = "Lists seen avatars.";
            Category = CommandCategory.Other;
            Parameters = new [] {  new NamedParam(typeof(GridClient), null) };
		}

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
		{
			StringBuilder result = new StringBuilder();
            if (args.Length > 0)
            {
                foreach(var A in  WorldObjects.SimAvatars)
                {
                    result.AppendLine(A.ToString());
                }
            }
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
                                Client.Objects.SelectObjects(Client.Network.Simulators[i], new uint[] { av.LocalID }, true);
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
