using System;
using System.Collections.Generic;
using System.Text;
using cogbot.Listeners;
using MushDLR223.Utilities;
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
            Parameters = CreateParams();
            ResultMap = CreateParams(
                "avatarList", typeof (List<Avatar>), "list of present avatars",
                "message", typeof(string), "if success was false, the reason why",
                "success", typeof (bool), "true if command was successful");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
		{
			StringBuilder result = new StringBuilder();
            if (args.Length > 0)
            {
                foreach(var A in  WorldObjects.SimAvatars)
                {
                    result.AppendLine(A.ToString());
                }
            }
            {
                foreach (Simulator sim in LockInfo.CopyOf(Client.Network.Simulators))
                {
                    if (sim.ObjectsAvatars.Count==0) continue;
                    result.AppendLine();
                    result.Append("Region: " + sim);
                    sim.ObjectsAvatars.ForEach(
                        delegate(Avatar av)
                        {
                            if (string.IsNullOrEmpty(av.Name))
                            {
                                Client.Objects.SelectObjects(sim, new uint[] { av.LocalID }, true);
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
