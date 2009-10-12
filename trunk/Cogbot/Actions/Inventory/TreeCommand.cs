using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class TreeCommand: Command
    {
        public TreeCommand(BotClient testClient)
		{
			Name = "tree";
			Description = "Rez a tree 3 meters overhead.";
            string usage = "Usage: !tree [";
            foreach (string value in Enum.GetNames(typeof(Tree)))
            {
                usage += value + ",";
            }
            usage = usage.TrimEnd(new char[] { ',' });
            usage += "]";
            Usage = usage;
            Category = CommandCategory.Objects;
		}

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
		{
		    if (args.Length > 0)
		    {
		        try
		        {
		            string treeName = args[0].Trim(new char[] { ' ' });
		            Tree tree = (Tree)EnumParse(typeof(Tree), treeName);

		            Vector3 treePosition = GetSimPosition();
		            treePosition.Z += 3.0f;

		            Client.Objects.AddTree(Client.Network.CurrentSim, new Vector3(0.5f, 0.5f, 0.5f),
		                Quaternion.Identity, treePosition, tree, TheBotClient.GroupID, false);

		            return Success("Attempted to rez a " + treeName + " tree");
		        }
		        catch (Exception e)
		        {
		            return Failure("" + e);
		        }
		    }
            return Failure(Usage);
		}
    }
}
