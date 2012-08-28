using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;
using PathSystem3D.Navigation;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Land
{
    public class TreeCommand : Command, RegionMasterCommand
    {
        public TreeCommand(BotClient testClient)
        {
            Name = "tree";
        }

        public override void MakeInfo()
        {
            Description = "Rez a tree 3 meters overhead.";
            string usage = "Usage: !tree [";
            foreach (string value in Enum.GetNames(typeof (Tree)))
            {
                usage += value + ",";
            }
            usage = usage.TrimEnd(new char[] {','});
            usage += "]";
            Details = usage;
            Category = CommandCategory.Objects;
            Parameters = CreateParams("tree", typeof (Tree), "the Tree foliage type you wish to rez",
                                      Optional("position", typeof (SimPosition),
                                               "the location you wish to create the tree"),
                                      Optional("size", typeof (Vector3), "the size you wish to create the tree"),
                                      Optional("new", typeof (bool), "the size you wish to create the tree"));
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length > 0)
            {
                try
                {
                    string treeName = args[0].Trim(new char[] {' '});
                    Tree tree = (Tree) 0;
                    if (!args.TryGetValue("tree", out tree))
                    {
                        object value;
                        int argsUsed;
                        if (TryEnumParse(typeof (Tree), args, 0, out argsUsed, out value))
                        {
                            tree = (Tree) value;
                        }
                    }

                    Vector3 treePosition = GetSimPosition();
                    treePosition.Z += 3.0f;
                    Vector3 size = new Vector3(0.5f, 0.5f, 0.5f);
                    Client.Objects.AddTree(Client.Network.CurrentSim, size,
                                           Quaternion.Identity, treePosition, tree, TheBotClient.GroupID, false);

                    return Success("Attempted to rez a " + treeName + " tree");
                }
                catch (Exception e)
                {
                    return Failure("" + e);
                }
            }
            return ShowUsage();
        }
    }
}