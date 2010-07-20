using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Appearance
{
    public class CloneCommand : Command, BotPersonalCommand
    {
        uint SerialNum = 2;

        public CloneCommand(BotClient testClient)
        {
            Name = "clone";
            Description = "Clone the appearance of a nearby avatar. Usage: clone [name]";
            Category = CommandCategory.Appearance;
            Parameters = new[] { new NamedParam(typeof(SimAvatar), typeof(UUID)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            string targetName = String.Empty;
            bool detatchAll = false;
            for (int ct = 0; ct < args.Length; ct++)
                targetName = targetName + args[ct] + " ";
            targetName = targetName.TrimEnd();

            if (targetName.Length == 0)
                return ShowUsage();// " clone [name]";
            UUID target = WorldSystem.GetUserID(targetName);

            //            if (Client.Directory.PeopleSearch(DirectoryManager.DirFindFlags.People, targetName, 0, 1000 * 10,
            //                out matches) && matches.Count > 0)
            if (target != UUID.Zero)
            {
                //   UUID target = matches[0].AgentID;
                targetName += String.Format(" ({0})", target);

                if (Client.Appearances.ContainsKey(target))
                {
                    #region AvatarAppearance to AgentSetAppearance

                    AvatarAppearancePacket appearance = TheBotClient.Appearances[target];

                    AgentSetAppearancePacket set = Client.Appearance.MakeAppearancePacket();
                    set.AgentData.AgentID = Client.Self.AgentID;
                    set.AgentData.SessionID = Client.Self.SessionID;
                    set.AgentData.SerialNum = SerialNum++;
                    set.AgentData.Size = new Vector3(2f, 2f, 2f); // HACK

                    set.WearableData = new AgentSetAppearancePacket.WearableDataBlock[0];
                    set.VisualParam = new AgentSetAppearancePacket.VisualParamBlock[appearance.VisualParam.Length];

                    for (int i = 0; i < appearance.VisualParam.Length; i++)
                    {
                        set.VisualParam[i] = new AgentSetAppearancePacket.VisualParamBlock();
                        set.VisualParam[i].ParamValue = appearance.VisualParam[i].ParamValue;
                    }

                    set.ObjectData.TextureEntry = appearance.ObjectData.TextureEntry;

                    #endregion AvatarAppearance to AgentSetAppearance

                    // Detach everything we are currently wearing
                    if (detatchAll) Client.Appearance.AddAttachments(new List<InventoryItem>(), true);

                    // Send the new appearance packet
                    Client.Network.SendPacket(set);

                    return Success("Cloned " + targetName);
                }
                else
                {
                    /// allow clone thyself
                    if (Client.Self.AgentID==target)
                    {
                        AgentSetAppearancePacket set = Client.Appearance.MakeAppearancePacket();

                        Client.Network.SendPacket(set);
                        Logger.DebugLog("Send AgentSetAppearance packet");

                        return Success("Cloned " + targetName);
                    }
                    return Failure("Don't know the appearance of avatar " + targetName);
                }
            }
            else
            {
                return Failure("Couldn't find avatar " + targetName);
            }
        }
    }
}
