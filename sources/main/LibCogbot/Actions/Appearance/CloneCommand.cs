using System;
using System.Collections.Generic;
using System.Threading;
using Cogbot.World;
using OpenMetaverse;
using OpenMetaverse.Packets;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Appearance
{
    public class CloneCommand : Command, BotPersonalCommand
    {
        private uint SerialNum = 2;

        public CloneCommand(BotClient testClient)
        {
            Name = "clone";
            TheBotClient = testClient;
        }

        public override void MakeInfo()
        {
            Description = "Clone the appearance of a nearby avatar.";
            Details = AddUsage(Name + " [agent-spec]", "use $self of OLD self");
            Category = CommandCategory.Appearance;
            Parameters = CreateParams(
                Optional("--detatchall", typeof (bool), "Detatch all first"),
                "target", typeof (AgentSpec), "the agent you wish to see " + Name);
            ResultMap = CreateParams(
                "message", typeof (string), "if success was false, the reason why",
                "success", typeof (bool), "true if command was successful");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            bool detatchAll = args.IsTrue("--detatchall");

            SimAvatar simAv;
            if (args.TryGetValue("target", out simAv))
            {
                UUID target = simAv.ID;
                string targetName = simAv.GetName();
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
                    if (Client.Self.AgentID == target)
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
                return Failure("Couldn't find avatar " + args.str);
            }
        }
    }
}