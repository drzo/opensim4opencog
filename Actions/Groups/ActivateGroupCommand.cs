using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;
using System.Text;

namespace cogbot.Actions
{
    /// <summary>
    /// Changes Avatars currently active group
    /// </summary>
    public class ActivateGroupCommand : Command
    {
        ManualResetEvent GroupsEvent = new ManualResetEvent(false);
        Dictionary<UUID, Group> groups = new Dictionary<UUID, Group>();
        string activeGroup;

        public ActivateGroupCommand(cogbot.TextForm testClient)
        {
            Name = "activategroup";
            Description = "Set a group as active. Usage: activategroup GroupName";
            Category = CommandCategory.Groups;
        }
        public override string Execute(string[] args, UUID fromAgentID)
        {
            if (args.Length < 1)
                return Description;

            groups.Clear();
            activeGroup = string.Empty;

            string groupName = String.Empty;
            for (int i = 0; i < args.Length; i++)
                groupName += args[i] + " ";
            groupName = groupName.Trim();

            GroupManager.CurrentGroupsCallback callback = new GroupManager.CurrentGroupsCallback(Groups_OnCurrentGroups);
            client.Groups.OnCurrentGroups += callback;
            client.Groups.RequestCurrentGroups();

            GroupsEvent.WaitOne(30000, false);

            client.Groups.OnCurrentGroups -= callback;
            GroupsEvent.Reset();

            if (groups.Count > 0)
            {
                foreach (Group currentGroup in groups.Values)
                    if (currentGroup.Name.ToLower() == groupName.ToLower())
                    {
                        NetworkManager.PacketCallback pcallback = new NetworkManager.PacketCallback(AgentDataUpdateHandler);
                        client.Network.RegisterCallback(PacketType.AgentDataUpdate, pcallback);

                        WriteLine("setting " + currentGroup.Name + " as active group");
                        client.Groups.ActivateGroup(currentGroup.ID);
                        GroupsEvent.WaitOne(30000, false);

                        client.Network.UnregisterCallback(PacketType.AgentDataUpdate, pcallback);
                        GroupsEvent.Reset();

                        /* A.Biondi 
                         * TODO: Handle titles choosing.
                         */

                        if (String.IsNullOrEmpty(activeGroup))
                            return client.ToString() + " failed to activate the group " + groupName;

                        return "Active group is now " + activeGroup;
                    }
                return client.ToString() + " doesn't seem to be member of the group " + groupName;
            }

            return client.ToString() + " doesn't seem member of any group";
        }

        void Groups_OnCurrentGroups(Dictionary<UUID, Group> cGroups)
        {
            groups = cGroups;
            GroupsEvent.Set();
        }

        private void AgentDataUpdateHandler(Packet packet, Simulator sim)
        {
            AgentDataUpdatePacket p = (AgentDataUpdatePacket)packet;
            if (p.AgentData.AgentID == client.Self.AgentID)
            {
                activeGroup = Utils.BytesToString(p.AgentData.GroupName) + " ( " + Utils.BytesToString(p.AgentData.GroupTitle) + " )";
                GroupsEvent.Set();
            }
        }
    }
}
