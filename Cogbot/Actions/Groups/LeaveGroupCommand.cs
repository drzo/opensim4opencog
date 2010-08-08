using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;
using System.Text;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Groups
{
    public class LeaveGroupCommand : Command, BotPersonalCommand
    {
        ManualResetEvent GroupsEvent = new ManualResetEvent(false);
        private bool leftGroup;

        public LeaveGroupCommand(BotClient testClient)
        {
            Name = "leavegroup";
            Description = "Leave a group. Usage: leavegroup GroupName";
            Category = CommandCategory.Groups;
        }
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();

            string groupName = String.Empty;
            for (int i = 0; i < args.Length; i++)
                groupName += args[i] + " ";
            groupName = groupName.Trim();

            UUID groupUUID = Client.GroupName2UUID(groupName);
            if (UUID.Zero != groupUUID) {
                Client.Groups.GroupLeaveReply += Groups_OnGroupLeft;
                Client.Groups.LeaveGroup(groupUUID);

                GroupsEvent.WaitOne(30000, false);
                Client.Groups.GroupLeaveReply -= Groups_OnGroupLeft;

                GroupsEvent.Reset();
                Client.ReloadGroupsCache();

                if (leftGroup)
                    return Success(Client.ToString() + " has left the group " + groupName);
                return Failure("failed to leave the group " + groupName);
            }
            return Failure(Client.ToString() + " doesn't seem to be member of the group " + groupName);
        }


        void Groups_OnGroupLeft(object sender, GroupOperationEventArgs e)
        {
            WriteLine(Client.ToString() + (e.Success ? " has left group " : " failed to left group ") + e.GroupID.ToString());

            leftGroup = e.Success;
            GroupsEvent.Set();
        }
    }
}
