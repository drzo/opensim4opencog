using System;
using OpenMetaverse;

namespace cogbot.Actions.Groups
{
    /// <summary>
    /// Shows group info Dialog using Radegast UI
    /// </summary>
    public class GroupInfoCommand : Command, GridMasterCommand
    {
        private string GroupName;
        private UUID GroupUUID;

        public GroupInfoCommand(BotClient testClient)
        {
            Name = "groupinfo";
            Description = "Shows the group UI. Usage: groupinfo GroupName";
            Category = CommandCategory.Groups;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();

            GroupName = String.Empty;
            for (int i = 0; i < args.Length; i++)
                GroupName += args[i] + " ";
            GroupName = GroupName.Trim();

            GroupUUID = Client.GroupName2UUID(GroupName);
            if (UUID.Zero != GroupUUID)
            {
                TheBotClient.Invoke(() =>
                {
                    Group group = new Group { ID = GroupUUID };
                    TheBotClient.TheRadegastInstance.MainForm.ShowGroupProfile(group);
                });

                return Failure("Shown group " + GroupName + " " + GroupUUID);
            }
            return Failure("Cannot find group " + GroupName);
        }
    }
}
