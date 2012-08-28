using System;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Groups
{
    /// <summary>
    /// Shows group info Dialog using Radegast UI
    /// </summary>
    public class GroupInfoCommand : Command, GridMasterCommand, GUICommand
    {
        private string GroupName;
        private UUID GroupUUID = UUID.Zero;

        public GroupInfoCommand(BotClient testClient)
        {
            Name = "groupinfo";
        }

        public override void MakeInfo()
        {
            Description = "Shows the group UI. Usage: groupinfo GroupName";
            Category = CommandCategory.Groups;
            AddVersion(CreateParams("groupid", typeof(Group), "groupid you are going to " + Name),
                       "shows the profile specified by groupid");

        }

        public override CmdResult ExecuteRequest(CmdRequest args)
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
                TheBotClient.InvokeGUI(() =>
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
