using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;
using System.Text;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Groups
{
    public class InviteGroupCommand : Command, BotPersonalCommand
    {
        public InviteGroupCommand(BotClient testClient)
        {
            Name = "invitegroup";
            Description = "invite an avatar into a group.";
            Category = CommandCategory.Groups;
            Details = AddUsage(Name + " AvatarUUID GroupUUID RoleUUID", Description);
            Parameters = CreateParams(
                "agent", typeof(Group), "agent you are inviting",
                "groupUUID", typeof(Group), "group uuid",
                "roleUUID", typeof(GroupRole), "group role");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 2)
                return ShowUsage();

            UUID avatar = UUID.Zero;
            UUID group = UUID.Zero;
            UUID role = UUID.Zero;
            List<UUID> roles = new List<UUID>();

            int argUsed;
            if (!UUIDTryParse(args, 0, out avatar, out argUsed))
                return Failure("parse error avatar UUID");
            if (!UUIDTryParse(args, argUsed, out group, out argUsed))
                return Failure("parse error group UUID");
            for (int i = 2; i < args.Length; i++)
                if (UUID.TryParse(args[i], out role))
                    roles.Add(role);
                
            Client.Groups.Invite(group, roles, avatar);

            return Success("invited "+avatar+" to "+group);
        }
    }
}
