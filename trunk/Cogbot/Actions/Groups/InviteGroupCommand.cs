using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;
using System.Text;

namespace cogbot.Actions.Groups
{
    public class InviteGroupCommand : Command, BotPersonalCommand
    {
        public InviteGroupCommand(BotClient testClient)
        {
            Name = "invitegroup";
            Description = "invite an avatar into a group. Usage: invitegroup AvatarUUID GroupUUID RoleUUID*";
            Category = CommandCategory.Groups;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
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
