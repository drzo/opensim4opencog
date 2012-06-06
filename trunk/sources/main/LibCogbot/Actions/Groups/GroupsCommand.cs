using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;
using System.Text;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Groups
{
    public class GroupsCommand : Command, BotPersonalCommand
    {        
        public GroupsCommand(BotClient testClient)
        {
            Name = "groups";
            Description = "List avatar groups. Usage: groups";
            Category = CommandCategory.Groups;
            Details = AddUsage(Name, Description);
            Parameters = CreateParams();
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            Client.ReloadGroupsCache();
            return getGroupsString();
        }

        CmdResult getGroupsString()
        {
            if (null == Client.GroupsCache)
                    return Failure( "Groups cache failed.");
            if (0 == Client.GroupsCache.Count)
                    return Success("No groups");
            StringBuilder sb = new StringBuilder();
            sb.AppendLine("got "+Client.GroupsCache.Count +" groups:");
            foreach (Group group in Client.GroupsCache.Values)
            {
                sb.AppendLine(group.ID + ", " + group.Name);
                
            }
            
            return Success(sb.ToString());
        }
    }
}
