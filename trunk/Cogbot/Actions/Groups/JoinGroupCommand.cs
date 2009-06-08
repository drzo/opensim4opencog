using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;
using System.Text;

namespace cogbot.Actions
{
    public class JoinGroupCommand : Command
    {

        public JoinGroupCommand(BotClient testClient)
        {
            Name = "joingroup";
            Description = "join a group. Usage: joingroup GroupName | joingroup UUID GroupId";
            Category = CommandCategory.Groups;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            UUID queryID = UUID.Zero;
            bool joinedGroup = false;

            if (args.Length < 1)
                return Description;

            string groupName = String.Empty;
            UUID resolvedGroupID = UUID.Zero;
            string resolvedGroupName = String.Empty;

            if (args[0].ToLower() == "uuid")
            {
                if (args.Length < 2)
                    return Description;

                if (!UUIDTryParse((resolvedGroupName = groupName = args[1]), out resolvedGroupID))
                    return resolvedGroupName + " doesn't seem a valid UUID";
            }

            List<DirectoryManager.GroupSearchData> matchedgroups = null;

            ManualResetEvent GetGroupsSearchEvent = new ManualResetEvent(false);

            for (int i = 0; i < args.Length; i++)
                groupName += args[i] + " ";
            groupName = groupName.Trim();
            DirectoryManager.DirGroupsReplyCallback
                callback = new DirectoryManager.DirGroupsReplyCallback(
                    (queryid, matchedgroups0) =>
                    {
                        if (queryID == queryid)
                        {
                            matchedgroups = matchedgroups0;
                            try
                            {
                                GetGroupsSearchEvent.Set();
                            }
                            catch (Exception)
                            {
                            }
                        }
                    });
            Client.Directory.OnDirGroupsReply += callback;
            queryID = Client.Directory.StartGroupSearch(DirectoryManager.DirFindFlags.Groups, groupName, 0);

            GetGroupsSearchEvent.WaitOne(60000, false);
            Client.Directory.OnDirGroupsReply -= callback;

            if (matchedgroups == null || matchedgroups.Count == 0)
                return "ERROR: Got an empty reply";

            /* A.Biondi 
             * The Group search doesn't work as someone could expect...
             * It'll give back to you a long list of groups even if the 
             * searchText (groupName) matches esactly one of the groups 
             * names present on the server, so we need to check each result.
             * UUIDs of the matching groups are written on the console.
             */

            WriteLine("Matching groups are:\n");
            foreach (DirectoryManager.GroupSearchData groupRetrieved in matchedgroups)
            {
                WriteLine("{0}\t\t\t({1} UUID {2})", groupRetrieved.GroupName, Name, groupRetrieved.GroupID.ToString());
                if (groupRetrieved.GroupName.ToLower() == groupName.ToLower())
                {
                    resolvedGroupID = groupRetrieved.GroupID;
                    resolvedGroupName = groupRetrieved.GroupName;
                    break;
                }
            }
            if (string.IsNullOrEmpty(resolvedGroupName))
                return "Ambiguous name. Found " + matchedgroups.Count.ToString() +
                                    " groups (UUIDs on console)";




            if (resolvedGroupID == UUID.Zero)
            {
                if (string.IsNullOrEmpty(resolvedGroupName))
                    return "Unable to obtain UUID for group " + groupName;
                else
                    return resolvedGroupName;
            }

            GetGroupsSearchEvent.Reset();


            GroupManager.GroupJoinedCallback gcallback = new GroupManager.GroupJoinedCallback((groupid, success) =>
            {
                WriteLine(Client.ToString() + (success ? " joined " : " failed to join ") + groupid.ToString());
                /* A.Biondi 
                                * This code is not necessary because it is yet present in the 
                                * GroupCommand.cs as well. So the new group will be activated by 
                                * the mentioned command. If the GroupCommand.cs would change, 
                                * just uncomment the following two lines.
            
                                if (success)
                                {
                                WriteLine(Client.ToString() + " setting " + groupID.ToString() + " as the active group");
                                Client.Groups.ActivateGroup(groupID);
                                }
            
                                */
                joinedGroup = success;
                GetGroupsSearchEvent.Set();
            });
            Client.Groups.OnGroupJoined += gcallback;
            Client.Groups.RequestJoinGroup(resolvedGroupID);
            /* A.Biondi 
                         * TODO: implement the pay to join procedure.
                         */
            GetGroupsSearchEvent.WaitOne(60000, false);
            Client.Groups.OnGroupJoined -= gcallback;
            GetGroupsSearchEvent.Reset();

            if (joinedGroup)
                return "Joined the group " + resolvedGroupName;
            return "Unable to join the group " + resolvedGroupName;
        }
    }
}
