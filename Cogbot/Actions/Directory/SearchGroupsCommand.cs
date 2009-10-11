using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

// the Namespace used for all BotClient commands
namespace cogbot.Actions
{
    class SearchGroupsCommand : Command
    {
        System.Threading.AutoResetEvent waitQuery = new System.Threading.AutoResetEvent(false);
        int resultCount = 0;

        public SearchGroupsCommand(BotClient testClient)
        {
            Name = "searchgroups";
            Description = "Searches groups. Usage: searchgroups [search text]";
            Category = CommandCategory.Groups;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            // process command line arguments
            if (args.Length < 1)
                return ShowUsage();

            string searchText = string.Empty;
            for (int i = 0; i < args.Length; i++)
                searchText += args[i] + " ";
            searchText = searchText.TrimEnd();

            waitQuery.Reset();

            Client.Directory.DirGroupsReply += Directory_DirGroups;
            
            // send the request to the directory manager
            Client.Directory.StartGroupSearch(searchText, 0);

            try
            {
                if (waitQuery.WaitOne(20000, false) && Client.Network.Connected)
                {
                    return Success("Your query '" + searchText + "' matched " + resultCount + " Events. ");
                }
                else
                {
                    return Failure("Timeout waiting for simulator to respond.");
                }

            }
            finally
            {
                Client.Directory.DirGroupsReply -= Directory_DirGroups;
            }
        }

        void Directory_DirGroups(object sender, DirGroupsReplyEventArgs e)
        {
            if (e.MatchedGroups.Count > 0)
            {
                foreach (DirectoryManager.GroupSearchData group in e.MatchedGroups)
                {
                    Console.WriteLine("Group {1} ({0}) has {2} members", group.GroupID, group.GroupName, group.Members);
                }
            }
            else
            {
                Console.WriteLine("Didn't find any groups that matched your query :(");
            }
            waitQuery.Set();
        }        
    }
}
