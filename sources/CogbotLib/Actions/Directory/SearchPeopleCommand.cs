using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;

// the Namespace used for all BotClient commands
using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Search
{
    class SearchPeopleCommand : Command, GridMasterCommand
    {
        AutoResetEvent waitQuery = new AutoResetEvent(false);
        int resultCount = 0;

        public SearchPeopleCommand(BotClient testClient)
        {
            Name = "searchpeople";
            Description = "Searches for other avatars. Usage: searchpeople [search text]";
            Category = CommandCategory.Friends;
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

            
            Client.Directory.DirPeopleReply += Directory_DirPeople;

            // send the request to the directory manager
            Client.Directory.StartPeopleSearch(searchText, 0);

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
                Client.Directory.DirPeopleReply -= Directory_DirPeople;
            }
        }

        void Directory_DirPeople(object sender, DirPeopleReplyEventArgs e)
        {
            if (e.MatchedPeople.Count > 0)
            {
                foreach (DirectoryManager.AgentSearchData agent in e.MatchedPeople)
                {
                    WriteLine("{0} {1} ({2})", agent.FirstName, agent.LastName, agent.AgentID);                   
                }
            }
            else
            {
                WriteLine("Didn't find any people that matched your query :(");
            }
            waitQuery.Set();
        }
    }
}
