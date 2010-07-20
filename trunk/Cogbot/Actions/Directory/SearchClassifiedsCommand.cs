using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;

// the Namespace used for all BotClient commands
using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Search
{
    class SearchClassifiedsCommand : Command, GridMasterCommand
    {
        AutoResetEvent waitQuery = new AutoResetEvent(false);
        int resultCount;

        public SearchClassifiedsCommand(BotClient testClient)
        {
            Name = "searchclassifieds";
            Description = "Searches Classified Ads. Usage: searchclassifieds [search text]";
            Category = CommandCategory.Other;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();// " searchclassifieds [search text]";

            string searchText = string.Empty;
            for (int i = 0; i < args.Length; i++)
                searchText += args[i] + " ";
            searchText = searchText.TrimEnd();
            waitQuery.Reset();

            StringBuilder result = new StringBuilder();

            EventHandler<DirClassifiedsReplyEventArgs> callback = delegate(object sender, DirClassifiedsReplyEventArgs e)
            {
                result.AppendFormat("Your search string '{0}' returned {1} classified ads" + Environment.NewLine,
                    searchText, e.Classifieds.Count);
                foreach (DirectoryManager.Classified ad in e.Classifieds)
                {
                    result.AppendLine(ad.ToString());
                }

                // classifieds are sent 16 ads at a time
                if (e.Classifieds.Count < 16)
                {
                    waitQuery.Set();
                }
            };

            Client.Directory.DirClassifiedsReply += callback;

            UUID searchID = Client.Directory.StartClassifiedSearch(searchText, DirectoryManager.ClassifiedCategories.Any, DirectoryManager.ClassifiedQueryFlags.Mature | DirectoryManager.ClassifiedQueryFlags.PG);

            if (!waitQuery.WaitOne(20000, false) && Client.Network.Connected)
            {
                result.AppendLine("Timeout waiting for simulator to respond to query.");
            }

            Client.Directory.DirClassifiedsReply -= callback;

            return Success(result.ToString()); ;
        }
    }
}
