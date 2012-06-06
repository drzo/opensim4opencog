using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;

// the Namespace used for all BotClient commands
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Search
{
    class SearchClassifiedsCommand : Command, GridMasterCommand
    {
        AutoResetEvent waitQuery = new AutoResetEvent(false);
        int resultCount;

        public SearchClassifiedsCommand(BotClient testClient)
        {
            Name = "searchclassifieds";
            Description = "Searches Classified Ads.";
            Details = AddUsage(Name + " [search text]", "searches " + Name.Replace("seaches", ""));
            Category = CommandCategory.Search;
            Parameters =
                CreateParams("searchText", typeof (string), "what you are searching for");
            ResultMap = CreateParams(
                "result", typeof (List<string>), "search results",
                "message", typeof (string), "if success was false, the reason why",
                "success", typeof (bool), "true if command was successful");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
                return ShowUsage();// " searchclassifieds [search text]";

            string searchText = string.Empty;
            for (int i = 0; i < args.Length; i++)
                searchText += args[i] + " ";
            searchText = searchText.TrimEnd();
            waitQuery.Reset();

            var result = new List<string>();
            EventHandler<DirClassifiedsReplyEventArgs> callback = delegate(object sender, DirClassifiedsReplyEventArgs e)
            {
                WriteLine("Your search string '{0}' returned {1} classified ads" + Environment.NewLine,
                    searchText, e.Classifieds.Count);
                foreach (DirectoryManager.Classified ad in e.Classifieds)
                {
                    result.Add(ad.ToString());
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
                WriteLine("Timeout waiting for simulator to respond to query.");
            }
            Results["result"] = result;
            Client.Directory.DirClassifiedsReply -= callback;
            return Success("search yeilded " + result.Count);
        }
    }
}
