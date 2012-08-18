using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Search
{
    class SearchEventsCommand : Command, GridMasterCommand
    {
        AutoResetEvent waitQuery = new AutoResetEvent(false);
        int resultCount;

        public SearchEventsCommand(BotClient testClient)
        {
            Name = "searchevents";
            TheBotClient = testClient;
        }

        override public void MakeInfo()
        {
            Description = "Searches Events list.";
            Details = AddUsage(Name + " [search text]", "searches " + Name.Replace("seaches", ""));
            Category = CommandCategory.Search;
            Parameters =
                CreateParams("searchText", typeof(string), "what you are searching for");
            ResultMap = CreateParams(
                "result", typeof(List<string>), "search results",
                "message", typeof(string), "if success was false, the reason why",
                "success", typeof(bool), "true if command was successful");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
                return ShowUsage();// " searchevents [search text]";

            string searchText = string.Empty;
            for (int i = 0; i < args.Length; i++)
                searchText += args[i] + " ";
            searchText = searchText.TrimEnd();

            waitQuery.Reset();

            Client.Directory.DirEventsReply += Directory_DirEvents;

            // send the request to the directory manager
            Client.Directory.StartEventsSearch(searchText, 0);

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
                Client.Directory.DirEventsReply -= Directory_DirEvents;
            }
        }

        void Directory_DirEvents(object sender, DirEventsReplyEventArgs e)
        {
            if (e.MatchedEvents[0].ID == 0 && e.MatchedEvents.Count == 1)
            {
                WriteLine("No Results matched your search string");
            }
            else
            {
                foreach (DirectoryManager.EventsSearchData ev in e.MatchedEvents)
                {
                    WriteLine("Event ID: {0} Event Name: {1} Event Date: {2}", ev.ID, ev.Name, ev.Date);
                }
            }
            resultCount = e.MatchedEvents.Count;
            waitQuery.Set();
        }
    }
}
