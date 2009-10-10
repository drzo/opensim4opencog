using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.Actions
{
    class SearchEventsCommand : Command
    {
        System.Threading.AutoResetEvent waitQuery = new System.Threading.AutoResetEvent(false);
        int resultCount;

        public SearchEventsCommand(BotClient testClient)
        {
            Name = "searchevents";
            Description = "Searches Events list. Usage: searchevents [search text]";
            Category = CommandCategory.Other;
            Parameters = new[] { new NamedParam(typeof(GridClient), null) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return Failure(Usage);// " searchevents [search text]";

            string searchText = string.Empty;
            for (int i = 0; i < args.Length; i++)
                searchText += args[i] + " ";
            searchText = searchText.TrimEnd();
            waitQuery.Reset();

            DirectoryManager.EventReplyCallback cb = new DirectoryManager.EventReplyCallback((queryid, matchedevents) =>
                                                                                          {
                                                                                              if (matchedevents[0].ID == 0 && matchedevents.Count == 1)
                                                                                              {
                                                                                                  WriteLine("No Results matched your search string");
                                                                                              }
                                                                                              else
                                                                                              {
                                                                                                  foreach (DirectoryManager.EventsSearchData ev in matchedevents)
                                                                                                  {
                                                                                                      WriteLine("Event ID: {0} Event Name: {1} Event Date: {2}", ev.ID, ev.Name, ev.Date);
                                                                                                  }
                                                                                              }
                                                                                              resultCount = matchedevents.Count;
                                                                                              waitQuery.Set();
                                                                                          });
            Client.Directory.OnEventsReply += cb;
            Client.Directory.StartEventsSearch(searchText, 0);//, "u", 0, DirectoryManager.EventCategories.All, UUID.Random());
            string result;
            try
            {
                if (waitQuery.WaitOne(20000, false) && Client.Network.Connected)
                {
                    return Success("$bot's query '" + searchText + "' matched " + resultCount + " Events. ");
                }
                else
                {
                    return Failure("Timeout waiting for simulator to respond.");
                }
            }
            finally
            {
                Client.Directory.OnEventsReply -= cb;
            }
        }
    }
}
