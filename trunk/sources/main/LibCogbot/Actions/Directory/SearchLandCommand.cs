using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;
// the Namespace used for all BotClient commands
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Search
{
    /// <summary>
    /// 
    /// </summary>
    public class SearchLandCommand : Command, GridMasterCommand
    {
        private AutoResetEvent waitQuery = new AutoResetEvent(false);
        private StringBuilder result = new StringBuilder();

        /// <summary>
        /// Construct a new instance of the SearchLandCommand
        /// </summary>
        /// <param name="testClient"></param>
        public SearchLandCommand(BotClient testClient)
        {
            Name = "searchland";
            TheBotClient = testClient;
        }

        public override void MakeInfo()
        {
            Description = "Searches for land for sale. for usage information type: searchland";
            Category = CommandCategory.Search;
            Details = AddUsage(Name + " [type] [max price] [min size]",
                               "\twhere [type] is one of: mainland, auction, estate, all\n" +
                               "\tif [max price] or [min size] are 0 that parameter will be ignored") +
                      AddUsage(Name + " mainland 0 512",
                               "shows the lowest priced mainland that is larger than 512/m2");
            Parameters =
                CreateParams(
                    "searchText", typeof (DirectoryManager.SearchTypeFlags), "search type flags",
                    "maxPrice", typeof (int), "max price",
                    "minSize", typeof (int), "min size");
            ResultMap = CreateParams(
                "result", typeof (List<string>), "search results",
                "message", typeof (string), "if success was false, the reason why",
                "success", typeof (bool), "true if command was successful");
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="args"></param>
        /// <param name="fromAgentID"></param>
        /// <returns></returns>
        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            // process command line arguments
            if (args.Length < 3)
                return ShowUsage();

            string searchType = args[0].Trim().ToLower();
            int maxPrice;
            int minSize;

            DirectoryManager.SearchTypeFlags searchTypeFlags = DirectoryManager.SearchTypeFlags.Any;

            if (searchType.StartsWith("au"))
                searchTypeFlags = DirectoryManager.SearchTypeFlags.Auction;
            else if (searchType.StartsWith("m"))
                searchTypeFlags = DirectoryManager.SearchTypeFlags.Mainland;
            else if (searchType.StartsWith("e"))
                searchTypeFlags = DirectoryManager.SearchTypeFlags.Estate;
            else if (searchType.StartsWith("al"))
                searchTypeFlags = DirectoryManager.SearchTypeFlags.Any;
            else
                return ShowUsage();

            // initialize some default flags we'll use in the search
            DirectoryManager.DirFindFlags queryFlags = DirectoryManager.DirFindFlags.SortAsc |
                                                       DirectoryManager.DirFindFlags.PerMeterSort
                                                       | DirectoryManager.DirFindFlags.IncludeAdult |
                                                       DirectoryManager.DirFindFlags.IncludePG |
                                                       DirectoryManager.DirFindFlags.IncludeMature;

            // validate the parameters passed
            if (int.TryParse(args[1], out maxPrice) && int.TryParse(args[2], out minSize))
            {
                // if the [max price] parameter is greater than 0, we'll enable the flag to limit by price
                if (maxPrice > 0)
                    queryFlags |= DirectoryManager.DirFindFlags.LimitByPrice;

                // if the [min size] parameter is greater than 0, we'll enable the flag to limit by area
                if (minSize > 0)
                    queryFlags |= DirectoryManager.DirFindFlags.LimitByArea;
            }
            else
            {
                return ShowUsage();
            }

            //waitQuery.Reset();

            // subscribe to the event that returns the search results
            Client.Directory.DirLandReply += Directory_DirLand;

            // send the request to the directory manager
            Client.Directory.StartLandSearch(queryFlags, searchTypeFlags, maxPrice, minSize, 0);

            if (!waitQuery.WaitOne(20000, false) && Client.Network.Connected)
            {
                result.AppendLine("Timeout waiting for simulator to respond.");
            }

            // unsubscribe to the event that returns the search results
            Client.Directory.DirLandReply -= Directory_DirLand;

            // return the results
            return Success(result.ToString());
        }

        /// <summary>
        /// Process the search reply
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void Directory_DirLand(object sender, DirLandReplyEventArgs e)
        {
            foreach (DirectoryManager.DirectoryParcel searchResult in e.DirParcels)
            {
                // add the results to the StringBuilder object that contains the results
                result.AppendLine(searchResult.ToString());
            }
            result.AppendFormat("{0} results" + Environment.NewLine, e.DirParcels.Count);
            // let the calling method know we have data
            waitQuery.Set();
        }
    }
}