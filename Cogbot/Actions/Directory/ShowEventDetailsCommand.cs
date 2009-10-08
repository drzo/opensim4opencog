using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.Actions
{
    class ShowEventDetailsCommand : Command
    {
        public ShowEventDetailsCommand(BotClient testClient)
        {
            Name = "showevent";
            Description = "Shows an Events details. Usage: showevent [eventID]";
            Category = CommandCategory.Other;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return "Usage: showevent [eventID] (use searchevents to get ID)";

            DirectoryManager.EventInfoCallback callback = new DirectoryManager.EventInfoCallback(matchedevent =>
                                                                                                     {
                                                                                                         float x,y;
                                                                                                         Helpers.GlobalPosToRegionHandle((float)matchedevent.GlobalPos.X, (float)matchedevent.GlobalPos.Y, out x, out y);
                                                                                                         StringBuilder sb = new StringBuilder();
                                                                                                         sb.AppendFormat("       Name: {0} ({1})" + System.Environment.NewLine, matchedevent.Name, matchedevent.ID);
                                                                                                         sb.AppendFormat("   Location: {0}/{1}/{2}" + System.Environment.NewLine, matchedevent.SimName, x, y);
                                                                                                         sb.AppendFormat("       Date: {0}" + System.Environment.NewLine, matchedevent.Date);
                                                                                                         sb.AppendFormat("Description: {0}" + System.Environment.NewLine, matchedevent.Desc);
                                                                                                         WriteLine(sb.ToString());
                                                                                                     });
            Client.Directory.OnEventInfo += callback;
            uint eventID;

            if (UInt32.TryParse(args[0], out eventID))
            {
                Client.Directory.EventInfoRequest(eventID);
                return "Query Sent";
            }
            else
            {
                return "Usage: showevent [eventID] (use searchevents to get ID)";
            }
        }
    }
}
