using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Search
{
    class ShowEventDetailsCommand : Command, GridMasterCommand
    {
        public ShowEventDetailsCommand(BotClient testClient)
        {
            Name = "showevent";
            Description = "Shows an Events details.";
            Details = AddUsage(Name + " [eventID]", "Display SL event with eventID");
            Category = CommandCategory.Other;
            Parameters =
                CreateParams("eventID", typeof (UUID), "event you want info for");
            ResultMap = CreateParams(
                "message", typeof (string), "if success was false, the reason why",
                "success", typeof (bool), "true if command was successful");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
                return ShowUsage();// " showevent [eventID] (use searchevents to get ID)";

            Client.Directory.EventInfoReply += Directory_EventDetails;
            uint eventID;

            if (UInt32.TryParse(args[0], out eventID))
            {
                Client.Directory.EventInfoRequest(eventID);
                return Success("Query Sent");
            }
            else
            {
                return ShowUsage();// " showevent [eventID] (use searchevents to get ID)";
            }
        }

        void Directory_EventDetails(object sender, EventInfoReplyEventArgs e)
        {
            float x, y;
            Helpers.GlobalPosToRegionHandle((float)e.MatchedEvent.GlobalPos.X, (float)e.MatchedEvent.GlobalPos.Y, out x, out y);
            StringBuilder sb = new StringBuilder("secondlife://" + e.MatchedEvent.SimName + "/" + x + "/" + y + "/0" + Environment.NewLine);
            sb.AppendLine(e.MatchedEvent.ToString());
            
            //sb.AppendFormat("       Name: {0} ({1})" + System.Environment.NewLine, e.MatchedEvent.Name, e.MatchedEvent.ID);
            //sb.AppendFormat("   Location: {0}/{1}/{2}" + System.Environment.NewLine, e.MatchedEvent.SimName, x, y);
            //sb.AppendFormat("       Date: {0}" + System.Environment.NewLine, e.MatchedEvent.Date);
            //sb.AppendFormat("Description: {0}" + System.Environment.NewLine, e.MatchedEvent.Desc);
            WriteLine(sb.ToString());
            Client.Directory.EventInfoReply -= Directory_EventDetails;
        }
    }
}
