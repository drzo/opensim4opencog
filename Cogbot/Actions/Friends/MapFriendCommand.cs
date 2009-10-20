using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Packets;
using System.Text;

namespace cogbot.Actions
{
    public class MapFriendCommand : Command, BotPersonalCommand
    {
        ManualResetEvent WaitforFriend = new ManualResetEvent(false);

        public MapFriendCommand(BotClient testClient)
        {
            Name = "mapfriend";
            Description = "Show a friends location. Usage: mapfriend UUID";
            Category = CommandCategory.Friends;
            Parameters = new[] { new NamedParam(typeof(SimAvatar), typeof(UUID)) };
        }
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();

            UUID targetID;

            int argsUsed;
            if (!UUIDTryParse(args, 0, out targetID, out argsUsed)) 
                return ShowUsage();

            StringBuilder sb = new StringBuilder();

            EventHandler<FriendFoundReplyEventArgs> del = 
                (sender, e) =>
                {
                    if (!e.RegionHandle.Equals(0))
                        sb.AppendFormat("Found Friend {0} in {1} at {2}/{3}", e.AgentID, e.RegionHandle, e.Location.X, e.Location.Y);
                    else
                        sb.AppendFormat("Found Friend {0}, But they appear to be offline", e.AgentID);

                    WaitforFriend.Set();
                };

            Client.Friends.FriendFoundReply += del;
            try
            {
                WaitforFriend.Reset();
                Client.Friends.MapFriend(targetID);
                if (!WaitforFriend.WaitOne(10000, false))
                {
                    return Failure(string.Format("Timeout waiting for reply, Do you have mapping rights on {0}?", targetID));
                }
            }
            finally
            {
                Client.Friends.FriendFoundReply -= del;                
            }
            return Success(sb.ToString());
        }
    }
}
