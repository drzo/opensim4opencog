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

            FriendsManager.FriendFoundEvent del = 
                delegate(UUID agentID, ulong regionHandle, Vector3 location) 
                {
                    if (!regionHandle.Equals(0))
                        sb.AppendFormat("Found Friend {0} in {1} at {2}/{3}", agentID, regionHandle, location.X, location.Y);
                    else
                        sb.AppendFormat("Found Friend {0}, But they appear to be offline", agentID);

                    WaitforFriend.Set();
                };

            Client.Friends.OnFriendFound += del;
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
                Client.Friends.OnFriendFound -= del;                
            }
            return Success(sb.ToString());
        }
    }
}
