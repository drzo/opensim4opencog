using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;
using System.Text;

namespace cogbot.Actions
{
    public class MapFriendCommand : Command
    {
        ManualResetEvent WaitforFriend = new ManualResetEvent(false);

        public MapFriendCommand(cogbot.TextForm testClient)
        {
            Name = "mapfriend";
            Description = "Show a friends location. Usage: mapfriend UUID";
            Category = CommandCategory.Friends;
        }
        public override string Execute(string[] args, UUID fromAgentID)
        {
            if (args.Length != 1)
                return Description;

            UUID targetID;

            if (!UUID.TryParse(args[0], out targetID))
                return Description;

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

            client.Friends.OnFriendFound += del;
            WaitforFriend.Reset();
            client.Friends.MapFriend(targetID);
            if (!WaitforFriend.WaitOne(10000, false))
            {
                sb.AppendFormat("Timeout waiting for reply, Do you have mapping rights on {0}?", targetID);
            }
            client.Friends.OnFriendFound -= del;
            return sb.ToString();
        }
    }
}
