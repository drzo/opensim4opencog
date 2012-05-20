using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Packets;
using System.Text;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Friends
{
    public class MapFriendCommand : Command, BotPersonalCommand
    {
        ManualResetEvent WaitforFriend = new ManualResetEvent(false);

        public MapFriendCommand(BotClient testClient)
        {
            Name = "Map Friend";
            Description = "Show a friends location.";
            Usage = Htmlize.Usage(Name + " agent", Description);
            Category = CommandCategory.Friends;
            Parameters = NamedParam.CreateParams("agent", typeof(UUID), "agent you are going to " + Name);
        }
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();

            UUID targetID = UUID.Zero;

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
