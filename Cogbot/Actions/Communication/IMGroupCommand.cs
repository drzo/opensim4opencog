using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class ImGroupCommand : Command
    {
        UUID ToGroupID = UUID.Zero;
        ManualResetEvent WaitForSessionStart = new ManualResetEvent(false);
        public ImGroupCommand(BotClient testClient)
        {

            Name = "imgroup";
            Description = "Send an instant message to a group. Usage: imgroup [group_uuid] [message]";
            Category = CommandCategory.Communication;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 2)
                return ShowUsage(); // " imgroup [group_uuid] [message]";


            int argsUsed;
            if (UUIDTryParse(args, 0, out ToGroupID, out argsUsed))
            {
                string message = String.Empty;
                for (int ct = argsUsed; ct < args.Length; ct++)
                    message += args[ct] + " ";
                message = message.TrimEnd();
                if (message.Length > 1023) message = message.Remove(1023);

                Client.Self.OnGroupChatJoin += new AgentManager.GroupChatJoinedCallback(Self_OnGroupChatJoin);
                if (!Client.Self.GroupChatSessions.ContainsKey(ToGroupID))
                {
                    WaitForSessionStart.Reset();
                    Client.Self.RequestJoinGroupChat(ToGroupID);
                }
                else
                {
                    WaitForSessionStart.Set();
                }

                if (WaitForSessionStart.WaitOne(20000, false))
                {
                    Client.Self.InstantMessageGroup(ToGroupID, message);
                }
                else
                {
                    return Failure("Timeout waiting for group session start");
                }

                Client.Self.OnGroupChatJoin -= new AgentManager.GroupChatJoinedCallback(Self_OnGroupChatJoin);
                return Success("Instant Messaged group " + ToGroupID.ToString() + " with message: " + message);
            }
            else
            {
                return Failure("failed to instant message group");
            }
        }

        void Self_OnGroupChatJoin(UUID groupChatSessionID, string sessionName, UUID tmpSessionID, bool success)
        {
            if (success)
            {
                WriteLine("Joined {0} Group Chat Success!", sessionName);
                WaitForSessionStart.Set();
            }
            else
            {
                WriteLine("Join Group Chat failed :(");
            }
        }
    }
}
