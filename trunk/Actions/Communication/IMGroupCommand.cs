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
        public ImGroupCommand(cogbot.TextForm testClient)
        {

            Name = "imgroup";
            Description = "Send an instant message to a group. Usage: imgroup [group_uuid] [message]";
            Category = CommandCategory.Communication;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            if (args.Length < 2)
                return "Usage: imgroup [group_uuid] [message]";



            if (UUID.TryParse(args[0], out ToGroupID))
            {
                string message = String.Empty;
                for (int ct = 1; ct < args.Length; ct++)
                    message += args[ct] + " ";
                message = message.TrimEnd();
                if (message.Length > 1023) message = message.Remove(1023);

                client.Self.OnGroupChatJoin += new AgentManager.GroupChatJoinedCallback(Self_OnGroupChatJoin);
                if (!client.Self.GroupChatSessions.ContainsKey(ToGroupID))
                {
                    WaitForSessionStart.Reset();
                    client.Self.RequestJoinGroupChat(ToGroupID);
                }
                else
                {
                    WaitForSessionStart.Set();
                }
                
                if (WaitForSessionStart.WaitOne(20000, false))
                {
                    client.Self.InstantMessageGroup(ToGroupID, message);
                }
                else
                {
                    return "Timeout waiting for group session start";
                }
                
                client.Self.OnGroupChatJoin -= new AgentManager.GroupChatJoinedCallback(Self_OnGroupChatJoin);
                return "Instant Messaged group " + ToGroupID.ToString() + " with message: " + message;
            }
            else
            {
                return "failed to instant message group";
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
