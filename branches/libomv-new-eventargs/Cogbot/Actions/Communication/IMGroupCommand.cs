using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class ImGroupCommand : Command, BotPersonalCommand
    {
        public ImGroupCommand(BotClient testClient)
        {

            Name = "imgroup";
            Description = "Send an instant message to a group. Usage: imgroup <group_uuid> [message]";
            Category = CommandCategory.Communication;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {

            if (args.Length < 2)
                return ShowUsage(); // " imgroup [group_uuid] [message]";

            UUID ToGroupID;
            ManualResetEvent WaitForSessionStart = new ManualResetEvent(false);

            int argsUsed;
            if (UUIDTryParse(args, 0, out ToGroupID, out argsUsed))
            {
                string message = String.Empty;
                for (int ct = argsUsed; ct < args.Length; ct++)
                    message += args[ct] + " ";
                message = message.TrimEnd();
                if (message.Length > 1023) message = message.Remove(1023);
                AgentManager.GroupChatJoinedCallback callback =
                    delegate(UUID groupchatsessionid, string sessionname, UUID tmpsessionid, bool success)
                        {
                            if (success)
                            {
                                WriteLine("Joined {0} Group Chat Success!", sessionname);
                                WaitForSessionStart.Set();
                            }
                            else
                            {
                                WriteLine("Join Group Chat failed :(");
                            }
                        };
                try
                {
                    Client.Self.OnGroupChatJoin += callback;
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
                }
                finally
                {
                    Client.Self.OnGroupChatJoin -= callback;
                }
                return Success("Instant Messaged group " + ToGroupID.ToString() + " with message: " + message);
            }
            else
            {
                return Failure("failed to instant message group");
            }
        }
    }
}
