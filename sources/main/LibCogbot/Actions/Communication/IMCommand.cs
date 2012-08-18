using System;
using System.Collections.Generic;
using System.Threading;
using Cogbot;
using Cogbot.World;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Communication
{
    public class ImCommand : Command, BotPersonalCommand, BotStatefullCommand
    {
        public UUID currentAvatar = UUID.Zero;
        public UUID currentSession = UUID.Zero;

        public ImCommand(BotClient testClient)
        {
            Name = "im";
        }

        override public void MakeInfo()
        {
            Description = "IM a user. Has nothing to do with SL 'whisper'";
            Details = AddUsage("im to <avatar name> <message>", "IM Avatar with Message") +
                    AddUsage("im <message>", "reply to the last person who IMed you");
            Category = CommandCategory.Communication;
            Parameters =
                CreateParams(
                            "to", typeof(Avatar), "who you are IMing",
                            "message", typeof(string), "what you IM");
            ResultMap = CreateParams(
                            "personFound", typeof(bool), "true iff we found the person to whisper to",
                            "sentCorrect", typeof(bool), "true iff we successfully sent the message");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 2)
                return ShowUsage(); // " im [firstname] [lastname] [message]";

            string message = String.Empty;

            int argsUsed;
            SimObject prim;
            if (WorldSystem.tryGetPrim(args, out prim, out argsUsed))
            {
                for (int ct = argsUsed; ct < args.Length; ct++)
                    message += args[ct] + " ";
                message = message.TrimEnd();
                int nfound = 0;
                //foreach (var prim in PS)
                {
                    TheBotClient.InstantMessage(prim.ID, message, UUID.Zero);
                    AddSuccess(Name + ": " + prim);
                    nfound++;
                }
                if (nfound > 0) return Success(Name + " found: " + nfound + " object/agent(s)");
            }


            // Build the message
            message = message.TrimEnd();
            string ToAvatarName = args[0] + " " + args[1];
            int skip = 2;
            if (ToAvatarName.StartsWith("$"))
            {
                ToAvatarName = args[0];
                skip = 1;
            }
            for (int ct = skip; ct < args.Length; ct++)
                message += args[ct] + " ";

            if (ToAvatarName.StartsWith("$"))
            {
                var col = WorldSystem.ResolveCollection(ToAvatarName, out argsUsed, null);
                if (col != null)
                {
                    foreach (object c in col)
                    {
                        AddSuccess("Send to " + c);
                        string s = "" + c;
                        if (s.Length > 0)
                        {
                            var so = WorldObjects.GetSimAvatarFromNameIfKnown(s);
                            if (so != null) TheBotClient.InstantMessage(so.ID, message, UUID.Zero);
                        }
                    }
                    return Success("Total Sent to " + col.Count);
                }
            }
            UUID found = WorldSystem.GetUserID(ToAvatarName);
            if (found != UUID.Zero)
            {
                if (message.Length > 1023) message = message.Remove(1023);
                TheBotClient.InstantMessage(found, message, UUID.Zero);
                return Success("Instant Messaged " + found.ToString() + " with message: " + message);
            }
            UUID ToGroupID = UUID.Zero;
            ManualResetEvent WaitForSessionStart = new ManualResetEvent(false);

            if (UUIDTryParse(args, 0, out ToGroupID, out argsUsed))
            {
                string messageGroup = String.Empty;
                for (int ct = argsUsed; ct < args.Length; ct++)
                    messageGroup += args[ct] + " ";
                messageGroup = messageGroup.TrimEnd();
                if (messageGroup.Length > 1023) messageGroup = messageGroup.Remove(1023);
                EventHandler<GroupChatJoinedEventArgs> callback =
                    delegate(object sender, GroupChatJoinedEventArgs e)
                        {
                            if (e.Success)
                            {
                                WriteLine("Joined {0} Group Chat Success!", e.SessionName);
                                WaitForSessionStart.Set();
                            }
                            else
                            {
                                WriteLine("Join Group Chat failed :(");
                            }
                        };
                try
                {
                    Client.Self.GroupChatJoined += callback;
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
                        Client.Self.InstantMessageGroup(ToGroupID, messageGroup);
                    }
                    else
                    {
                        return Failure("Timeout waiting for group session start");
                    }
                }
                finally
                {
                    Client.Self.GroupChatJoined -= callback;
                }
                return Success("Instant Messaged group " + ToGroupID.ToString() + " with message: " + messageGroup);
            }
            else
            {
                return Failure("failed to instant message group/avatar " + ToAvatarName);
            }
        }

        #region Implementation of IDisposable

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {

        }

        #endregion

    }
}
