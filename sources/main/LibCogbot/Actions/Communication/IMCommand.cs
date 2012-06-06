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
                return ShowUsage();// " im [firstname] [lastname] [message]";

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
                    Success(Name + ": " + prim);
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
                        Success("Send to " + c);
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
            if (found==UUID.Zero)
            {
                return Failure( "Name lookup for " + ToAvatarName + " failed");
            }
            if (message.Length > 1023) message = message.Remove(1023);
            TheBotClient.InstantMessage(found, message, UUID.Zero);
            return Success("Instant Messaged " + found.ToString() + " with message: " + message);
        }

        public CmdResult acceptInputOBSOLETE(string verb, Parser args, OutputDelegate WriteLine)
        {
            //base.acceptInput(verb, args);

            string to = args["to"];

            if (to.Length > 0)
            {
                int argsUsed;
                List<SimObject> PS =
                    WorldSystem.GetPrimitives(to.Split(new[] { " " }, StringSplitOptions.RemoveEmptyEntries), out argsUsed);
                if (!IsEmpty(PS))
                {
                    foreach (var prim in PS)
                    {
                        currentAvatar = prim.ID;
                        break;
                    }
                }
                else
                {
                    SimAvatar avatar;
                    if (!WorldSystem.tryGetAvatar(to, out avatar))
                    {
                        return Failure("I don't know who " + to + "is.");
                    }
                    currentAvatar = avatar.ID;
                }

            }
            else if (currentAvatar == UUID.Zero)
            {
                return Failure("Please provide a name to whisper to.");
            }

            if (currentSession != UUID.Zero)
                Client.Self.InstantMessage(currentAvatar, args.objectPhrase, currentSession);
            else
                Client.Self.InstantMessage(currentAvatar, args.objectPhrase);
            return Success("sent message");
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
