using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.Listeners;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Communication
{
    public class ImCommand : Command, BotPersonalCommand
    {

        public ImCommand(BotClient testClient)
        {
            Name = "im";
            Description = "Instant message someone. Usage: im [firstname] [lastname] [message]";
            Category = CommandCategory.Communication;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 3)
                return ShowUsage();// " im [firstname] [lastname] [message]";

            string ToAvatarName = String.Empty;
        

            ToAvatarName = args[0] + " " + args[1];

            // Build the message
            string message = String.Empty;
            for (int ct = 2; ct < args.Length; ct++)
                message += args[ct] + " ";
            message = message.TrimEnd();
            UUID found = WorldSystem.GetUserID(ToAvatarName);
            if (found==UUID.Zero) return Failure( "Name lookup for " + ToAvatarName + " failed");
            if (message.Length > 1023) message = message.Remove(1023);
            Client.Self.InstantMessage(found, message);
            return Success("Instant Messaged " + found.ToString() + " with message: " + message);


        }
    }
}
