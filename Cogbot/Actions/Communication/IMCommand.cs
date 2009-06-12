using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.Listeners;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class ImCommand : Command
    {

        public ImCommand(BotClient testClient)
        {
            Name = "im";
            Description = "Instant message someone. Usage: im [firstname] [lastname] [message]";
            Category = CommandCategory.Communication;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 3)
                return "Usage: im [firstname] [lastname] [message]";

            string ToAvatarName = String.Empty;
        

            ToAvatarName = args[0] + " " + args[1];

            // Build the message
            string message = String.Empty;
            for (int ct = 2; ct < args.Length; ct++)
                message += args[ct] + " ";
            message = message.TrimEnd();
            UUID found = WorldSystem.GetUserID(ToAvatarName);
            if (found==UUID.Zero) return "Name lookup for " + ToAvatarName + " failed";
            if (message.Length > 1023) message = message.Remove(1023);
            Client.Self.InstantMessage(found, message);
            return "Instant Messaged " + found.ToString() + " with message: " + message;


        }
    }
}
