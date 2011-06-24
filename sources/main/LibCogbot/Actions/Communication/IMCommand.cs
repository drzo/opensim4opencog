using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.Listeners;
using cogbot.TheOpenSims;
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

            string message = String.Empty;

            int argsUsed;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (!IsEmpty(PS))
            {
                for (int ct = argsUsed; ct < args.Length; ct++)
                    message += args[ct] + " ";
                message = message.TrimEnd();
                int nfound = 0;
                foreach (var prim in PS)
                {
                    TheBotClient.InstantMessage(prim.ID, message, UUID.Zero);
                    Success(Name + ": " + prim);
                    nfound++;
                }
                if (nfound > 0) return Success(Name + " found: " + nfound + " object/agent(s)");
            }


            // Build the message
            for (int ct = 2; ct < args.Length; ct++)
                message += args[ct] + " ";
            message = message.TrimEnd();
            string ToAvatarName = args[0] + " " + args[1];
            UUID found = WorldSystem.GetUserID(ToAvatarName);
            if (found==UUID.Zero) return Failure( "Name lookup for " + ToAvatarName + " failed");
            if (message.Length > 1023) message = message.Remove(1023);
            TheBotClient.InstantMessage(found, message, UUID.Zero);
            return Success("Instant Messaged " + found.ToString() + " with message: " + message);


        }
    }
}
