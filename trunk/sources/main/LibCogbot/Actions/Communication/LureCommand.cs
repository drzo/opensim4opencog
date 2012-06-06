using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using Cogbot.World;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Communication
{
    class LureCommand : Command, BotPersonalCommand
    {
        public LureCommand(BotClient testClient)
        {
            Name = "lure";
            Description = "Send a lure to a user.";
            Category = CommandCategory.Friends;
            Details = AddUsage(Name + " [agent-spec]", "lure agent-spec to our location");
            Parameters = CreateParams("target", typeof (AgentSpec),
                                                 "the agent you wish to see " + Name +
                                                 " (see meets a specified <a href='wiki/BotCommands#AvatarSpec'>Avatar Spec</a>.)");
            ResultMap = CreateParams(
                "message", typeof(string), "if success was false, the reason why",
                "success", typeof(bool), "true if command was successful");
        }


        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            int argsUsed;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (!IsEmpty(PS))
            {
                int nfound = 0;
                foreach (var prim in PS)
                {
                    Client.Self.SendTeleportLure(prim.ID);
                    Success(Name + ": " + prim);
                    nfound++;
                }
                if (nfound > 0) return Success(Name + " found: " + nfound + " object/agent(s)");
            }
            string user = args.str;
            UUID id = WorldSystem.GetUserID(user);
            if (id == UUID.Zero) return Failure("Cannot find " + user);
            Client.Self.SendTeleportLure(id);
            return Success("teleport Lure sent to " + user);
        }
    }
}
