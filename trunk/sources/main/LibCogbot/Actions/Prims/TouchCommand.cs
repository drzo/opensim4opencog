using System;
using System.Collections.Generic;
using Cogbot.World;
using OpenMetaverse;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions
{
    public class TouchCommand : Command, BotPersonalCommand
    {
        public TouchCommand(BotClient testClient)
        {
            Name = "touch";
        }

        public override void MakeInfo()
        {
            Description =
                @"Touch a prim that meets a specified <a href='wiki/BotCommands#PrimSpec'>Prim Spec</a>.
Ignores the object's default action, always touches.";
            Category = CommandCategory.Objects;
            AddVersion(CreateParams("targets", typeof (PrimSpec),
                                    "The object to touch"), "Ignores the object's default action, always touches.");
            ResultMap = CreateParams(
                "message", typeof (string), "if success was false, the reason why",
                "success", typeof (bool), "true if we touched the object");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
                return ShowUsage(); // " touch UUID";

            List<SimObject> PS;
            if (!args.TryGetValue("targets", out PS) || IsEmpty(PS))
            {
                return Failure("Cannot find objects from " + args.GetString("targets"));
            } 
            GridClient client = TheBotClient;
            foreach (var targetPrim in PS)
            {
                AddSuccess(Name + " on " + targetPrim);
                Client.Self.Touch(targetPrim.LocalID);
            }
            return SuccessOrFailure();
        }
    }
}