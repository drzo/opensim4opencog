using System;
using System.Collections.Generic;
using Cogbot.World;
using OpenMetaverse;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Objects
{
    public class DeRezCommand : Command, RegionMasterCommand
    {
        public DeRezCommand(BotClient testClient)
        {
            Name = "derez";
        }

        public override void MakeInfo()
        {
            Description = "De-Rezes a specified prim. " + "Usage: derez [prim-uuid]";
            Parameters = CreateParams("targets", typeof (PrimSpec), "The targets of " + Name);
            Category = CommandCategory.Objects;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            UUID primID = UUID.Zero;

            if (args.Length < 1)
            {
                return ShowUsage();
            }

            List<SimObject> PS;
            if (!args.TryGetValue("targets", out PS) || IsEmpty(PS))
            {
                return Failure("Cannot find objects from " + args.GetString("targets"));
            } 
            foreach (var target in PS)
            {
                WorldSystem.DeletePrim(target.Prim);
                WriteLine("\n {0}", target);
                AddSuccess("Done.");
            }
            return SuccessOrFailure();
        }
    }
}