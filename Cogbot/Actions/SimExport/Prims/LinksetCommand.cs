using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;
using PathSystem3D.Navigation;
using Radegast;

namespace cogbot.Actions.Objects
{
    public class LinksetCommand : cogbot.Actions.Command, RegionMasterCommand
    {
        public LinksetCommand(BotClient client)
        {
            Name = "Linkset";
            Description = "Takes from a prim. Usage: Take [prim]";
            Category = cogbot.Actions.CommandCategory.Objects;
            Parameters = new[]
                             {
                                 new NamedParam(typeof (SimObject), typeof (UUID)),
                                 new NamedParam(typeof (List<SimObject>), typeof (List<UUID>))
                             };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length == 0)
            {
                return ShowUsage();
            }
            int argsUsed;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (PS.Count != 1) return Failure("Cannot find single object from " + string.Join(" ", args));
            Primitive rootPrim = PS[0].Prim;
            Simulator worldSystemGetSimulator = WorldSystem.GetSimulator(rootPrim);
            if (rootPrim.ParentID != 0)
                return
                    Failure("Root Prim Has Parent "
                            + WorldSystem.GetSimObject(rootPrim.ParentID, worldSystemGetSimulator));
            Success(Name + " on " + WorldSystem.GetSimObject(rootPrim));
            PS = WorldSystem.GetPrimitives(Parser.SplitOff(args, argsUsed), out argsUsed);
            try
            {
                PS.Remove(PS[0]);
                List<uint> linkSet = new List<uint>();
                PS.ForEach(o => linkSet.Add(o.LocalID));
                linkSet.Add(rootPrim.LocalID);
                TheBotClient.Objects.LinkPrims(worldSystemGetSimulator, linkSet);
                return Success("linked count =" + PS.Count);
            }
            catch (Exception e)
            {

                return Failure("" + e);
            }
        }
    }
}