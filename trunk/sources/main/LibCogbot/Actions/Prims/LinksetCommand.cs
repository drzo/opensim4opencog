using System;
using System.Collections.Generic;
using System.Threading;
using Cogbot;
using Cogbot.World;
using OpenMetaverse;
using PathSystem3D.Navigation;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Objects
{
    public class LinksetCommand : Cogbot.Actions.Command, RegionMasterCommand
    {
        public LinksetCommand(BotClient client)
        {
            Name = "Linkset";
        }

        public override void MakeInfo()
        {
            Description = "Takes from a prim. Usage: Take [prim]";
            Category = Cogbot.Actions.CommandCategory.Objects;
            Parameters = CreateParams("root", typeof (PrimSpec), "The root of " + Name,
                                      "childs", typeof (PrimSpec), "The childs of " + Name);
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length == 0)
            {
                return ShowUsage();
            }
            int argsUsed;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (PS.Count != 1) return Failure("Cannot find single object from " + args.str);
            Primitive rootPrim = PS[0].Prim;
            Simulator worldSystemGetSimulator = WorldSystem.GetSimulator(rootPrim);
            if (rootPrim.ParentID != 0)
                return
                    Failure("Root Prim Has Parent "
                            + WorldSystem.GetSimObject(rootPrim.ParentID, worldSystemGetSimulator));
            AddSuccess(Name + " on " + WorldSystem.GetSimObject(rootPrim));
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