using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Assets;
using OpenMetaverse.StructuredData;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Land
{
    public class LowerSimCommand : Command, RegionMasterCommand
    {

        public LowerSimCommand(BotClient testClient)
        {

            Name = "lowersim";
            Description = "Lowers all parent prims on a simulator. Usage: lowersim DaxlandWest -10";
            Category = CommandCategory.Objects;
        }


        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int len = args.Length;
            int argsUsed;
            Simulator CurSim = TryGetSim(args, out argsUsed) ?? Client.Network.CurrentSim;
            Simulator sim = CurSim;
            if (len==0)
            {
                //prep
                List<uint> prep = new List<uint>();
                lock (sim.ObjectsPrimitives.Dictionary)
                {
                    prep.AddRange(sim.ObjectsPrimitives.Dictionary.Keys);
                }
                foreach (var u in prep)
                {
                    Client.Objects.RequestObject(sim,u);
                    Client.Objects.SelectObject(sim,u);
                }
                return ShowUsage();

            }
            if (len>1)
            {
                string simName = string.Join(" ", args, 0, len - 1);
                foreach (var list in Client.Network.Simulators)
                {
                    if (simName==list.Name) sim = list;
                }
            }
            WriteLine("about to lower sim: " + sim.Name + " with " + sim.ObjectsPrimitives.Count);
            List<Primitive> prims = new List<Primitive>();
            lock (sim.ObjectsPrimitives.Dictionary)
            {
                prims.AddRange(sim.ObjectsPrimitives.Dictionary.Values);
            }
                                
            Vector3 offset = new Vector3(0,0,float.Parse(args[len-1]));
            int moved = 0;
            foreach (Primitive prim in prims)
            {
                if (prim.ParentID==0)
                {
                    moved++;
                    Vector3 primPosition = prim.Position;
                    Client.Objects.SetPosition(sim,prim.LocalID,primPosition-offset);
                }
            }
            return Success("moved " + moved + " on sim " + sim);
        }
    }
}
