using System;
using cogbot.Listeners;
using MushDLR223.Utilities;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Land
{
    public class PrimCountCommand : Command, RegionMasterCommand
    {
        public PrimCountCommand(BotClient testClient)
		{
			Name = "primcount";
			Description = "Shows the number of objects currently being tracked.";
            Category = CommandCategory.Simulator;
            Parameters = new [] {  new NamedParam(typeof(GridClient), null) };
		}

        public override CmdResult ExecuteRequest(CmdRequest args)
		{
            int count = 0;

            {
                foreach (Simulator sim in LockInfo.CopyOf(Client.Network.Simulators))
                {
                    int avcount = sim.ObjectsAvatars.Count;
                    int primcount = sim.ObjectsPrimitives.Count;

                    WriteLine("{0} (Avatars: {1} Primitives: {2})", 
                        sim.Name, avcount, primcount);

                    count += avcount;
                    count += primcount;
                }
            }
            WriteLine("{0} (Avatars: {1} Primitives: {2})",
                        "WorldSystem", WorldObjects.SimAvatars.Count, WorldObjects.SimObjects.Count);

            return Success("Tracking a total of " + count + " objects");
		}
    }
}
