using System;
using cogbot.Listeners;
using OpenMetaverse;

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

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
		{
            int count = 0;

            lock (Client.Network.Simulators)
            {
                for (int i = 0; i < Client.Network.Simulators.Count; i++)
                {
                    int avcount = Client.Network.Simulators[i].ObjectsAvatars.Count;
                    int primcount = Client.Network.Simulators[i].ObjectsPrimitives.Count;

                    WriteLine("{0} (Avatars: {1} Primitives: {2})", 
                        Client.Network.Simulators[i].Name, avcount, primcount);

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
