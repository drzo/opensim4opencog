using System;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class PrimCountCommand: Command
    {
        public PrimCountCommand(cogbot.TextForm testClient)
		{
			Name = "primcount";
			Description = "Shows the number of objects currently being tracked.";
            Category = CommandCategory.TestClient;
		}

        public override string Execute(string[] args, UUID fromAgentID)
		{
            int count = 0;

            lock (Client.Network.Simulators)
            {
                for (int i = 0; i < Client.Network.Simulators.Count; i++)
                {
                    Simulator sim = Client.Network.Simulators[i];
                    int avcount = sim.ObjectsAvatars.Count;
                    int primcount = sim.ObjectsPrimitives.Count;

                    WriteLine("" + sim + " {0} (Avatars: {1} Primitives: {2})", 
                        Client.Network.Simulators[i].Name, avcount, primcount);

                    count += avcount;
                    count += primcount;
                }
            }

			return "Tracking a total of " + count + " objects";
		}
    }
}
