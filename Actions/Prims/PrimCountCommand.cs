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

            lock (client.Network.Simulators)
            {
                for (int i = 0; i < client.Network.Simulators.Count; i++)
                {
                    int avcount = client.Network.Simulators[i].ObjectsAvatars.Count;
                    int primcount = client.Network.Simulators[i].ObjectsPrimitives.Count;

                    WriteLine("{0} (Avatars: {1} Primitives: {2})", 
                        client.Network.Simulators[i].Name, avcount, primcount);

                    count += avcount;
                    count += primcount;
                }
            }

			return "Tracking a total of " + count + " objects";
		}
    }
}
