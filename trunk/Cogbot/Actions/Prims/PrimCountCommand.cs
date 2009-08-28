using System;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class PrimCountCommand: Command
    {
        public PrimCountCommand(BotClient testClient)
		{
			Name = "primcount";
			Description = "Shows the number of objects currently being tracked.";
            Category = CommandCategory.TestClient;
		}

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
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
                        "WorldSystem", WorldSystem.numAvatars(), cogbot.Listeners.WorldObjects.SimObjects.Count);

			return "Tracking a total of " + count + " objects";
		}
    }
}
