using System;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class PacketLogCommand : Command
    {
        public PacketLogCommand(BotClient testClient)
        {
            Name = "packetlog";
            Description = "Logs a given number of packets to an xml file. Usage: packetlog 10 tenpackets.xml";
            Category = CommandCategory.TestClient;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length != 2)
                return "Usage: packetlog 10 tenpackets.xml";

            return "This function is currently unimplemented";
        }
    }
}
