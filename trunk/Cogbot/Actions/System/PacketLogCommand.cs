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

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length != 2)
                return Failure(Usage);// " packetlog 10 tenpackets.xml";

            return Success("This function is currently unimplemented");
        }
    }
}
