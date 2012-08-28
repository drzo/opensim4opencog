using System;
using OpenMetaverse;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    [Obsolete]
    public class PacketLogCommand : Command, BotSystemCommand
    {
        public PacketLogCommand(BotClient testClient)
        {
            Name = "packetlog";
        }

        public override void MakeInfo()
        {
            Description =
                "Unimplemented. Logs a given number of packets to an xml file. Usage: packetlog 10 tenpackets.xml";
            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length != 2)
                return ShowUsage(); // " packetlog 10 tenpackets.xml";

            return Success("This function is currently unimplemented");
        }
    }
}