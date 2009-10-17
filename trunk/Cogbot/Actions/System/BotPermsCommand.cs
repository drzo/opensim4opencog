using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;
using cogbot.TheOpenSims;

namespace cogbot.Actions
{
    public class BotPermsCommand : Command, BotSystemCommand
    {
        public BotPermsCommand(BotClient testClient)
        {
            Name = "botperms";
            Description = "Sets the bot use permissions. Usage: botperms name [Base] [Owner] [Group] [Ignore]";
            Category = CommandCategory.TestClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();
            int argsUsed;
            List<SimObject> worldSystemGetPrimitives = WorldSystem.GetPrimitives(args, out argsUsed);
            if (IsEmpty(worldSystemGetPrimitives))
            {
                return Failure("Cannot find objects from " + string.Join(" ", args));
            }
            BotPermissions who =
                (BotPermissions)
                (EnumParse(typeof (BotPermissions), args, argsUsed, out argsUsed) ?? BotPermissions.Base);

            foreach (var p in worldSystemGetPrimitives)
            {

                BotPermissions perms = TheBotClient.GetSecurityLevel(p.ID);
                if (argsUsed==0)
                {
                    Success("Perms for " + p + " was " + perms);
                    continue;    
                }
                Success("Perms for " + p + " was " + perms + " now setting to " + who);
                TheBotClient.SecurityLevels[p.ID] = who;
            }
            return SuccessOrFailure();
        }
    }
}
