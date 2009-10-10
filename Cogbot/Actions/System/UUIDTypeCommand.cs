using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using OpenMetaverse;
using cogbot.Listeners;

namespace cogbot.Actions
{
    public class UUIDTypeCommand : Command
    {
        public UUIDTypeCommand(BotClient testClient)
        {
            Name = "UUID Type";
            Description = "Resolve the type of Object the UUID represents.  Usage: uuidtype b06c0586-da9a-473d-aa94-ee3ab5606e4d";
            Category = CommandCategory.TestClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1) return Failure(Usage);
            var v = WorldObjects.uuidTypeObject;
            UUID uuid;
            string botcmd = String.Join(" ", args, 0, args.Length).Trim();
            UUIDTryParse(botcmd, out uuid);

            object obj = null;
            lock (v)
            {
                v.TryGetValue(uuid, out obj);
            }
            if (obj != null)
            {
                WriteLine("Object is of Type " + obj.GetType());
            }
            else
            {
                WriteLine("Object not found for UUID " + uuid);
            }
            return Success("Done with UUID " + uuid + " obj= " + obj);
        }
    }
}
