using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using OpenMetaverse;
using cogbot.Listeners;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Search
{
    public class UUIDTypeCommand : Command, GridMasterCommand
    {
        public UUIDTypeCommand(BotClient testClient)
        {
            Name = "UUID Type";
            Description = "Resolve the type of Object the UUID represents.  Usage: uuidtype b06c0586-da9a-473d-aa94-ee3ab5606e4d";
            Category = CommandCategory.BotClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1) return ShowUsage();
            UUID uuid = UUID.Zero;
            string botcmd = String.Join(" ", args, 0, args.Length).Trim();
            int argsUsed;
            UUIDTryParse(args,0, out uuid, out argsUsed);

            object obj = null;
            lock (WorldObjects.UUIDTypeObject)
                WorldObjects.UUIDTypeObjectTryGetValue(uuid, out obj);
            if (obj != null)
            {
                string typeString = "" + obj.GetType();
                string objString;
                try
                {
                    objString = "" + obj;
                }
                catch (Exception e)
                {
                    objString = "" + e;
                }
                // some structs .ToString return only their type names
                if (objString == typeString)
                {
                    objString = Helpers.StructToString(obj);
                }
                WriteLine("UUID={0} is of Type='{1}' toString='{2}'", uuid, typeString, objString);
            }
            else
            {
                WriteLine("Object not found for UUID=" + uuid);
            }
            return Success("Done with UUID " + uuid + " obj= " + obj);
        }
    }
}
