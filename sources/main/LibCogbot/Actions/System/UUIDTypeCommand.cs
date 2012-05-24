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
            Description = "<p>Given a UUID, describes the thing. Unlike priminfo, uuidtype will describe any sort of uuid" +
                "- an object, avatar, texture, sound, etc. Note that to get useful info about textures you'll have to set textures" +
                "to download. You can do it from botcmd via</p><p>sysvar DownloadTextures True</p><p> or by adding</p><pre>" +
@"(setj  ClientManager:DownloadTextures True)
(setj SimAssetStore:EnableDownloadAssetDefault  True)" +
"</pre><p>in botconfig.xml</p>";
            Details = AddUsage("uuidtype <uuid>", "print what sort of  this UUID is") +
                Example("/uuidtype  3a3e92ed-a94f-46dc-9f92-88c790b5701e",
@"[12:54] UUID=3a3e92ed-a94f-46dc-9f92-88c790b5701e is of Type='cogbot.TheOpenSims.SimAnimation' toString='3a3e92ed-a94f-46dc-9f92-88c790b5701e NODATA'
[12:54] UUID Type: Success: Done with UUID 3a3e92ed-a94f-46dc-9f92-88c790b5701e obj= 3a3e92ed-a94f-46dc-9f92-88c790b5701e NODATA
[12:54] UUID Type: Success: Done with UUID 3a3e92ed-a94f-46dc-9f92-88c790b5701e obj= 3a3e92ed-a94f-46dc-9f92-88c790b5701e NODATA");
            Parameters = CreateParams("uuid", typeof(UUID), "uuid to resolve to type");
            ResultMap = CreateParams("description", typeof(string), "string, usually the C# type name");
            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
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
