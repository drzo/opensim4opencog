using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using OpenMetaverse;
using System.Diagnostics;

namespace cogbot.Actions.Inventory
{
    public class CreateScriptCommand : Command, BotPersonalCommand
    {
        public CreateScriptCommand(BotClient testClient)
        {
            Name = "createscript";
            Description = "Creates a script in your inventory from a local .lsl file.";
            Category = CommandCategory.Inventory;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();// " createscript filename.lsl";

            string file = String.Empty;
            for (int ct = 0; ct < args.Length; ct++)
                file = String.Format("{0}{1} ", file, args[ct]);
            file = file.TrimEnd();

            WriteLine("Filename: {0}", file);
            if (!File.Exists(file))
                return Failure( String.Format("Filename '{0}' does not exist", file));


            // FIXME: Upload the script asset first. When that completes, call RequestCreateItem
            try
            {
                using (StreamReader reader = new StreamReader(file))
                {
                    string body = reader.ReadToEnd();
                    string desc = String.Format("{0} created by OpenMetaverse BotClient {1}", file, DateTime.Now);
                    // create the asset
                    Client.Inventory.RequestCreateItem(Client.Inventory.FindFolderForType(AssetType.LSLText), file, desc, AssetType.LSLText, UUID.Random(), InventoryType.LSL, PermissionMask.All,
                    delegate(bool success, InventoryItem item)
                    {
                        if (success)
                            // upload the asset
                            Client.Inventory.RequestUpdateScriptAgentInventory(EncodeScript(body), item.UUID, false, new InventoryManager.ScriptUpdatedCallback(delegate(bool success1, string status, bool itemid, List<string>
                                                                                                                                                                    assetid, UUID itemid1, UUID assetid1)
                            {
                                if (success1)
                                    WriteLine("Script successfully uploaded, ItemID {0} AssetID {1}", itemid, assetid);
                            }));
                    });
                }
                return Success("Done");

            }
            catch (Exception e)
            {
                Logger.Log(e.ToString(), Helpers.LogLevel.Error, Client);
                return Failure("Error creating script.");
            }
        }
        /// <summary>
        /// </summary>
        /// <param name="body"></param>
        public static byte[] EncodeScript(string body)
        {
            // Assume this is a string, add 1 for the null terminator ?
            byte[] stringBytes = Encoding.UTF8.GetBytes(body);
            byte[] assetData = new byte[stringBytes.Length]; //+ 1];
            Array.Copy(stringBytes, 0, assetData, 0, stringBytes.Length);
            return assetData;
        }
    }
}