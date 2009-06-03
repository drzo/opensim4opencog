using System;
using System.IO;
using OpenMetaverse;
using System.Diagnostics;

namespace cogbot.Actions
{
    public class CreateScriptCommand : Command
    {
        public CreateScriptCommand(BotClient testClient)
        {
            Name = "createscript";
            Description = "Creates a script in your inventory from a local .lsl file.";
            Category = CommandCategory.Inventory;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return "Usage: createscript filename.lsl";

            string file = String.Empty;
            for (int ct = 0; ct < args.Length; ct++)
                file = String.Format("{0}{1} ", file, args[ct]);
            file = file.TrimEnd();

            WriteLine("Filename: {0}", file);
            if (!File.Exists(file))
                return String.Format("Filename '{0}' does not exist", file);


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
                            Client.Inventory.RequestUpdateScriptAgentInventory(EncodeScript(body), item.UUID, false, new InventoryManager.ScriptUpdatedCallback(delegate(bool success1, string status, UUID itemid, UUID assetid)
                            {
                                if (success1)
                                    WriteLine("Script successfully uploaded, ItemID {0} AssetID {1}", itemid, assetid);
                            }));
                    });
                }
                return "Done";

            }
            catch (System.Exception e)
            {
                Logger.Log(e.ToString(), Helpers.LogLevel.Error, Client);
                return "Error creating script.";
            }
        }
        /// <summary>
        /// </summary>
        /// <param name="body"></param>
        public static byte[] EncodeScript(string body)
        {
            // Assume this is a string, add 1 for the null terminator ?
            byte[] stringBytes = System.Text.Encoding.UTF8.GetBytes(body);
            byte[] assetData = new byte[stringBytes.Length]; //+ 1];
            Array.Copy(stringBytes, 0, assetData, 0, stringBytes.Length);
            return assetData;
        }
    }
}