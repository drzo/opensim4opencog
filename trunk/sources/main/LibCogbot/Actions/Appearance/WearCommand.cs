using System;
using System.Collections.Generic;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Appearance
{
    public class WearCommand : Command, BotPersonalCommand
    {
        public WearCommand(BotClient testClient)
        {
            TheBotClient = testClient;
            Name = "wear";
            Category = CommandCategory.Appearance;
            Description = @"<p>Same as right clicking and choosing 'replace outfit' in a normal client.</p>
<p>See <a href='wiki/BotCommands#Inventory'>Inventory</a> for Inventory FormatException</p>
<p>If the argument is a folder the entire folder is worn (but not items in contained folders).</p>
<p>Adding 'nobake' doesn't rebake the avatar's textures.</p>";
            Details = "wear [nobake] /Clothing/Dance Party";
            ParameterVersions = CreateParamVersions(
                CreateParams(
                   Optional("nobake", typeof(bool), "Do not rebake the avatar's textures"),
                   "outfit", typeof(InventoryFolder),
                   "Folder of items to wear. See <a href='wiki/BotCommands#Inventory'>Inventory</a> for format."),
                CreateParams(
                   Optional("nobake", typeof(bool), "Do not rebake the avatar's textures"),
                   "outfit", typeof(InventoryItem),
                   "Item to wear. See <a href='wiki/BotCommands#Inventory'>Inventory</a> for format.")
               );
            ResultMap = CreateParams(
                 "message", typeof(string), "if success was false, the reason why",
                 "success", typeof(bool), "true if outfit was worn");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
                return ShowUsage();// " wear [outfit name] eg: 'wear /My Outfit/Dance Party";

            string target = String.Empty;
            bool bake = true;

            for (int ct = 0; ct < args.Length; ct++)
            {
                if (args[ct].Equals("nobake"))
                    bake = false;
                else
                    target = target + args[ct] + " ";
            }

            target = target.TrimEnd();

            try
            {
                List<InventoryItem> outfit = Client.GetFolderItems(target);
                Client.Appearance.ReplaceOutfit(outfit);
            }
            catch (Exception ex)
            {
                return Failure( "Invalid outfit (" + ex.Message + ")");
            }

            return Success(string.Empty);
        }
    }
}
