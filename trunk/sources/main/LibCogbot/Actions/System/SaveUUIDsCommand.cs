using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Cogbot.World;
using OpenMetaverse;
using Cogbot;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Search
{
    public class SaveUUIDsCommand : Command, GridMasterCommand
    {
        public SaveUUIDsCommand(BotClient testClient)
        {
            Name = "Save UUIDs";
        }

        public override void MakeInfo()
        {
            Description = "Saves resolution of UUID types in a file.";
            Details = AddUsage("saveuuids <filename>", "create file filename") +
                      AddExample(
                          @"
/saveuuids c:\somepath\somefile.xml  might produce a file like

<assets>
  <asset type=""Texture"" uuid=""e97cf410-8e61-7005-ec06-629eba4cd1fb"" name=""alpha_gradient"" />
  <asset type=""Texture"" uuid=""38b86f85-2575-52a9-a531-23108d8da837"" name=""alpha_gradient_2d"" />
  <asset type=""Texture"" uuid=""6c1594de-1e66-273c-a2ab-8f0ffa8b4633"" name=""oi_hud_cus_3_0"" />
  <asset type=""Texture"" uuid=""bb31fe48-8566-eec0-e96b-64025f832b63"" name=""oi_hud_cus_2_4"" />
  <asset type=""Texture"" uuid=""c946959a-26ae-eb66-efa0-20154057789d"">
    <name>oi_hud_cus_2_2</name>
    <name>oi_hud_cus_2_1</name>
  </asset>
  <asset type=""Sound"" uuid=""ed124764-705d-d497-167a-182cd9fa2e6c"" name=""bell_ting"" />
  <asset type=""Sound"" uuid=""4c8c3c77-de8d-bde2-b9b8-32635e0fd4a6"" name=""click"" />
  <asset type=""Animation"" uuid=""46bb4359-de38-4ed8-6a22-f1f52fe8f506"">
    <name>aim_l_bow</name>
    <name>avatar_aim_l_bow</name>
    <name>aim_bow_l</name>
  </asset>
  <asset type=""Animation"" uuid=""758547a2-7212-d533-43e3-1666eda1705e"" name=""drinking_or_snowcone"" />
  <!--avatar_curtsy NODATA-->
  <!--avatar_slowwalk NODATA-->
  <!--avatar_uphillwalk NODATA-->
</assets>
",
                          @"makes this file
an asset can have more than one name because the name comes from the containing object (inventory or object)
and could be expressed in more than one location");
            Parameters = CreateParams("path", typeof (string), "Path to file to save uuids in");

            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            string filename = "AssetMapping3.xml";
            if (args.Length > 0) filename = String.Join(" ", args, 0, args.Length).Trim();
            return Success("Done with UUIDs " + SimAssetStore.SaveAssetFile(filename, false));
        }
    }
}