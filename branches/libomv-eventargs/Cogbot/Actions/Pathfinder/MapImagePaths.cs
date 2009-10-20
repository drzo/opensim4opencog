using System;
using System.Drawing;
using System.Net;
using OpenMetaverse;

namespace cogbot.Actions.Pathfinder
{
    public class MapImagePaths : cogbot.Actions.Command, SystemApplicationCommand
    {
        public MapImagePaths(BotClient client)
        {
            Name = GetType().Name;
            Description = "Reads the sim map for improving routes";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            Image I = null;// WorldSystem.miniMap.Image;
            if (I == null)
            {

                String picUri = "http://71.197.210.170:9000/index.php?method=regionImaged63a88fe7db448c6b1a52b7628fe8d0d";
                // Create the requests.
                WebRequest requestPic = WebRequest.Create(picUri);

                WebResponse responsePic = requestPic.GetResponse();

                I = Image.FromStream(responsePic.GetResponseStream());

            }

            WorldSystem.SimPaths.UpdateFromImage(I);
            return Success("Ran " + Name);
        }
    }
}