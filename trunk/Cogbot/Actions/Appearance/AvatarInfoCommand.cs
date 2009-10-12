using System;
using System.Collections.Generic;
using System.Text;
using cogbot.Listeners;
using OpenMetaverse;
using cogbot.TheOpenSims;

namespace cogbot.Actions.Appearance
{
    public class AvatarInfoCommand : Command
    {
        public AvatarInfoCommand(BotClient testClient)
        {
            Name = "avatarinfo";
            Description = "Print out information on a nearby avatar. Usage: avatarinfo [firstname] [lastname]";
            Category = CommandCategory.Appearance;
            Parameters = new[] { new NamedParam(typeof(SimAvatar), typeof(UUID)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length == 0)
            {
                //return Failure(Usage);// " avatarinfo [firstname] [lastname]";
                int count = 0;
                foreach (var s in WorldObjects.SimAvatars)
                {
                    WriteLine(" {0}: {1}", s.GetName(), s.DebugInfo());
                    count++;
                }
                return Success("Avatars shown: " + count);
            }

            string targetName = String.Join(" ", args);

            int argsUsed;
            List<Primitive> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));
            foreach (var foundAv in PS)
            {
                StringBuilder output = new StringBuilder();
                targetName = String.Join(" ", args, 0, argsUsed);
                output.AppendFormat("{0} ({1})", targetName, foundAv.ID);
                output.AppendLine();
                for (int i = 0; i < foundAv.Textures.FaceTextures.Length; i++)
                {
                    if (foundAv.Textures.FaceTextures[i] != null)
                    {
                        Primitive.TextureEntryFace face = foundAv.Textures.FaceTextures[i];
                        AvatarTextureIndex type = (AvatarTextureIndex)i;

                        output.AppendFormat("{0}: {1}", type, face.TextureID);
                        output.AppendLine();
                    }
                }
                Success(output.ToString());
            }
            return SuccessOrFailure();
        }
    }
}
