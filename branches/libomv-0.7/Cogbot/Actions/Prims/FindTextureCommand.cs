using System;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class FindTextureCommand : Command, RegionMasterCommand
    {
        public FindTextureCommand(BotClient testClient)
        {
            Name = "findtexture";
            Description = "Checks if a specified texture is currently visible on a specified face. " +
                "Usage: findtexture [face-index] [texture-uuid]";
            Category = CommandCategory.Objects;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int faceIndex;
            UUID textureID;

            if (args.Length < 2)
                return ShowUsage();// " findtexture [face-index] [texture-uuid]";
            int argsUsed;
            Simulator CurSim = TryGetSim(args, out argsUsed) ?? Client.Network.CurrentSim;

            if (Int32.TryParse(args[argsUsed], out faceIndex) &&
                UUIDTryParse(args, argsUsed+1, out textureID, out argsUsed))
            {
                CurSim.ObjectsPrimitives.ForEach(
                    delegate(Primitive prim)
                    {
                        if (prim.Textures != null && prim.Textures.FaceTextures[faceIndex] != null)
                        {
                            if (prim.Textures.FaceTextures[faceIndex].TextureID == textureID)
                            {
                                Logger.Log(String.Format("Primitive {0} ({1}) has face index {2} set to {3}",
                                    prim.ID.ToString(), prim.LocalID, faceIndex, textureID.ToString()),
                                    Helpers.LogLevel.Info, Client);
                            }
                        }
                    }
                );

                return Success("Done searching");
            }
            else
            {
                return ShowUsage();// " findtexture [face-index] [texture-uuid]";
            }
        }
    }
}
