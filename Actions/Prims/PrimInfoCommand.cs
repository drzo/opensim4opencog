using System;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class PrimInfoCommand : Command
    {
        public PrimInfoCommand(BotClient testClient)
        {
            Name = "priminfo";
            Description = "Dumps information about a specified prim. " + "Usage: priminfo [prim-uuid]";
            Category = CommandCategory.Objects;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            UUID primID;

            if (args.Length < 1)
                return "Usage: priminfo [prim-uuid]";

            if (UUIDTryParse(args, 0, out primID))
            {
                Primitive target = Client.Network.CurrentSim.ObjectsPrimitives.Find(
                    delegate(Primitive prim) { return prim.ID == primID; }
                );

                if (target==null) target = Client.Network.CurrentSim.ObjectsAvatars.Find(
                    delegate(Avatar prim) { return prim.ID == primID; }
                );

                if (target != null)
                {
                    decscribePrim(target);

                    return "Done.";
                }
                else
                {
                    return "Could not find prim " + primID.ToString();
                }
            }
            else
            {
                return "Usage: priminfo [prim-uuid]";
            }
        }

        private void decscribePrim(Primitive target)
        {
            WriteLine("PrimInfo: " + target.ToString());
            WriteLine(" Type: " + WorldSystem.GetPrimTypeName(target));
            WriteLine(" Light: " + target.Light);

            if (target.ParticleSys.CRC != 0)
                WriteLine("Particles: " + target.ParticleSys);

            WriteLine(" TextureEntry:");
            if (target.Textures != null)
            {
                WriteLine(String.Format("  Default texure: {0}",
                    target.Textures.DefaultTexture.TextureID.ToString()));

                for (int i = 0; i < target.Textures.FaceTextures.Length; i++)
                {
                    if (target.Textures.FaceTextures[i] != null)
                    {
                        WriteLine(String.Format("  Face {0}: {1}", i,
                            target.Textures.FaceTextures[i].TextureID.ToString()));
                    }
                }
            }
            else
            {
                Logger.Log("decscribePrim null", Helpers.LogLevel.Info, Client);
            }
        }
    }
}
