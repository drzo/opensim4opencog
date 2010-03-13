using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions.Appearance
{
    public class SetTextureIndexCommand : Command, BotPersonalCommand
    {
        uint SerialNum = 666;

        public SetTextureIndexCommand(BotClient testClient)
        {
            Name = "SetTexture";
            Description = "Set appearance texture of avatar. Usage: SetTexture [face-index] [texture-uuid]";
            Category = CommandCategory.Appearance;
            Parameters = new[]
                             {
                                 new NamedParam(typeof (AvatarTextureIndex), typeof (AvatarTextureIndex)),
                                 new NamedParam(typeof (SimTexture), typeof (UUID))
                             };
        }
        /// <summary>
        /// //settexture 5 8dcd4a48-2d37-4909-9f78-f7a9eb4ef903
        /// </summary>
        /// <param name="args"></param>
        /// <param name="fromAgentID"></param>
        /// <param name="WriteLine"></param>
        /// <returns></returns>
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            UUID asset = UUID.Zero;
            AvatarTextureIndex index = AvatarTextureIndex.Unknown;
            if (args.Length > 1)
            {

                Object val;
                int argsUsed;
                if (TryEnumParse(typeof(AvatarTextureIndex), args, 0, out argsUsed, out val))
                {
                    index = (AvatarTextureIndex) val;
                }
                string targetName = args[argsUsed];
                if (!UUID.TryParse(targetName, out asset))
                {
                    asset = WorldSystem.SimAssetSystem.GetAssetUUID(targetName, AssetType.Texture);
                }
            }

            // Get our current appearance
            UUID target = Client.Self.AgentID;
            AgentSetAppearancePacket set = Client.Appearance.MakeAppearancePacket();
            Primitive.TextureEntry te;
            if (!Client.Appearances.ContainsKey(target))
            {                
                te = new Primitive.TextureEntry(set.ObjectData.TextureEntry, 0,
                                                set.ObjectData.TextureEntry.Length);
            }
            else
            {

                #region AvatarAppearance to AgentSetAppearance

                AvatarAppearancePacket appearance = TheBotClient.Appearances[target];

                set.AgentData.AgentID = Client.Self.AgentID;
                set.AgentData.SessionID = Client.Self.SessionID;
                set.AgentData.SerialNum = SerialNum++;
                set.AgentData.Size = new Vector3(2f, 2f, 2f); // HACK

                set.WearableData = new AgentSetAppearancePacket.WearableDataBlock[0];
                set.VisualParam = new AgentSetAppearancePacket.VisualParamBlock[appearance.VisualParam.Length];

                for (int i = 0; i < appearance.VisualParam.Length; i++)
                {
                    set.VisualParam[i] = new AgentSetAppearancePacket.VisualParamBlock();
                    set.VisualParam[i].ParamValue = appearance.VisualParam[i].ParamValue;
                }
                set.ObjectData.TextureEntry = appearance.ObjectData.TextureEntry;

                te = new Primitive.TextureEntry(appearance.ObjectData.TextureEntry, 0,
                   appearance.ObjectData.TextureEntry.Length);

                #endregion AvatarAppearance to AgentSetAppearance

            }


            #region TextureEntry

            if (index != AvatarTextureIndex.Unknown)
            {
                Primitive.TextureEntryFace face = te.CreateFace((uint) index);
                face.TextureID = asset;
            }

            set.ObjectData.TextureEntry = te.GetBytes();

            #endregion TextureEntry
            // Send the new appearance packet
            Client.Network.SendPacket(set);
            return Success("Setting texture entry for " + (AvatarTextureIndex)index + " to " + asset);

        }

    }

}
