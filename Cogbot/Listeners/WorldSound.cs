using OpenMetaverse.Packets;
using OpenMetaverse;

namespace cogbot.Listeners
{
    public partial class WorldObjects
    {
        protected void AttachedSoundHandler(Packet packet, Simulator simulator)
        {
            if (!MaintainSounds) return;
            if (!IsMaster(simulator)) return;
            AttachedSoundPacket sound = (AttachedSoundPacket)packet;
            Sound_OnAttachSound(sound.DataBlock.SoundID, sound.DataBlock.OwnerID, sound.DataBlock.ObjectID, sound.DataBlock.Gain, (SoundFlags)sound.DataBlock.Flags);
        }

        protected void AttachedSoundGainChangeHandler(Packet packet, Simulator simulator)
        {
            if (!MaintainSounds) return;
            if (!IsMaster(simulator)) return;

            AttachedSoundGainChangePacket change = (AttachedSoundGainChangePacket)packet;

            Sound_OnAttachSoundGainChange(change.DataBlock.ObjectID, change.DataBlock.Gain);
        }

        protected void PreloadSoundHandler(Packet packet, Simulator simulator)
        {
            if (!MaintainSounds) return;
            if (!IsMaster(simulator)) return;

            PreloadSoundPacket preload = (PreloadSoundPacket)packet;
            foreach (PreloadSoundPacket.DataBlockBlock data in preload.DataBlock)
            {

                Sound_OnPreloadSound(data.SoundID, data.OwnerID, data.ObjectID);
            }
        }

        protected void SoundTriggerHandler(Packet packet, Simulator simulator)
        {
            if (!MaintainSounds) return;
            if (!IsMaster(simulator)) return;

            SoundTriggerPacket trigger = (SoundTriggerPacket)packet;

            Sound_OnSoundTrigger(
                trigger.SoundData.SoundID,
                trigger.SoundData.OwnerID,
                trigger.SoundData.ObjectID,
                trigger.SoundData.ParentID,
                trigger.SoundData.Gain,
                trigger.SoundData.Handle,
                trigger.SoundData.Position);

        }
    }
}
