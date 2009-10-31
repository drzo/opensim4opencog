using OpenMetaverse.Packets;
using OpenMetaverse;

namespace cogbot.Listeners
{
    public partial class WorldObjects
    {
        protected void AttachedSoundHandler(object sender, PacketReceivedEventArgs e)
        {
            var simulator = e.Simulator;
            var packet = e.Packet;
            if (!MaintainSounds) return;
            if (!IsMaster(simulator)) return;
            AttachedSoundPacket sound = (AttachedSoundPacket)packet;
            Sound_OnAttachSound(this,
                                new AttachedSoundEventArgs(sound.DataBlock.SoundID, sound.DataBlock.OwnerID,
                                                           sound.DataBlock.ObjectID, sound.DataBlock.Gain,
                                                           (SoundFlags) sound.DataBlock.Flags));
        }

        protected void AttachedSoundGainChangeHandler(object sender, PacketReceivedEventArgs e)
        {
            var simulator = e.Simulator;
            var packet = e.Packet;

            if (!MaintainSounds) return;
            if (!IsMaster(simulator)) return;

            AttachedSoundGainChangePacket change = (AttachedSoundGainChangePacket)packet;

            Sound_OnAttachSoundGainChange(this,new AttachedSoundGainChangeEventArgs(change.DataBlock.ObjectID, change.DataBlock.Gain));
        }

        protected void PreloadSoundHandler(object sender, PacketReceivedEventArgs e)
        {
            var simulator = e.Simulator;
            var packet = e.Packet;

            if (!MaintainSounds) return;
            if (!IsMaster(simulator)) return;

            PreloadSoundPacket preload = (PreloadSoundPacket)packet;
            foreach (PreloadSoundPacket.DataBlockBlock data in preload.DataBlock)
            {

                Sound_OnPreloadSound(this, new PreloadSoundEventArgs(data.SoundID, data.OwnerID, data.ObjectID));
            }
        }

        protected void SoundTriggerHandler(object sender, PacketReceivedEventArgs e)
        {
            var simulator = e.Simulator;
            var packet = e.Packet;

            if (!MaintainSounds) return;
            if (!IsMaster(simulator)) return;

            SoundTriggerPacket trigger = (SoundTriggerPacket)packet;

            Sound_OnSoundTrigger(this, new SoundTriggerEventArgs(
                                           trigger.SoundData.SoundID,
                                           trigger.SoundData.OwnerID,
                                           trigger.SoundData.ObjectID,
                                           trigger.SoundData.ParentID,
                                           trigger.SoundData.Gain,
                                           trigger.SoundData.Handle,
                                           trigger.SoundData.Position));

        }
    }
}
