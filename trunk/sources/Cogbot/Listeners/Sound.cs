using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Listeners
{
    class Sound : Listener
    {
        public Sound(BotClient parent)
            : base(parent)
        {
            client.Sound.OnPreloadSound += new SoundManager.PreloadSoundCallback(Sound_OnPreloadSound);
            client.Sound.OnSoundTrigger += new SoundManager.SoundTriggerCallback(Sound_OnSoundTrigger);
        }

        void Sound_OnSoundTrigger(UUID soundID, UUID ownerID, UUID objectID, UUID parentID, 
            float gain, ulong regionHandle, Vector3 position)
        {
            //parent.WriteLine("sound trigger " + soundID);
        }

        void Sound_OnPreloadSound(UUID soundID, UUID ownerID, UUID objectID)
        {
            //parent.WriteLine("preload sound " + soundID);
        }
    }
}
