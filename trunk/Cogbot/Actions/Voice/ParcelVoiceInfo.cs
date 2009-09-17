using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class ParcelVoiceInfoCommand : Command, RegionMasterCommand
    {
        private AutoResetEvent ParcelVoiceInfoEvent = new AutoResetEvent(false);
        private string VoiceRegionName = null;
        private int VoiceLocalID = -1;
        private string VoiceChannelURI = null;

        public ParcelVoiceInfoCommand(BotClient testClient)
        {
            Name = "voiceparcel";
            Description = "obtain parcel voice info. Usage: voiceparcel";
            Category = CommandCategory.Other;

            TheBotClient = testClient;
            Parameters = new [] {  new NamedParam(typeof(GridClient), null) };
        }

        private bool registered = false;

        private bool IsVoiceManagerRunning() 
        {
            BotClient Client = TheBotClient;
            if (null == Client.VoiceManager) return false;
            
            if (!registered)
            {
                Client.VoiceManager.OnParcelVoiceInfo += Voice_OnParcelVoiceInfo;
                registered = true;
            }
            return true;           
        }


        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            BotClient Client = TheBotClient;
            if (!IsVoiceManagerRunning()) 
                return String.Format("VoiceManager not running for {0}", fromAgentID);

            if (!Client.VoiceManager.RequestParcelVoiceInfo()) 
            {
                return "RequestParcelVoiceInfo failed. Not available for the current grid?";
            }
            ParcelVoiceInfoEvent.WaitOne(30 * 1000, false);

            if (String.IsNullOrEmpty(VoiceRegionName) && -1 == VoiceLocalID)
            {
                return String.Format("Parcel Voice Info request for {0} failed.", Client.Self.Name);
            }

            return String.Format("Parcel Voice Info request for {0}: region name \"{1}\", parcel local id {2}, channel URI {3}",
                                 Client.Self.Name, VoiceRegionName, VoiceLocalID, VoiceChannelURI);
        }

        void Voice_OnParcelVoiceInfo(string regionName, int localID, string channelURI)
        {
            VoiceRegionName = regionName;
            VoiceLocalID = localID;
            VoiceChannelURI = channelURI;

            ParcelVoiceInfoEvent.Set();
        }
    }
}
