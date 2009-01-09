
using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;
using OpenMetaverse.Utilities;

namespace cogbot.Actions
{
    public class VoiceAccountCommand : Command
    {
        private AutoResetEvent ProvisionEvent = new AutoResetEvent(false);
        private string VoiceAccount = null;
        private string VoicePassword = null;

        public VoiceAccountCommand(cogbot.TextForm testClient)
        {
            Name = "voiceaccount";
            Description = "obtain voice account info. Usage: voiceaccount";
            Category = CommandCategory.Voice;

           // Client = testClient;
        }

        private bool registered = false;

        private bool IsVoiceManagerRunning()
        {
            VoiceManager voiceManager = parent.GetVoiceManager();
            if (null == voiceManager) return false;

            if (!registered)
            {
                voiceManager.OnProvisionAccount += Voice_OnProvisionAccount;
                registered = true;
            }
            return true;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            VoiceManager voiceManager = parent.GetVoiceManager();
            if (!IsVoiceManagerRunning())
                return String.Format("VoiceManager not running for {0}", Client.Self.Name);

            if (!voiceManager.RequestProvisionAccount())
            {
                return "RequestProvisionAccount failed. Not available for the current grid?";
            }
            ProvisionEvent.WaitOne(30 * 1000, false);

            if (String.IsNullOrEmpty(VoiceAccount) && String.IsNullOrEmpty(VoicePassword))
            {
                return String.Format("Voice account information lookup for {0} failed.", Client.Self.Name);
            }

            return String.Format("Voice Account for {0}: user \"{1}\", password \"{2}\"",
                                 Client.Self.Name, VoiceAccount, VoicePassword);
        }

        void Voice_OnProvisionAccount(string username, string password)
        {
            VoiceAccount = username;
            VoicePassword = password;

            ProvisionEvent.Set();
        }
    }
}