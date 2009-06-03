
using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class VoiceAccountCommand : Command
    {
        private AutoResetEvent ProvisionEvent = new AutoResetEvent(false);
        private string VoiceAccount = null;
        private string VoicePassword = null;

        public VoiceAccountCommand(BotClient testClient)
        {
            Name = "voiceaccount";
            Description = "obtain voice account info. Usage: voiceaccount";
            Category = CommandCategory.Voice;

            TheBotClient = testClient;
        }

        private bool registered = false;

        private bool IsVoiceManagerRunning()
        {
            if (null == TheBotClient.VoiceManager) return false;

            if (!registered)
            {
                TheBotClient.VoiceManager.OnProvisionAccount += Voice_OnProvisionAccount;
                registered = true;
            }
            return true;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (!IsVoiceManagerRunning())
                return String.Format("VoiceManager not running for {0}", Client.Self.Name);

            if (!TheBotClient.VoiceManager.RequestProvisionAccount())
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