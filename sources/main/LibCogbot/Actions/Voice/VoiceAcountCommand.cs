using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Voice
{
    public class VoiceAccountCommand : Command, RegionMasterCommand, AsynchronousCommand
    {
        private AutoResetEvent ProvisionEvent = new AutoResetEvent(false);
        private string VoiceAccount = null;
        private string VoicePassword = null;

        public VoiceAccountCommand(BotClient testClient)
        {
            Name = "voiceaccount";
            TheBotClient = testClient;
        }

        public override void MakeInfo()
        {
            Description = "obtain voice account info. Usage: voiceaccount";
            Category = CommandCategory.Voice;
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

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (!IsVoiceManagerRunning())
                return Failure(String.Format("VoiceManager not running for {0}", Client.Self.Name));

            if (!TheBotClient.VoiceManager.RequestProvisionAccount())
            {
                return Failure("RequestProvisionAccount failed. Not available for the current grid?");
            }
            ProvisionEvent.WaitOne(30*1000, false);

            if (String.IsNullOrEmpty(VoiceAccount) && String.IsNullOrEmpty(VoicePassword))
            {
                return Failure(String.Format("Voice account information lookup for {0} failed.", Client.Self.Name));
            }

            return Success(String.Format("Voice Account for {0}: user \"{1}\", password \"{2}\"",
                                         Client.Self.Name, VoiceAccount, VoicePassword));
        }

        private void Voice_OnProvisionAccount(string username, string password)
        {
            VoiceAccount = username;
            VoicePassword = password;

            ProvisionEvent.Set();
        }
    }
}