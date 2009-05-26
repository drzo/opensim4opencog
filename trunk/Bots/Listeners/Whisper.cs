using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Listeners
{
    class Whisper : Listener
    {
        public Whisper(TextForm parent)
            : base(parent)
        {
            client.Self.OnInstantMessage += new AgentManager.InstantMessageCallback(Self_OnInstantMessage);
        }

        void Self_OnInstantMessage(InstantMessage im, Simulator simulator)
        {
            if (im.Message.Length > 0 && im.Dialog == InstantMessageDialog.MessageFromAgent)
            {
                parent.output(im.FromAgentName + " whispers, \"" + im.Message + "\".");
                parent.enqueueLispTask("(on-instantmessage (@\"" + im.FromAgentName + "\") (@\"" + im.Message + "\") )");

                Actions.Whisper whisper = (Actions.Whisper)parent.actions["whisper"];
                whisper.currentAvatar = im.FromAgentID;
                whisper.currentSession = im.IMSessionID;
            }
        }
    }
}
