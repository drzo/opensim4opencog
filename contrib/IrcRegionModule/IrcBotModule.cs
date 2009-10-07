using System;
using cogbot.Listeners;
using cogbot;
using OpenMetaverse;

namespace IrcRegionModule
{
    public class IrcBotModule : WorldObjectsModule
    {
        public static IrcCommand IrcCommand;

        public IrcBotModule(BotClient parent)
            : base(parent)
        {
        }

        public override string GetModuleName()
        {
            return GetType().Name;
        }

        public override void StartupListener()
        {
            client.Self.OnChat += IrcBot_OnChat;
            //throw new NotImplementedException();
        }

        private void IrcBot_OnChat(string message, ChatAudibleLevel audible, ChatType type, ChatSourceType sourcetype, string fromname, UUID id, UUID ownerid, Vector3 position)
        {
            if (!client.IsRegionMaster) return;
            if (IrcCommand == null) return;
            if (type == ChatType.StartTyping || type == ChatType.StopTyping) return;
            if (client.Self.AgentID == id)
            {
                int hash = message.IndexOf(" ");
                int colon = message.IndexOf(":");
                if (hash>0 && colon>hash) return;
            }
            if (IrcCommand.IsChannelAgent(fromname)) return;
            if ((type == ChatType.Normal))
            {
                IrcCommand.IrcSend("" + fromname + ": " + message);
                return;
            }
            if ((type == ChatType.Shout))
            {
                IrcCommand.IrcSend(fromname + " Shouts: " + message);
                return;
            }
            if ((type == ChatType.Whisper))
            {
                IrcCommand.IrcSend(fromname + " Whispers: " + message);
                return;
            }
            IrcCommand.IrcSend(fromname + " " + type + ": " + message);
           
        }

        public override void Dispose()
        {
            //throw new NotImplementedException();
        }
    }
}
