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
            client.Self.ChatFromSimulator += IrcBot_OnChat;
            //throw new NotImplementedException();
        }

        private void IrcBot_OnChat(object sender, ChatEventArgs e)
        {
            var type = e.Type;
            var message = e.Message;
            var id = e.SourceID;
            var fromname = e.FromName;
            //string message, ChatAudibleLevel audible, ChatType type, ChatSourceType sourcetype, string fromname, UUID id, UUID ownerid, Vector3 position
            if (!client.IsRegionMaster) return;
            if (IrcCommand == null) return;
            if (type == ChatType.StartTyping || type == ChatType.StopTyping) return;
            if (client.Self.AgentID == id || e.FromName == client.Self.Name)
            {
                int hash = message.IndexOf(" ");
                int colon = message.IndexOf(":");
                if (hash > 0 && colon > hash)
                {
                    fromname = message.Substring(0, colon);
                    message = message.Substring(colon + 1);
                    if (!IrcCommand.IsFromIRC(fromname)) return;
                    client.Self.OnChat(new ChatEventArgs(e.Simulator, message, e.AudibleLevel, e.Type, e.SourceType,
                                                         fromname,
                                                         id, e.OwnerID, e.Position));
                    return;
                } else
                {
                    // need to hear ourselves talk to others!!
                   // so no "return;"
                }
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
