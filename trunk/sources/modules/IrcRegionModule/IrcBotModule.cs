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
            client.Self.IM += IrcBot_IM;
        }

        private void IrcBot_IM(object sender, InstantMessageEventArgs im)
        {
            if (!client.IsRegionMaster)
            {
                return;
            }
            var e = im.IM;
            var type = e.Dialog;
            if (type == InstantMessageDialog.StartTyping || type == InstantMessageDialog.StopTyping) return;
            var message = e.Message;
            var id = e.FromAgentID;
            var fromname = e.FromAgentName;
            if (IrcCommand.IsChannelAgent(fromname)) return;
            if ((type == InstantMessageDialog.MessageFromAgent))
            {
                IrcCommand.IrcSend("" + fromname + ": " + message);
                return;
            }
            IrcCommand.IrcSend(fromname + " " + type + ": " + message);     
        }

        private void IrcBot_OnChat(object sender, ChatEventArgs e)
        {
            var type = e.Type;
            if (type == ChatType.StartTyping || type == ChatType.StopTyping) return;
            var message = e.Message;
            var id = e.SourceID;
            var fromname = e.FromName;
            System.Action<int> selfChat = (oo) =>
                                  {
                                      client.Self.OnChat(new ChatEventArgs(e.Simulator, message, e.AudibleLevel, e.Type,
                                                                           e.SourceType,
                                                                           fromname,
                                                                           id, e.OwnerID, e.Position));
                                  };
            //string message, ChatAudibleLevel audible, ChatType type, ChatSourceType sourcetype, string fromname, UUID id, UUID ownerid, Vector3 position
            if (!client.IsRegionMaster)
            {
                return;
            }
            if (IrcCommand == null) return;
            if (client.Self.AgentID == id || fromname == client.Self.Name)
            {
                int hash = message.IndexOf(" ");
                int colon = message.IndexOf(":");
                if (hash > 0 && colon > hash)
                {
                    fromname = message.Substring(0, colon);
                    message = message.Substring(colon + 1);
                    if (!IrcCommand.IsFromIRC(fromname))
                    {
                        return;
                    }
                    if (fromname == client.Self.Name)
                    {
                        return;
                    }
                    selfChat(0);
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
