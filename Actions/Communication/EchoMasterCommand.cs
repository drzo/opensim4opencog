using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class EchoMasterCommand: Command
    {
        public EchoMasterCommand(cogbot.TextForm testClient)
		{
			Name = "echoMaster";
			Description = "Repeat everything that master says.";
            Category = CommandCategory.Communication;
		}

        public override string Execute(string[] args, UUID fromAgentID)
		{
			if (!Active)
			{
				Active = true;
                client.Self.OnChat += new AgentManager.ChatCallback(Self_OnChat);
				return "Echoing is now on.";
			}
			else
			{
				Active = false;
                client.Self.OnChat -= new AgentManager.ChatCallback(Self_OnChat);
				return "Echoing is now off.";
			}
		}

		void Self_OnChat(string message, ChatAudibleLevel audible, ChatType type, 
            ChatSourceType sourcetype, string fromName, UUID id, UUID ownerid, Vector3 position)
		{
            if (message.Length > 0 && (parent.MasterKey == id || (parent.MasterName == fromName && !parent.AllowObjectMaster)))
			    client.Self.Chat(message, 0, ChatType.Normal);
		}
    }
}
