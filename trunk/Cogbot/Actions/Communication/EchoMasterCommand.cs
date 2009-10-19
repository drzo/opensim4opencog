using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class EchoMasterCommand: Command, BotPersonalCommand
    {
        public EchoMasterCommand(BotClient testClient)
		{
			Name = "echoMaster";
			Description = "Repeat everything that master says.";
            Category = CommandCategory.Communication;
		}

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
		{
			if (!Active)
			{
				Active = true;
                Client.Self.OnChat += new AgentManager.ChatCallback(Self_OnChat);
				return Success("Echoing is now on.");
			}
			else
			{
				Active = false;
                Client.Self.OnChat -= new AgentManager.ChatCallback(Self_OnChat);
				return Success("Echoing is now off.");
			}
		}

		void Self_OnChat(string message, ChatAudibleLevel audible, ChatType type, 
            ChatSourceType sourcetype, string fromName, UUID id, UUID ownerid, Vector3 position)
		{
			if (message.Length > 0 && (Client.MasterKey == id || (Client.MasterName == fromName && !Client.AllowObjectMaster)))
			    Client.Self.Chat(message, 0, ChatType.Normal);
		}
    }
}
