using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions.Communication
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
                Client.Self.ChatFromSimulator += Self_ChatFromSimulator;
				return Success("Echoing is now on.");
			}
			else
			{
				Active = false;
                Client.Self.ChatFromSimulator -= Self_ChatFromSimulator;
				return Success("Echoing is now off.");
			}
		}

        void Self_ChatFromSimulator(object sender, ChatEventArgs e)
		{
            if (e.Message.Length > 0 && (Client.MasterKey == e.SourceID || (Client.MasterName == e.FromName && !Client.AllowObjectMaster)))
                Client.Self.Chat(e.Message, 0, ChatType.Normal);
		}
    }
}
