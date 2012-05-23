using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Communication
{
    public class EchoMasterCommand: Command, BotPersonalCommand, BotStatefullCommand
    {
        public EchoMasterCommand(BotClient testClient)
            : base(testClient)
        {
            Name = "echoMaster";
            Description = "Repeat everything that master says from open channel to open channel.";
            Usage = Htmlize.Usage("echomaster", "toggles this commnand on/off");
            Category = CommandCategory.Communication;
            Parameters = NamedParam.CreateParams();
            ResultMap = NamedParam.CreateParams(
                "message", typeof (string), "if success was false, the reason why",
                "success", typeof (bool), "true if command was successful");
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
            if (e.Message.Length > 0 &&
                (Client.MasterKey == e.SourceID || (Client.MasterName == e.FromName && !Client.AllowObjectMaster)))
                Client.Self.Chat(e.Message, 0, ChatType.Normal);
        }


        #region Implementation of IDisposable

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {
            
        }

        #endregion
    }
}
