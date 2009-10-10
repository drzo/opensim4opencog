using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class LoadCommand : Command, BotSystemCommand
    {
        public LoadCommand(BotClient testClient)
		{
			Name = "load";
			Description = "Loads commands from a dll. (Usage: load AssemblyNameWithoutExtension)";
            Category = CommandCategory.TestClient;
		}

		public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
		{
			if (args.Length < 1)
				return Failure(Usage);// " load AssemblyNameWithoutExtension";

            BotClient Client = TheBotClient;

			string filename = AppDomain.CurrentDomain.BaseDirectory + args[0];
		    string loadfilename = filename;
            if (!filename.EndsWith(".dll") && !filename.EndsWith(".exe"))
            {
                loadfilename += ".dll";
            }
            try
            {
                Assembly assembly = Assembly.LoadFile(loadfilename);
                ClientManager.SingleInstance.RegisterAssembly(assembly);
                Client.LoadAssembly(assembly);
                return Success("Assembly " + filename + " loaded.");
            }
            catch (Exception e)
            {                
                   return Failure("failed: load " + filename + " " +e);
            }
		}
    }
}
