using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class SetMasterCommand: Command//,BotSystemCommand
    {
		public DateTime Created = DateTime.Now;
        private UUID resolvedMasterKey = UUID.Zero;
        private ManualResetEvent keyResolution = new ManualResetEvent(false);
        private UUID query = UUID.Zero;

        public SetMasterCommand(BotClient testClient)
		{
			Name = "setmaster";
            Description = "Sets the user name of the master user. The master user can IM to run commands. Usage: setmaster [name]";
            Category = CommandCategory.TestClient;
		}

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
		{
			string masterName = String.Empty;
			for (int ct = 0; ct < args.Length;ct++)
				masterName = masterName + args[ct] + " ";
            masterName = masterName.TrimEnd();
            if (masterName.Length == 0)
                return Failure(Usage);// " setmaster [name or uuid]";
            UUID masterUUID;
            if (UUID.TryParse(masterName, out masterUUID))
            {
                Client.MasterKey = masterUUID;
                if (String.IsNullOrEmpty(Client.MasterName))
                {
                    Client.MasterName = WorldSystem.GetUserName(masterUUID);
                }
                Client.Self.InstantMessage(
                    Client.MasterKey, "You are now my master.  IM me with \"help\" for a command list.");
                return Success("Set master UUID with name = " + Client.MasterName);
            }
            masterUUID = WorldSystem.GetUserID(masterName);
            //if (String.IsNullOrEmpty(Client.MasterName))                 
            Client.MasterName = masterName;
            if (masterUUID!=UUID.Zero)
            {
                Client.MasterName = masterName;
                Client.MasterKey = masterUUID;
                Client.Self.InstantMessage(
                    Client.MasterKey, "You are now my master.  IM me with \"help\" for a command list.");

                return Success("Set master UUID with name = " + Client.MasterName);
            }

            DirectoryManager.DirPeopleReplyCallback callback = new DirectoryManager.DirPeopleReplyCallback(KeyResolvHandler);
            Client.Directory.OnDirPeopleReply += callback;

            query = Client.Directory.StartPeopleSearch(DirectoryManager.DirFindFlags.People, masterName, 0);

            if (keyResolution.WaitOne(TimeSpan.FromMinutes(1), false))
            {
                Client.MasterKey = resolvedMasterKey;
                Client.MasterName = masterName;
                keyResolution.Reset();
                Client.Directory.OnDirPeopleReply -= callback;
            }
            else
            {
                keyResolution.Reset();
                Client.Directory.OnDirPeopleReply -= callback;
                return Failure("Unable to obtain UUID for \"" + masterName + "\". Master unchanged.");
            }
            
            // Send an Online-only IM to the new master
            Client.Self.InstantMessage(
                Client.MasterKey, "You are now my master.  IM me with \"help\" for a command list.");

            return Success(string.Format("Master set to {0} ({1})", masterName, Client.MasterKey.ToString()));
		}

        private void KeyResolvHandler(UUID queryid, List<DirectoryManager.AgentSearchData> matches)
        {
            if (query != queryid)
                return;

            if (matches.Count > 0) resolvedMasterKey = matches[0].AgentID;
            keyResolution.Set();
            query = UUID.Zero;
        }
    }
}
