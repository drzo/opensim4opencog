using System;
using System.Collections.Generic;
using System.Reflection;
using System.Xml;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;
using cogbot.Actions;

namespace cogbot
{
    public class LoginDetails
    {
        public string FirstName;
        public string LastName;
        public string Password;
        public string StartLocation;
        public bool GroupCommands;
        public string MasterName;
        public UUID MasterKey;
        public string URI;
    }

    public class StartPosition
    {
        public string sim;
        public int x;
        public int y;
        public int z;

        public StartPosition()
        {
            this.sim = null;
            this.x = 0;
            this.y = 0;
            this.z = 0;
        }
    }



    // WOW WHAT A HACK!
    public static class ClientManagerRef
    {
        public static ClientManager ClientManager;
    }

   
    public class ClientManager
    {
        public Dictionary<string, BotClient> BotByName = new Dictionary<string, BotClient>();
        public BotClient lastBotClient = null;
        public BotClient CreateBotClient(string first, string last, string passwd, string simurl)
        {
            BotClient bc = new BotClient(this);
            if (!String.IsNullOrEmpty(first))
            {
                bc.BotLoginParams.FirstName = first;
            }
            if (!String.IsNullOrEmpty(last))
            {
                bc.BotLoginParams.LastName = last;
            }
            if (!String.IsNullOrEmpty(passwd))
            {
                bc.BotLoginParams.Password = passwd;
            }
            if (!String.IsNullOrEmpty(simurl))
            {
                bc.BotLoginParams.URI = simurl;
            }
            bc.Network.OnLogin += new NetworkManager.LoginCallback(delegate(LoginStatus login, string message) {
                if (login == LoginStatus.Success)
                {
                    Clients.Add(bc.Self.AgentID, bc);
                }
            });
            BotByName[""+bc.BotLoginParams.FirstName+" "+bc.BotLoginParams.LastName] = bc;
            //LoginParams loginParams = bc.Network.DefaultLoginParams(account.FirstName, account.LastName, account.Password, "BotClient", version);
            bc.TextFormClient(TextForm.SingleInstance);
            return bc;
        }

        public Utilities.TcpServer CreateHttpServer(int port, string botname)
        {

            BotClient cl = lastBotClient;
            foreach(BotClient bc in Clients.Values) {
                if (bc.Self.Name.Equals(botname)) {
                    cl = bc;
                }
            }
            return new Utilities.TcpServer(port,cl);
        }

        public Dictionary<UUID, BotClient> Clients = new Dictionary<UUID, BotClient>();
        public Dictionary<Simulator, Dictionary<uint, Primitive>> SimPrims = new Dictionary<Simulator, Dictionary<uint, Primitive>>();

        public bool Running = true;
        public bool GetTextures = true; //needed for iniminfo

        string version = "1.0.0";
        /// <summary>
        /// 
        /// </summary>
        /// <param name="accounts"></param>
        public ClientManager(List<LoginDetails> accounts, bool getTextures)
        {
            ClientManagerRef.ClientManager = this;

            GetTextures = getTextures;

            foreach (LoginDetails account in accounts)
                Login(account);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="account"></param>
        /// <returns></returns>
        public BotClient Login(LoginDetails account)
        {
            // Check if this CurrentClient is already logged in
            foreach (BotClient c in Clients.Values)
            {
                if (c.Self.FirstName == account.FirstName && c.Self.LastName == account.LastName)
                {
                    Logout(c);
                    break;
                }
            }

            BotClient client = new BotClient(this);

            // Optimize the throttle
            client.Throttle.Wind = 0;
            client.Throttle.Cloud = 0;
            client.Throttle.Land = 1000000;
            client.Throttle.Task = 1000000;

            client.GroupCommands = account.GroupCommands;
			client.MasterName = account.MasterName;
            client.MasterKey = account.MasterKey;
            client.AllowObjectMaster = client.MasterKey != UUID.Zero; // Require UUID for object master.

            LoginParams loginParams = client.Network.DefaultLoginParams(account.FirstName, account.LastName, account.Password, "BotClient", version);

            if (!String.IsNullOrEmpty(account.StartLocation))
                loginParams.Start = account.StartLocation;

            if (!String.IsNullOrEmpty(account.URI))
                loginParams.URI = account.URI;
            
            if (client.Network.Login(loginParams))
            {
                Clients[client.Self.AgentID] = client;

                if (client.MasterKey == UUID.Zero)
                {
                    UUID query = UUID.Random();
                    DirectoryManager.DirPeopleReplyCallback peopleDirCallback =
                        delegate(UUID queryID, List<DirectoryManager.AgentSearchData> matchedPeople)
                        {
                            if (queryID == query)
                            {
                                if (matchedPeople.Count != 1)
                                {
                                    Logger.Log("Unable to resolve master key from " + client.MasterName, Helpers.LogLevel.Warning);
                                }
                                else
                                {
                                    client.MasterKey = matchedPeople[0].AgentID;
                                    Logger.Log("Master key resolved to " + client.MasterKey, Helpers.LogLevel.Info);
                                }
                            }
                        };

                    client.Directory.OnDirPeopleReply += peopleDirCallback;
                    client.Directory.StartPeopleSearch(DirectoryManager.DirFindFlags.People, client.MasterName, 0, query);
                }

                Logger.Log("Logged in " + client.ToString(), Helpers.LogLevel.Info);
            }
            else
            {
                Logger.Log("Failed to login " + account.FirstName + " " + account.LastName + ": " +
                    client.Network.LoginMessage, Helpers.LogLevel.Warning);
            }

            return client;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="args"></param>
        /// <returns></returns>
        public BotClient Login(string[] args)
        {
            
            if (args.Length < 3)
            {
                WriteLine("Usage: login firstname lastname password [simname] [login server url]");
                return null;
            }
            LoginDetails account = new LoginDetails();
            account.FirstName = args[0];
            account.LastName = args[1];
            account.Password = args[2];

            if (args.Length > 3)
                account.StartLocation = NetworkManager.StartLocation(args[3], 128, 128, 40);

            if (args.Length > 4)
                if(args[4].StartsWith("http://"))
                    account.URI = args[4];

            if (string.IsNullOrEmpty(account.URI))
                account.URI = cogbot.OtherMainProgram.LoginURI;
            Logger.Log("Using login URI " + account.URI, Helpers.LogLevel.Info);

            return Login(account);
        }

        /// <summary>
        /// 
        /// </summary>
        public void Run()
        {
            WriteLine("Type quit to exit.  Type help for a command list.");

            while (Running)
            {
                PrintPrompt();
                string input = Console.ReadLine();
                DoCommandAll(input, UUID.Zero);
            }

            foreach (GridClient client in Clients.Values)
            {
                if (client.Network.Connected)
                    client.Network.Logout();
            }
        }

        private void PrintPrompt()
        {
            int online = 0;

            foreach (GridClient client in Clients.Values)
            {
                if (client.Network.Connected) online++;
            }

            Console.Write(online + " avatars online> ");
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="cmd"></param>
        /// <param name="fromAgentID"></param>
        /// <param name="imSessionID"></param>
        public void DoCommandAll(string cmd, UUID fromAgentID)
        {
            string[] tokens = cmd.Trim().Split(new char[] { ' ', '\t' });
            if (tokens.Length == 0)
                return;
            
            string firstToken = tokens[0].ToLower();
            if (String.IsNullOrEmpty(firstToken))
                return;

            string[] args = new string[tokens.Length - 1];
            if (args.Length > 0)
                Array.Copy(tokens, 1, args, 0, args.Length);

            if (firstToken == "login")
            {
                Login(args);
            }
            else if (firstToken == "quit")
            {
                Quit();
                Logger.Log("All clients logged out and program finished running.", Helpers.LogLevel.Info);
            }
            else if (firstToken == "help")
            {
                if (Clients.Count > 0)
                {
                    foreach (BotClient client in Clients.Values)
                    {
                        WriteLine(client.Commands["help"].Execute(args, UUID.Zero));
                        break;
                    }
                }
                else
                {
                    WriteLine("You must login at least one bot to use the help command");
                }
            }
            else if (firstToken == "script")
            {
                // No reason to pass this to all bots, and we also want to allow it when there are no bots
                ScriptCommand command = new ScriptCommand(null);
                Logger.Log(command.Execute(args, UUID.Zero), Helpers.LogLevel.Info);
            }
            else
            {
                // Make an immutable copy of the Clients dictionary to safely iterate over
                Dictionary<UUID, BotClient> clientsCopy = new Dictionary<UUID, BotClient>(Clients);

                int completed = 0;

                foreach (BotClient client in clientsCopy.Values)
                {
                    ThreadPool.QueueUserWorkItem((WaitCallback)
                        delegate(object state)
                        {
                            BotClient testClient = (BotClient)state;
                            if (testClient.Commands.ContainsKey(firstToken))
                                Logger.Log(testClient.Commands[firstToken].Execute(args, fromAgentID),
                                    Helpers.LogLevel.Info, testClient);
                            else
                                Logger.Log("Unknown command " + firstToken, Helpers.LogLevel.Warning);

                            ++completed;
                        },
                        client);
                }

                while (completed < clientsCopy.Count)
                    Thread.Sleep(50);
            }
        }

        private void WriteLine(string p)
        {
            throw new Exception("The method or operation is not implemented.");
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="CurrentClient"></param>
        public void Logout(BotClient client)
        {
            Clients.Remove(client.Self.AgentID);
            client.Network.Logout();
        }

        /// <summary>
        /// 
        /// </summary>
        public void Quit()
        {
            Running = false;
            // TODO: It would be really nice if we could figure out a way to abort the ReadLine here in so that Run() will exit.
        }

        internal void AddBotClientToTextForm(BotClient botClient)
        {
            throw new Exception("The method or operation is not implemented.");
        }

        internal void AddTextFormCommands(BotClient botClient)
        {
            throw new Exception("The method or operation is not implemented.");
        }
    }
}
