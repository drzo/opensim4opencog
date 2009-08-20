using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Windows.Forms;
using System.Threading;
using cogbot.ScriptEngines;
using OpenMetaverse;
using cogbot.Actions;
using Radegast;
using Action = cogbot.Actions.Action;
using NotImplementedException=sun.reflect.generics.reflectiveObjects.NotImplementedException;

//using Radegast;
namespace cogbot
{
    public delegate void DescribeDelegate(bool detailed, OutputDelegate WriteLine);
    enum Modes { normal, tutorial };
    public delegate void OutputDelegate(string str, params object[] args);

    public class ClientManager
    {

        public void AddTool(string name, string text, EventHandler threadStart)
        {
            //      base.Invoke(new AddToolDelegate(AddTool0), new object[] {name, text, threadStart});
        }

        public delegate void AddToolDelegate(string name, string text, EventHandler threadStart);
        public void AddTool0(string name, string text, EventHandler threadStart)
        {
            // SuspendLayout();
            ToolStripMenuItem stripMenuItem =
                new ToolStripMenuItem { Name = name, Size = new System.Drawing.Size(168, 22), Text = text };
            stripMenuItem.Click += threadStart;
            //this.toolsToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {stripMenuItem});
            //ResumeLayout();
        }

        public ICollection<BotClient> BotClients
        {
            get
            {
                // Make an immutable copy of the Clients list to safely iterate over
                lock (BotByName) return new List<BotClient>(BotByName.Values);
            }
        }

        public static bool DownloadTextures = false;
        public static int nextTcpPort = 5555;
        static public ClientManager SingleInstance = null;
        public static int debugLevel = 2;

        public BotClient OnlyOneCurrentBotClient;

        //public bool GetTextures = false;

        //public UUID GroupID = UUID.Zero;
        //public Dictionary<UUID, GroupMember> GroupMembers = null;// new Dictionary<UUID, GroupMember>();
        //public Dictionary<UUID, AvatarAppearancePacket> Appearances = null;// new Dictionary<UUID, AvatarAppearancePacket>();
        // public Dictionary<string, cogbot.Actions.Action> Commands = null;//new Dictionary<string, cogbot.Actions.Action>();
        //public bool Running = true;
        //public bool GroupCommands = false;
        //public string MasterName = String.Empty;
        //public UUID MasterKey = UUID.Zero;
        //public bool AllowObjectMaster = false;
        //  public cogbot.ClientManager ClientManager;
        //  public VoiceManager VoiceManager;
        // Shell-like inventory commands need to be aware of the 'current' inventory folder.
        //public InventoryFolder CurrentDirectory = null;

        //public BotClient CurrentClient;
        public OutputDelegate outputDelegate;
        ///public Dictionary<string, DescribeDelegate> describers;

        public List<Type> registrationTypes;
        public List<Type> registeredSystemApplicationCommandTypes = new List<Type>();
        public Dictionary<string, Actions.Action> groupActions;
        public Dictionary<string, Tutorials.Tutorial> tutorials;

        public bool describeNext;
        private int describePos;
        private string currTutorial;

        public int BoringNamesCount = 0;
        public int GoodNamesCount = 0;
        public int RunningMode = (int)Modes.normal;
        public UUID AnimationFolder = UUID.Zero;

        //InventoryEval searcher =null; // new InventoryEval(this);
        //public Inventory Inventory;
        //public InventoryManager Manager;
        public Configuration config;
        //Utilities.BotTcpServer UtilitiesTcpServer;
        public String taskInterperterType = "DotLispInterpreter";// DotLispInterpreter,CycInterpreter or ABCLInterpreter
        static List<LoginDetails> accounts = new List<LoginDetails>();
        ///public static ClientManager this = new ClientManager(accounts, false);
        // public static Simian.Simian simulator = new Simian.Simian();

        public ClientManager()
        {
            SingleInstance = this;
            config = new Configuration();
            config.loadConfig();
            nextTcpPort = config.tcpPort;
            //LoginDetails details = GetDetailsFromConfig(config);
            //  CurrentClient = new BotClient(this);// Login(details);
            //  CurrentClient.TextFormClient(this);
            //GroupMembers = new Dictionary<UUID, GroupMember>();
            //Appearances = new Dictionary<UUID, AvatarAppearancePacket>();

            // CurrentClient.Settings.ALWAYS_DECODE_OBJECTS = true;
            // CurrentClient.Settings.ALWAYS_REQUEST_OBJECTS = true;
            //  CurrentClient.Settings.OBJECT_TRACKING = true;

            //Manager = CurrentClient.Inventory;
            //Inventory = Manager.Store;
            groupActions = new Dictionary<string, cogbot.Actions.Action>();
            //groupActions["login"] = new Login(null);


            //   CurrentClient.Settings.LOGIN_SERVER = config.simURL;
            // extraSettings();


            // CurrentClient.Network.OnConnected += new NetworkManager.ConnectedCallback(Network_OnConnected);
            // CurrentClient.Network.OnDisconnected += new NetworkManager.DisconnectedCallback(Network_OnDisconnected);
            // CurrentClient.Network.OnLogin += new NetworkManager.LoginCallback(Network_OnLogin);

            //outputDelegate = new OutputDelegate(WriteLine);

            registrationTypes = new List<Type>();
            //registrationTypes["chat"] = new Listeners.Chat(this);
            //registrationTypes["avatars"] = new Listeners.Avatars(this);
            //registrationTypes["teleport"] = new Listeners.Teleport(this);
            //registrationTypes["whisper"] = new Listeners.Whisper(this);
            //ObjectSystem = new Listeners.Objects(this);
            //registrationTypes["bump"] = new Listeners.Bump(this);
            //registrationTypes["sound"] = new Listeners.Sound(this);


            tutorials = new Dictionary<string, cogbot.Tutorials.Tutorial>();
            //  tutorials["tutorial1"] = new Tutorials.Tutorial1(this);

            describeNext = true;

            //   RegisterAllCommands(Assembly.GetExecutingAssembly());

            //   UtilitiesTcpServer = new cogbot.Utilities.BotTcpServer(this);
            // Start the server
            ///UtilitiesTcpServer.startSocketListener();

            //Client.Network.RegisterCallback(PacketType.AgentDataUpdate, new NetworkManager.PacketCallback(AgentDataUpdateHandler));
            //  CurrentClient.Network.OnLogin += new NetworkManager.LoginCallback(LoginHandler);
            //Client.Self.OnInstantMessage += new AgentManager.InstantMessageCallback(Self_OnInstantMessage);
            //Client.Groups.OnGroupMembers += new GroupManager.GroupMembersCallback(GroupMembersHandler);
            //Client.Inventory.OnObjectOffered += new InventoryManager.ObjectOfferedCallback(Inventory_OnInventoryObjectReceived);

            //Client.Network.RegisterCallback(PacketType.AvatarAppearance, new NetworkManager.PacketCallback(AvatarAppearanceHandler));
            //Client.Network.RegisterCallback(PacketType.AlertMessage, new NetworkManager.PacketCallback(AlertMessageHandler));
            RegisterAssembly(Assembly.GetExecutingAssembly());
        }

        public void RegisterBotSystemCommands(Assembly assembly)
        {
            foreach (Type t in assembly.GetTypes())
            {
                try
                {
                    if (t.IsSubclassOf(typeof(Command)))
                    {
                        if (!typeof(SystemApplicationCommand).IsAssignableFrom(t)) continue;
                        RegisterSystemCommand(t);
                    }
                }
                catch (Exception e)
                {
                    WriteLine(e.ToString());
                }
            }
        }
        private LoginDetails GetDetailsFromConfig(Configuration config)
        {
            LoginDetails details = new LoginDetails();
            details.FirstName = config.firstName;
            details.Password = config.password;
            details.LastName = config.lastName;
            details.URI = config.simURL;
            return details;
            //details.StartLocation =.
        }



        public void SetOnlyOneCurrentBotClient(string currentBotClient)
        {
            if (String.IsNullOrEmpty(currentBotClient))
            {
                OnlyOneCurrentBotClient = null;
                return;
            }
            BotClient oBotClient = GetBotByName(currentBotClient);
            if (oBotClient == null)
                WriteLine("SetOnlyOneCurrentBotClient to unkown bot: " + currentBotClient);
            else OnlyOneCurrentBotClient = oBotClient;

        }

        public static BotClient GetBotByName(string CurrentBotClient)
        {
            if (BotByName.ContainsKey(CurrentBotClient))
                return BotByName[CurrentBotClient];
            else
            {
                String lCurrentBotClient = CurrentBotClient.ToLower();
                foreach (string name in BotByName.Keys)
                {
                    if (name.ToLower() == lCurrentBotClient)
                    {
                        return BotByName[name];
                    }
                }
                foreach (string name in BotByName.Keys)
                {
                    if (name.ToLower().Contains(lCurrentBotClient))
                    {
                        return BotByName[name];
                    }
                }
            }
            return null;
        }


        public string ExecuteCommand(string text, OutputDelegate WriteLine)
        {
            WriteLine("textform> " + text);
            String res = ExecuteBotsCommand(text, WriteLine);
            if (!String.IsNullOrEmpty(res)) return res;
            return ExecuteSystemCommand(text, WriteLine);
        }

        private string ExecuteBotsCommand(string text, OutputDelegate WriteLine)
        {
            string res = String.Empty;
            if (string.IsNullOrEmpty(text)) return res;
            text = text.Trim();
            if (string.IsNullOrEmpty(text)) return res;
            if (BotByName.Count == 0 && LastBotClient != null) return LastBotClient.ExecuteBotCommand(text, WriteLine);
            if (OnlyOneCurrentBotClient != null)
            {
                return OnlyOneCurrentBotClient.ExecuteBotCommand(text, WriteLine);

            }

            foreach (BotClient currentClient in BotClients)
                if (currentClient != null)
                {

                    res += currentClient.ExecuteBotCommand(text, WriteLine).Trim();
                    if (!String.IsNullOrEmpty(res))
                    {
                        res += "\n";
                    }
                }
            return res;
        }

        public string ExecuteSystemCommand(string text, OutputDelegate WriteLine)
        {
            string res = String.Empty;
            try
            {
                {
                    if (string.IsNullOrEmpty(text)) return res;
                    text = text.Trim();
                    if (string.IsNullOrEmpty(text)) return res;
                   
                    string verb = text.Split(null)[0];
                    if (groupActions.ContainsKey(verb))
                    {
                        if (text.Length > verb.Length)
                            res += groupActions[verb].acceptInputWrapper(verb, text.Substring(verb.Length + 1), WriteLine);
                        else
                            res += groupActions[verb].acceptInputWrapper(verb, "", WriteLine);
                        return res;
                    }
                    WriteLine("I don't understand the ExecuteSystemCommand " + verb + ".");
                }
                return res;
            }
            catch (Exception e)
            {
                WriteLine("ClientManager:" + e);
                return res;
            }
        }


        public void ShutDown()
        {
            logout();
            foreach (BotClient CurrentClient in BotClients)
            {
                CurrentClient.ShutDown();
            }
            Application.DoEvents();
            Application.Exit();
            Application.ExitThread();
        }


        public void logout()
        {
            foreach (BotClient CurrentClient in BotClients)
                if (CurrentClient.Network.Connected)
                    CurrentClient.Network.Logout();
            config.saveConfig();
        }

        // for lisp to call
        public void output(string txt)
        {
            WriteLine(txt);
        }

        public void WriteLine(string str, params object[] args)
        {
            if (outputDelegate == null || outputDelegate == WriteLine)
            {
                Console.WriteLine(str, args);
            }
            else outputDelegate(str, args);
        }

        public string ExecuteCommand(string text)
        {
            return ExecuteCommand(text, WriteLine);
        }




        ScriptEventListener _scriptEventListener = null;
        ScriptInterpreter _lispTaskInterperter;

        public ScriptInterpreter initTaskInterperter()
        {
            try
            {
                WriteLine("Start Loading TaskInterperter ... '" + taskInterperterType + "' \n");
                _lispTaskInterperter = ScriptEngines.ScriptManager.LoadScriptInterpreter(taskInterperterType);
                _lispTaskInterperter.LoadFile("boot.lisp");
                _lispTaskInterperter.LoadFile("extra.lisp");
                _lispTaskInterperter.LoadFile("cogbot.lisp");
                _lispTaskInterperter.Intern("clientManager", this);
                _scriptEventListener = new ScriptEventListener(_lispTaskInterperter, null);
                _lispTaskInterperter.Intern("thisClient", this);
                WriteLine("Completed Loading TaskInterperter '" + taskInterperterType + "'\n");
                // load the initialization string
            }
            catch (Exception e)
            {
                WriteLine("!Exception: " + e.GetBaseException().Message);
                WriteLine("error occured: " + e.Message);
                WriteLine("        Stack: " + e.StackTrace.ToString());
            }
            return _lispTaskInterperter;
        }

        public void enqueueLispTask(object p)
        {
            _scriptEventListener.enqueueLispTask(p);
        }

        public string evalLispString(string lispCode)
        {
            try
            {
                if (string.IsNullOrEmpty(lispCode)) return null;
                if (_lispTaskInterperter == null)
                {
                    _lispTaskInterperter = initTaskInterperter();
                }
                //lispCode = "(load-assembly \"libsecondlife\")\r\n" + lispCode;                
                WriteLine("Eval> {0}", lispCode);
                Object r = null;
                using (var stringCodeReader = new StringReader(lispCode))
                {
                    r = _lispTaskInterperter.Read("evalLispString", stringCodeReader);
                    return _lispTaskInterperter.Eof(r)
                               ? r.ToString()
                               : _lispTaskInterperter.Str(_lispTaskInterperter.Eval(r));
                }
            }
            catch (Exception e)
            {
                WriteLine("!Exception: {0}", e.GetBaseException().Message);
                WriteLine("error occured: {0}", e.Message);
                WriteLine("        Stack: {0}", e.StackTrace.ToString());
                throw e;
            }
        }
        // TODO's
        // Play Animations(the
        // private static UUID type_anim_uuid = new UUID("c541c47f-e0c0-058b-ad1a-d6ae3a4584d9");
        // Client.Self.AnimationStart(type_anim_uuid,false);
        // Client.Self.AnimationStop(type_anim_uuid,false);

        // animationFolder = Client.Inventory.FindFolderForType(AssetType.Animation);
        // animationUUID = Client.Inventory.FindObjectByPath(animationFolder, Client.Self.AgentID, AnimationPath, 500);
        // Client.Self.AnimationStart(animationLLUUID,false);
        // Client.Self.AnimationStop(animationLLUUID,false);


        // Reflect events into lisp
        // 



        /// <summary>
        /// Initialize everything that needs to be initialized once we're logged in.
        /// </summary>
        /// <param name="login">The status of the login</param>
        /// <param name="message">Error message on failure, MOTD on success.</param>
        public void LoginHandler(LoginStatus login, string message)
        {
            // EnableIt();
        }

        static public Dictionary<string, BotClient> BotByName = new Dictionary<string, BotClient>();
        public BotClient LastBotClient = null;
        readonly static object OneAtATime = new object();
        private bool _wasFirstGridClient = true;
        private readonly object _wasFirstGridClientLock = new object();
        public BotClient CreateBotClient(string first, string last, string passwd, string simurl, string location)
        {
            lock (OneAtATime)
            {
                string fullName = string.Format("{0} {1}", first, last);
                BotClient bc;
                lock (BotByName)
                {
                    if (BotByName.TryGetValue(fullName, out bc))
                    {
                        WriteLine(";; Reusing {0}", fullName);
                        EnsureStarting(bc);
                        return bc;
                    }
                }
                lock (_wasFirstGridClientLock)
                {
                    GridClient gridClient;
                    if (_wasFirstGridClient)
                    {
                        _wasFirstGridClient = false;
                        RadegastInstance inst = RadegastInstance.GlobalInstance;
                        gridClient = inst.Client;
                        bc = new BotClient(this, gridClient);
                        bc.TheRadegastInstance = inst;
                    }
                    else
                    {
                        if (UsingCogbotFromRadgast)
                        {
                            //return null;
                            throw new InvalidProgramException("UsingCogbotFromRadgast for more than one client");
                        }
                        //RadegastInstance inst = new RadegastInstance();
                        //gridClient = inst.Client;
                        //bc = new BotClient(this, gridClient);
                        //bc.TheRadegastInstance = inst;
                    }

                }
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
                if (String.IsNullOrEmpty(location))
                {
                    location = "last";
                }
                bc.BotLoginParams.Start = location;
                //LoginParams loginParams = bc.Network.DefaultLoginParams(account.FirstName, account.LastName, account.Password, "BotClient", version);            
                EnsureStarting(bc);
                return bc;
            }
        }

        private void EnsureStarting(BotClient client)
        {
            AddTypesToBotClient(client);
            client.StartupClientLisp();
        }

        private void AddTypesToBotClient(BotClient bc)
        {
            BotByName[string.Format("{0} {1}", bc.BotLoginParams.FirstName, bc.BotLoginParams.LastName)] = bc;
            lock (registrationTypes) foreach (Type t in registrationTypes)
                {
                    bc.RegisterType(t);
                }
        }

        public Utilities.BotTcpServer CreateHttpServer(int port, string botname)
        {

            BotClient cl = LastBotClient;
            foreach (BotClient bc in BotClients)
                {
                    if (bc.GetName().Equals(botname))
                    {
                        cl = bc;
                    }
                }
            return new Utilities.BotTcpServer(port, cl);
        }

    //    public Dictionary<UUID, BotClient> Clients = new Dictionary<UUID, BotClient>();
        //public Dictionary<Simulator, Dictionary<uint, Primitive>> SimPrims = new Dictionary<Simulator, Dictionary<uint, Primitive>>();

        public bool Running = true;
        public bool GetTextures = true; //needed for iniminfo

        string version = "1.0.0";
        public static bool UsingCogbotFromRadgast = false;
        public static bool UsingRadgastFromCogbot = false;

        /// <summary>
        /// 
        /// </summary>
        /// <param name="accounts"></param>
        public ClientManager(IEnumerable<LoginDetails> accounts, bool getTextures)
            : this()
        {

            GetTextures = getTextures;

            foreach (LoginDetails account in accounts)
                Login(account);
        }

        public void StartUpLisp()
        {
            initTaskInterperter();
            new Thread(() =>
            {
                if (config.startupLisp.Length > 1)
                {
                    evalLispString("(progn " + config.startupLisp + ")");
                }
            }).Start();
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="account"></param>
        /// <returns></returns>
        public BotClient Login(LoginDetails account)
        {
            // Check if this CurrentClient is already logged in
            foreach (BotClient c in BotClients)
                {
                    if (c.Self.FirstName == account.FirstName && c.Self.LastName == account.LastName)
                    {
                        Logout(c);
                        break;
                    }
                }

            BotClient client = new BotClient(this, new GridClient());
            BotByName[string.Format("{0} {1}", account.FirstName, account.LastName)] = client;

            // Optimize the throttle
            client.Throttle.Wind = 0;
            client.Throttle.Cloud = 0;
            client.Throttle.Land = 1000000;
            client.Throttle.Task = 1000000;

            client.GroupCommands = account.GroupCommands;
            client.MasterName = account.MasterName;
            client.MasterKey = account.MasterKey;
            //client.AllowObjectMaster = client.MasterKey != UUID.Zero; // Require UUID for object master.

            LoginParams loginParams = client.Network.DefaultLoginParams(account.FirstName, account.LastName, account.Password, "BotClient", version);

            if (!String.IsNullOrEmpty(account.StartLocation))
                loginParams.Start = account.StartLocation;

            if (!String.IsNullOrEmpty(account.URI))
                loginParams.URI = account.URI;

            if (client.Network.Login(loginParams))
            {
                if (client.MasterKey == UUID.Zero && !string.IsNullOrEmpty(client.MasterName))
                {
                    UUID query = UUID.Random();
                    DirectoryManager.DirPeopleReplyCallback peopleDirCallback =
                        (queryID, matchedPeople) =>
                            {
                                if (queryID == query)
                                {
                                    if (matchedPeople.Count != 1)
                                    {
                                        Logger.Log("Unable to resolve master key from " + client.MasterName,
                                                   Helpers.LogLevel.Warning);
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
                if (args[4].StartsWith("http://"))
                    account.URI = args[4];

            if (string.IsNullOrEmpty(account.URI))
                account.URI = cogbot.Program.LoginURI;
            Logger.Log("Using login URI " + account.URI, Helpers.LogLevel.Info);

            return Login(account);
        }

        /// <summary>
        /// 
        /// </summary>
        public void Run()
        {
         //   WriteLine("Type quit to exit.  Type help for a command list.");

            while (Running)
            {
                ;
                string input = Program.consoleBase.CmdPrompt(GetPrompt());
                if (string.IsNullOrEmpty(input)) continue;
                outputDelegate(ExecuteCommand(input, outputDelegate));
            }

            foreach (BotClient client in BotClients)
                {
                    if (client.Network.Connected)
                        client.Network.Logout();
                }
        }

        private string GetPrompt()
        {
            int online = 0;
            int offline = 0;
            foreach (BotClient client in BotClients)
                {
                    if (client.Network.Connected) online++;
                    else offline++;
                }
            string result = (online + " avatars online");
            if (offline > 0) result += " " + offline + " avatars offline";
            return result;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="cmd"></param>
        /// <param name="fromAgentID"></param>
        /// <param name="imSessionID"></param>
        public void DoCommandAll(string cmd, UUID fromAgentID, OutputDelegate WriteLine)
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
                if (BotClients.Count > 0)
                {
                    foreach (BotClient client in BotClients)
                        {
                            WriteLine(client.Commands["help"].Execute(args, UUID.Zero, WriteLine));
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
                Logger.Log(command.Execute(args, UUID.Zero, WriteLine), Helpers.LogLevel.Info);
            }
            else
            {
                // Make an immutable copy of the Clients dictionary to safely iterate over
                int completed = 0;

                foreach (BotClient client in BotClients)
                {
                    ThreadPool.QueueUserWorkItem((WaitCallback)
                        delegate(object state)
                        {
                            BotClient testClient = (BotClient)state;
                            if (testClient.Commands.ContainsKey(firstToken))
                                Logger.Log(testClient.Commands[firstToken].Execute(args, fromAgentID, WriteLine),
                                    Helpers.LogLevel.Info, testClient.gridClient);
                            else
                                Logger.Log("Unknown command " + firstToken, Helpers.LogLevel.Warning);

                            ++completed;
                        },
                        client);
                }

                while (completed < BotClients.Count)
                    Thread.Sleep(50);
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="CurrentClient"></param>
        public void Logout(BotClient client)
        {
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


        internal void RegisterAssembly(Assembly assembly)
        {
            lock (registrationTypes)
            {
                bool newTypes = false;
                RegisterBotSystemCommands(assembly);
                foreach (Type t in assembly.GetTypes())
                {
                    if (registrationTypes.Contains(t)) continue;
                    registrationTypes.Add(t);
                    newTypes = true;
                }
                if (newTypes)
                    foreach (BotClient client in BotClients)
                    {
                        AddTypesToBotClient(client);
                    }
            }
        }


        public void RegisterSystemCommand(Type t)
        {
            if (!typeof(SystemApplicationCommand).IsAssignableFrom(t)) return;
            if (registeredSystemApplicationCommandTypes.Contains(t)) return;
            registeredSystemApplicationCommandTypes.Add(t);
            ConstructorInfo info = t.GetConstructor(new Type[] { typeof(BotClient) });
            try
            {
                Command command = (Command)info.Invoke(new object[] { null });
                groupActions.Add(command.Name, command);
            }
            catch (Exception e)
            {
                WriteLine("RegisterBotSystemCommands: " + e.ToString() + "\n" + e.InnerException + "\n In " + t.Name);
            }
        }
    }


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
}
