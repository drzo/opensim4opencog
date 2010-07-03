using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Windows.Forms;
using System.Threading;
using cogbot.Actions.Agent;
using cogbot.ScriptEngines;
using cogbot.Utilities;
using CommandLine.Utility;
using OpenMetaverse;
using cogbot.Actions;
using Radegast;
using cogbot.Actions.Scripting;
using Settings=OpenMetaverse.Settings;

//using Radegast;
namespace cogbot
{
    public delegate void DescribeDelegate(bool detailed, OutputDelegate WriteLine);
    enum Modes { normal, tutorial };
    public delegate void OutputDelegate(string str, params object[] args);

    public class ClientManager : IDisposable
    {
        static private ClientManagerHttpServer clientManagerHttpServer;
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
        public Dictionary<string, Actions.Command> groupActions;
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
        //static List<LoginDetails> accounts = new List<LoginDetails>();
        ///public static ClientManager this = new ClientManager(accounts, false);
        // public static Simian.Simian simulator = new Simian.Simian();

        public ClientManager()
        {
            if (SingleInstance!=null)
            {
                throw new AbandonedMutexException();
            }
            SingleInstance = this;
            config = new Configuration();
            config.loadConfig();
            DefaultAccount.LoadFromConfig(config);
            nextTcpPort = config.tcpPort;
            if (clientManagerHttpServer == null)
            {
                clientManagerHttpServer = new ClientManagerHttpServer(this, 5580);
            }
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
            groupActions = new Dictionary<string, cogbot.Actions.Command>();
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


        public CmdResult ExecuteCommand(string text, OutputDelegate WriteLine)
        {
            if (String.IsNullOrEmpty(text)) return null;
            while (text.StartsWith("/"))
            {
                text = text.Substring(1);
            }
            if (String.IsNullOrEmpty(text)) return null;
            WriteLine("textform> {0}", text);
            CmdResult res = ExecuteBotsCommand(text, WriteLine);
            if (res != null && res.Success) return res;
            res = ExecuteSystemCommand(text, WriteLine);
            if (res != null) return res;
            WriteLine("I don't understand the ExecuteCommand " + text + ".");
            return null;
        }

        private CmdResult ExecuteBotsCommand(string text, OutputDelegate WriteLine)
        {
            if (string.IsNullOrEmpty(text)) return null;
            text = text.Trim();
            while (text.StartsWith("/"))
            {
                text = text.Substring(1);
                text = text.Trim();
            }
            if (string.IsNullOrEmpty(text)) return null;
            if (BotByName.Count == 0 && LastBotClient != null) return LastBotClient.ExecuteBotCommand(text, WriteLine);
            if (OnlyOneCurrentBotClient != null)
            {
                return OnlyOneCurrentBotClient.ExecuteBotCommand(text, WriteLine);

            }
            string res = null;
            int success = 0;
            int failure = 0;
            foreach (BotClient currentClient in BotClients)
                if (currentClient != null)
                {
                    CmdResult t = currentClient.ExecuteBotCommand(text, WriteLine);
                    if (t==null) continue;
                    if (t.Success)
                    {
                        success++; 
                    } else
                    {
                        failure++;
                    }
                    res += t.ToString();
                    if (!String.IsNullOrEmpty(res))
                    {
                        res += "\n";
                    }
                }
            if (success == 0)
            {
                return new CmdResult(res + " " + failure + " failures ", false);
            }
            if (failure > 0)
            {
                return new CmdResult(res + " " + failure + " failures and " + success + " successes", false);
            }
            return new CmdResult(res + " " + success + " successes", true);
        }

        public CmdResult ExecuteSystemCommand(string text, OutputDelegate WriteLine)
        {
            string res = String.Empty;
            try
            {
                {
                    if (string.IsNullOrEmpty(text)) return null;
                    text = text.Trim();
                    while (text.StartsWith("/"))
                    {
                        text = text.Substring(1);
                        text = text.Trim();
                    }
                    if (string.IsNullOrEmpty(text)) return null;

                    string verb = Parser.ParseArguments(text)[0];
                    verb = verb.ToLower();
                    if (groupActions.ContainsKey(verb))
                    {
                        if (text.Length > verb.Length)
                            return groupActions[verb].acceptInputWrapper(verb, text.Substring(verb.Length + 1), WriteLine);
                        else
                            return groupActions[verb].acceptInputWrapper(verb, "", WriteLine);
                    }
                    return null;
                }
            }
            catch (Exception e)
            {
                string newVariable = "ClientManager: " + e;
                WriteLine(newVariable);
                return new CmdResult(newVariable, false);
            }
        }


        public void ShutDown()
        {
            try
            {
                logout();
            }
            catch (Exception) { }
            foreach (BotClient CurrentClient in BotClients)
            {
                try
                {
                    CurrentClient.Dispose();
                }
                catch (Exception e)
                {
                    Console.WriteLine("" + e);
                }
            }
            _scriptEventListener.Dispose();
            _lispTaskInterperter.Dispose();
        }


        public void logout()
        {
            foreach (BotClient CurrentClient in BotClients)
                try
                {
                    if (CurrentClient.Network.Connected)
                        CurrentClient.Network.Logout();
                }
                catch (Exception) { }
            try
            {
                //TODO maybe add back after we have config settings
                // config.saveConfig();
            }
            catch (Exception e)
            {
                Console.WriteLine("" + e);
            }
        }

        // for lisp to call
        public void output(string txt)
        {
            WriteLine(txt);
        }

        public void WriteLine(string str, params object[] args)
        {
            if (args == null || args.Length == 0)
            {
                args = new object[] { str };
                str = "{0}";
            }
            if (outputDelegate == null || outputDelegate == WriteLine)
            {
                Console.WriteLine(str, args);
            }
            else
            {
                Console.WriteLine(str, args);
                outputDelegate(str, args);
            }
        }

        public CmdResult ExecuteCommand(string text)
        {
            return ExecuteCommand(text, WriteLine);
        }




        ScriptEventListener _scriptEventListener = null;
        ScriptInterpreter _lispTaskInterperter;
        public readonly object LispTaskInterperterLock = new object();

        public ScriptInterpreter initTaskInterperter()
        {
            lock (LispTaskInterperterLock)
            {
                if (_lispTaskInterperter == null)
                {
                    try
                    {
                        WriteLine("Start Loading Main TaskInterperter ... '" + taskInterperterType + "' \n");
                        _lispTaskInterperter = ScriptEngines.ScriptManager.LoadScriptInterpreter(taskInterperterType, this);
                        _lispTaskInterperter.LoadFile("boot.lisp",WriteLine);
                        _lispTaskInterperter.LoadFile("extra.lisp",WriteLine);
                        _lispTaskInterperter.LoadFile("cogbot.lisp",WriteLine);
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
                }
                else
                {
                    return _lispTaskInterperter;
                }
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
                if (!Running) return "No running";
                if (string.IsNullOrEmpty(lispCode)) return null;
                initTaskInterperter();
                //lispCode = "(load-assembly \"libsecondlife\")\r\n" + lispCode;                
                WriteLine("Eval> {0}", lispCode);
                Object r = null;
                using (var stringCodeReader = new StringReader(lispCode))
                {
                    r = _lispTaskInterperter.Read("evalLispString", stringCodeReader,WriteLine);
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
                WriteLine("        Lisp: {0}", lispCode);
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

        static public readonly Dictionary<string, LoginDetails> Accounts = new Dictionary<string, LoginDetails>();
        static public readonly Dictionary<string, BotClient> BotByName = new Dictionary<string, BotClient>();
        public string LoginURI;
        static private readonly LoginDetails DefaultAccount = new LoginDetails(null);

        public BotClient LastBotClient = null;
        readonly static object OneAtATime = new object();
        private bool _wasFirstGridClient = true;
        private readonly object _wasFirstGridClientLock = new object();
        public void CreateBotClient(string first, string last, string passwd, string simurl, string location)
        {
            StarupLispCreatedBotClients = true;
            lock (OneAtATime)
            {
                string fullName = string.Format("{0} {1}", first, last);
                if (DoNotCreateBotClientsFromLispScript)
                {
                    WriteLine(";; DoNotCreateBotClientsFromLispScript: {0}", fullName);
                    return;
                }
                BotClient bc = GetBotByName(fullName);
                if (bc!=null)
                {
                    WriteLine(";; Reusing {0}", fullName);
                    EnsureStarting(bc);
                    return;// bc;                    
                }
                LoginDetails BotLoginParams = GetBotLoginParams(first, last, passwd, simurl, location);
            }
        }

        private void EnsureAcct(LoginDetails details, BotClient bc)
        {
            lock (OneAtATime)
            {
                lock (_wasFirstGridClientLock)
                {
                    GridClient gridClient;
                    RadegastInstance inst;
                    if (_wasFirstGridClient)
                    {
                        _wasFirstGridClient = false;
                        inst = RadegastInstance.GlobalInstance;
                        gridClient = inst.Client;
                        bc.TheRadegastInstance = inst;
                    }
                    else
                    {
                        if (bc != null)
                        {
                            gridClient = bc;
                        }
                        else
                        {
                            gridClient = new GridClient();
                        }
                        inst = new RadegastInstance(gridClient);
                    }
                    if (bc == null)
                    {
                        bc = new BotClient(this, gridClient,details);
                        bc.TheRadegastInstance = inst;
                    }
                }
                string fullName = details.BotLName;
                Thread t = new Thread(new ThreadStart(() =>
                {
                    // Thread.CurrentThread.SetApartmentState(ApartmentState.STA);
                    try
                    {

                        if (bc.TheRadegastInstance.MainForm.IsHandleCreated)
                        {
                            //bc.TheRadegastInstance.MainForm.Visible = false;
                        }
                        else
                        {
                            Application.Run(bc.TheRadegastInstance.MainForm);
                        }
                        bc.SetRadegastLoginOptions();
                        EnsureStarting(bc);
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine(fullName + " " + e);
                    }
                }));
                t.Name = "MainThread " + fullName;
                try
                {
                    t.SetApartmentState(ApartmentState.STA);
                }
                catch (Exception)
                {
                }
                t.Start();
                // return bc;
            }
        }


        public LoginDetails GetBotLoginParams(string first, string last, string passwd, string simurl, string location)
        {
            String fullName = KeyFromName(first, last);
            BotClient bc =  GetBotByName(fullName);
            if (bc != null)
            {
                return bc.BotLoginParams;
            }

            LoginDetails BotLoginParams = FindOrCreateAccount(first, last);
            if (!String.IsNullOrEmpty(first))
            {
                BotLoginParams.FirstName = first;
            }
            if (!String.IsNullOrEmpty(last))
            {
                BotLoginParams.LastName = last;
            }
            if (!String.IsNullOrEmpty(passwd))
            {
                BotLoginParams.Password = passwd;
            }
            if (!String.IsNullOrEmpty(simurl))
            {
                BotLoginParams.URI = simurl;
            }
            if (String.IsNullOrEmpty(location))
            {
                location = "last";
            }
            BotLoginParams.Start = location;
            return BotLoginParams;
        }

        public static string KeyFromName(string first, string last)
        {
            string bn = string.Format("{0} {1}", first.ToLower(), last.ToLower());
            return bn;
        }

        static LoginDetails DefaultLoginParams()
        {
            LoginDetails BotLoginParams = new LoginDetails(DefaultAccount);
            return BotLoginParams;
        }

        private void EnsureStarting(BotClient client)
        {
            AddTypesToBotClient(client);
            client.StartupClientLisp();
        }

        private void AddTypesToBotClient(BotClient bc)
        {
            BotByName[bc.NameKey()] = bc;
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
        public bool StarupLispCreatedBotClients;
        public static bool IsVisualStudio;
        public static Arguments arguments;
        public static bool DoNotCreateBotClientsFromLispScript = false;

        /// <summary>
        /// 
        /// </summary>
        /// <param name="accounts"></param>
        public ClientManager(IEnumerable<LoginDetails> accounts, bool getTextures)
            : this()
        {

            GetTextures = getTextures;

            foreach (LoginDetails account in accounts)
                BotClientForAcct(account);
        }

        public void StartUpLisp()
        {
            initTaskInterperter();
            if (config.startupLisp.Length > 1)
            {
                EnsureAutoExec();
                if (!StarupLispCreatedBotClients)
                {
                    LastBotClient = new BotClient(this, RadegastInstance.GlobalInstance.Client, DefaultLoginParams());
                    LastBotClient.TheRadegastInstance = RadegastInstance.GlobalInstance;
                    // LastBotClient.SetRadegastLoginOptions();
                    AddTypesToBotClient(LastBotClient);
                    LastBotClient.Network.LoginProgress += (s, e) =>
                                                               {
                                                                   if (e.Status == LoginStatus.Success)
                                                                   {
                                                                       LastBotClient.StartupClientLisp();
                                                                   }
                                                               };
                }

            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="account"></param>
        /// <returns></returns>
        public BotClient BotClientForAcct(LoginDetails account)
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

            GridClient gc;
            if (ClientManager.UsingCogbotFromRadgast)
            {
                gc = Radegast.RadegastInstance.GlobalInstance.Client;
            }
            else
            {
                gc = new GridClient();
            }

            // Optimize the throttle
            gc.Throttle.Wind = 0;
            gc.Throttle.Cloud = 0;
            gc.Throttle.Land = 1000000;
            gc.Throttle.Task = 1000000;

            LoginParams loginParams = gc.Network.DefaultLoginParams(account.FirstName, account.LastName, account.Password, "BotClient", version);
            account.UseNetworkDefaults(loginParams);

            if (!String.IsNullOrEmpty(account.StartLocation))
                loginParams.Start = account.StartLocation;

            if (!String.IsNullOrEmpty(account.URI))
                loginParams.URI = account.URI;

            BotClient client = new BotClient(this, gc, account);
            account.Client = client;
            BotByName[account.BotLName] = client;
            client.GroupCommands = account.GroupCommands;
            if (!string.IsNullOrEmpty(account.MasterName)) client.MasterName = account.MasterName;
            if (account.MasterKey != UUID.Zero) client.MasterKey = account.MasterKey;
            //client.AllowObjectMaster = client.MasterKey != UUID.Zero; // Require UUID for object master.

            // TODO confirm the set/get of the Master system does what it needs
            //if (client.Network.Login(loginParams))
            //{
            //    if (client.MasterKey == UUID.Zero && !string.IsNullOrEmpty(client.MasterName))
            //    {
            //        UUID query = UUID.Zero;
            //        EventHandler<DirPeopleReplyEventArgs> peopleDirCallback =
            //           delegate(object sender, DirPeopleReplyEventArgs e)
            //            {
            //                if (e.QueryID == query)
            //                {
            //                    if (e.MatchedPeople.Count != 1)
            //                    {
            //                        Logger.Log("Unable to resolve master key from " + client.MasterName,
            //                                   Helpers.LogLevel.Warning);
            //                    }
            //                    else
            //                    {
            //                        client.MasterKey = e.MatchedPeople[0].AgentID;
            //                        Logger.Log("Master key resolved to " + client.MasterKey, Helpers.LogLevel.Info);
            //                    }
            //                }
            //            };

            //        client.Directory.DirPeopleReply += peopleDirCallback;
            //        client.Directory.StartPeopleSearch(client.MasterName, 0);
            //    }

            //    Logger.Log("Logged in " + client.ToString(), Helpers.LogLevel.Info);
            //}
            //else
            //{
            //    Logger.Log("Failed to login " + account.FirstName + " " + account.LastName + ": " +
            //        client.Network.LoginMessage, Helpers.LogLevel.Warning);
            //}

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
            LoginDetails account = FindOrCreateAccount(args[0],args[1]);
            account.Password = args[2];

            if (args.Length > 3)
                account.StartLocation = NetworkManager.StartLocation(args[3], 128, 128, 40);

            if (args.Length > 4)
                if (args[4].StartsWith("http://"))
                    account.URI = args[4];

            if (string.IsNullOrEmpty(account.URI))
                account.URI = LoginURI;
            Logger.Log("Using login URI " + account.URI, Helpers.LogLevel.Info);


            BotClient client = BotClientForAcct(account);
            account.Client = client;
            client.Login();
            return client;
        }

        /// <summary>
        /// 
        /// </summary>
        public void Run()
        {
            //   WriteLine("Type quit to exit.  Type help for a command list.");
            EnsureManagerStarting();

            while (Running)
            {
                ;
                string input = Program.consoleBase.CmdPrompt(GetPrompt());
                if (string.IsNullOrEmpty(input)) continue;
                outputDelegate(ExecuteCommand(input, outputDelegate).ToString());
            }

            foreach (BotClient client in BotClients)
            {
                if (client.Network.Connected)
                    client.Network.Logout();
            }
        }

        public void EnsureManagerStarting()
        {

            ClientManager manager = this;
            EnsureAutoExec();

            // Login the accounts and run the input loop
            lock (Accounts) foreach (LoginDetails ld in Accounts.Values)
            {
                BotClient bc = manager.BotClientForAcct(ld);
                EnsureAcct(ld,bc);
            }
        }

        private static bool RanAutoExec = false;
        private static object RunningAutoExec = new object();
        public static bool NoLoadConfig;

        private void EnsureAutoExec()
        {
            lock (RunningAutoExec)
            {

                if (RanAutoExec) return;
                RanAutoExec = true;
                ClientManager manager = this;
                var ti = manager.initTaskInterperter();
                //manager.StartUpLisp();
                var config = manager.config;
                if (config.startupLisp.Length > 1)
                {
                   if (!NoLoadConfig) manager.evalLispString("(progn " + config.startupLisp + ")");
                }
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
            string[] tokens = Parser.ParseArguments(cmd);
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
                        WriteLine(client.Commands["help"].ExecuteCmd(args, UUID.Zero, WriteLine).ToString());
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
                Logger.Log(command.ExecuteCmd(args, UUID.Zero, WriteLine), Helpers.LogLevel.Info);
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
                                Logger.Log(testClient.Commands[firstToken].ExecuteCmd(args, fromAgentID, WriteLine),
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
            foreach (BotClient client in BotClients)
            {
                client.Dispose();
            }
            Running = false;
            ShutDown();
            // TODO: It would be really nice if we could figure out a way to abort the ReadLine here in so that Run() will exit.
        }


        internal void RegisterAssembly(Assembly assembly)
        {
            WriteLine("RegisterAssembly: " + assembly);
            lock (registrationTypes)
            {
                bool newTypes = false;
                WriteLine("Bot RegisterAssembly: " + assembly);

                foreach (Type t in assembly.GetTypes())
                {
                    try
                    {
                        if (RegisterType(t)) newTypes = true;
                    }
                    catch (Exception e)
                    {
                        WriteLine(e.ToString());
                    }
                }
                if (newTypes)
                {
                    foreach (BotClient client in BotClients)
                    {
                        AddTypesToBotClient(client);
                    }
                }
            }
        }


        public bool RegisterType(Type t)
        {
            bool newType = !registrationTypes.Contains(t);
            if (newType)
            {
                registrationTypes.Add(t);
            }
            if (typeof(SystemApplicationCommand).IsAssignableFrom(t))
            {
                if (!registeredSystemApplicationCommandTypes.Contains(t))
                {
                    registeredSystemApplicationCommandTypes.Add(t);
                    if (!t.IsInterface)
                    {
                        ConstructorInfo info = t.GetConstructor(new Type[] { typeof(BotClient) });
                        try
                        {
                            Command command = (Command)info.Invoke(new object[] { null });
                            RegisterType(command.Name, command);
                        }
                        catch (Exception e)
                        {
                            WriteLine("RegisterType: " + e + "\n" + e.InnerException + "\n In " + t.Name);
                        }
                    }
                }
            }
            return newType;
        }

        public void RegisterType(string name, cogbot.Actions.Command command)
        {
            string orginalName = name;
            name = name.Replace(" ", "").ToLower();
            while (name.EndsWith(".")) name = name.Substring(0, name.Length - 1);

            if (!groupActions.ContainsKey(name))
            {
                groupActions.Add(name, command);
                command.Name = orginalName;
            }
            else
            {
                RegisterType("!" + orginalName, command);
            }
        }

        public void Dispose()
        {
            ShutDown();
        }

        public bool ProcessCommandArgs()
        {      
            DefaultAccount.FirstName = arguments["first"];
            DefaultAccount.LastName = arguments["last"];
            DefaultAccount.Password = arguments["pass"];
            DefaultAccount.GroupCommands = true;

            if (arguments["masterkey"] != null)
                DefaultAccount.MasterKey = UUID.Parse(arguments["masterkey"]);
            if (arguments["master"] != null)
                DefaultAccount.MasterName = arguments["master"];

            if (arguments["loginuri"] != null)
                DefaultAccount.URI = LoginURI = arguments["loginuri"];

            if (String.IsNullOrEmpty(LoginURI))
                LoginURI = Settings.AGNI_LOGIN_SERVER;

            Logger.Log("Using login URI " + LoginURI, Helpers.LogLevel.Info);

            arguments.GetBoolean("gettextures", ref GetTextures);

            if (arguments["startpos"] != null)
            {
                char sep = '/';
                string[] startbits = arguments["startpos"].Split(sep);
                DefaultAccount.StartLocation = NetworkManager.StartLocation(startbits[0], Int32.Parse(startbits[1]),
                        Int32.Parse(startbits[2]), Int32.Parse(startbits[3]));
            }

            if (arguments["scriptfile"] != null)
            {
                string scriptFile = String.Empty;
                scriptFile = arguments["scriptfile"];
                if (!File.Exists(scriptFile))
                {
                    Logger.Log(String.Format("File {0} Does not exist", scriptFile), Helpers.LogLevel.Error);
                    return false;
                }
                if (!String.IsNullOrEmpty(scriptFile))
                    DoCommandAll(String.Format("script {0}", scriptFile), UUID.Zero, Console.WriteLine);
                // Then Run the ClientManager normally
            }


            if (arguments["file"] != null)
            {
                DoNotCreateBotClientsFromLispScript = true;
                string file = String.Empty;
                file = arguments["file"];
                LoadAcctsFromFile(file);
            } else if (arguments["first"] != null && arguments["last"] != null && arguments["pass"] != null)
            {
                ClientManager.DoNotCreateBotClientsFromLispScript = true;
                // Taking a single login off the command-line
                var account = FindOrCreateAccount(arguments["first"], arguments["last"]);
            }
            else if (arguments["help"] != null)
            {
                return false;
            } else
            {
                DoNotCreateBotClientsFromLispScript = false;
            }
            return true;
        }

        private void LoadAcctsFromFile(string file)
        {
            if (!File.Exists(file))
            {
                Logger.Log(String.Format("File {0} Does not exist", file), Helpers.LogLevel.Error);
                return;
            }

            // Loading names from a file
            try
            {
                using (StreamReader reader = new StreamReader(file))
                {
                    string line;
                    int lineNumber = 0;

                    while ((line = reader.ReadLine()) != null)
                    {
                        lineNumber++;
                        string[] tokens = line.Trim().Split(new char[] { ' ', ',' });

                        if (tokens.Length >= 3)
                        {
                            LoginDetails account = FindOrCreateAccount(tokens[0], tokens[1]);
                            account.Password = tokens[2];

                            if (tokens.Length >= 4) // Optional starting position
                            {
                                char sep = '/';
                                string[] startbits = tokens[3].Split(sep);
                                account.StartLocation = NetworkManager.StartLocation(startbits[0], Int32.Parse(startbits[1]),
                                    Int32.Parse(startbits[2]), Int32.Parse(startbits[3]));
                            }

                            Accounts.Add(account.LastName,account);
                        }
                        else
                        {
                            Logger.Log("Invalid data on line " + lineNumber +
                                ", must be in the format of: FirstName LastName Password [Sim/StartX/StartY/StartZ]",
                                Helpers.LogLevel.Warning);
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.Log("Error reading from " + file, Helpers.LogLevel.Error, ex);
                return;
            }
        }

        static LoginDetails FindOrCreateAccount(string first, string last)
        {
            lock (Accounts)
            {
                string fullName = KeyFromName(first, last);
                LoginDetails account;
                if (Accounts.TryGetValue(fullName, out account)) return account;
                account = DefaultLoginParams();
                account.FirstName = first;
                account.LastName = last;
                lock (Accounts) Accounts.Add(account.BotLName, account);
                return account;
            }
        }
    }


    public class LoginDetails
    {
        public LoginDetails(LoginDetails defaults)
        {
            if (defaults == null) return;
            loginParams = defaults.loginParams;
            FirstName = defaults.FirstName;
            LastName = defaults.LastName;
            Password = defaults.Password;
            StartLocation = defaults.StartLocation;

            GroupCommands = defaults.GroupCommands;
            MasterName = defaults.MasterName;
            MasterKey = defaults.MasterKey;
            URI = defaults.URI;
        }

        public override string ToString()
        {
            return BotLName;
        }

        LoginParams loginParams;
        public string FirstName
        {
            get { return loginParams.FirstName; }
            set { loginParams.FirstName = value; }
        }
        public string LastName
        {
            get { return loginParams.LastName; }
            set { loginParams.LastName = value; }
        }
        public string Password
        {
            get { return loginParams.Password; }
            set { loginParams.Password = value; }
        }
        public string URI
        {
            get { return loginParams.URI; }
            set { loginParams.URI = value; }
        }
        public string StartLocation
        {
            get { return loginParams.Start; }
            set { loginParams.Start = value; }
        }

        public string BotLName
        {
            get { return ClientManager.KeyFromName(FirstName, LastName); }
        }

        public string Version
        {
            get { return loginParams.Version; }
            set { loginParams.Version = value; }
        }

        public string Start
        {
            get { return loginParams.Start; }
            set { loginParams.Start = value; }
        }

        public string Channel
        {
            get { return loginParams.Channel; }
            set { loginParams.Channel = value; }
        }

        public bool GroupCommands;
        public string MasterName;
        public UUID MasterKey;
        public BotClient Client;

        public void UseNetworkDefaults(LoginParams @params)
        {
            loginParams = @params;
        }

        public void LoadFromConfig(Configuration config)
        {
            FirstName = config.firstName;
            LastName = config.lastName;
            Password = config.password;
            URI = config.simURL;
        }
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

    public interface IConsoleBase
    {
        void WriteLine(ConsoleColor color, string format, params object[] args);
        void WriteLine(string format, params object[] args);
        string CmdPrompt(string p);
    }
}
