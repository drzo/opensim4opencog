using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Reflection;
using System.Threading;
using System.Windows.Forms;
using cogbot.Actions;
using cogbot.ScriptEngines;
using OpenMetaverse; //using libsecondlife;
using Action = cogbot.Actions.Action;

namespace cogbot
{
#pragma warning disable 0168
    
    //the Simain stuff in the opensim4opencog stopped being used for a bit.. it was going to do scene management.. but still
    public delegate void OutputDelegate(string str, params object[] args);

    public delegate void DescribeDelegate(bool detailed, OutputDelegate WriteLine);
    enum Modes { normal, tutorial };

    public partial class TextForm : Form
    {
        private ICollection<BotClient> BotClients
        {
            get
            {
                // Make an immutable copy of the Clients list to safely iterate over
                List<BotClient> clientsCopy = new List<BotClient>();
                lock (BotByName) clientsCopy.AddRange(BotByName.Values);
                return clientsCopy;
            }
        }

        public static bool DownloadTextures = false;
        public static int nextTcpPort = 5555;
        static public TextForm SingleInstance = null;
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
        //  public cogbot.TextForm ClientManager;
        //  public VoiceManager VoiceManager;
        // Shell-like inventory commands need to be aware of the 'current' inventory folder.
        //public InventoryFolder CurrentDirectory = null;

        //public BotClient CurrentClient;
        public OutputDelegate outputDelegate;
        ///public Dictionary<string, DescribeDelegate> describers;

        public List<Type> registrationTypes;
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
        //Utilities.TcpServer UtilitiesTcpServer;
        public String taskInterperterType = "DotLispInterpreter";// DotLispInterpreter,CycInterpreter or ABCLInterpreter
        static List<LoginDetails> accounts = new List<LoginDetails>();
        ///public static ClientManager this = new ClientManager(accounts, false);
       // public static Simian.Simian simulator = new Simian.Simian();

        public TextForm()
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
            groupActions = new Dictionary<string, Action>();
            //groupActions["login"] = new Login(null);


            //   CurrentClient.Settings.LOGIN_SERVER = config.simURL;
            // extraSettings();


            // CurrentClient.Network.OnConnected += new NetworkManager.ConnectedCallback(Network_OnConnected);
            // CurrentClient.Network.OnDisconnected += new NetworkManager.DisconnectedCallback(Network_OnDisconnected);
            // CurrentClient.Network.OnLogin += new NetworkManager.LoginCallback(Network_OnLogin);

            outputDelegate = new OutputDelegate(WriteLine);

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

            InitializeComponent();
            this.Size = new Size(this.Size.Width + 300, this.Size.Height);
            this.ResizeEnd += TextForm_ResizeEnd;
            TextForm_ResizeEnd(null, null);
            Show();
            consoleInputText.Enabled = true;
            consoleInputText.Focus();
            //   RegisterAllCommands(Assembly.GetExecutingAssembly());

            //   UtilitiesTcpServer = new cogbot.Utilities.TcpServer(this);
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
            new Thread(new ThreadStart(delegate()
            {
                if (config.startupLisp.Length > 1)
                {
                    evalLispString("(progn " + config.startupLisp + ")");
                }
            })).Start();
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


        private delegate void EnableItDelegate();
        private void EnableIt()
        {
            try
            {
                if (consoleInputText.InvokeRequired)
                {
                    // This is a worker thread so delegate the task.        
                    this.Invoke(new EnableItDelegate(EnableIt));
                }
                else
                {        // This is the UI thread so perform the task.
                    consoleInputText.Enabled = true;
                    loginToolStripMenuItem.Enabled = false;
                    logoutToolStripMenuItem.Enabled = true;
                    submitButton.Enabled = true;

                    consoleInputText.Focus();

                    System.Threading.Thread.Sleep(3000);

                    // describeSituation();

                }
            }
            catch (Exception e)
            {

            }
        }
        public void SetOnlyOneCurrentBotClient(string CurrentBotClient)
        {
            if (String.IsNullOrEmpty(CurrentBotClient)) {
                OnlyOneCurrentBotClient = null;
                return;
            }
            BotClient OBotClient = GetBotByName(CurrentBotClient);
            if (OBotClient == null)
                WriteLine("SetOnlyOneCurrentBotClient to unkown bot: " + CurrentBotClient);
            else OnlyOneCurrentBotClient = OBotClient;
            
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
            String res = ExecuteBotsCommand(text,WriteLine);
            if (!String.IsNullOrEmpty(res)) return res;
            return ExecuteSystemCommand(text,WriteLine);
        }

        private string ExecuteBotsCommand(string text, OutputDelegate WriteLine)
        {
            string res = String.Empty;
            if (BotByName.Count == 0 && lastBotClient != null) return lastBotClient.ExecuteBotCommand(text, WriteLine);
            if (OnlyOneCurrentBotClient != null)
            {
                return OnlyOneCurrentBotClient.ExecuteBotCommand(text,WriteLine);

            }

            foreach (BotClient CurrentClient in BotClients)
                if (CurrentClient != null)
                {

                    res += CurrentClient.ExecuteBotCommand(text, WriteLine).Trim();
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
                    string verb = text.Split(null)[0];
                    if (groupActions.ContainsKey(verb))
                    {
                        if (text.Length > verb.Length)
                            res += groupActions[verb].acceptInputWrapper(verb, text.Substring(verb.Length + 1), WriteLine);
                        else
                            res += groupActions[verb].acceptInputWrapper(verb, "", WriteLine);
                        return res;
                    }
                    WriteLine("I don't understand the verb " + verb + ".");
                    WriteLine("Type \"help\" for help.");
                }
                return res;
            }
            catch (Exception e)
            {
                WriteLine("TextForm:" + e);
                return res;
            }
        }



        private void TextForm_FormClosed(object sender, FormClosedEventArgs e)
        {
            ShutDown();
            //UtilitiesTcpServer.closeTcpListener();
        }

        private void ShutDown()
        {
            logout();
            foreach (BotClient CurrentClient in BotByName.Values)
            {
                CurrentClient.ShutDown();
            }
            Application.DoEvents();
            Application.Exit();
            Application.ExitThread();
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Close(); //            TextForm_FormClosed(sender, null);
        }

        private void loginToolStripMenuItem_Click(object sender, EventArgs e)
        {
            // LoginForm loginForm = new LoginForm(Client);
            foreach (BotClient CurrentClient in Clients.Values)
            {
                LoginForm loginForm = new LoginForm(CurrentClient, this);
                loginForm.ShowDialog();
            }
        }

        private void logoutToolStripMenuItem_Click(object sender, EventArgs e)
        {
            logout();
        }

        public void logout()
        {
            foreach (BotClient CurrentClient in Clients.Values)
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
            try
            {
                if (str == null) return;
                if (args.Length>0) str = String.Format(str, args);
                str = str.Trim();
                if (str == "") return;

                str = str.Replace("\r\n", "\n").Replace("\r", "\n").Replace("\n", "\r\n");
                //if (str.ToLower().Contains("look")) return;
                if (IsDisposed) return; // for (un)clean exits
                if (this.InvokeRequired)
                {
                    this.Invoke(new OutputDelegate(doOutput), str, new object[0]);
                }
                else
                {
                    doOutput(str);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("" + e);                
                if (IsDisposed)
                {
                    logout();
                    Application.Exit();
                }
            }
        }

        public void doOutput(string str, params object[] args)
        {
            if (str == null) return;
            if (args.Length > 0) str = String.Format(str, args);
            str = str.Trim(); 
            if (str == "") return;
            try
            {
               // lock (consoleText)
                {
                    if (consoleText.IsDisposed) return; // for (un)clean exits
                    consoleText.AppendText(str + "\r\n");
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("" + e);
                // probably dead anyway ...
            }
            return;
            string oldInput = consoleInputText.Text;
            consoleInputText.Clear();
            consoleInputText.Text = str;
            consoleInputText.SelectAll();
            System.Threading.Thread.Sleep(100);
            consoleInputText.Text = oldInput;
        }


        public string ExecuteCommand(string text)
        {
            return ExecuteCommand(text, WriteLine);
        }

        public void acceptConsoleInput()
        {
            string text = consoleInputText.Text;
            if (text.Length > 0)
            {
                //WriteLine(text);
                consoleInputText.Text = "";

                WriteLine(ExecuteCommand(text,WriteLine));

                //if (describeNext)
                //{
                //    describeNext = false;
                //    describeSituation();
                //}
            }
        }

        private void consoleInputText_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (Convert.ToInt32(e.KeyChar) == 13)
                acceptConsoleInput();
        }

        ScriptEventListener scriptEventListener = null;
        ScriptInterpreter lispTaskInterperter;

        public void initTaskInterperter()
        {
            try
            {
                WriteLine("Start Loading TaskInterperter ... '" + taskInterperterType + "' \n");
                lispTaskInterperter = ScriptEngines.ScriptManager.LoadScriptInterpreter(taskInterperterType);
                lispTaskInterperter.LoadFile("boot.lisp");
                lispTaskInterperter.LoadFile("extra.lisp");
                lispTaskInterperter.LoadFile("cogbot.lisp");
                lispTaskInterperter.Intern("clientManager", this);
                scriptEventListener = new ScriptEventListener(lispTaskInterperter, null);
                lispTaskInterperter.Intern("thisClient", this);
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

        public void enqueueLispTask(object p)
        {
            scriptEventListener.enqueueLispTask(p);
        }

        public string evalLispString(string lispCode)
        {
            try
            {
                if (lispCode == null || lispCode.Length == 0) return null;
                if (lispTaskInterperter == null)
                {
                    initTaskInterperter();
                }
                //lispCode = "(load-assembly \"libsecondlife\")\r\n" + lispCode;                
                WriteLine("Eval> " + lispCode);
                Object r = null;
                StringReader stringCodeReader = new StringReader(lispCode);
                r = lispTaskInterperter.Read("evalLispString", stringCodeReader);
                if (lispTaskInterperter.Eof(r))
                    return r.ToString();
                return lispTaskInterperter.Str(lispTaskInterperter.Eval(r));
            }
            catch (Exception e)
            {
                WriteLine("!Exception: " + e.GetBaseException().Message);
                WriteLine("error occured: " + e.Message);
                WriteLine("        Stack: " + e.StackTrace.ToString());
                throw e;
            }
        }
        // TODO's
        // Play Animations
        // private static UUID type_anim_uuid = new UUID("c541c47f-e0c0-058b-ad1a-d6ae3a4584d9");
        // Client.Self.AnimationStart(type_anim_uuid,false);
        // Client.Self.AnimationStop(type_anim_uuid,false);

        // animationFolder = Client.Inventory.FindFolderForType(AssetType.Animation);
        // animationUUID = Client.Inventory.FindObjectByPath(animationFolder, Client.Self.AgentID, AnimationPath, 500);
        // Client.Self.AnimationStart(animationLLUUID,false);
        // Client.Self.AnimationStop(animationLLUUID,false);


        // Reflect events into lisp
        // 



        private void TextForm_Load(object sender, EventArgs e)
        {
            //cycConnection = new DotCYC.CycConnectionForm();
        }

        private void consoleText_TextChanged(object sender, EventArgs e)
        {

        }

        private void TextForm_ResizeBegin(object sender, EventArgs e)
        {

        }
        private void TextForm_ResizeEnd(object sender, EventArgs e)
        {
            consoleText.Size = new Size(this.Size.Width - 35, this.Size.Height - 100);
            consoleInputText.Top = this.Size.Height - 60;
            consoleInputText.Size = new Size(this.Size.Width - 125, consoleInputText.Height);
            this.submitButton.Location = new System.Drawing.Point(this.Size.Width - 90, consoleInputText.Top);
        }

        /// <summary>
        /// Initialize everything that needs to be initialized once we're logged in.
        /// </summary>
        /// <param name="login">The status of the login</param>
        /// <param name="message">Error message on failure, MOTD on success.</param>
        public void LoginHandler(LoginStatus login, string message)
        {
            EnableIt();
        }

        private void bot1ToolStripMenuItem_Click(object sender, EventArgs e)
        {

        }
        static public Dictionary<string, BotClient> BotByName = new Dictionary<string, BotClient>();
        public BotClient lastBotClient = null;
        public BotClient CreateBotClient(string first, string last, string passwd, string simurl, string location)
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
            if (String.IsNullOrEmpty(location))
            {
                location = "last";
            }
            bc.BotLoginParams.Start = location;
            bc.Network.OnLogin += new NetworkManager.LoginCallback(delegate(LoginStatus login, string message) {
                if (login == LoginStatus.Success)
                {
                    Clients.Add(bc.Self.AgentID, bc);
                }
            });
            //LoginParams loginParams = bc.Network.DefaultLoginParams(account.FirstName, account.LastName, account.Password, "BotClient", version);            
            AddTypesToBotClient(bc);
            bc.StartupClientLisp();
            return bc;
        }

        private void AddTypesToBotClient(BotClient bc)
        {
            BotByName["" + bc.BotLoginParams.FirstName + " " + bc.BotLoginParams.LastName] = bc;
            lock (registrationTypes) foreach (Type t in registrationTypes)
            {
                bc.RegisterType(t);
            }
        }

        public Utilities.TcpServer CreateHttpServer(int port, string botname)
        {

            BotClient cl = lastBotClient;
            foreach(BotClient bc in Clients.Values) {
                if (bc.GetName().Equals(botname)) {
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
        public TextForm(List<LoginDetails> accounts, bool getTextures)
            : this()
        {
            
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
            //client.AllowObjectMaster = client.MasterKey != UUID.Zero; // Require UUID for object master.

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
                DoCommandAll(input, UUID.Zero, outputDelegate);
            }

            foreach (BotClient client in Clients.Values)
            {
                if (client.Network.Connected)
                    client.Network.Logout();
            }
        }

        private void PrintPrompt()
        {
            int online = 0;

            foreach (BotClient client in Clients.Values)
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
                if (Clients.Count > 0)
                {
                    foreach (BotClient client in Clients.Values)
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
                                Logger.Log(testClient.Commands[firstToken].Execute(args, fromAgentID,WriteLine),
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

        public void AddTool(string name, string text, EventHandler threadStart)
        {
               base.Invoke(new AddToolDelegate(AddTool0), new object[] {name, text, threadStart});
        }

        public delegate void AddToolDelegate(string name, string text, EventHandler threadStart);
        public void AddTool0(string name, string text, EventHandler threadStart)
        {           
            SuspendLayout();
            ToolStripMenuItem stripMenuItem =
                new ToolStripMenuItem {Name = name, Size = new System.Drawing.Size(168, 22), Text = text};
            stripMenuItem.Click += threadStart;
            this.toolsToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {stripMenuItem});
            ResumeLayout();
        }
    }

    #region -- Configuration Class --
    /// <summary>
    /// This Configuration class is basically just a set of 
    /// properties with a couple of static methods to manage
    /// the serialization to and deserialization from a
    /// simple XML file.
    /// </summary>
    [Serializable]
    public class Configuration
    {
        int _Version;
        int _TcpPort;
        int _TcpPortOffset;
        string _TcpIpAddress;
        string _FirstName;
        string _LastName;
        string _Password;
        string _SimURL;
        string _startupLisp;
        string _startupClientLisp;

        public Configuration()
        {
            _Version = 1;
            _FirstName = "";
            _LastName = "";
            _Password = "";
            _SimURL = "http://127.0.0.1:8002/"; // The sim server we talk to
            _TcpPort = 5555; // The TCP port WE provide
            _TcpPortOffset = 10; // The TCP offset between bots
            _TcpIpAddress = "127.0.0.1"; // The IPAddress WE Listen on
            _startupLisp = "";
            _startupClientLisp = "";
        }

        public static void Serialize(string file, Configuration c)
        {
            System.Xml.Serialization.XmlSerializer xs
               = new System.Xml.Serialization.XmlSerializer(c.GetType());
            StreamWriter writer = File.CreateText(file);
            xs.Serialize(writer, c);
            writer.Flush();
            writer.Close();
        }
        public static Configuration Deserialize(string file)
        {
            System.Xml.Serialization.XmlSerializer xs
               = new System.Xml.Serialization.XmlSerializer(
                  typeof(Configuration));
            StreamReader reader = File.OpenText(file);
            Configuration c = (Configuration)xs.Deserialize(reader);
            reader.Close();
            return c;
        }
        public void loadConfig()
        {
            try
            {
                Configuration c2 = Configuration.Deserialize("botconfig.xml");
                this.Version = c2.Version;
                this.firstName = c2.firstName;
                this.lastName = c2.lastName;
                this.password = c2.password;
                this.simURL = c2.simURL;
                this.tcpPort = c2.tcpPort;
                this.tcpIPAddress = c2.tcpIPAddress;
                this.startupLisp = c2.startupLisp;
                this.startupClientLisp = c2.startupClientLisp;

            }
            catch (Exception e)
            {
            }

        }

        public void saveConfig()
        {
            try
            {
                Configuration.Serialize("botconfig.xml", this);
            }
            catch (Exception e)
            {
            }

        }

        public int Version
        {
            get { return _Version; }
            set { _Version = value; }
        }
        public int tcpPort
        {
            get { return _TcpPort; }
            set { _TcpPort = value; }
        }
        public int tcpPortOffset
        {
            get { return _TcpPortOffset; }
            set { _TcpPortOffset = value; }
        }
        public string tcpIPAddress
        {
            get { return _TcpIpAddress; }
            set { _TcpIpAddress = value; }
        }
        public string firstName
        {
            get { return _FirstName; }
            set { _FirstName = value; }
        }
        public string lastName
        {
            get { return _LastName; }
            set { _LastName = value; }
        }
        public string password
        {
            get { return _Password; }
            set { _Password = value; }
        }
        public string simURL
        {
            get { return _SimURL; }
            set { _SimURL = value; }
        }
        public string startupLisp
        {
            get { return _startupLisp; }
            set { _startupLisp = value; }
        }
        public string startupClientLisp
        {
            get { return _startupClientLisp; }
            set { _startupClientLisp = value; }
        }

    }
    #endregion

#pragma warning restore 0168
}
