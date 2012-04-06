using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Reflection;
using System.Windows.Forms;
using System.Xml;
using cogbot.Actions.Land;
using cogbot.Actions.Movement;
using cogbot.Actions.Scripting;
using cogbot.Actions.System;
using cogbot.Actions.WebUtil;
using cogbot.Utilities;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Packets;
using OpenMetaverse.Utilities;
using cogbot.Actions;
using System.Threading;
using System.Collections;
using cogbot.ScriptEngines;
using System.IO;
using cogbot.Listeners;
using Radegast;
using Radegast.Netcom;
using cogbot.TheOpenSims;
using System.Drawing;
using Settings=OpenMetaverse.Settings;
using cogbot.Actions.Agent;
using System.Text;
using Type=System.Type;

//using RadegastTab = Radegast.SleekTab;

// older LibOMV
//using TeleportFlags = OpenMetaverse.AgentManager.TeleportFlags;
//using TeleportStatus = OpenMetaverse.AgentManager.TeleportStatus;

namespace cogbot
{
    public class BotClient : SimEventSubscriber, IDisposable, ScriptExecutor
    {

        public static implicit operator GridClient(BotClient m)
        {
            return m.gridClient;
        }

        /// <summary>Networking subsystem</summary>
        public NetworkManager Network { get { return gridClient.Network; } }
        /// <summary>Settings class including constant values and changeable
        /// parameters for everything</summary>
        public Settings Settings { get { return gridClient.Settings; } }
        /// <summary>Parcel (subdivided simulator lots) subsystem</summary>
        public ParcelManager Parcels { get { return gridClient.Parcels; } }
        /// <summary>Our own avatars subsystem</summary>
        public AgentManager Self { get { return gridClient.Self; } }
        /// <summary>Other avatars subsystem</summary>
        public AvatarManager Avatars { get { return gridClient.Avatars; } }
        /// <summary>Friends list subsystem</summary>
        public FriendsManager Friends { get { return gridClient.Friends; } }
        /// <summary>Grid (aka simulator group) subsystem</summary>
        public OpenMetaverse.GridManager Grid { get { return gridClient.Grid; } }
        /// <summary>Object subsystem</summary>
        public ObjectManager Objects { get { return gridClient.Objects; } }
        /// <summary>Group subsystem</summary>
        public GroupManager Groups { get { return gridClient.Groups; } }
        /// <summary>Asset subsystem</summary>
        public AssetManager Assets { get { return gridClient.Assets; } }
        /// <summary>Asset subsystem</summary>
        public EstateTools Estate { get { return gridClient.Estate; } }
        /// <summary>Appearance subsystem</summary>
        public AppearanceManager Appearance { get { return gridClient.Appearance; } }
        /// <summary>Inventory subsystem</summary>
        public InventoryManager Inventory { get { return gridClient.Inventory; } }
        /// <summary>Directory searches including classifieds, people, land 
        /// sales, etc</summary>
        public DirectoryManager Directory { get { return gridClient.Directory; } }
        /// <summary>Handles land, wind, and cloud heightmaps</summary>
        public TerrainManager Terrain { get { return gridClient.Terrain; } }
        /// <summary>Handles sound-related networking</summary>
        public SoundManager Sound { get { return gridClient.Sound; } }
        /// <summary>Throttling total bandwidth usage, or allocating bandwidth
        /// for specific data stream types</summary>
        public AgentThrottle Throttle { get { return gridClient.Throttle; } }


        /// <summary>The event subscribers. null if no subcribers</summary>
        private EventHandler<SimObjectEvent> m_EachSimEvent;

        /// <summary>Raises the EachSimEvent event</summary>
        /// <param name="e">An EachSimEventEventArgs object containing the
        /// data returned from the data server</param>
        protected virtual void OnEachSimEvent(SimObjectEvent e)
        {
            EventHandler<SimObjectEvent> handler = m_EachSimEvent;
            if (handler != null)
                handler(this, e);
        }

        /// <summary>Thread sync lock object</summary>
        private readonly object m_EachSimEventLock = new object();

        /// <summary>Triggered when Each Sim Event packet is received,
        /// telling us what our avatar is currently wearing
        /// <see cref="RequestAgentWearables"/> request.</summary>
        public event EventHandler<SimObjectEvent> EachSimEvent
        {
            add { lock (m_EachSimEventLock) { m_EachSimEvent += value; } }
            remove { lock (m_EachSimEventLock) { m_EachSimEvent -= value; } }
        }


        readonly public TaskQueueHandler OneAtATimeQueue;
        readonly public GridClient gridClient;
        // TODO's
        // Play Animations
        // private static UUID type_anim_uuid = UUIDFactory.GetUUID("c541c47f-e0c0-058b-ad1a-d6ae3a4584d9");
        // Client.Self.AnimationStart(type_anim_uuid,false);
        // Client.Self.AnimationStop(type_anim_uuid,false);

        // animationFolder = Client.Inventory.FindFolderForType(AssetType.Animation);
        // animationUUID = Client.Inventory.FindObjectByPath(animationFolder, Client.Self.AgentID, AnimationPath, 500);
        // Client.Self.AnimationStart(animationLLUUID,false);
        // Client.Self.AnimationStop(animationLLUUID,false);

        public bool IsLoggedInAndReady {
            get
            {
                var gridClient = this.gridClient;
                if (gridClient == null) return false;
                var net = gridClient.Network;
                if (net == null) return false;
                if (net != Network) return false;
                if (net.CurrentSim == null) return false;
                if (!Network.Connected) return false;
                if (gridClient.Self.AgentID == UUID.Zero) return false;
                //something is oput of date!?
                if (gridClient != this.gridClient) return false; 
                return true;
            }
        }
                
        // Reflect events into lisp
        //        
        int LoginRetriesFresh = 2; // for the times we are "already logged in"
        int LoginRetries; // set to Fresh in constructor
        public bool ExpectConnected;
        public void Login()
        {
            Login(false);
        }

        public void Login(bool blocking)
        {
            SetRadegastLoginOptions();
            if (IsLoggedInAndReady) return;
            if (ExpectConnected) return;
            if (Network.CurrentSim != null)
            {
                if (Network.CurrentSim.Connected) return;
            }
            //if (ClientManager.simulator.periscopeClient == null)
            //{
            //    ClientManager.simulator.periscopeClient = this;
            //    ClientManager.simulator.Start();
            //    Settings.LOG_LEVEL = Helpers.LogLevel.Info;
            //}
            try
            {
                Settings.LOGIN_SERVER = BotLoginParams.URI;
                if (TheRadegastInstance != null)
                {
                    var LoginEvent = BotLoginParams.LoginEvent; 
                    LoginEvent.Reset();
                    // non blocking
                    InvokeGUI(TheRadegastInstance.MainForm, TheRadegastInstance.Netcom.Login);
                    
                    if (blocking)
                    {
                        bool madeIt = LoginEvent.WaitOne(BotLoginParams.loginParams.Timeout, false);

                    }
                    return;
                }
                //SetLoginOptionsFromRadegast();
                if (!blocking)
                {
                    Network.BeginLogin(BotLoginParams.loginParams);
                } else
                {
                    Network.Login(BotLoginParams.loginParams);
                }
            }
            catch (Exception ex)
            {
                LogException("Login", ex);
            }

        }

        private void LogException(string p, Exception ex)
        {
            Logger.Log(GetName() + ": Exception " + p + "\n" + ex, Helpers.LogLevel.Error, ex);
            StringWriter sw = new StringWriter();
            sw.WriteLine("ERROR !Exception: " + ex.GetBaseException().Message);
            sw.WriteLine("error occured: " + ex.Message);
            sw.WriteLine("        Stack: " + ex.StackTrace.ToString());
            Exception inner = ex.InnerException;
            if (inner!=null && inner!=ex)
            {
                LogException("Inner of " + p, inner);
            }
            WriteLine("{0}", sw.ToString());
        }

        readonly int thisTcpPort;
        public LoginDetails BotLoginParams
        {
            get
            {
                return _BotLoginParams;
            }
        }

        private LoginDetails _BotLoginParams = null;
        private readonly SimEventPublisher botPipeline;
        public IList<Thread> GetBotCommandThreads()
        {
            lock (botCommandThreads) return botCommandThreads;
        }

        public void AddThread(Thread thread)
        {
            lock (botCommandThreads)
            {
                botCommandThreads.Add(thread);
            }
        }

        public void RemoveThread(Thread thread)
        {
            lock (botCommandThreads)
            {
                botCommandThreads.Remove(thread);
            }
        }

        private readonly IList<Thread> botCommandThreads = new ListAsSet<Thread>();
        readonly public XmlScriptInterpreter XmlInterp;
        public UUID GroupID = UUID.Zero;
        public Dictionary<UUID, GroupMember> GroupMembers = null; // intialized from a callback
        public Dictionary<UUID, AvatarAppearancePacket> Appearances = new Dictionary<UUID, AvatarAppearancePacket>();
        ///public Dictionary<string, Command> Commands = new Dictionary<string, Command>();
        public bool Running = true;
        public bool GroupCommands = true;
        private string _masterName = string.Empty;
        public Hashtable PosterBoard = new Hashtable();

        public object getPosterBoard(object slot)
        {
            lock (PosterBoard)
            {   
                if (!PosterBoard.Contains(slot)) return null;
                object v = PosterBoard[slot];                    
                PosterBoard.Remove(slot); // consume the data from the queue
                return v;
            }
        }

        public string MasterName
        {
            get
            {
                if (string.IsNullOrEmpty(_masterName) && UUID.Zero != _masterKey)
                {
                    MasterName = WorldSystem.GetUserName(_masterKey);
                }
                return _masterName;
            }
            set
            {
                if (!string.IsNullOrEmpty(value))
                {
                    UUID found;                   
                    if (UUID.TryParse(value, out found))
                    {
                        MasterKey = found;
                        return;
                    }
                    _masterName = value;
                    lock (SecurityLevelsByName) SecurityLevelsByName[value] = BotPermissions.Owner;
                    found = WorldSystem.GetUserID(value);
                    if (found != UUID.Zero)
                    {
                        MasterKey = found;
                    }
                }
            }
        }

        // permissions "NextOwner" means banned "Wait until they are an owner before doing anything!"
        public Dictionary<UUID, BotPermissions> SecurityLevels = new Dictionary<UUID, BotPermissions>();
        public Dictionary<string, BotPermissions> SecurityLevelsByName = new Dictionary<string, BotPermissions>();

        private UUID _masterKey = UUID.Zero;
        public UUID MasterKey
        {
            get
            {
                if (UUID.Zero == _masterKey && !string.IsNullOrEmpty(_masterName))
                {
                    UUID found = WorldSystem.GetUserID(_masterName);
                    if (found != UUID.Zero)
                    {
                        MasterKey = found;
                    }
                }
                return _masterKey;
            }
            set
            {
                if (UUID.Zero != value)
                {
                    _masterKey = value;
                    if (string.IsNullOrEmpty(_masterName))
                    {
                        string maybe = WorldSystem.GetUserName(value);
                        if (!string.IsNullOrEmpty(maybe)) MasterName = maybe;
                    }
                    lock (SecurityLevels) SecurityLevels[value] = BotPermissions.Owner;
                }
            }
        }
        public bool AllowObjectMaster
        {
            get
            {
                return _masterKey != UUID.Zero;
            }
        }

        public bool IsRegionMaster
        {
            get { return WorldSystem.IsRegionMaster; }
        }

        private Radegast.RadegastInstance __TheRadegastInstance;
        public Radegast.RadegastInstance TheRadegastInstance
        {
            get
            {

                if (false && __TheRadegastInstance == null)
                {
                    WriteLine("getting radegast null");
                }
                return __TheRadegastInstance;
            }
            set
            {
                if (value == null)
                {
                    WriteLine("setting radegast null");
                }
                if (__TheRadegastInstance == value) return;
                if (__TheRadegastInstance != null)
                {
                    var nc = __TheRadegastInstance.Netcom;
                    if (nc != null) nc.InstantMessageSent -= IMSent;
                }
                __TheRadegastInstance = value;
                if (value==null) return;
                var vc = value.Client;
                if (gridClient != vc)
                {
                    WriteLine("wierd gridclients");
                }                
                value.Netcom.InstantMessageSent += IMSent;

                if (false) ClientManager.SetDebugConsole(value);
            }
        }

        private void IMSent(object sender, InstantMessageSentEventArgs e)
        {
            if (OnInstantMessageSent!=null)
                try
                {
                    OnInstantMessageSent(this, new IMessageSentEventArgs(e.Message, e.TargetID, e.SessionID, e.Timestamp));
                }
                catch (Exception ex)
                {
                    LogException("ImSent", ex);
                }
        }

        public event InstantMessageSentArgs OnInstantMessageSent; 

        readonly public VoiceManager VoiceManager;
        // Shell-like inventory commands need to be aware of the 'current' inventory folder.
        public InventoryFolder CurrentDirectory = null;

        private Quaternion bodyRotation = Quaternion.Identity;
        private Vector3 forward = new Vector3(0, 0.9999f, 0);
        private Vector3 left = new Vector3(0.9999f, 0, 0);
        private Vector3 up = new Vector3(0, 0, 0.9999f);
        readonly private System.Timers.Timer updateTimer;

        public Listeners.WorldObjects WorldSystem;
        static public BotClient SingleInstance
        {
            get { return cogbot.ClientManager.SingleInstance.LastBotClient; }
        }
        public static int debugLevel = 2;
        public bool GetTextures = cogbot.ClientManager.DownloadTextures;

        //  public cogbot.ClientManager ClientManager;
        //  public VoiceManager VoiceManager;
        // Shell-like inventory commands need to be aware of the 'current' inventory folder.

        //  public GridClient this = null;
        //  public OutputDelegate outputDelegate;
        ///public DotCYC.CycConnectionForm cycConnection;
        public Dictionary<string, DescribeDelegate> describers;

        readonly public Dictionary<string, Listeners.Listener> listeners;
        public SortedDictionary<string, Command> Commands;
        public Dictionary<string, Tutorials.Tutorial> tutorials;
        //public Utilities.BotTcpServer UtilitiesTcpServer;

        public bool describeNext;
        private int describePos;
        private string currTutorial;

        public int BoringNamesCount = 0;
        public int GoodNamesCount = 0;
        public int RunningMode = (int)Modes.normal;
        public UUID AnimationFolder = UUID.Zero;

        BotInventoryEval searcher = null; // new InventoryEval(this);
        //public Inventory Inventory;
        //public InventoryManager Manager;
        // public Configuration config;
        public String taskInterperterType = "DotLispInterpreter";// DotLispInterpreter,CycInterpreter or ABCLInterpreter
        ScriptEventListener scriptEventListener = null;
        readonly public ClientManager ClientManager;

        //public List<string> muteList;
        public bool muted = false;

        private UUID GroupMembersRequestID = UUID.Zero;
        public Dictionary<UUID, Group> GroupsCache = null;
        private ManualResetEvent GroupsEvent = new ManualResetEvent(false);

        /// <summary>
        /// 
        /// </summary>
        public BotClient(ClientManager manager, GridClient g)
        {
            LoginRetries = LoginRetriesFresh;
            ClientManager = manager;
            gridClient = g;
            manager.AddBotClient(this);
            NeedRunOnLogin = true;
            //manager.LastRefBotClient = this;
            updateTimer = new System.Timers.Timer(500);
            updateTimer.Elapsed += new System.Timers.ElapsedEventHandler(updateTimer_Elapsed);

            //            manager.AddTextFormCommands(this);
            //          RegisterAllCommands(Assembly.GetExecutingAssembly());

            Settings.USE_INTERPOLATION_TIMER = false;
            Settings.LOG_LEVEL = Helpers.LogLevel.None;

            //   Settings.LOG_RESENDS = false;
            //   Settings.ALWAYS_DECODE_OBJECTS = true;
            //   Settings.ALWAYS_REQUEST_OBJECTS = true;
            //   Settings.SEND_AGENT_UPDATES = true;
            ////   Settings.SYNC_PACKETCALLBACKS = true;
            //   Settings.OBJECT_TRACKING = true;
            //   //Settings.STORE_LAND_PATCHES = true;
            //   //Settings.USE_TEXTURE_CACHE = true;
            //   //Settings.PARCEL_TRACKING = true;
            //   //Settings.FETCH_MISSING_INVENTORY = true;
            //   // Optimize the throttle
            //   Throttle.Wind = 0;
            //   Throttle.Cloud = 0;
            //   Throttle.Land = 1000000;
            //   Throttle.Task = 1000000;
            ////Throttle.Total = 250000;
            // Settings.CAPS_TIMEOUT = 6 * 1000;
            Settings.RESEND_TIMEOUT = 40 * 1000;
            Settings.MAX_RESEND_COUNT = 10;
            Settings.LOGIN_TIMEOUT = 120 * 1000;
            //Settings.LOGOUT_TIMEOUT = 120 * 1000;
            Settings.SIMULATOR_TIMEOUT = int.MaxValue;
            Settings.SEND_PINGS = true;
            Settings.SEND_AGENT_APPEARANCE = true;
            //Settings.USE_LLSD_LOGIN = true;
            ////Settings.MULTIPLE_SIMS = false;

            VoiceManager = new VoiceManager(gridClient);
            //manager.AddBotClientToTextForm(this);

            botPipeline = new SimEventMulticastPipeline(GetName());
            OneAtATimeQueue = new TaskQueueHandler(this, new TimeSpan(0, 0, 0, 0, 10), true, true);

            SetSecurityLevel(OWNERLEVEL, null, BotPermissions.Owner);
            ClientManager.PostAutoExecEnqueue(OneAtATimeQueue.Start);

            botPipeline.AddSubscriber(new SimEventTextSubscriber(WriteLine, this));
            // SingleInstance = this;
            ///this = this;// new GridClient();


            Settings.ALWAYS_DECODE_OBJECTS = true;
            Settings.ALWAYS_REQUEST_OBJECTS = true;
            Settings.OBJECT_TRACKING = true;
            Settings.AVATAR_TRACKING = true;
            Settings.STORE_LAND_PATCHES = true;


            //  Manager = Inventory;
            //Inventory = Manager.Store;

            // config = new Configuration();
            // config.loadConfig();
            /// Settings.LOGIN_SERVER = config.simURL;
            // Opensim recommends 250k total

            //Settings.ENABLE_CAPS = true;
            Self.Movement.Camera.Far = 512f;
            //Settings.LOG_ALL_CAPS_ERRORS = true;
            //Settings.FETCH_MISSING_INVENTORY = true;
            //Settings.SEND_AGENT_THROTTLE = false;

            //muteList = new List<string>();

            // outputDelegate = new OutputDelegate(doOutput);

            describers = new Dictionary<string, DescribeDelegate>();
            describers["location"] = new DescribeDelegate(describeLocation);
            describers["people"] = new DescribeDelegate(describePeople);
            describers["objects"] = new DescribeDelegate(describeObjects);
            describers["buildings"] = new DescribeDelegate(describeBuildings);

            describePos = 0;

            listeners = new Dictionary<string, cogbot.Listeners.Listener>();
            //registrationTypes["avatars"] = new Listeners.Avatars(this);
            //registrationTypes["chat"] = new Listeners.Chat(this);
            WorldSystem = new Listeners.WorldObjects(this);
            //registrationTypes["teleport"] = new Listeners.Teleport(this);
            //registrationTypes["whisper"] = new Listeners.Whisper(this);
            //ObjectSystem = new Listeners.Objects(this);
            //registrationTypes["bump"] = new Listeners.Bump(this);
            //registrationTypes["sound"] = new Listeners.Sound(this);
            //registrationTypes["sound"] = new Listeners.Objects(this);

            Commands = new SortedDictionary<string, Command>();
            Commands["login"] = new Login(this);
            Commands["logout"] = new Logout(this);
            Commands["stop"] = new Stop(this);
            Commands["teleport"] = new Teleport(this);
            Command desc = new Describe(this);
            Commands["describe"] = desc;
            Commands["look"] = desc;
            Commands["say"] = new Actions.Say(this);
            Commands["whisper"] = new Actions.Whisper(this);
            Commands["help"] = new cogbot.Actions.System.Help(this);
            Commands["sit"] = new Sit(this);
            Commands["stand"] = new Stand(this);
            Commands["jump"] = new Jump(this);
            Commands["crouch"] = new Crouch(this);
            Commands["mute"] = new Actions.Mute(this);
            Commands["unmute"] = new Actions.Mute(this);
            Commands["move"] = new Actions.Move(this);
            Commands["use"] = new Use(this);
            Commands["eval"] = new Eval(this);

            Commands["fly"] = new Fly(this);
            Commands["stop-flying"] = new StopFlying(this);
            Commands["locate"] = Commands["location"] = Commands["where"] = new Actions.Movement.LocationCommand(this);
            Follow follow = new Follow(this);
            Commands["follow"] = follow;
            Commands["wear"] = new Actions.Wear(this);
            //Commands["simexport"] = new cogbot.Actions.SimExport.ExportCommand(this);
            Commands["stop following"] = follow;
            Commands["stop-following"] = follow;

            tutorials = new Dictionary<string, cogbot.Tutorials.Tutorial>();
            tutorials["tutorial1"] = new Tutorials.Tutorial1(manager, this);


            describeNext = true;

            XmlInterp = new XmlScriptInterpreter(this);
            XmlInterp.BotClient = this;
            if (false)
                ClientManager.PostAutoExecEnqueue(LoadTaskInterpreter);

            // Start the server
            lock (ClientManager.config)
            {
                thisTcpPort = ClientManager.nextTcpPort;
                ClientManager.nextTcpPort += ClientManager.config.tcpPortOffset;
                ClientManager.PostAutoExecEnqueue(() =>
                {
                    Utilities.BotTcpServer UtilitiesTcpServer = new Utilities.BotTcpServer(thisTcpPort, this);
                    UtilitiesTcpServer.ServerPortIncr = ClientManager.config.tcpPortOffset;
                    UtilitiesTcpServer.startSocketListener();
                    ClientManager.nextTcpPort = UtilitiesTcpServer.ServerPort + UtilitiesTcpServer.ServerPortIncr;
                });
            }

            Network.RegisterCallback(PacketType.AgentDataUpdate, AgentDataUpdateHandler);
            Network.RegisterCallback(PacketType.AlertMessage, AlertMessageHandler);
            Network.RegisterCallback(PacketType.AvatarAppearance, AvatarAppearanceHandler);

            //Move to effects Appearance.OnAppearanceUpdated += new AppearanceManager.AppearanceUpdatedCallback(Appearance_OnAppearanceUpdated);

            Inventory.InventoryObjectOffered += Inventory_OnInventoryObjectReceived;
            Groups.GroupMembersReply += new EventHandler<GroupMembersReplyEventArgs>(GroupMembersHandler);
            Logger.OnLogMessage += new Logger.LogCallback(client_OnLogMessage);
            Network.EventQueueRunning += Network_OnEventQueueRunning;
            Network.LoginProgress += Network_OnLogin;
            Network.LoggedOut += Network_OnLogoutReply;
            Network.SimConnected += Network_OnSimConnected;
            Network.SimDisconnected += Network_OnSimDisconnected;
            //Network.OnConnected += Network_OnConnected;
            Network.Disconnected += Network_OnDisconnected;
            Self.IM += Self_OnInstantMessage;
            //Self.OnScriptDialog += new AgentManager.ScriptDialogCallback(Self_OnScriptDialog);
            //Self.OnScriptQuestion += new AgentManager.ScriptQuestionCallback(Self_OnScriptQuestion);
            Self.TeleportProgress += Self_OnTeleport;
            Self.ChatFromSimulator += Self_OnChat;
            var callback = new EventHandler<CurrentGroupsEventArgs>(Groups_OnCurrentGroups);
            Groups.CurrentGroups += callback;

            ClientManager.PostAutoExecEnqueue(() => { updateTimer.Start(); });
            searcher = new BotInventoryEval(this);
            ClientManager.PostAutoExecEnqueue(() =>
            {
                if (useLispEventProducer)
                {
                    lispEventProducer = new LispEventProducer(this, LispTaskInterperter);
                }
            });

        }
        void SetLoginName(string firstName, string lastName)
        {
            if (!string.IsNullOrEmpty(firstName) || !string.IsNullOrEmpty(lastName))
            {
                var details = ClientManager.FindOrCreateAccount(firstName, lastName);
                SetLoginAcct(details);
            }
            else
            {
                DebugWriteLine("nameless still");
            }
        }

        public void SetLoginAcct(LoginDetails details)
        {
            details.Client = this;
            _BotLoginParams = details;
            ClientManager.OnBotClientUpdatedName(GetName(), this);
        }

        private MethodInvoker CatchUpInterns = () => { };

        private void LoadTaskInterpreter()
        {
            lock (LispTaskInterperterLock)
                try
                {
                    if (_LispTaskInterperter != null) return;
                    //WriteLine("Start Loading TaskInterperter ... '" + TaskInterperterType + "' \n");
                    _LispTaskInterperter = ScriptManager.LoadScriptInterpreter(taskInterperterType, this);
                    _LispTaskInterperter.LoadFile("cogbot.lisp", DebugWriteLine);
                    Intern("clientManager", ClientManager);
                    Intern("client", this);
                    if (scriptEventListener == null)
                    {
                        scriptEventListener = new ScriptEventListener(_LispTaskInterperter, this);
                        botPipeline.AddSubscriber(scriptEventListener);
                    }

                    //  WriteLine("Completed Loading TaskInterperter '" + TaskInterperterType + "'\n");
                    // load the initialization string
                    CatchUpInterns();
                }
                catch (Exception e)
                {
                    LogException("LoadTaskInterperter", e);
                }
        }

        private bool useLispEventProducer = false;
        private LispEventProducer lispEventProducer;
        public bool RunStartupClientLisp = true;
        public object RunStartupClientLisplock = new object();
        public void StartupClientLisp()
        {
            lock (RunStartupClientLisplock)
            {
                if (!RunStartupClientLisp) return;
                DebugWriteLine("Running StartupClientLisp");
                RunStartupClientLisp = false;
                string startupClientLisp = ClientManager.config.startupClientLisp;
                if (startupClientLisp.Length > 1)
                {
                    try
                    {
                        LoadTaskInterpreter();
                        //InvokeJoin("Waiting on StartupClientLisp");
                        evalLispString("(progn " + ClientManager.config.startupClientLisp + ")");
                    }
                    catch (Exception ex)
                    {
                        LogException("StartupClientLisp", ex);
                    }
                }
                DebugWriteLine("Ran StartupClientLisp");
            }
        }
        public void RunOnLogin()
        {
            lock (RunStartupClientLisplock)
            {
                StartupClientLisp();
                InvokeJoin("Waiting on RunOnLogin");
                if (!NeedRunOnLogin) return;
                NeedRunOnLogin = false;
                string onLogin = ClientManager.config.onLogin;
                if (onLogin.Length > 1)
                {
                    try
                    {
                        evalLispString("(progn " + onLogin + ")");
                    }
                    catch (Exception ex)
                    {
                        LogException("RunOnLogin: " + onLogin, ex);
                    }
                }
            }
        }
        //breaks up large responses to deal with the max IM size


        private void updateTimer_Elapsed(object sender, System.Timers.ElapsedEventArgs e)
        {
            List<Command> actions = new List<Command>();
            lock (Commands)
            {
                actions.AddRange(Commands.Values);
            }
            foreach (var c in actions)
                if (c.Active)
                    c.Think();
        }

        private void AgentDataUpdateHandler(object sender, PacketReceivedEventArgs e)
        {
            var sim = e.Simulator;
            var packet = e.Packet;
            AgentDataUpdatePacket p = (AgentDataUpdatePacket)packet;
            if (p.AgentData.AgentID == sim.Client.Self.AgentID)
            {
                //TODO MAKE DEBUG MESSAGE  WriteLine(String.Format("Got the group ID for {0}, requesting group members...", sim.Client));
                GroupID = p.AgentData.ActiveGroupID;
                if (!CogbotHelpers.IsNullOrZero(GroupID)) sim.Client.Groups.RequestGroupMembers(GroupID);
            }
        }

        private void GroupMembersHandler(object sender, GroupMembersReplyEventArgs e)
        {
            //TODO MAKE DEBUG MESSAGE  WriteLine(String.Format("Got {0} group members.", members.Count));
            GroupMembers = e.Members;
        }

        private void AvatarAppearanceHandler(object sender, PacketReceivedEventArgs e)
        {
            var sim = e.Simulator;
            var packet = e.Packet;
            AvatarAppearancePacket appearance = (AvatarAppearancePacket)packet;

            lock (Appearances) Appearances[appearance.Sender.ID] = appearance;
        }

        private void AlertMessageHandler(object sender, PacketReceivedEventArgs e)
        {
            var sim = e.Simulator;
            var packet = e.Packet;
            AlertMessagePacket message = (AlertMessagePacket)packet;
            WriteLine("[AlertMessage] " + Utils.BytesToString(message.AlertData.Message));
        }

        public void ReloadGroupsCache()
        {
            //         GroupManager.CurrentGroupsCallback callback =
            //               new GroupManager.CurrentGroupsCallback(Groups_OnCurrentGroups);
            var callback = new EventHandler<CurrentGroupsEventArgs>(Groups_OnCurrentGroups);
            Groups.CurrentGroups += callback;
            Groups.RequestCurrentGroups();
            GroupsEvent.WaitOne(10000, false);
            Groups.CurrentGroups -= callback;
            GroupsEvent.Reset();
        }

        public UUID GroupName2UUID(String groupName)
        {
            UUID tryUUID = UUID.Zero;
            if (UUID.TryParse(groupName, out tryUUID))
                return tryUUID;
            if (null == GroupsCache)
            {
                ReloadGroupsCache();
                if (null == GroupsCache)
                    return UUID.Zero;
            }
            lock (GroupsCache)
            {
                if (GroupsCache.Count > 0)
                {
                    foreach (Group currentGroup in GroupsCache.Values)
                        if (currentGroup.Name.ToLower() == groupName.ToLower())
                            return currentGroup.ID;
                }
            }
            return WorldSystem.FindUUIDForName(groupName);
        }

        private void Groups_OnCurrentGroups(object sender, CurrentGroupsEventArgs e)
        {
            if (null == GroupsCache)
                GroupsCache = e.Groups;
            else
                lock (GroupsCache) { GroupsCache = e.Groups; }
            GroupsEvent.Set();
        }


        void Self_OnTeleport(object sender, TeleportEventArgs e)
        {
            var status = e.Status;
            if (status == TeleportStatus.Finished || status == TeleportStatus.Failed || status == TeleportStatus.Cancelled)
            {
                OutputDelegate WriteLine = DisplayNotificationInChat;
                WriteLine("Teleport " + status);
                if (!cogbot.Actions.SimExport.ExportCommand.IsExporting) describeSituation(WriteLine);
            }
        }


        void Self_OnChat(object sender, ChatEventArgs e)
        {
            InstantMessageDialog Dialog = InstantMessageDialog.MessageFromAgent;
            switch (e.SourceType)
            {
                case ChatSourceType.System:
                    break;
                case ChatSourceType.Agent:
                    break;
                case ChatSourceType.Object:
                    Dialog = InstantMessageDialog.MessageFromObject;
                    break;
            }

            Self_OnMessage(e.FromName, e.SourceID, e.OwnerID,
                           e.Message, UUID.Zero, false,
                           e.Simulator.RegionID, e.Position,
                           Dialog, e.Type, e);
            ;
           
        }

        private void Self_OnInstantMessage(object sender, InstantMessageEventArgs e)
        {
            InstantMessage im = e.IM;
            ChatType Type = ChatType.Normal;
            switch (im.Dialog)
            {
                case InstantMessageDialog.StartTyping:
                    Type = ChatType.StartTyping;
                    break;
                case InstantMessageDialog.StopTyping:
                    Type = ChatType.StopTyping;
                    break;
            }
            Self_OnMessage(im.FromAgentName, im.FromAgentID, im.ToAgentID,
                           im.Message, im.IMSessionID, im.GroupIM,
                           im.RegionID, im.Position,
                           im.Dialog, Type, e);
        }

        private void Self_OnMessage(string FromAgentName, UUID FromAgentID, UUID ToAgentID,
            string Message, UUID IMSessionID, bool GroupIM,
            UUID RegionID, Vector3 Position,
            InstantMessageDialog Dialog, ChatType Type, EventArgs origin)
        {
            bool IsOwner = (Type == ChatType.OwnerSay);
            if (Dialog == InstantMessageDialog.GroupNotice)
            {
                GroupIM = true;
            }

            // Received an IM from someone that is authenticated
            if (FromAgentID == MasterKey || FromAgentName == MasterName)
            {
                IsOwner = true;
            }
            BotPermissions perms = GetSecurityLevel(FromAgentID, FromAgentName);

            if (perms == BotPermissions.Owner)
            {
                IsOwner = true;
            }

            bool displayedMessage = false;
            if (origin is ChatEventArgs && Message.Length > 0 && Dialog == InstantMessageDialog.MessageFromAgent)
            {
                WriteLine(String.Format("{0} says, \"{1}\".", FromAgentName, Message));
                PosterBoard["/posterboard/onchat"] = Message;
                if (FromAgentName == Self.Name)
                {
                    PosterBoard["/posterboard/onchat-said"] = Message;
                }
                else
                {
                    PosterBoard["/posterboard/onchat-heard"] = Message;
                }
            }

            bool groupIM = GroupIM && GroupMembers != null && GroupMembers.ContainsKey(FromAgentID) ? true : false;


            switch (Dialog)
            {
                case InstantMessageDialog.MessageBox:
                    break;
                case InstantMessageDialog.GroupInvitation:
                    if (IsOwner)
                    {
                        string groupName = Message;
                        int found = groupName.IndexOf("Group:");
                        if (found > 0) groupName = groupName.Substring(found + 6);
                        Self.InstantMessage(Self.Name, FromAgentID, string.Empty, IMSessionID,
                                            InstantMessageDialog.GroupInvitationAccept, InstantMessageOnline.Offline,
                                            Self.SimPosition,
                                            UUID.Zero, new byte[0]);
                        found = groupName.IndexOf(":");
                        if (found > 0)
                        {
                            groupName = groupName.Substring(0, found).Trim();
                            ExecuteCommand("joingroup " + groupName);
                        }
                    }
                    break;
                case InstantMessageDialog.InventoryOffered:
                    break;
                case InstantMessageDialog.InventoryAccepted:
                    break;
                case InstantMessageDialog.InventoryDeclined:
                    break;
                case InstantMessageDialog.GroupVote:
                    break;
                case InstantMessageDialog.TaskInventoryOffered:
                    break;
                case InstantMessageDialog.TaskInventoryAccepted:
                    break;
                case InstantMessageDialog.TaskInventoryDeclined:
                    break;
                case InstantMessageDialog.NewUserDefault:
                    break;
                case InstantMessageDialog.SessionAdd:
                    break;
                case InstantMessageDialog.SessionOfflineAdd:
                    break;
                case InstantMessageDialog.SessionGroupStart:
                    break;
                case InstantMessageDialog.SessionCardlessStart:
                    break;
                case InstantMessageDialog.SessionSend:
                    break;
                case InstantMessageDialog.SessionDrop:
                    break;
                case InstantMessageDialog.BusyAutoResponse:
                    break;
                case InstantMessageDialog.ConsoleAndChatHistory:
                    break;
                case InstantMessageDialog.Lure911:
                case InstantMessageDialog.RequestTeleport:
                    if (IsOwner)
                    {
                        TheSimAvatar.StopMoving();
                        if (RegionID != UUID.Zero)
                        {
                            if (!displayedMessage)
                            {
                                DisplayNotificationInChat("TP to Lure from " + FromAgentName);
                                displayedMessage = true;
                            }
                            SimRegion R = SimRegion.GetRegion(RegionID, gridClient);
                            if (R != null)
                            {
                                Self.Teleport(R.RegionHandle, Position);
                                return;
                            }
                        }
                        DisplayNotificationInChat("Accepting TP Lure from " + FromAgentName);
                        displayedMessage = true;
                        Self.TeleportLureRespond(FromAgentID, IMSessionID, true);
                    }
                    break;
                case InstantMessageDialog.AcceptTeleport:
                    break;
                case InstantMessageDialog.DenyTeleport:
                    break;
                case InstantMessageDialog.GodLikeRequestTeleport:
                    break;
                case InstantMessageDialog.CurrentlyUnused:
                    break;
                case InstantMessageDialog.GotoUrl:
                    break;
                case InstantMessageDialog.Session911Start:
                    break;
                case InstantMessageDialog.FromTaskAsAlert:
                    break;
                case InstantMessageDialog.GroupNotice:
                    break;
                case InstantMessageDialog.GroupNoticeInventoryAccepted:
                    break;
                case InstantMessageDialog.GroupNoticeInventoryDeclined:
                    break;
                case InstantMessageDialog.GroupInvitationAccept:
                    break;
                case InstantMessageDialog.GroupInvitationDecline:
                    break;
                case InstantMessageDialog.GroupNoticeRequested:
                    break;
                case InstantMessageDialog.FriendshipOffered:
                    if (IsOwner)
                    {
                        DisplayNotificationInChat("Accepting Friendship from " + FromAgentName);
                        Friends.AcceptFriendship(FromAgentID, IMSessionID);
                        displayedMessage = true;
                    }
                    break;
                case InstantMessageDialog.FriendshipAccepted:
                    break;
                case InstantMessageDialog.FriendshipDeclined:
                    break;
                case InstantMessageDialog.StartTyping:
                    break;
                case InstantMessageDialog.StopTyping:
                    break;
                case InstantMessageDialog.MessageFromObject:
                case InstantMessageDialog.MessageFromAgent:                    
                    // message from self
                    if (FromAgentName == GetName()) return;
                    // message from system
                    if (FromAgentName == "System") return;
                    // message from others
                    Actions.Whisper whisper = (Actions.Whisper) Commands["whisper"];
                    whisper.currentAvatar = FromAgentID;
                    whisper.currentSession = IMSessionID;
                    if (IsOwner)
                    {
                        OutputDelegate WriteLine;
                        if (origin is InstantMessageEventArgs)
                        {
                            WriteLine = new OutputDelegate((string text, object[] ps) =>
                            {
                                string reply0 = DLRConsole.SafeFormat(text, ps);
                                InstantMessage(FromAgentID, reply0, IMSessionID);
                            });
                        }
                        else
                        {
                            WriteLine = new OutputDelegate((string text, object[] ps) =>
                            {
                                string reply0 = DLRConsole.SafeFormat(text, ps);
                                Talk(reply0, 0, Type);
                            });
                        }
                        string cmd = Message;

                        if (cmd.StartsWith("cmcmd "))
                        {
                            cmd = cmd.Substring(6);
                            WriteLine("");
                            WriteLine(string.Format("invokecm='{0}'", cmd));
                            ClientManager.DoCommandAll(cmd, FromAgentID, WriteLine);
                        }
                        else if (cmd.StartsWith("cmd "))
                        {
                            cmd = cmd.Substring(4);
                            WriteLine(string.Format("invoke='{0}'", cmd));
                            var res = ExecuteCommand(cmd, FromAgentID, WriteLine);
                            WriteLine("iresult='" + res + "'");
                        }
                        else if (cmd.StartsWith("/") || cmd.StartsWith("@"))
                        {
                            cmd = cmd.Substring(1);
                            WriteLine("");
                            WriteLine(string.Format("invoke='{0}'", cmd));
                            var res = ExecuteCommand(cmd, FromAgentID, WriteLine);
                            WriteLine("iresult='" + res + "'");
                        }
                    }
                    break;
                default:
                    break;
            }
            if (Dialog != InstantMessageDialog.MessageFromAgent && Dialog != InstantMessageDialog.MessageFromObject)
            {
                string debug = String.Format("{0} {1} {2} {3} {4}: {5}",
                                             IsOwner ? "IsOwner" : "NonOwner",
                                             groupIM ? "GroupIM" : "IM", Dialog, Type, perms,
                                             Helpers.StructToString(origin));
                if (!displayedMessage)
                {
                    DisplayNotificationInChat(debug);
                    displayedMessage = true;
                }
            }
        }

        protected UUID TheAvatarID
        {
            get { return Self.AgentID; }
        }
        public void DisplayNotificationInChat(string str)
        {
            DisplayNotificationInChatReal(str);
        }
        public void DisplayNotificationInChat(string str, params object[] args)
        {
            DisplayNotificationInChatReal(DLRConsole.SafeFormat(str, args));
        }

        String stringBuff = "";
        object stringBuffLock = new object();
        public void DisplayNotificationInChatReal(string str)
        {
            lock (stringBuffLock)
            {
                if (stringBuff == "")
                {
                    stringBuff = str;
                } else
                {
                    stringBuff = stringBuff + "\r\n" + str;
                    return;
                }
            }
            InvokeGUI(
                () =>
                    {
                        ChatConsole cc = (ChatConsole) TheRadegastInstance.TabConsole.Tabs["chat"].Control;
                        RichTextBoxPrinter tp = (RichTextBoxPrinter) cc.ChatManager.TextPrinter;
                        InvokeGUI(cc.rtbChat, () =>
                                                  {
                                                      string s = tp.Content;
                                                      if (s.Length > 30000)
                                                      {
                                                          tp.Content = s.Substring(s.Length - 30000);
                                                      }
                                                      //if (cc.)
                                                      lock (stringBuffLock)
                                                      {
                                                          str = stringBuff;
                                                          stringBuff = "";
                                                      }
                                                      ClientManager.WriteLine(str);
                                                      TheRadegastInstance.TabConsole.DisplayNotificationInChat(str);
                                                  });

                    });
        }


        private void Inventory_OnInventoryObjectReceived(object sender, InventoryObjectOfferedEventArgs e)
        {
            if (true)
            {
                e.Accept = true;
                return; // accept everything}
            }
            if (_masterKey != UUID.Zero)
            {
                if (e.Offer.FromAgentID != _masterKey)
                {
                    e.Accept = false;
                    return;
                }
            }
            else if (GroupMembers != null && !GroupMembers.ContainsKey(e.Offer.FromAgentID))
            {
                e.Accept = false;
                return;
            }

            e.Accept = true;
        }

        // EVENT CALLBACK SECTION
        void Network_OnDisconnected(object sender, DisconnectedEventArgs e)
        {
            var message = e.Message;
            var reason = e.Reason;
            try
            {
                if (message.Length > 0)
                    WriteLine("Disconnected from server. Reason is " + message + ". " + reason);
                else
                    WriteLine("Disconnected from server. " + reason);

                SendNetworkEvent("On-Network-Disconnected", reason, message);

                WriteLine("Bad Names: " + BoringNamesCount);
                WriteLine("Good Names: " + GoodNamesCount);
            }
            catch (Exception ex)
            {
                LogException("Network_OnDisconnected", ex);
            }
            EnsureConnectedCheck(reason);
        }

        private void EnsureConnectedCheck(NetworkManager.DisconnectType reason)
        {
            if (ExpectConnected && reason != NetworkManager.DisconnectType.ClientInitiated)
            {
                List<Simulator> sims = new List<Simulator>();
                lock (Network.Simulators)
                {
                    sims.AddRange(Network.Simulators);
                }
                return;
                ExpectConnected = false;
                foreach (var s in sims)
                {
                    //lock (s)
                    {
                        if (s.Connected) s.Disconnect(true);
                    }

                }
                //gridClient = new GridClient();
                //Settings.USE_LLSD_LOGIN = true;
                new Thread(() =>
                {
                    Thread.Sleep(10000);
                    Login(true);
                }).Start();
            }
        }

        void Network_OnConnected(object sender)
        {
            try
            {

                //System.Threading.Thread.Sleep(3000);

                //  describeAll();
                //  describeSituation();
                SendNetworkEvent("On-Network-Connected");
                ExpectConnected = true;
            }
            catch (Exception e)
            {
                LogException("Network-On-Connected", e);
            }
        }


        void Network_OnSimDisconnected(object sender, SimDisconnectedEventArgs e)
        {
            var simulator = e.Simulator;
            var reason = e.Reason;
            SendNetworkEvent("On-Sim-Disconnected", this, simulator, reason);
            if (simulator == Network.CurrentSim)
            {
                EnsureConnectedCheck(reason);
            }
        }

        void client_OnLogMessage(object message, Helpers.LogLevel level)
        {                 
            string mes = "" + message;
            if (Settings.LOG_NAMES)
            {
                if (mes.Contains(">: "))
                {
                    string selfmessage = String.Format("<{0}>: ", Self.Name);                    
                    if (!mes.StartsWith(selfmessage))
                    {
                        if (WorldSystem.IsGridMaster)
                        {
                            SendNetworkEvent("On-GridLog-Message", message, level);
                        }
                        return;
                    }
                }
            }
            SendNetworkEvent("On-Log-Message", message, level);
        }

        void Network_OnEventQueueRunning(object sender, EventQueueRunningEventArgs e)
        {
            var simulator = e.Simulator;
            SendNetworkEvent("On-Event-Queue-Running", simulator);
        }

        void Network_OnSimConnected(object sender, SimConnectedEventArgs e)
        {
            ExpectConnected = true;
            var simulator = e.Simulator;
            if (simulator == Network.CurrentSim)
            {
                if (!Settings.SEND_AGENT_APPEARANCE)
                {
                    Appearance.RequestAgentWearables();
                }
                Self.RequestMuteList();
            }
            if (Self.AgentID != UUID.Zero)
            {
                SetSecurityLevel(Self.AgentID, Self.Name, BotPermissions.Owner);
            }
            SendNetworkEvent("On-Simulator-Connected", simulator);
            //            SendNewEvent("on-simulator-connected",simulator);
        }

        bool Network_OnSimConnecting(Simulator simulator)
        {
            SendNetworkEvent("On-Simulator-Connecing", simulator);
            return true;
        }

        void Network_OnLogoutReply(object sender, LoggedOutEventArgs e)
        {
            SendNetworkEvent("On-Logout-Reply", e.InventoryItems);
        }

        //=====================
        // 
        // Notes:
        //   1. When requesting folder contents Opensim/libomv may react differently than SL/libomv
        //       in particular the boolean for 'folder' may not respond as expected
        //       one option is to set it to 'false' and expect folders as 'items,type folder' than
        //       as seperate folder fields.

        /// <summary>
        /// UseInventoryItem("wear","Pink Dress");
        /// UseInventoryItem("attach","Torch!");
        /// UseInventoryItem("animationStart","Dance Loop");
        /// </summary>
        /// <param name="usage"></param>
        /// <param name="Item"></param>
        public void UseInventoryItem(string usage, string itemName)
        {

            if (Inventory != null && Inventory.Store != null)
            {

                InventoryFolder rootFolder = Inventory.Store.RootFolder;
                //InventoryEval searcher = new InventoryEval(this);
                searcher.evalOnFolders(rootFolder, usage, itemName);
            }
            else
            {
                WriteLine("UseInventoryItem " + usage + " " + itemName + " is not yet ready");

            }
        }

        public void ListObjectsFolder()
        {
            // should be just the objects folder 
            InventoryFolder rootFolder = Inventory.Store.RootFolder;
            //InventoryEval searcher = new InventoryEval(this);
            searcher.evalOnFolders(rootFolder, "print", "");
        }

        public void wearFolder(string folderName)
        {
            // what we simply want
            //    Client.Appearance.WearOutfit(folderName.Split('/'), false);

            UUID folderID = UUID.Zero;
            InventoryFolder rootFolder = Inventory.Store.RootFolder;
            //List<FolderData> folderContents;
            // List<ItemData> folderItems;
            BotInventoryEval searcher = new BotInventoryEval(this);

            folderID = searcher.findInFolders(rootFolder, folderName);

            if (folderID != UUID.Zero)
            {
                Self.Chat("Wearing folder \"" + folderName + "\"", 0, ChatType.Normal);
                WriteLine("Wearing folder \"" + folderName + "\"");

                Appearance.AddToOutfit(GetFolderItems(folderName));
                /*
                List<InventoryBase> folderContents=  Client.Inventory.FolderContents(folderID, Client.Self.AgentID,
                                                                false, true,
                                                                InventorySortOrder.ByDate, new TimeSpan(0, 0, 0, 0, 10000));

                if (folderContents != null)
                {
                    folderContents.ForEach(
                        delegate(ItemData i)
                        {
                            Client.Appearance.Attach(i, AttachmentPoint.Default);
                            Client.Self.Chat("Attaching item: " + i.Name, 0, ChatType.Normal);
                            WriteLine("Attaching item: " + i.Name);
                        }
                    );
                }
                */
            }
            else
            {
                Self.Chat("Can't find folder \"" + folderName + "\" to wear", 0, ChatType.Normal);
                WriteLine("Can't find folder \"" + folderName + "\" to wear");
            }

        }

        public void PrintInventoryAll()
        {
            InventoryFolder rootFolder = Inventory.Store.RootFolder;
            //InventoryEval searcher = new InventoryEval(this);
            searcher.evalOnFolders(rootFolder, "print", "");

        }


        public UUID findInventoryItem(string name)
        {
            InventoryFolder rootFolder = Inventory.Store.RootFolder;  //  .Inventory.InventorySkeleton.Folders;// .RootUUID;
            //InventoryEval searcher = new InventoryEval(this);

            return searcher.findInFolders(rootFolder, name);

        }


        public void logout()
        {
            ExpectConnected = false;
            if (Network.Connected)
                Network.Logout();
        }

        public void WriteLineReal(string str)
        {
            try
            {
                if (str == null) return;
                if (str == "") return;
                if (str.StartsWith("$bot")) str = str.Substring(4);
                str = str.Replace("\r\n", "\n").Replace("\r", "\n").Replace("\n", "\r\n").Trim();
                string SelfName = String.Format("{0}", GetName());
                str = str.Replace("$bot", SelfName);
                if (str.StartsWith(SelfName)) str = str.Substring(SelfName.Length).Trim();
                ClientManager.SetDebugConsole(__TheRadegastInstance);
                ClientManager.WriteLine(str);
            }
            catch (Exception ex)
            {
                Logger.Log(GetName() + " WriteLineReal Exception " + ex, Helpers.LogLevel.Error, ex);
            }

        }

        public void DebugWriteLine(string str, params object[] args)
        {
            try
            {
                if (str == null) return;
                if (args != null && args.Length > 0) str = String.Format(str, args);
                if (str == "") return;
                if (str.StartsWith("$bot")) str = str.Substring(4);
                str = str.Replace("\r\n", "\n").Replace("\r", "\n").Replace("\n", "\r\n").Trim();
                string SelfName = String.Format("{0}", GetName());
                str = str.Replace("$bot", SelfName);
                if (str.StartsWith(SelfName)) str = str.Substring(SelfName.Length).Trim();
                if (false) ClientManager.SetDebugConsole(__TheRadegastInstance);
                ClientManager.DebugWriteLine(str);
            }
            catch (Exception ex)
            {
                Logger.Log(GetName() + " DebugWriteLine Exception " + ex, Helpers.LogLevel.Error, ex);
            }            
        }
        public void WriteLine(string str)
        {
            try
            {
                if (str == null) return;
                WriteLineReal(str);
            }
            catch (Exception ex)
            {
                Logger.Log(GetName() + " WriteLine Exception " + ex, Helpers.LogLevel.Error, ex);
            }
        }
        public void WriteLine(string str, params object[] args)
        {
            try
            {
                if (str == null) return;
                if (args != null && args.Length > 0) str = String.Format(str, args);
                WriteLineReal(str);
            }
            catch (Exception ex)
            {
                Logger.Log(GetName() + " WriteLine Exception " + ex, Helpers.LogLevel.Error, ex);
            }
        }
        // for lisp to call
        public void output(string txt)
        {
            WriteLine(txt);
        }

        public void describeAll(bool detailed, OutputDelegate outputDel)
        {
            foreach (string dname in describers.Keys)
                describers[dname].Invoke(detailed, outputDel);
        }

        public void describeSituation(OutputDelegate outputDel)
        {
            int i = 0;
            string name = "";
            foreach (string dname in describers.Keys)
                if (i++ == describePos)
                    name = dname;
            describePos = (describePos + 1) % describers.Count;
            describers[name].Invoke(false, outputDel);
        }

        public void describeLocation(bool detailed, OutputDelegate WriteLine)
        {
            WriteLine("$bot is in " + (Network.CurrentSim != null ? Network.CurrentSim.Name : "<Unknown>") + ".");
        }

        public void describePeople(bool detailed, OutputDelegate WriteLine)
        {
            //if (detailed) {
            //    List<Avatar> avatarList = WorldSystem.getAvatarsNear(Self.RelativePosition, 8);
            //    if (avatarList.Count > 1) {
            //        string str = "$bot sees the people ";
            //        for (int i = 0; i < avatarList.Count - 1; ++i)
            //            str += WorldSystem.getAvatarName(avatarList[i]) + ", ";
            //        str += "and " + WorldSystem.getAvatarName(avatarList[avatarList.Count - 1]) + ".";
            //        WriteLine(str);
            //    } else if (avatarList.Count == 1) {
            //        WriteLine("$bot sees one person: " + WorldSystem.getAvatarName(avatarList[0]) + ".");
            //    } else
            //        WriteLine("doesn't see anyone around.");
            //} else {
            //    WriteLine("$bot sees " + WorldSystem.numAvatars() + " people.");
            //}
        }

        public void describeObjects(bool detailed, OutputDelegate WriteLine)
        {
            List<Primitive> prims = WorldSystem.getPrimitives(16);
            if (prims.Count > 1)
            {
                WriteLine("$bot sees the objects:");
                for (int i = 0; i < prims.Count; ++i)
                    WriteLine(WorldSystem.describePrim(prims[i], detailed));
                //str += "and " + WorldSystem.GetSimObject(prims[prims.Count - 1]) + ".";
                
            }
            else if (prims.Count == 1)
            {
                WriteLine("$bot sees one object: " + WorldSystem.describePrim(prims[0], detailed));
            }
            else
            {
                WriteLine("$bot doesn't see any objects around.");
            }
        }

        public void describeBuildings(bool detailed, OutputDelegate WriteLine)
        {
            List<Vector3> buildings = WorldSystem.getBuildings(8);
            WriteLine("$bot sees " + buildings.Count + " buildings.");
        }



        public ScriptInterpreter _LispTaskInterperter;
        public ScriptInterpreter LispTaskInterperter
        {
            get
            {
                if (_LispTaskInterperter == null)
                {
                    LoadTaskInterpreter();
                }
                return _LispTaskInterperter;
            }
        }

        public readonly object LispTaskInterperterLock = new object();
        readonly private List<Type> registeredTypes = new List<Type>();

        public void enqueueLispTask(object p)
        {
            scriptEventListener.enqueueLispTask(p);
        }

        public Object evalLispReader(TextReader stringCodeReader)
        {
            try
            {
                Object r = LispTaskInterperter.Read("evalLispString", stringCodeReader, WriteLine);
                if (LispTaskInterperter.Eof(r))
                    return r.ToString();
                return evalLispCode(r);
            }
            catch (Exception e)
            {
                LogException("evalLispInterp stringCodeReader", e);
                throw e;
            }
        }

        public string evalLispReaderString(TextReader reader)
        {
            return LispTaskInterperter.Str(evalLispReader(reader));
        }


        public string evalXMLString(TextReader reader)
        {
            return XmlInterp.evalXMLString(reader);
        }

        /// <summary>
        /// (thisClient.XML2Lisp2 "http://myserver/myservice/?q=" chatstring) 
        /// </summary>
        /// <param name="URL"></param>
        /// <param name="args"></param>
        /// <returns></returns>
        public string XML2Lisp2(string URL, string args)
        {
            return XmlInterp.XML2Lisp2(URL, args);
        } // method: XML2Lisp2


        public string XML2Lisp(string xcmd)
        {
            return XmlInterp.XML2Lisp(xcmd);
        }

        public string evalLispString(string lispCode)
        {
            try
            {
                if (lispCode == null || lispCode.Length == 0) return null;
                Object r = null;
                //lispCode = "(load-assembly \"libsecondlife\")\r\n" + lispCode;                
                StringReader stringCodeReader = new StringReader(lispCode);
                return evalLispReaderString(stringCodeReader);
            }
            catch (Exception e)
            {
                LogException("evalLispInterp lispCode=" + lispCode, e);
                throw e;
            }
        }

        public Object evalLispCode(Object lispCode)
        {
            try
            {
                if (lispCode == null) return null;
                if (lispCode is String)
                {
                    StringReader stringCodeReader = new StringReader(lispCode.ToString());
                    lispCode = LispTaskInterperter.Read("evalLispString", stringCodeReader, WriteLine);
                }
                WriteLine("Eval> " + lispCode);
                if (LispTaskInterperter.Eof(lispCode))
                    return lispCode.ToString();
                return LispTaskInterperter.Eval(lispCode);
            }
            catch (Exception e)
            {
                LogException("evalLispInterp lispCode=" + lispCode, e);
                throw e;
            }
        }

        public override string ToString()
        {
            return "'(thisClient \"" + GetName() + "\")";
        }

        /// <summary>
        /// Initialize everything that needs to be initialized once we're logged in.
        /// </summary>
        /// <param name="login">The status of the login</param>
        /// <param name="message">Error message on failure, MOTD on success.</param>
        public void Network_OnLogin(object sender, LoginProgressEventArgs e)
        {
            var message = e.Message;
            var login = e.Status;
            if (_BotLoginParams == null)
            {
                SetLoginName(gridClient.Self.FirstName, gridClient.Self.LastName);
            } else
            {
                _BotLoginParams.Status = login;
            }
            if (login == LoginStatus.Success)
            {
                // Start in the inventory root folder.
                if (Inventory.Store != null)
                    CurrentDirectory = Inventory.Store.RootFolder; //.RootFolder;
                else
                {
                    Logger.Log("Cannot get Inventory.Store.RootFolder", OpenMetaverse.Helpers.LogLevel.Error);
                    CurrentDirectory = null;
                }
                OneAtATimeQueue.Enqueue(RunOnLogin);
            } // anyhitng other than success NeedRunOnLogin
            else
            {
                NeedRunOnLogin = true;
            }
            //            WriteLine("ClientManager Network_OnLogin : [" + login.ToString() + "] " + message);
            //SendNewEvent("On-Login", login, message);

            if (login == LoginStatus.Failed)
            {
                ExpectConnected = false;
                SendNetworkEvent("On-Login-Fail", login, message);
                WriteLine("Login Failed " + message + " LoginRetries: " + LoginRetries);
                if (LoginRetries <= 0)
                {
                    if (_BotLoginParams != null)
                    {
                        _BotLoginParams.LoginEvent.Set();
                    }
                    return;
                }
                LoginRetries--;
                Login(false);
            }
            else if (login == LoginStatus.Success)
            {
                if (!ClientManager.StarupLispCreatedBotClients)
                {
                    GetLoginOptionsFromRadegast();
                }
                LoginRetries = 0; // maybe LoginRetriesFresh??
                WriteLine("Logged in successfully");
                ExpectConnected = true;
                SendNetworkEvent("On-Login-Success", login, message);
                //                SendNewEvent("on-login-success",login,message);
                if (_BotLoginParams != null)
                {
                    _BotLoginParams.LoginEvent.Set();
                }
            }
            else
            {
                SendNetworkEvent("On-Login", login, message);
            }

        }

        readonly Dictionary<Assembly, List<Listener>> KnownAssembies = new Dictionary<Assembly, List<Listener>>();
        public void InvokeAssembly(Assembly assembly, string args, OutputDelegate output)
        {
            LoadAssembly(assembly);
            List<Listener> items = null;
            lock (KnownAssembies)
            {
                if (!KnownAssembies.TryGetValue(assembly, out items))
                {
                    items = new List<Listener>();
                    KnownAssembies.Add(assembly, items);
                }
            }
            lock (items)
            {
                foreach (Listener item in items)
                {
                    item.InvokeCommand(args, output);
                }
            }
        }

        private bool ConstructType(Assembly assembly, Type type, string name, Predicate<Type> when, Action<Type> action)
        {
            bool found = false;
            foreach (Type t in assembly.GetTypes())
            {
                try
                {
                    if (t.IsSubclassOf(type) && when(t))
                    {
                        try
                        {
                            found = true;
                            Type type1 = t;
                            InvokeNext(name + " " + t, () => action(type1));
                        }
                        catch (Exception e)
                        {
                            e = InnerMostException(e);
                            LogException("ERROR!  " + name + " " + t + " " + e + "\n In " + t.Name, e);
                        }
                    }
                }
                catch (Exception e)
                {
                    WriteLine(e.ToString());
                }
            }
            return found;
        }

        public Dictionary<Assembly, List<Listener>> AssemblyListeners = new Dictionary<Assembly, List<Listener>>();

        public List<Listener> LoadAssembly(Assembly assembly)
        {
            ClientManager.RegisterAssembly(assembly);
            List<Listener> items = null;
            lock (KnownAssembies)
            {
                if (KnownAssembies.TryGetValue(assembly, out items))
                {
                    return items;
                }
                items = new List<Listener>();
                KnownAssembies.Add(assembly, items);
            }
            bool found = false;               

            foreach (Type t in assembly.GetTypes())
            {
                try
                {
                    if (t.IsSubclassOf(typeof(WorldObjectsModule)))
                    {
                        ConstructorInfo info = t.GetConstructor(new Type[] { typeof(BotClient) });
                        try
                        {
                            found = true;
                            InvokeNext("LoadAssembly " + assembly, () =>
                                       {

                                           try
                                           {
                                               Listener command = (Listener)info.Invoke(new object[] { this });
                                               RegisterListener(command);
                                               items.Add(command);
                                           }
                                           catch (Exception e1)
                                           {
                                               e1 = InnerMostException(e1);
                                               LogException("ERROR! RegisterListener: " + e1 + "\n In " + Thread.CurrentThread.Name, e1);   
                                           }
                                       });

                        }
                        catch (Exception e)
                        {
                            e = InnerMostException(e);
                            LogException("ERROR! RegisterListener: " + e + "\n In " + t.Name, e);
                        }
                    }
                }
                catch (Exception e)
                {
                    WriteLine(e.ToString());
                }
            }
            if (!found)
            {
                // throw new Exception("missing entry point " + assembly);
            }
            return items;
        }
        public static Exception InnerMostException(Exception exception)
        {
            Exception inner = exception.InnerException;
            if (inner != null && inner != exception)
                return InnerMostException(inner);
            return exception;
        }

        /// <summary>
        /// Initialize everything that needs to be initialized once we're logged in.
        /// </summary>
        /// <param name="login">The status of the login</param>
        /// <param name="message">Error message on failure, MOTD on success.</param>
        public void RegisterCommand(string name, cogbot.Actions.Command command)
        {
            string orginalName = name;
            name = name.Replace(" ", "").ToLower();
            while (name.EndsWith(".")) name = name.Substring(0, name.Length - 1);
            Monitor.Enter(Commands);
            Command prev;
            if (!Commands.TryGetValue(name, out prev))
            {
                Commands.Add(name, command);
                command.Name = orginalName;
                command.TheBotClient = this;
            }
            else
            {
                if (prev.GetType() != command.GetType()) RegisterCommand("!" + orginalName, command);
            }
            Monitor.Exit(Commands);
        }

        public void RegisterCommand(Command command)
        {
            RegisterCommand(command.Name, command);
        }

        internal void DoCommandAll(string line, UUID uUID, OutputDelegate outputDelegate)
        {
            ClientManager.DoCommandAll(line, uUID, outputDelegate);
        }

        //internal void LogOut(GridClient Client)
        //{
        //Client.Network.Logout();
        //}

        internal OpenMetaverse.Utilities.VoiceManager GetVoiceManager()
        {
            return VoiceManager;
        }


        public void Dispose()
        {
            //scriptEventListener.
            lock (this)
            {
                if (!Running) return;
                Running = false;
                logout();
                if (updateTimer!=null)
                {
                    updateTimer.Enabled = false;
                    updateTimer.Close();
                }
                //botPipeline.Shut
                if (botPipeline != null) botPipeline.Dispose();
                if (lispEventProducer != null) lispEventProducer.Dispose();
                WorldSystem.Dispose();
                //thrJobQueue.Abort();
                //lock (lBotMsgSubscribers)
                //{   
                if (_LispTaskInterperter != null) _LispTaskInterperter.Dispose();
                foreach (var ms in listeners.Values)
                {
                    ms.Dispose();
                }
                ClientManager.Remove(this);
            }
        }

        //List<BotMessageSubscriber> lBotMsgSubscribers = new List<BotMessageSubscriber>();
        public interface BotMessageSubscriber
        {
            void msgClient(string serverMessage);
            void ShuttingDown();
        }
        public void AddBotMessageSubscriber(SimEventSubscriber tcpServer)
        {
            botPipeline.AddSubscriber(tcpServer);
        }
        public void RemoveBotMessageSubscriber(SimEventSubscriber tcpServer)
        {
            botPipeline.RemoveSubscriber(tcpServer);
        }

        public void SendNetworkEvent(string eventName, params object[] args)
        {
            SendPersonalEvent(SimEventType.NETWORK, eventName, args);
        }


        public void SendPersonalEvent(SimEventType type, string eventName, params object[] args)
        {
            if (args.Length > 0)
            {
                if (args[0] is BotClient)
                {
                    args[0] = ((BotClient)args[0]).GetAvatar();
                }
            }
            SimObjectEvent evt = botPipeline.CreateEvent(type, SimEventClass.PERSONAL, eventName, args);
            evt.AddParam("recipientOfInfo", GetAvatar());
            SendPipelineEvent(evt);
        }

        public void SendPipelineEvent(SimObjectEvent evt)
        {
            OnEachSimEvent(evt);
            botPipeline.SendEvent(evt);
        }


        internal string argsListString(IEnumerable list)
        {
            if (scriptEventListener == null) return "" + list;
            return ScriptEventListener.argsListString(list);
        }

        internal string argString(object p)
        {
            if (scriptEventListener == null) return "" + p;
            return ScriptEventListener.argString(p);
        }

        public CmdResult ExecuteCommand(string text)
        {
            // done inside the callee InvokeJoin("ExecuteCommand " + text);
            OutputDelegate WriteLine = DisplayNotificationInChat;
            return ExecuteCommand(text, this, WriteLine);
        }

        private bool InvokeJoin(string s)
        {
            return InvokeJoin(s, -1);
        }
        private bool InvokeJoin(string s, int millisecondsTimeout)
        {
            return OneAtATimeQueue.InvokeJoin(s, millisecondsTimeout);
        }
        internal bool InvokeJoin(string s, int millisecondsTimeout, ThreadStart task1, ThreadStart task2)
        {
            return OneAtATimeQueue.InvokeJoin(s, millisecondsTimeout, task1, task2);
        }

        public void InvokeNext(string s, ThreadStart e)
        {
            OneAtATimeQueue.Enqueue(s, () =>
                                        {
                                            e();
                                        });
        }

        public CmdResult ExecuteCommand(string text, object session, OutputDelegate WriteLine)
        {
            if (string.IsNullOrEmpty(text)) return null;
            text = text.Trim();
            while (text.StartsWith("/"))
            {
                text = text.Substring(1).TrimStart();
            }
            if (string.IsNullOrEmpty(text)) return null;
            CmdResult res = ExecuteBotCommand(text, session, WriteLine);
            if (res != null) return res;
            res = ClientManager.ExecuteSystemCommand(text, session, WriteLine);           
            if (res != null) return res;
            WriteLine("I don't understand the ExecuteCommand " + text + ".");
            return null;
        }


        public CmdResult ExecuteBotCommand(string text, object session, OutputDelegate WriteLine)
        {
            if (text == null)
            {
                return null;
            }
            text = text.Trim();
            while (text.StartsWith("/")) text = text.Substring(1).TrimStart();
            if (text.Length == 0)
            {
                return null;
            }
            try
            {

                if (text.StartsWith("("))
                {
                    InvokeJoin("ExecuteBotCommand " + text);
                    return new CmdResult(evalLispString(text).ToString(), true);
                }
                //            Settings.LOG_LEVEL = Helpers.LogLevel.Debug;
                //text = text.Replace("\"", "");
                string verb = Parser.ParseArgs(text)[0];
                verb = verb.ToLower();

                Command act = GetCommand(verb, false);
                if (act != null)
                {
                    if (act is GridMasterCommand)
                    {
                        if (!WorldSystem.IsGridMaster)
                        {
                            return null;
                        }
                    }
                    if (act is RegionMasterCommand)
                    {
                        if (!IsRegionMaster)
                        {
                            return null;
                        }
                    }
                    try
                    {
                        CmdResult res;
                        if (text.Length > verb.Length)
                            return DoCmdAct(act, verb, text.Substring(verb.Length + 1), session, WriteLine);
                        else
                            return DoCmdAct(act, verb, "", session, WriteLine);
                    }
                    catch (Exception e)
                    {
                        LogException("ExecuteBotCommand " + text, e);
                        return new CmdResult("ExecuteBotCommand " + text + "cuased " + e, false);
                    }
                }
                else
                {
                    if (WorldSystem == null || WorldSystem.SimAssetSystem == null)
                        return new CmdResult("no world yet for gesture", false);
                    UUID assetID = WorldSystem.SimAssetSystem.GetAssetUUID(text, AssetType.Gesture);
                    if (assetID != UUID.Zero) return ExecuteBotCommand("gesture " + assetID, session, WriteLine);
                    assetID = WorldSystem.SimAssetSystem.GetAssetUUID(text, AssetType.Animation);
                    if (assetID != UUID.Zero) return ExecuteBotCommand("anim " + assetID, session, WriteLine);
                    return null;
                }
            }
            catch (Exception e)
            {
                LogException("ExecuteBotCommand " + text, e);
                return null;
            }
        }

        static public CmdResult DoCmdAct(Command command, string verb, string args, object callerSession, OutputDelegate del)
        {
            var callerID = SessionToCallerId(callerSession);
            string cmdStr = "ExecuteActBotCommand " + verb + " " + args;
            BotClient robot = command.TheBotClient;
            if (command is BotPersonalCommand)
            {
                robot.InvokeJoin(cmdStr);
            }
            return command.acceptInputWrapper(verb, args, callerID, del);
            //robot.OneAtATimeQueue.Enqueue(cmdStr, () => command.acceptInputWrapper(verb, args, callerID, del));
        }

        public bool IsValidCommand(string cmd)
        {
            return GetCommand(cmd, true) != null;
        }

        public Command GetCommand(string text, bool clientCmds)
        {
            if (string.IsNullOrEmpty(text)) return null;
            text = text.Trim();
            while (text.StartsWith("/"))
            {
                text = text.Substring(1).TrimStart();
            }
            if (string.IsNullOrEmpty(text)) return null;
            text = Parser.ParseArguments(text)[0].ToLower();
            Command fnd;

            if (Commands.TryGetValue(text, out fnd)) return fnd;
            if (clientCmds)
            {
                var bc = ClientManager;
                if (bc != null)
                {
                    if (bc.groupActions.TryGetValue(text, out fnd)) return fnd;
                }
            }
            if (text.EndsWith("s")) return GetCommand(text.Substring(0, text.Length - 1), clientCmds);
            return null;
        }

        public readonly static UUID OWNERLEVEL = UUID.Parse("ffffffff-ffff-ffff-ffff-ffffffffffff");
 
        public static UUID SessionToCallerId(object callerSession)
        {
            if (callerSession is BotClient) return OWNERLEVEL;
            if (callerSession == null)
            {
                return UUID.Zero;
            }
            UUID callerId = UUID.Zero;
            if (callerSession is UUID) callerId = (UUID)callerSession;
            if (callerId != UUID.Zero) return callerId;
            CmdRequest request = callerSession as CmdRequest;
            if (request != null) return SessionToCallerId(request.CallerAgent);
            return OWNERLEVEL; 
            throw new NotImplementedException();
        }

        public string GetName()
        {
            string n = Self.Name;
            if (n != null && !String.IsNullOrEmpty(n.Trim())) return n;
            if (_BotLoginParams == null)
            {
                return "Noname" + GetHashCode();
            }
            if (String.IsNullOrEmpty(BotLoginParams.FirstName))
            {
                return string.Format("Unnamed Robot: {0} {1}", BotLoginParams.FirstName, BotLoginParams.LastName);
                throw new NullReferenceException("GEtName");
            }
            return string.Format("{0} {1}", BotLoginParams.FirstName, BotLoginParams.LastName);
        }


        void SimEventSubscriber.OnEvent(SimObjectEvent evt)
        {
            if (evt.GetVerb() == "On-Execute-Command")
            {
                ExecuteCommand(evt.GetArgs()[0].ToString(), null, WriteLine);
            }
        }

        void SimEventSubscriber.Dispose()
        {
            ((BotClient)this).Dispose();
        }


        public void TalkExact(string str, int channel, ChatType type)
        {
            if(!TalkingAllowed)
            {
                WriteLine("!!NOTE!! skipping saying " + str);
                return;
            }
            if (str.Contains("\n"))
            {
                string[] split = str.Split(new char[] { '\n' });
                foreach (var s in split)
                {
                    TalkExact(s, channel, type);
                }
                return;
            }
            if (!Network.Connected)
            {
                if (OnInstantMessageSent != null)
                {
                    OnInstantMessageSent(this, new IMessageSentEventArgs(str, UUID.Zero, UUID.Zero, DateTime.Now));
                }
                return;
            }
            Self.Chat(str, channel, type);
        }

        public void Intern(string n, object v)
        {
            if (_LispTaskInterperter != null)
            {
                _LispTaskInterperter.Intern(n, v);
            }
            var PrevCode = CatchUpInterns;
            CatchUpInterns = () =>
                                 {
                                     PrevCode();
                                     _LispTaskInterperter.Intern(n, v);
                                 };
        }


        public void InternType(Type t)
        {
//          LispTaskInterperter.InternType(t);
            ScriptManager.AddType(t);
        }

        private void RegisterListener(Listener listener)
        {
            // listeners[listener.GetModuleName()] = listener;

            string mname = listener.GetModuleName();
            string taskName = "LISTENER STARTUP: " + mname;
            OneAtATimeQueue.Enqueue(taskName, () =>
                                        {
                                            try
                                            {
                                                WriteLine(taskName);
                                                listener.StartupListener();
                                                WriteLine("LISTENER STARTUP COMPLETE: " + listener.GetModuleName());
                                            }
                                            catch (Exception e)
                                            {
                                                WriteLine("LISTENER STARTUP FAILED: " + listener.GetModuleName() + " " + e);
                                            }
                                        });
        }

        internal void RegisterType(Type t)
        {
            ClientManager.RegisterType(t);
            if (registeredTypes.Contains(t)) return;
            registeredTypes.Add(t);
            try
            {
                if (t.IsSubclassOf(typeof(Command)))
                {
                    if (!typeof(SystemApplicationCommand).IsAssignableFrom(t))
                    {
                        ConstructorInfo info = t.GetConstructor(new Type[] { typeof(BotClient) });
                        try
                        {
                            Command command = (Command)info.Invoke(new object[] { this });
                            RegisterCommand(command);
                        }
                        catch (Exception e)
                        {
                            LogException("RegisterCommand " + t.Name, e);
                        }
                    }
                }
            }
            catch (Exception e)
            {
                LogException("RegisterType! " + t, e);
            }
        }

        public SimAvatarClient TheSimAvatar
        {
            get
            {
                return (SimAvatarClient)WorldSystem.TheSimAvatar;
            }
        }
        internal object GetAvatar()
        {
            if (gridClient.Self.AgentID != UUID.Zero) return TheSimAvatar;
            return this;
        }

        public void FakeEvent(Object target, String infoName, params object[] parameters)
        {

            Type type = target.GetType();
            EventInfo eventInfo = type.GetEvent(infoName, BindingFlags.Instance | BindingFlags.Public |
                                                          BindingFlags.NonPublic | BindingFlags.IgnoreCase);
            MethodInfo m = null;
            if (eventInfo != null)
            {
                infoName = eventInfo.Name;
                m = eventInfo.GetRaiseMethod(true);
            }
            
            Exception lastException = null;
            if (m != null)
            {
                try
                {


                    m.Invoke(target, parameters);
                    return;
                }
                catch (Exception e)
                {
                    lastException = e;
                }
            }
            else
            {
                {
                    foreach (var o in new[] {"", "_", "m_"})
                    {


                        FieldInfo fieldInfo = type.GetField(o + infoName,
                                                            BindingFlags.Instance | BindingFlags.NonPublic) ??
                                              type.GetField(o + infoName,
                                                            BindingFlags.Instance | BindingFlags.Public)
                                              ?? type.GetField(o + infoName,
                                                               BindingFlags.Instance | BindingFlags.Public |
                                                               BindingFlags.NonPublic | BindingFlags.IgnoreCase);

                        if (fieldInfo != null)
                        {
                            Delegate del = fieldInfo.GetValue(target) as Delegate;

                            if (del != null)
                            {
                                del.DynamicInvoke(parameters);
                                return;
                            }
                        }
                    }
                }
                if (eventInfo != null)
                {
                    m = eventInfo.EventHandlerType.GetMethod("Invoke");

                    if (m != null)
                    {
                        Type dt = m.DeclaringType;
                        try
                        {


                            m.Invoke(target, parameters);
                            return;
                        }
                        catch (Exception e)
                        {
                            lastException = e;
                        }
                    }
                }
                var ms = eventInfo.GetOtherMethods(true);
                foreach (MethodInfo info in ms)
                {
                }
            }

            if (lastException != null) throw lastException;
            //MethodInfo m = eventInfo.GetOtherMethods(true);
            throw new NotSupportedException();
        }

        public List<InventoryItem> GetFolderItems(string target)
        {
            if (Inventory.Store == null)
            {
                return null;
            }
            if (Inventory.Store.RootNode == null)
            {
                return null;
            }
            if (Inventory.Store.RootFolder == null)
            {
                return null;
            }
            UUID folderID = Inventory.FindObjectByPath(Inventory.Store.RootFolder.UUID, Self.AgentID, target, 10000);
            return GetFolderItems(folderID);
        }

        public List<InventoryItem> GetFolderItems(UUID folderID)
        {
            List<InventoryItem> items = new List<InventoryItem>();
            var list = Inventory.FolderContents(folderID, Self.AgentID, false, true, InventorySortOrder.ByDate, 10000);
            if (list != null) foreach (var i in list)
                {
                    if (i is InventoryItem) items.Add((InventoryItem)i);
                }
            return items;
        }

        public void SetRadegastLoginOptions()
        {
            if (TheRadegastInstance == null) return;
            ClientManager.EnsureRadegastForm(this, TheRadegastInstance, "EnsureRadegastForm from SetRadegastLoginOptions " + GetName());
            var to = TheRadegastInstance.Netcom.LoginOptions;
            to.FirstName = BotLoginParams.FirstName;
            to.LastName = BotLoginParams.LastName;
            to.Password = BotLoginParams.Password;
            string loginURI = BotLoginParams.URI;

            MainProgram.CommandLine.LoginUri = loginURI;
            MainProgram.CommandLine.Location = BotLoginParams.Start;
            int gidx; 
            Grid G = GetGridIndex(loginURI, out gidx);
            if (G == null)
            {
                G = new Grid(BotLoginParams.URI, BotLoginParams.URI, loginURI);
                to.GridCustomLoginUri = loginURI;
            }
            else
            {
                BotLoginParams.URI = G.LoginURI;
            }
            TheRadegastInstance.Netcom.LoginOptions.Grid = G;
            to.Grid = G;
            string botStartAt = BotLoginParams.Start;            

            if (botStartAt == "home")
            {
                to.StartLocation = StartLocationType.Home;
            }
            else if (botStartAt == "last")
            {
                to.StartLocation = StartLocationType.Last;                
            } else
            {
                to.StartLocation = StartLocationType.Custom;
                to.StartLocationCustom = botStartAt;
            }            
            to.Version = BotLoginParams.Version;
            to.Channel = BotLoginParams.Channel;
            RadegastTab tab;
            if (TheRadegastInstance.TabConsole.Tabs.TryGetValue("login", out tab))
            {
                tab.AllowDetach = true;
                tab.AllowClose = false;
                tab.AllowMerge = false;
                tab.AllowHide = false;  
                LoginConsole form = (LoginConsole)tab.Control;
                DLRConsole.InvokeControl(form, () => SetRadegastLoginForm(form, to));
            }
        }

        public Grid GetGridIndex(String gridName, out int gridIx)
        {
            var instance = TheRadegastInstance;
            gridIx = -1;
            for (int i = 0; i < instance.GridManger.Count; i++)
            {
                Grid testGrid = instance.GridManger[i];
                // cbxGrid.Items.Add(instance.GridManger[i]);
                if (gridName == testGrid.ID || gridName == testGrid.LoginURI || gridName == testGrid.Name)
                {
                    gridIx = i;
                    return testGrid;
                }
            }
            return null;
        }
        private void SetRadegastLoginForm(LoginConsole console, LoginOptions options)
        {
            console.cbxUsername.Text = (string.Format("{0} {1}", options.FirstName, options.LastName)).Trim();

            switch (options.StartLocation)
            {
                case StartLocationType.Last:
                    //console.cbxLocation.Text = options.StartLocationCustom = "last";
                    console.cbxLocation.SelectedIndex = 1;
                    break;
                case StartLocationType.Home:
                    //console.cbxLocation.Text = options.StartLocationCustom = "home";
                    console.cbxLocation.SelectedIndex = 0;
                    break;
                default:
                    console.cbxLocation.SelectedIndex = -1;
                    console.cbxLocation.Text = options.StartLocationCustom;
                    break;
            }
            console.cbTOS.Checked = true;
            var G = options.Grid;
            string gridName = options.GridCustomLoginUri;        
            int gridIx = -1;
            String LoginURI = null;
            G = GetGridIndex(gridName, out gridIx) ?? G;
            if (gridIx == -1)
            {
                if (G != null && !String.IsNullOrEmpty(G.ID))
                {
                    LoginURI = G.LoginURI;
                    if (LoginURI != null) console.txtCustomLoginUri.Text = LoginURI;
                    console.cbxGrid.Text = G.Name ?? G.ID;
                    TheRadegastInstance.Netcom.LoginOptions.Grid = G;
                }
                else { console.cbxGrid.Text = "Custom"; }
                if (LoginURI == null) console.txtCustomLoginUri.Text = options.GridCustomLoginUri;
            } else {
                console.txtCustomLoginUri.Text = G.LoginURI;
                console.cbxGrid.SelectedIndex = gridIx;
            }
        }

        public void GetLoginOptionsFromRadegast()
        {
            var from = TheRadegastInstance.Netcom.LoginOptions;
            BotLoginParams.FirstName = from.FirstName;
            BotLoginParams.LastName = from.LastName;
            BotLoginParams.Password = from.Password;
            BotLoginParams.URI = from.GridCustomLoginUri;
            Grid g = from.Grid;
            if (g != null)
            {
                BotLoginParams.URI = g.LoginURI;
            }
            switch (from.StartLocation)
            {
                case StartLocationType.Last:
                    BotLoginParams.Start = "last";
                    break;
                case StartLocationType.Home:
                    BotLoginParams.Start = "home";
                    break;
                default:
                    BotLoginParams.Start = from.StartLocationCustom;
                    break;
            }
            BotLoginParams.Version = from.Version;
            BotLoginParams.Channel = from.Channel;
            SetRadegastLoginOptions();
        }

        public void ShowTab(string name)
        {
            InvokeGUI(() => TheRadegastInstance.TabConsole.GetTab(name.ToLower()).Select());
        }

        public void AddTab(string name, string label, UserControl _debugWindow, EventHandler CloseDebug)
        {
            InvokeGUI(() =>
                       {
                           RadegastTab tab = TheRadegastInstance.TabConsole.AddTab(name.ToLower(), label, _debugWindow);
                           tab.AllowDetach = true;
                           tab.AllowMerge = true;
                           //tab.TabClosed += ((sender, args) => _debugWindow.Dispose());
                           if (CloseDebug != null)
                           {
                               tab.AllowClose = true;
                               tab.TabClosed += CloseDebug;
                           }
                           else
                           {
                               tab.AllowClose = false;
                           }
                           //Application.EnableVisualStyles();
                           //Application.Run(new Form(_debugWindow));
                       });
        }

        public Thread InvokeThread(String name, ThreadStart action)
        {
            lock (botCommandThreads)
            {
                Thread tr = new Thread(() =>
                                           {
                                               try
                                               {
                                                   try
                                                   {
                                                       action();
                                                   }
                                                   catch (Exception e)
                                                   {
                                                       WriteLine("Exception in " + name + ": " + e);                                                      
                                                   }
                                               } 
                                               finally
                                               {
                                                   try
                                                   {
                                                       RemoveThread(Thread.CurrentThread);
                                                   }
                                                   catch (OutOfMemoryException) { }
                                                   catch (StackOverflowException) { }
                                                   catch (Exception) { } 
                                               }
                                           }) {Name = name};
                AddThread(tr);
                tr.Start();
                return tr;
            }
        }
        public void InvokeGUI(Control mf, ThreadStart o)
        {
            try
            {
                if (false && mf.IsHandleCreated)
                {
                }
                if (mf.InvokeRequired)
                {
                    mf.Invoke(o);
                }
                else o();
            }
            catch (Exception e)
            {
                WriteLine("ERROR! InvokeGUI " + e);
            }
        }

        public void InvokeGUI(ThreadStart o)
        {
            InvokeGUI(TheRadegastInstance.MainForm, o);
        }

        public BotPermissions GetSecurityLevel(UUID uuid, string name)
        {
            BotPermissions bp;
            if (uuid != UUID.Zero)
            {
                lock (SecurityLevels)
                    if (SecurityLevels.TryGetValue(uuid, out bp))
                    {
                        return bp;
                    }
            }
            if (!string.IsNullOrEmpty(name))
            {
                lock (SecurityLevelsByName)
                    if (SecurityLevelsByName.TryGetValue(name, out bp))
                    {
                        return bp;
                    }
            }
            return BotPermissions.Base;
        }

        public void SetSecurityLevel(UUID uuid, string name, BotPermissions perms)
        {
            BotPermissions bp;
            if (uuid != UUID.Zero)
            {
                lock (SecurityLevels) SecurityLevels[uuid] = perms;
            }
            if (!string.IsNullOrEmpty(name))
            {
                // dont take whitepaces
                name = name.Trim();
                if (name != "") lock (SecurityLevelsByName) SecurityLevelsByName[name] = perms;
            }
        }

        public CmdResult ExecuteTask(string scripttype, TextReader reader, OutputDelegate WriteLine)
        {
            var si = ScriptManager.LoadScriptInterpreter(scripttype, this);
            object o = si.Read(scripttype, reader, WriteLine);
            if (o is CmdResult) return (CmdResult)o;
            if (o == null) return new CmdResult("void", true);
            if (si.Eof(o)) return new CmdResult("EOF " + o, true);
            o = si.Eval(o);
            if (o is CmdResult) return (CmdResult)o;
            if (o == null) return new CmdResult("void", true);
            if (si.Eof(o)) return new CmdResult("EOF " + o, true);
            return new CmdResult("" + o, true);
        }

        public string DoHttpGet(string url)
        {
            return Encoding.UTF8.GetString((new System.Net.WebClient()).DownloadData(url)); ;
        }

        public string DoHttpPost(Object[] args)
        {
            NameValueCollection dict = new NameValueCollection();
            for (int i = 1; i < args.Length; i++)
            {
                dict.Add(args[i++].ToString(), args[i].ToString());
            }
            return HttpPost.DoHttpPost(args[0].ToString(), dict);
        }


        public CmdResult ExecuteXmlCommand(string cmd, object session, OutputDelegate line)
        {
            return XmlInterp.ExecuteXmlCommand(cmd, session, line);
        }


        /// <summary>
        /// Example text: <sapi> <silence msec="100" /> <bookmark mark="anim:hello.csv"/> Hi there </sapi>
        /// </summary>
        /// <param name="text"></param>
        /// <param name="gate"></param>
        public void XmlTalk(string text, OutputDelegate gate)
        {
            if (true)
            {
                //  SendCleanText(text);
                // return;
            }
            try
            {

                if (!text.Trim().StartsWith("<sapi"))
                {
                    text = "<sapi>" + text + "</sapi>";
                    //  Talk(text);
                    //  return;
                }
                // var sr = new StringReader("<xml>" + text + "</xml>");
                String toSay = "";
                HashSet<String> toAnimate = new HashSet<string>();
                // var reader = new XmlTextReader(sr);
                //while (reader.Read())
                {
                    var doc = new XmlDocumentLineInfo("XmlTalk: " + text, false);
                    doc.Load(new XmlTextReader(new StringReader(text)));
                    //reader.MoveToElement();
                    foreach (var n in doc.DocumentElement.ChildNodes)
                    {
                        XmlNode node = (XmlNode) n;
                        if (node == null) continue;
                        string lname = node.Name.ToLower();

                        //XmlNode node = null;
                        if (lname == "sapi")
                        {
                            XmlTalk(node.InnerXml, gate);
                            continue;
                        }
                        if (lname == "silence")
                        {
                            double sleepms = Double.Parse(node.Attributes["msec"].Value);
                            // todo
                            continue;
                        }
                        if (lname == "bookmark")
                        {
                            toSay = toSay.Trim();
                            if (!String.IsNullOrEmpty(toSay))
                                TalkExact(toSay, 0, ChatType.Normal);
                            toSay = String.Empty;
                            toAnimate.Add(node.Attributes["mark"].Value);
                            continue;
                        }
                        if (node.NodeType == XmlNodeType.Text)
                        {
                            toSay += " " + node.InnerText;
                            continue;
                        }
                        toSay += " <parseme>" + node.OuterXml + "</parseme>";
                    }
                }
                toSay = toSay.Trim();
                if (!String.IsNullOrEmpty(toSay))
                    TalkExact(toSay, 0, ChatType.Normal);
                int maxAnims = 2;
                foreach (var animation in toAnimate)
                {
                    if (maxAnims <= 0) return;
                    maxAnims--;
                    DoAnimation(animation);
                }
                return;
            }
            catch (Exception e)
            {
                LogException("XMlTalk " + text, e);
                // example fragment
                // <sapi> <silence msec="100" /> <bookmark mark="anim:hello.csv"/> Hi there </sapi>
                Talk(text);
            }
        }

        private void DoAnimation(string animate)
        {
            if (Self.AgentID == UUID.Zero)
            {
                WriteLine(";; DEBUG NOT ONLINE! anim: " + animate);
                return;
            }
            SimAvatarClient av = TheSimAvatar as SimAvatarClient;
            if (av != null)
            {
                SimAsset asset = GetAnimationOrGesture(animate);
                if (asset != null) av.WithAnim(asset, () => Thread.Sleep(2000));
            }       
        }

        private SimAsset GetAnimationOrGesture(string animate)
        {
            SimAsset asset = SimAssetStore.TheStore.GetAnimationOrGesture(animate);
            if (asset == null)
            {
                WriteLine(";; DEBUG no anim: " + animate);
            } else
            {
                WriteLine(";; DEBUG SUCCESS anim: " + asset);
            }
            return asset;
        }

        bool TalkingAllowed = true;
        public bool IsEnsuredRunning;
        public bool EnsuredRadegastRunning;
        public bool InvokedMakeRunning;
        public bool AddingTypesToBotClientNow;
        private bool NeedRunOnLogin = true;

        public void Talk(string text)
        {
            Talk(text, 0, ChatType.Normal);
        }
        public void Talk(string text, int channel, ChatType type)
        {
            text = text.Replace("<sapi>", "");
            text = text.Replace("</sapi>", "");
            text = text.Trim();

            int len = text.IndexOf("<");
            if (len>0)
            {
                TalkExact(text.Substring(0, len), channel, type);
                text = text.Substring(len);
                Talk(text, channel, type);
                return;                
            } else if (len==0)
            {
                int p2 = text.IndexOf(">") + 1;
                if (p2 > 0 && p2<text.Length)
                {
                    Talk(text.Substring(0, p2), channel, type);
                    Talk(text.Substring(p2), channel, type);
                    return;
                }
            }
            while (text.Contains("<"))
            {
                int p1 = text.IndexOf("<");
                int p2 = text.IndexOf(">", p1);
                if (p2 > p1)
                {
                    string fragment = text.Substring(p1, (p2 + 1) - p1);
                    if (fragment.Contains("Begin Meta"))
                    {
                        TalkingAllowed = false;
                    } else if (fragment.Contains("End Meta"))
                    {
                        TalkingAllowed = true;
                    }
                    text = text.Replace(fragment, " ");
                }
            }
            text = text.Trim();
            if (!String.IsNullOrEmpty(text))
                TalkExact(text, channel, type);
        }

        /// <summary>
        /// Sends instance message via radegast system
        /// </summary>
        /// <param name="text"></param>
        /// <param name="gate"></param>
        public void InstantMessage(UUID target, String text, UUID session)
        {
            if (text == null) return;
            if (text.EndsWith("Meta !-->")) return;
            int len = text.Length;
            if (len > 1023)
            {
                if (text.Contains("\n"))
                {
                    string[] split = text.Split(new char[] { '\n' });
                    foreach (var s in split)
                    {
                        InstantMessage(target, s, session);
                    }
                    return;
                }
                string message = text.Substring(0, 1020);
                InstantMessage(target, message, session);
                InstantMessage(target, text.Substring(1020), session);
                return;
            }
            if (TheRadegastInstance == null)
            {
                this.Self.InstantMessage(target, text, session);
                return;
            }
            InvokeGUI(() =>
                      TheRadegastInstance.Netcom.SendInstantMessage(text, target, session));
        }

        public string NameKey()
        {
            return BotLoginParams.BotLName;
        }

        #region SimEventSubscriber Members

        private bool _EventsEnabled = true;
        public bool EventsEnabled
        {
            get
            {
                return _EventsEnabled;// throw new NotImplementedException();
            }
            set
            {
                _EventsEnabled = value;
            }
        }

        #endregion
    }

    public delegate void InstantMessageSentArgs(object sender, IMessageSentEventArgs args);
    public class IMessageSentEventArgs : EventArgs
    {
        private string message;
        private UUID targetID = UUID.Zero;
        private UUID sessionID = UUID.Zero;
        private DateTime timestamp;

        public IMessageSentEventArgs(string message, UUID targetID, UUID sessionID, DateTime timestamp)
        {
            this.message = message;
            this.targetID = targetID;
            this.sessionID = sessionID;
            this.timestamp = timestamp;
        }

        public string Message
        {
            get { return message; }
        }

        public UUID TargetID
        {
            get { return targetID; }
        }

        public UUID SessionID
        {
            get { return sessionID; }
        }

        public DateTime Timestamp
        {
            get { return timestamp; }
        }
    }

    /// <summary>
    ///  
    /// </summary>
    [Flags]
    public enum BotPermissions : byte
    {
        /// <summary>Recognise</summary>
        Base = 0x01,
        /// <summary>Execute owner commands</summary>
        Owner = 0x02,
        /// <summary>Execute group level perms</summary>
        Group = 0x04,
        /// <summary>Ignore like for bots and users we dont chat with</summary>
        Ignore = 0x80
    }

    public class CmdRequest
    {
        public UUID CallerAgent;
        public OutputDelegate Output;

        public CmdRequest(UUID caller)
        {
            CallerAgent = caller;
        }
    }

}