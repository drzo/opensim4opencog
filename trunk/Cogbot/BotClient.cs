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

        readonly TaskQueueHandler OneAtATimeQueue;
        readonly public GridClient gridClient;
        // TODO's
        // Play Animations
        // private static UUID type_anim_uuid = new UUID("c541c47f-e0c0-058b-ad1a-d6ae3a4584d9");
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
                //SetLoginOptionsFromRadegast();
                Network.Login(BotLoginParams.FirstName, BotLoginParams.LastName,
                              BotLoginParams.Password, BotLoginParams.UserAgent, BotLoginParams.Start,
                              BotLoginParams.Version);
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
        public LoginDetails BotLoginParams;// = null;
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

        private IList<Thread> botCommandThreads = new ListAsSet<Thread>();
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
                    SecurityLevels[value] = BotPermissions.Owner;
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

        public VoiceManager VoiceManager;
        // Shell-like inventory commands need to be aware of the 'current' inventory folder.
        public InventoryFolder CurrentDirectory = null;

        private Quaternion bodyRotation = Quaternion.Identity;
        private Vector3 forward = new Vector3(0, 0.9999f, 0);
        private Vector3 left = new Vector3(0.9999f, 0, 0);
        private Vector3 up = new Vector3(0, 0, 0.9999f);
        readonly private System.Timers.Timer updateTimer;

        public Listeners.WorldObjects WorldSystem;
        //   static public ClientManager SingleInstance = null;
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

        public List<string> muteList;
        public bool muted = false;

        private UUID GroupMembersRequestID;
        public Dictionary<UUID, Group> GroupsCache = null;
        private ManualResetEvent GroupsEvent = new ManualResetEvent(false);

        /// <summary>
        /// 
        /// </summary>
        public BotClient(ClientManager manager, GridClient g, LoginDetails lp)
        {
            LoginRetries = LoginRetriesFresh;
            ClientManager = manager;
            gridClient = g;
            lp.Client = this;
            BotLoginParams = lp;
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
            OneAtATimeQueue = new TaskQueueHandler("BotStartupQueue " + GetName(), new TimeSpan(0, 0, 0, 0, 10), false);
            ClientManager.PostAutoExec.Enqueue(() =>
            {
                OneAtATimeQueue.Start();
            });
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

            muteList = new List<string>();

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
            Commands["move"] = new Actions.Move(this);
            Commands["use"] = new Use(this);
            Commands["eval"] = new Eval(this);

            Commands["fly"] = new Fly(this);
            Commands["stop-flying"] = new StopFlying(this);
            Commands["locate"] = Commands["location"] = Commands["where"] = new Actions.Movement.LocationCommand(this);
            Follow follow = new Follow(this);
            Commands["follow"] = follow;
            Commands["wear"] = new Actions.Wear(this);
            Commands["stop following"] = follow;
            Commands["stop-following"] = follow;

            tutorials = new Dictionary<string, cogbot.Tutorials.Tutorial>();
            tutorials["tutorial1"] = new Tutorials.Tutorial1(manager, this);


            describeNext = true;

            XmlInterp = new XmlScriptInterpreter(this);
            XmlInterp.BotClient = this;
            ClientManager.PostAutoExec.Enqueue(() =>
            {
                LoadTaskInterpreter();
            });

            // Start the server
            lock (ClientManager.config)
            {
                thisTcpPort = ClientManager.nextTcpPort;
                ClientManager.nextTcpPort += ClientManager.config.tcpPortOffset;
                ClientManager.PostAutoExec.Enqueue(() =>
                {
                    Utilities.BotTcpServer UtilitiesTcpServer = new Utilities.BotTcpServer(thisTcpPort, this);
                    UtilitiesTcpServer.startSocketListener();
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

            ClientManager.PostAutoExec.Enqueue(() => { updateTimer.Start(); });
            searcher = new BotInventoryEval(this);
            ClientManager.PostAutoExec.Enqueue(() =>
            {
                lispEventProducer = new LispEventProducer(this, LispTaskInterperter);
            });

        }

        private void LoadTaskInterpreter()
        {
            lock (LispTaskInterperterLock)
                try
                {
                    if (_LispTaskInterperter != null) return;
                    //WriteLine("Start Loading TaskInterperter ... '" + TaskInterperterType + "' \n");
                    _LispTaskInterperter = ScriptManager.LoadScriptInterpreter(taskInterperterType, this);
                    _LispTaskInterperter.LoadFile("cogbot.lisp", WriteLine);
                    _LispTaskInterperter.Intern("clientManager", ClientManager);
                    if (scriptEventListener == null)
                    {
                        scriptEventListener = new ScriptEventListener(_LispTaskInterperter, this);
                        botPipeline.AddSubscriber(scriptEventListener);
                    }

                    //  WriteLine("Completed Loading TaskInterperter '" + TaskInterperterType + "'\n");
                    // load the initialization string
                }
                catch (Exception e)
                {
                    LogException("LoadTaskInterperter", e);
                }
        }

        private LispEventProducer lispEventProducer;
        public bool RunStartupClientLisp = true;
        public object RunStartupClientLisplock = new object();
        public void StartupClientLisp()
        {
            if (!RunStartupClientLisp) return;
            lock (RunStartupClientLisplock)
            {
                if (!RunStartupClientLisp) return;
                RunStartupClientLisp = false;
                if (ClientManager.config.startupClientLisp.Length > 1)
                {
                    try
                    {
                        evalLispString("(progn " + ClientManager.config.startupClientLisp + ")");
                    }
                    catch (Exception ex)
                    {
                        LogException("StartupClientLisp", ex);
                    }
                }
            }
        }

        //breaks up large responses to deal with the max IM size
        private void SendResponseIM(GridClient client, UUID fromAgentID, OutputDelegate WriteLine, string data)
        {
            for (int i = 0; i < data.Length; i += 1024)
            {
                int y;
                if ((i + 1023) > data.Length)
                {
                    y = data.Length - i;
                }
                else
                {
                    y = 1023;
                }
                string message = data.Substring(i, y);
                client.Self.InstantMessage(fromAgentID, message);
            }
        }

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

                sim.Client.Groups.RequestGroupMembers(GroupID);
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
            UUID tryUUID;
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
            return UUID.Zero;
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
                WriteLine("Teleport " + status);
                describeSituation(WriteLine);
            }
        }


        void Self_OnChat(object sender, ChatEventArgs e)
        {
            var sourceType = e.SourceType;
            var message = e.Message;
            var fromName = e.FromName;
            if (message.Length > 0 && sourceType == ChatSourceType.Agent && !muteList.Contains(fromName))
            {
                WriteLine(String.Format("{0} says, \"{1}\".", fromName, message));
                PosterBoard["/posterboard/onchat"] = message;
                if (fromName == Self.Name)
                {
                    PosterBoard["/posterboard/onchat-said"] = message;
                }
                else
                {
                    PosterBoard["/posterboard/onchat-heard"] = message;
                }
            }
        }

        private void Self_OnInstantMessage(object sender, InstantMessageEventArgs e)
        {
            InstantMessage im = e.IM;
            if (im.Dialog == InstantMessageDialog.GroupNotice)
            {
                im.GroupIM = true;
            }
            if (im.FromAgentName == GetName()) return;
            if (im.FromAgentName == "System") return;
            if (im.Message.Length > 0 && im.Dialog == InstantMessageDialog.MessageFromAgent)
            {
                WriteLine(String.Format("{0} whispers, \"{1}\".", im.FromAgentName, im.Message));

                Actions.Whisper whisper = (Actions.Whisper)Commands["whisper"];
                whisper.currentAvatar = im.FromAgentID;
                whisper.currentSession = im.IMSessionID;
            }

            if (im.Message.Length > 0 && im.Dialog == InstantMessageDialog.GroupInvitation)
            {
                if (im.FromAgentID == _masterKey || im.FromAgentName == MasterName)
                {
                    string groupName = im.Message;
                    int found = groupName.IndexOf("Group:");
                    if (found > 0) groupName = groupName.Substring(found + 6);
                    Self.InstantMessage(Self.Name, im.FromAgentID, string.Empty, im.IMSessionID,
                            InstantMessageDialog.GroupInvitationAccept, InstantMessageOnline.Offline, Self.SimPosition,
                            UUID.Zero, new byte[0]);
                    found = groupName.IndexOf(":");
                    if (found > 0)
                    {
                        groupName = groupName.Substring(0, found).Trim();
                        ExecuteCommand("joingroup " + groupName);
                    }

                }
            }

            bool groupIM = im.GroupIM && GroupMembers != null && GroupMembers.ContainsKey(im.FromAgentID) ? true : false;

            if (im.FromAgentID == _masterKey || (GroupCommands && groupIM) || im.FromAgentName == MasterName)
            {
                // Received an IM from someone that is authenticated
                WriteLine(String.Format("<{0} ({1})> {2}: {3} (@{4}:{5})", im.GroupIM ? "GroupIM" : "IM", im.Dialog,
                                        im.FromAgentName, im.Message, im.RegionID, im.Position));

                if (im.Dialog == InstantMessageDialog.RequestTeleport)
                {
                    if (im.RegionID != UUID.Zero)
                    {
                        DisplayNotificationInChat("TP to Lure from " + im.FromAgentName);
                        SimRegion R = SimRegion.GetRegion(im.RegionID, gridClient);
                        if (R != null)
                        {
                            Self.Teleport(R.RegionHandle, im.Position);
                            return;
                        }
                    }
                    DisplayNotificationInChat("Accepting TP Lure from " + im.FromAgentName);
                    Self.TeleportLureRespond(im.FromAgentID, im.IMSessionID, true);
                }
                else if (im.Dialog == InstantMessageDialog.FriendshipOffered)
                {
                    DisplayNotificationInChat("Accepting Friendship from " + im.FromAgentName);
                    Friends.AcceptFriendship(im.FromAgentID, im.IMSessionID);
                }
                else if (im.Dialog == InstantMessageDialog.MessageFromAgent ||
                    im.Dialog == InstantMessageDialog.MessageFromObject)
                {
                    //   ClientManager.DoCommandAll(im.Message, im.FromAgentID, WriteLine);
                }
                return;
            }
            {
                // Received an IM from someone that is not the bot's master, ignore
                DisplayNotificationInChat(String.Format("UNTRUSTED <{0} ({1})> {2} (not master): {3} (@{4}:{5})", im.GroupIM ? "GroupIM" : "IM",
                                        im.Dialog, im.FromAgentName, im.Message,
                                        im.RegionID, im.Position));
                return;
            }
        }

        public void DisplayNotificationInChat(string str)
        {
            InvokeGUI(
                () =>
                    {
                        WriteLine(str);
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
                Settings.USE_LLSD_LOGIN = true;
                new Thread(() =>
                {
                    Thread.Sleep(10000);
                    Login();
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

            UUID folderID;
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

        public void WriteLine(string str)
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
                if (false) ClientManager.SetDebugConsole(__TheRadegastInstance);
                ClientManager.WriteLine(str);
            }
            catch (Exception ex)
            {
                Logger.Log(GetName() + " exeption " + ex, Helpers.LogLevel.Error, ex);
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
                Logger.Log(GetName() + " exeption " + ex, Helpers.LogLevel.Error, ex);
            }            
        }
        public void WriteLine(string str, params object[] args)
        {
            try
            {
                if (str == null) return;
                if (args != null && args.Length > 0) str = String.Format(str, args);
                WriteLine(str);
            }
            catch (Exception ex)
            {
                Logger.Log(GetName() + " exeption " + ex, Helpers.LogLevel.Error, ex);
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
                string str = "$bot sees the objects:\n";
                for (int i = 0; i < prims.Count; ++i)
                    str += WorldSystem.describePrim(prims[i], detailed) + "\n";
                //str += "and " + WorldSystem.GetSimObject(prims[prims.Count - 1]) + ".";
                WriteLine(str);
            }
            else if (prims.Count == 1)
            {
                WriteLine("$bot sees one object: " + WorldSystem.getObjectName(prims[0]));
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
            }
            //            WriteLine("ClientManager Network_OnLogin : [" + login.ToString() + "] " + message);
            //SendNewEvent("On-Login", login, message);

            if (login == LoginStatus.Failed)
            {
                ExpectConnected = false;
                SendNetworkEvent("On-Login-Fail", login, message);
                WriteLine("Login Failed " + message + " LoginRetries: " + LoginRetries);
                if (LoginRetries <= 0) return;
                LoginRetries--;               
                Login();
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
            }
            else
            {
                SendNetworkEvent("On-Login", login, message);
            }

        }

        readonly List<Assembly> KnownAssembies = new List<Assembly>();

        public void LoadAssembly(Assembly assembly)
        {
            lock (KnownAssembies)
            {
                if (KnownAssembies.Contains(assembly))
                    return;
                KnownAssembies.Add(assembly);
            }
            ClientManager.RegisterAssembly(assembly);
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
                                           Listener command = (Listener)info.Invoke(new object[] { this });
                                           RegisterListener(command);
                                       });

                        }
                        catch (Exception e)
                        {
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
        }

        /// <summary>
        /// Initialize everything that needs to be initialized once we're logged in.
        /// </summary>
        /// <param name="login">The status of the login</param>
        /// <param name="message">Error message on failure, MOTD on success.</param>
        private void RegisterCommand(string name, cogbot.Actions.Command command)
        {
            string orginalName = name;
            name = name.Replace(" ", "").ToLower();
            while (name.EndsWith(".")) name = name.Substring(0, name.Length - 1);
            Monitor.Enter(Commands);
            if (!Commands.ContainsKey(name))
            {
                Commands.Add(name, command);
                command.Name = orginalName;
                command.TheBotClient = this;
            }
            else
            {
                RegisterCommand("!" + orginalName, command);
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
                updateTimer.Enabled = false;
                updateTimer.Close();
                //botPipeline.Shut
                botPipeline.Dispose();
                lispEventProducer.Dispose();
                WorldSystem.Dispose();
                //thrJobQueue.Abort();
                //lock (lBotMsgSubscribers)
                //{   
                LispTaskInterperter.Dispose();
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
            InvokeJoin("ExecuteCommand " + text);
            return ExecuteCommand(text, WriteLine); 
        }

        private bool InvokeJoin(string s)
        {
            return InvokeJoin(s, -1);
        }
        private bool InvokeJoin(string s, int millisecondsTimeout)
        {
            return OneAtATimeQueue.InvokeJoin(s, millisecondsTimeout);
        }

        private void InvokeNext(string s, ThreadStart e)
        {
            OneAtATimeQueue.Enqueue(() =>
                                        {
                                            e();
                                        });
        }

        public CmdResult ExecuteCommand(string text, OutputDelegate WriteLine)
        {
            if (string.IsNullOrEmpty(text)) return null;
            text = text.Trim();
            while (text.StartsWith("/"))
            {
                text = text.Substring(1).TrimStart();
            }
            if (string.IsNullOrEmpty(text)) return null;
            CmdResult res = ExecuteBotCommand(text, WriteLine);
            if (res != null) return res;
            res = ClientManager.ExecuteSystemCommand(text, WriteLine);
            if (res != null) return res;
            WriteLine("I don't understand the ExecuteCommand " + text + ".");
            return null;
        }


        public CmdResult ExecuteBotCommand(string text, OutputDelegate WriteLine)
        {
            InvokeJoin("ExecuteBotCommand " + text);
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
                    return new CmdResult(evalLispString(text).ToString(), true);
                }
                //            Settings.LOG_LEVEL = Helpers.LogLevel.Debug;
                //text = text.Replace("\"", "");
                string verb = Parser.ParseArgs(text)[0];
                verb = verb.ToLower();
                if (Commands != null && Commands.ContainsKey(verb))
                {
                    Command act = Commands[verb];
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
                            return DoCmdAct(act, verb, text.Substring(verb.Length + 1), WriteLine);
                        else
                            return DoCmdAct(act, verb, "", WriteLine);
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
                    if (assetID != UUID.Zero) return ExecuteBotCommand("gesture " + assetID,WriteLine);
                    assetID = WorldSystem.SimAssetSystem.GetAssetUUID(text, AssetType.Animation);
                    if (assetID != UUID.Zero) return ExecuteBotCommand("anim " + assetID, WriteLine);
                    return null;
                }
            }
            catch (Exception e)
            {
                LogException("ExecuteBotCommand " + text, e);
                return null;
            }
        }

        private CmdResult DoCmdAct(Command command, string verb, string args, OutputDelegate del)
        {
            InvokeJoin("ExecuteBotCommand " + verb + " " + args);
            return command.acceptInputWrapper(verb, args, del);
        }

        public string GetName()
        {
            string n = Self.Name;
            if (n != null && !String.IsNullOrEmpty(n.Trim())) return n;
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
                ExecuteCommand(evt.GetArgs()[0].ToString(), WriteLine);
            }
        }

        void SimEventSubscriber.Dispose()
        {
            ((BotClient)this).Dispose();
        }


        public void TalkExact(string str)
        {
            if(!TalkingAllowed)
            {
                WriteLine("!!NOTE!! skipping saying " + str);
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
            Self.Chat(str, 0, ChatType.Normal);
        }

        public void Intern(string n, object v)
        {
            LispTaskInterperter.Intern(n, v);
        }


        public void InternType(Type t)
        {
//          LispTaskInterperter.InternType(t);
            ScriptManager.AddType(t);
        }

        private void RegisterListener(Listener listener)
        {
            // listeners[listener.GetModuleName()] = listener;

            OneAtATimeQueue.Enqueue(() =>
                                        {
                                            string mname = listener.GetModuleName();
                                            try
                                            {
                                                WriteLine("LISTENER STARTUP: " + mname);
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

        internal object GetAvatar()
        {
            if (gridClient.Self.AgentID != UUID.Zero) return WorldSystem.TheSimAvatar;
            return this;
        }

        public void FakeEvent(Object target, String infoName, params object[] parameters)
        {

            Type type = target.GetType();
            EventInfo eventInfo = type.GetEvent(infoName);
            MethodInfo m = eventInfo.GetRaiseMethod();

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
                    FieldInfo fieldInfo = type.GetField(eventInfo.Name,
                                                        BindingFlags.Instance | BindingFlags.NonPublic |
                                                        BindingFlags.Public);
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
            if (lastException != null) throw lastException;
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

            ClientManager.EnsureRadegastForm(this,TheRadegastInstance, "EnsureRadegastForm1 " + GetName());

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
            TheRadegastInstance.Netcom.Grid = G;
            to.Grid = G;
            to.StartLocation = StartLocationType.Custom;
            to.StartLocationCustom = BotLoginParams.Start;            
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
                if (form.InvokeRequired)
                {
                    form.BeginInvoke(new MethodInvoker(() => SetRadegastLoginForm(form, to)));
                }
                else
                {
                    SetRadegastLoginForm(form, to);
                }
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
            console.txtUsername.Text = (string.Format("{0} {1}", options.FirstName, options.LastName)).Trim();

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
                    TheRadegastInstance.Netcom.Grid = G;
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
                if (mf.IsHandleCreated)
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

        public BotPermissions GetSecurityLevel(UUID uuid)
        {
            BotPermissions bp;
            if (SecurityLevels.TryGetValue(uuid, out bp))
            {
                return bp;
            }
            return BotPermissions.Base;
        }

        public CmdResult ExecuteTask(string scripttype, TextReader reader, OutputDelegate WriteLine)
        {
            var si = ScriptManager.LoadScriptInterpreter(scripttype, this);
            object o = si.Read(scripttype, reader, WriteLine);
            if (o is CmdResult) return (CmdResult)o;
            if (o == null) return new CmdResult("void", true);
            if (si.Eof(o)) return new CmdResult("EOF " + o, true);
            return new CmdResult("" + si.Eval(o), true);
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


        public CmdResult ExecuteXmlCommand(string cmd, OutputDelegate line)
        {
            return XmlInterp.ExecuteXmlCommand(cmd, line);
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
                                TalkExact(toSay);
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
                    TalkExact(toSay);
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
            SimAvatarImpl av = WorldSystem.TheSimAvatar as SimAvatarImpl;
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
                TalkExact(text.Substring(0, len));
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
                TalkExact(text);
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
            if (TheRadegastInstance==null)
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
    }

    public delegate void InstantMessageSentArgs(object sender, IMessageSentEventArgs args);
    public class IMessageSentEventArgs : EventArgs
    {
        private string message;
        private UUID targetID;
        private UUID sessionID;
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
}