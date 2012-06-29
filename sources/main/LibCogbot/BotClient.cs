/******************************************************************************************
  Cogbot -- Copyright (c) 2008-2012, Douglas Miles, Kino Coursey, Daxtron Labs, Logicmoo
      and the Cogbot Development Team.
   
  Major contributions from (and special thanks to):
      Latif Kalif, Anne Ogborn and Openmeteverse Foundation

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**************************************************************************************************/
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Reflection;
using System.Windows.Forms;
using System.Xml;
using Cogbot.Actions.Appearance;
using Cogbot.Actions.Communication;
using Cogbot.Actions.Land;
using Cogbot.Actions.Movement;
using Cogbot.Actions.Scripting;
using Cogbot.Actions.System;
using Cogbot.Actions.WebUtil;
using Cogbot.Library;
using Cogbot.Utilities;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Packets;
using OpenMetaverse.Utilities;
using Cogbot.Actions;
using System.Threading;
using System.Collections;
using Cogbot.ScriptEngines;
using System.IO;
using Cogbot.World;
using System.Drawing;
using Settings=OpenMetaverse.Settings;
using Cogbot.Actions.Agent;
using System.Text;
using Type=System.Type;
#if USE_SAFETHREADS
using Thread = MushDLR223.Utilities.SafeThread;
#endif
//using RadegastTab = Radegast.SleekTab;

// older LibOMV
//using TeleportFlags = OpenMetaverse.AgentManager.TeleportFlags;
//using TeleportStatus = OpenMetaverse.AgentManager.TeleportStatus;

namespace Cogbot
{
    public partial class BotClient : IDisposable
    {

        public static implicit operator GridClient(BotClient m)
        {
            return m.gridClient;
        }

        public object GridClientNullLock = new object();
        /// <summary>Networking subsystem</summary>
        public NetworkManager Network { get { return gridClient.Network; } }

        /// <summary>Settings class including constant values and changeable
        /// parameters for everything</summary>
        public Settings Settings /* = new Settings(new GridClient());//*/ { get { return gridClient.Settings; } }
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


        static void AddTodo(Delegate[] lst, List<Delegate> todo)
        {
            if (lst == null || lst.Length == 0) return;
            foreach (var del in lst)
            {
                if (del is MulticastDelegate)
                {
                    var mc = (MulticastDelegate) del;
                    Delegate[] innerds = mc.GetInvocationList();
                    MethodInfo fo = mc.Method;
                    if (innerds == null || innerds.Length == 0 ||
                        (innerds.Length == 1 && innerds[0] == mc) || (fo != null && fo.Name == "Invoke"))
                    {
                        todo.Add(mc);
                        continue;
                    }
                    AddTodo(innerds, todo);
                }
                else
                {
                    todo.Add(del);
                }
            }
        }


        readonly public TaskQueueHandler OneAtATimeQueue;
        public GridClient gridClient
        {
            get
            {
                lock (GridClientNullLock)
                {
                    GridClientAccessed = true;
                    return _gridClient;
                }
            }
        }
        private bool GridClientAccessed;
        private GridClient _gridClient;
     
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


        public bool IsRegionMaster
        {
            get { return WorldSystem.IsRegionMaster; }
        }

        readonly List<ThreadStart> OnRadegastSet = new List<ThreadStart>();
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
                var vc = value.Client;
                if (gridClient != vc)
                {
                    WriteLine("wierd gridclients");
                }
                value.Netcom.InstantMessageSent += IMSent;
                foreach (ThreadStart start in LockInfo.CopyOf(OnRadegastSet))
                {
                    InvokeGUI(start);
                }

                if (false) CogbotGUI.SetDebugConsole(value);
            }
        }

        public event InstantMessageSentArgs OnInstantMessageSent;

        private void IMSent(object sender, Radegast.Netcom.InstantMessageSentEventArgs e)
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

        readonly public VoiceManager VoiceManager;
        // Shell-like inventory commands need to be aware of the 'current' inventory folder.
        public InventoryFolder CurrentDirectory = null;

        private Quaternion bodyRotation = Quaternion.Identity;
        private Vector3 forward = new Vector3(0, 0.9999f, 0);
        private Vector3 left = new Vector3(0.9999f, 0, 0);
        private Vector3 up = new Vector3(0, 0, 0.9999f);
        readonly private System.Timers.Timer updateTimer;

        public Cogbot.WorldObjects WorldSystem;
        static public BotClient SingleInstance
        {
            get { return ClientManager.SingleInstance.LastBotClient; }
        }
        public static int debugLevel = 2;
        public bool GetTextures = ClientManager.DownloadTextures;
        //  public Cogbot.ClientManager ClientManager;
        //  public VoiceManager VoiceManager;
        // Shell-like inventory commands need to be aware of the 'current' inventory folder.

        //  public GridClient this = null;
        //  public OutputDelegate outputDelegate;
        ///public DotCYC.CycConnectionForm cycConnection;
        public Dictionary<string, DescribeDelegate> describers;

        readonly public Dictionary<string, Cogbot.Listener> Plugins;
        public SortedDictionary<string, CommandInfo> Commands;
        public Dictionary<string, Tutorials.Tutorial> tutorials;
        //public Utilities.BotTcpServer UtilitiesTcpServer;

        public bool describeNext;
        private int describePos;
        private string currTutorial;

        public int RunningMode = (int)Modes.normal;
        public UUID AnimationFolder = UUID.Zero;

        public BotInventoryEval BotInventory = null; // new InventoryEval(this);

        public String taskInterperterType = "DotLispInterpreter";// DotLispInterpreter,CycInterpreter or ABCLInterpreter

        [ConfigSetting(Description = "If log messages are to be treated as Client Events or not")]
        static public bool SendLogMessages = false;

        ScriptEventListener scriptEventListener = null;
        readonly public ClientManager ClientManager;

        private UUID GroupMembersRequestID = UUID.Zero;
        public Dictionary<UUID, Group> GroupsCache = null;
        private ManualResetEvent GroupsEvent = new ManualResetEvent(false);

        /// <summary>
        /// 
        /// </summary>
        public BotClient(ClientManager manager, GridClient g)
        {
            OpenMetaverse.Utils.InternStrings = true;
            LoginRetries = LoginRetriesFresh;
            ClientManager = manager;
            _gridClient = g;
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
            Settings.RESEND_TIMEOUT = 20 * 1000;
            Settings.MAX_RESEND_COUNT = 10;
            Settings.LOGIN_TIMEOUT = 120 * 1000;
            //Settings.LOGOUT_TIMEOUT = 120 * 1000;
            //Settings.SIMULATOR_TIMEOUT = int.MaxValue;
            //Settings.SEND_PINGS = true;
            Settings.SEND_AGENT_APPEARANCE = true;
            Settings.LOG_DISKCACHE = false;
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
            Settings.CACHE_PRIMITIVES = true;
            Settings.POOL_PARCEL_DATA = true;


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

            Plugins = new Dictionary<string, Cogbot.Listener>();
            //registrationTypes["avatars"] = new Cogbot.Avatars(this);
            //registrationTypes["chat"] = new Cogbot.Chat(this);
            WorldSystem = new Cogbot.WorldObjects(this);
            //registrationTypes["teleport"] = new Cogbot.Teleport(this);
            //registrationTypes["whisper"] = new Cogbot.Whisper(this);
            //ObjectSystem = new Cogbot.Objects(this);
            //registrationTypes["bump"] = new Cogbot.Bump(this);
            //registrationTypes["sound"] = new Cogbot.Sound(this);
            //registrationTypes["sound"] = new Cogbot.Objects(this);
            var gc = gridClient;
            //_gridClient = null;
            Commands = new SortedDictionary<string, CommandInfo>();
			RegisterCommand("login", new Login(this));
			RegisterCommand("logout", new Logout(this));
			RegisterCommand("stop", new StopCommand(this));
			RegisterCommand("teleport", new Teleport(this));
			var desc = newCommandInfo(new Describe(this));
			Commands["describe"] = desc;
			Commands["look"] = desc;
            RegisterCommand("say", new Cogbot.Actions.Communication.SayCommand(this));
			RegisterCommand("help", new Cogbot.Actions.System.Help(this));
            RegisterCommand("setmaster", new Cogbot.Actions.System.SetMasterKeyCommand(this));
            RegisterCommand("setmasterkey", new Cogbot.Actions.System.SetMasterKeyCommand(this));
			RegisterCommand("sit", new Sit(this));
			RegisterCommand("stand", new StandCommand(this));
			RegisterCommand("jump", new JumpCommand(this));
			RegisterCommand("crouch", new CrouchCommand(this));
			RegisterCommand("mute", new MuteCommand(this));
			RegisterCommand("unmute", new UnmuteCommand(this));
			RegisterCommand("move", new Move(this));
			RegisterCommand("use", new Use(this));
			RegisterCommand("eval", new Eval(this));
			RegisterCommand("wear", new ReplaceOutfitCommand(this));

            Commands["locate"] = Commands["location"] = Commands["where"] = newCommandInfo(new Actions.Movement.LocationCommand(this));
            var follow = newCommandInfo(new Follow(this));
            Commands["follow"] = follow;
            //Commands["simexport"] = new Cogbot.Actions.SimExport.ExportCommand(this);
            Commands["stop following"] = follow;
            Commands["stop-following"] = follow;

            tutorials = new Dictionary<string, Cogbot.Tutorials.Tutorial>();
            tutorials["tutorial1"] = new Tutorials.Tutorial1(manager, this);

            // ensure all commands are registered when this constructor completes
            foreach (Type type in GetType().Assembly.GetTypes())
            {
                RegisterType(type);
            }
            
            _gridClient = gc;
            describeNext = true;

            XmlInterp = new XmlScriptInterpreter(this);
            XmlInterp.BotClient = this;
            if (false)
                ClientManager.PostAutoExecEnqueue(LoadTaskInterpreter);

            // Start the server
            lock (ClientManager.config)
            {
                int poff = ClientManager.config.GetValue("tcpPortOffset", 10);
                thisTcpPort = ClientManager.nextTcpPort;
                ClientManager.nextTcpPort += poff;
                ClientManager.PostAutoExecEnqueue(() =>
                {
                    Utilities.BotTcpServer UtilitiesTcpServer = new Utilities.BotTcpServer(thisTcpPort, this);
                    UtilitiesTcpServer.ServerPortIncr = poff;
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
            BotInventory = new BotInventoryEval(this);
            ClientManager.PostAutoExecEnqueue(() =>
            {
                if (useLispEventProducer)
                {
                    lispEventProducer = new LispEventProducer(this, LispTaskInterperter);
                }
            });

        }

        //breaks up large responses to deal with the max IM size


        private void updateTimer_Elapsed(object sender, System.Timers.ElapsedEventArgs e)
        {
            List<Command> actions = new List<Command>();
            lock (Commands)
            {
                foreach (var cmd in Commands.Values)
                {
                    if (cmd.IsStateFul)
                    {
                        actions.Add(cmd.WithBotClient);
                    }
                }
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
                //if (!Cogbot.Actions.SimExport.ExportCommand.IsExporting) describeSituation(WriteLine);
            }
        }


        public void DisplayNotificationInChat(string str)
        {
            DisplayNotificationInChatReal(str);
        }
        public void DisplayNotificationInChat(string str, params object[] args)
        {
            DisplayNotificationInChatReal(DLRConsole.SafeFormat(str, args));
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
            if (!SendLogMessages) return;
            SendNetworkEvent("On-Log-Message", message, level);
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
                BotInventory.evalOnFolders(rootFolder, usage, itemName);
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
            BotInventory.evalOnFolders(rootFolder, "print", "");
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
            BotInventory.evalOnFolders(rootFolder, "print", "");

        }


        public UUID findInventoryItem(string name)
        {
            InventoryFolder rootFolder = Inventory.Store.RootFolder;  //  .Inventory.InventorySkeleton.Folders;// .RootUUID;
            //InventoryEval searcher = new InventoryEval(this);

            return BotInventory.findInFolders(rootFolder, name);

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
                CogbotGUI.SetDebugConsole(__TheRadegastInstance);
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
                if (false) CogbotGUI.SetDebugConsole(__TheRadegastInstance);
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
                foreach (var ms in LockInfo.CopyOf(Plugins).Values)
                {
                    ms.Dispose();
                }
                ClientManager.Remove(this);
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
            if (str.Length > 800)
            {
                int indexOF = str.Substring(750).IndexOfAny(new[] {' ', '<', '\t', '.'});
                if (indexOF < 0) indexOF = 0;
                if (indexOF > 60) indexOF = 0;
                indexOF++;
                Self.Chat(str.Substring(0, indexOF + 750), channel, type);
                TalkExact(str.Substring(indexOF + 750), channel, type);
                return;
            }
            Self.Chat(str, channel, type);
        }


        private void RegisterListener(Listener listener)
        {
            // Cogbot[listener.GetModuleName()] = listener;

            string mname = listener.GetModuleName();
            string taskName = "LISTENER STARTUP: " + mname;
            OneAtATimeQueue.NamedTask(taskName, () =>
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
                                        })();
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


        public void InvokeThread(String name, ThreadStart action)
        {
            ThreadPool.QueueUserWorkItem((o) => NamedAction(name, action));
        }

        private void NamedAction(string name, ThreadStart action)
        {
            Thread ct = Thread.CurrentThread;
            try
            {
                try
                {
                    AddThread(ct);
                    ct.Name = name;
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
                    RemoveThread(ct);
                }
                catch (OutOfMemoryException)
                {
                }
                catch (StackOverflowException)
                {
                }
                catch (Exception)
                {
                }
            }
        }


        public void InvokeGUI(ThreadStart o)
        {
            if (TheRadegastInstance == null)
            {
                OnRadegastSet.Add(o);
                return;
            }
            CogbotGUI.InvokeGUI(TheRadegastInstance.MainForm, o);
        }
        public void InvokeGUI(bool noRadegastRequired, ThreadStart o)
        {
            if (noRadegastRequired && TheRadegastInstance == null)
            {
                o();
                return;
            }
            InvokeGUI(o);
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

            BotClient TheBot
        {
            get { return this; }
        }
        String stringBuff = "";
        readonly object stringBuffLock = new object();
        public void DisplayNotificationInChatReal(string str)
        {
            var instance = TheRadegastInstance;
            lock (stringBuffLock)
            {
                if (stringBuff == "")
                {
                    stringBuff = str;
                }
                else
                {
                    stringBuff = stringBuff + "\r\n" + str;
                    return;
                }
            }
            if (instance == null)
            {
                lock (stringBuffLock)
                {
                    str = stringBuff;
                    stringBuff = "";
                }
                ClientManager.WriteLine(str);
                return;
            }
            TheBot.InvokeGUI(
                () =>
                {
                    var cc = (Radegast.ChatConsole)instance.TabConsole.Tabs["chat"].Control;
                    var tp = (Radegast.RichTextBoxPrinter)cc.ChatManager.TextPrinter;
                    CogbotGUI.InvokeGUI(cc.rtbChat, () =>
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
                        instance.TabConsole.DisplayNotificationInChat(str);
                    });

                });
        }
        public void ShowTab(string name)
        {
            TheBot.InvokeGUI(() => TheRadegastInstance.TabConsole.GetTab(name.ToLower()).Select());
        }

        public void AddTab(string name, string label, UserControl _debugWindow, EventHandler CloseDebug)
        {
            TheBot.InvokeGUI(() =>
            {
                Radegast.RadegastTab tab = TheRadegastInstance.TabConsole.AddTab(name.ToLower(), label, _debugWindow);
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
}