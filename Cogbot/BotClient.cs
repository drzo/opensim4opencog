using System;
using System.Collections.Generic;
using System.Reflection;
using System.Xml;
using OpenMetaverse;
using OpenMetaverse.Packets;
using OpenMetaverse.Utilities;
using cogbot.Actions;
using System.Threading;
using System.Collections;
using cogbot.ScriptEngines;
using System.IO;
using cogbot.Listeners;
using Action=cogbot.Actions.Action;
using cogbot.TheOpenSims;

// older LibOMV
//using TeleportFlags = OpenMetaverse.AgentManager.TeleportFlags;
//using TeleportStatus = OpenMetaverse.AgentManager.TeleportStatus;

namespace cogbot
{
    public class BotClient : SimEventSubscriber
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
        public GridManager Grid { get { return gridClient.Grid; } }
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


        // Reflect events into lisp
        //        
        int LoginRetries = 2; // for the times we are "already logged in"
        public void Login()
        {
            //if (TextForm.simulator.periscopeClient == null)
            //{
            //    TextForm.simulator.periscopeClient = this;
            //    TextForm.simulator.Start();
            //    Settings.LOG_LEVEL = Helpers.LogLevel.Info;
            //}
            try
            {
                Network.Login(BotLoginParams.FirstName, BotLoginParams.LastName,
                              BotLoginParams.Password, "OnRez", BotLoginParams.Start, "UNR");
            } catch(Exception e)
            {
                
            }
        }

        readonly int thisTcpPort;
        public OpenMetaverse.LoginParams BotLoginParams;// = null;
        readonly SimEventPublisher botPipeline = new SimEventMulticastPipeline();
        public List<Thread> botCommandThreads = new ListAsSet<Thread>();
        public UUID GroupID = UUID.Zero;
        public Dictionary<UUID, GroupMember> GroupMembers = null; // intialized from a callback
        public Dictionary<UUID, AvatarAppearancePacket> Appearances = new Dictionary<UUID, AvatarAppearancePacket>();
        ///public Dictionary<string, Command> Commands = new Dictionary<string, Command>();
        public bool Running = true;
        public bool GroupCommands = true;
        public string MasterName = String.Empty;
        public UUID MasterKey = UUID.Zero;
        public bool AllowObjectMaster
        {
            get
            {
                return MasterKey != UUID.Zero;
            }   
        }
        public VoiceManager VoiceManager;
        // Shell-like inventory commands need to be aware of the 'current' inventory folder.
        public InventoryFolder CurrentDirectory = null;

        private Quaternion bodyRotation = Quaternion.Identity;
        private Vector3 forward = new Vector3(0, 0.9999f, 0);
        private Vector3 left = new Vector3(0.9999f, 0, 0);
        private Vector3 up = new Vector3(0, 0, 0.9999f);
        readonly private System.Timers.Timer updateTimer;

        public Listeners.WorldObjects WorldSystem;
        //   static public TextForm SingleInstance = null;
        public static int debugLevel = 2;
        public bool GetTextures = TextForm.DownloadTextures;

        //  public cogbot.TextForm ClientManager;
        //  public VoiceManager VoiceManager;
        // Shell-like inventory commands need to be aware of the 'current' inventory folder.

        //  public GridClient this = null;
        //  public OutputDelegate outputDelegate;
        ///public DotCYC.CycConnectionForm cycConnection;
        public Dictionary<string, DescribeDelegate> describers;

        readonly public Dictionary<string, Listeners.Listener> listeners;
        public SortedDictionary<string, Actions.Action> Commands;
        public Dictionary<string, Tutorials.Tutorial> tutorials;
        //public Utilities.TcpServer UtilitiesTcpServer;

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
        public TextForm ClientManager;

        public List<string> muteList;
        public bool muted = false;

        /// <summary>
        /// 
        /// </summary>
        public BotClient(TextForm manager)
        {
            ClientManager = manager;
            manager.lastBotClient = this;


            updateTimer = new System.Timers.Timer(500);
            updateTimer.Elapsed += new System.Timers.ElapsedEventHandler(updateTimer_Elapsed);

            //            manager.AddTextFormCommands(this);
            //          RegisterAllCommands(Assembly.GetExecutingAssembly());

            Settings.USE_INTERPOLATION_TIMER = false;
            Settings.LOG_LEVEL = Helpers.LogLevel.Info;

            gridClient = new GridClient();
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
            Settings.LOGOUT_TIMEOUT = 16 * 1000;
            Settings.SIMULATOR_TIMEOUT = int.MaxValue;
            Settings.SEND_PINGS = false;
            //Settings.USE_LLSD_LOGIN = true;
            ////Settings.MULTIPLE_SIMS = false;

            VoiceManager = new VoiceManager(gridClient);
            //manager.AddBotClientToTextForm(this);
            SetDefaultLoginDetails(TextForm.SingleInstance.config);

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

            Commands = new SortedDictionary<string, cogbot.Actions.Action>();
            Commands["login"] = new Actions.Login(this);
            Commands["logout"] = new Actions.Logout(this);
            Commands["stop"] = new Actions.Stop(this);
            Commands["teleport"] = new Actions.Teleport(this);
            Action desc = new Actions.Describe(this);
            Commands["describe"] = desc;
            Commands["look"] = desc;
            Commands["say"] = new Actions.Say(this);
            Commands["whisper"] = new Actions.Whisper(this);
            Commands["help"] = new Actions.Help(this);
            Commands["sit"] = new Actions.Sit(this);
            Commands["stand"] = new Actions.Stand(this);
            Commands["jump"] = new Actions.Jump(this);
            Commands["crouch"] = new Actions.Crouch(this);
            Commands["mute"] = new Actions.Mute(this);
            Commands["move"] = new Actions.Move(this);
            Commands["use"] = new Actions.Use(this);
            Commands["eval"] = new Actions.Eval(this);

            Commands["fly"] = new Actions.Fly(this);
            Commands["stop-flying"] = new Actions.StopFlying(this);
            Commands["where"] = new Actions.Where(this);
            Commands["locate"] = new Actions.Locate(this);
            Actions.Follow follow = new Actions.Follow(this);
            Commands["follow"] = follow;
            Commands["wear"] = new Actions.Wear(this);
            Commands["stop following"] = follow;
            Commands["stop-following"] = follow;

            tutorials = new Dictionary<string, cogbot.Tutorials.Tutorial>();
            tutorials["tutorial1"] = new Tutorials.Tutorial1(manager, this);


            describeNext = true;


            // Start the server
            lock (TextForm.SingleInstance.config)
            {
                thisTcpPort = TextForm.nextTcpPort;
                TextForm.nextTcpPort += TextForm.SingleInstance.config.tcpPortOffset;
                Utilities.TcpServer UtilitiesTcpServer = new Utilities.TcpServer(thisTcpPort, this);
                UtilitiesTcpServer.startSocketListener();
            }

            Network.RegisterCallback(PacketType.AgentDataUpdate, new NetworkManager.PacketCallback(AgentDataUpdateHandler));
            Network.RegisterCallback(PacketType.AlertMessage, new NetworkManager.PacketCallback(AlertMessageHandler));
            Network.RegisterCallback(PacketType.AvatarAppearance, new NetworkManager.PacketCallback(AvatarAppearanceHandler));

           //Move to effects Appearance.OnAppearanceUpdated += new AppearanceManager.AppearanceUpdatedCallback(Appearance_OnAppearanceUpdated);

            Inventory.OnObjectOffered += new InventoryManager.ObjectOfferedCallback(Inventory_OnInventoryObjectReceived);
            Groups.OnGroupMembers += new GroupManager.GroupMembersCallback(GroupMembersHandler);
            Logger.OnLogMessage += new Logger.LogCallback(client_OnLogMessage);
            Network.OnEventQueueRunning += new NetworkManager.EventQueueRunningCallback(Network_OnEventQueueRunning);
            Network.OnLogin += new NetworkManager.LoginCallback(Network_OnLogin);
            Network.OnLogoutReply += new NetworkManager.LogoutCallback(Network_OnLogoutReply);
            Network.OnSimConnected += new NetworkManager.SimConnectedCallback(Network_OnSimConnected);
            Network.OnSimDisconnected += new NetworkManager.SimDisconnectedCallback(Network_OnSimDisconnected);
            Network.OnConnected += new NetworkManager.ConnectedCallback(Network_OnConnected);
            Network.OnDisconnected += new NetworkManager.DisconnectedCallback(Network_OnDisconnected);
            Self.OnInstantMessage += new AgentManager.InstantMessageCallback(Self_OnInstantMessage);
            //Self.OnScriptDialog += new AgentManager.ScriptDialogCallback(Self_OnScriptDialog);
            //Self.OnScriptQuestion += new AgentManager.ScriptQuestionCallback(Self_OnScriptQuestion);
            Self.OnTeleport += new AgentManager.TeleportCallback(Self_OnTeleport);
            Self.OnChat += new AgentManager.ChatCallback(Self_OnChat);

            updateTimer.Start();
            searcher = new BotInventoryEval(this);
            initTaskInterperter();
            lispEventProducer = new LispEventProducer(this,lispTaskInterperter);
        }

        private LispEventProducer lispEventProducer;

        public void StartupClientLisp()
        {
            initTaskInterperter();
            if (TextForm.SingleInstance.config.startupClientLisp.Length > 1)
            {
                try
                {
                    evalLispString("(progn " + TextForm.SingleInstance.config.startupClientLisp + ")");
                }
                catch (Exception e)
                {
                    Console.WriteLine("" + e);
                }
            }
        }

        private void SetDefaultLoginDetails(Configuration configuration)
        {

            BotLoginParams.FirstName = configuration.firstName;
            BotLoginParams.LastName = configuration.lastName;
            BotLoginParams.Password = configuration.password;
            BotLoginParams.URI = configuration.simURL;
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
            foreach (Action c in Commands.Values)
                if (c.Active)
                    c.Think();
        }

        private void AgentDataUpdateHandler(Packet packet, Simulator sim)
        {
            AgentDataUpdatePacket p = (AgentDataUpdatePacket)packet;
            if (p.AgentData.AgentID == sim.Client.Self.AgentID)
            {
                WriteLine(String.Format("Got the group ID for {0}, requesting group members...", sim.Client));
                GroupID = p.AgentData.ActiveGroupID;

                sim.Client.Groups.RequestGroupMembers(GroupID);
            }
        }

        private void GroupMembersHandler(UUID requestID, UUID groupID, int totalCount, Dictionary<UUID, GroupMember> members)
        {
            WriteLine(String.Format("Got {0} group members out of {1}.", members.Count, totalCount));
            GroupMembers = members;
        }

        private void AvatarAppearanceHandler(Packet packet, Simulator simulator)
        {
            AvatarAppearancePacket appearance = (AvatarAppearancePacket)packet;

            lock (Appearances) Appearances[appearance.Sender.ID] = appearance;
        }

        private void AlertMessageHandler(Packet packet, Simulator simulator)
        {
            AlertMessagePacket message = (AlertMessagePacket)packet;
            WriteLine("[AlertMessage] " + Utils.BytesToString(message.AlertData.Message));
        }


        void Self_OnTeleport(string message, TeleportStatus status, TeleportFlags flags)
        {
            if (status == TeleportStatus.Finished || status == TeleportStatus.Failed || status == TeleportStatus.Cancelled)
            {
                WriteLine("Teleport " + status);
                describeSituation(WriteLine);
            }
        }


        void Self_OnChat(string message, ChatAudibleLevel audible, ChatType type, ChatSourceType sourceType, string fromName, UUID id, UUID ownerid, Vector3 position)
        {
            if (message.Length > 0 && sourceType == ChatSourceType.Agent && !muteList.Contains(fromName))
            {
                WriteLine(String.Format("{0} says, \"{1}\".", fromName, message));
            }
        }

        private void Self_OnInstantMessage(InstantMessage im, Simulator simulator)
        {
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
                if (im.FromAgentID == MasterKey || im.FromAgentName == MasterName)
                {
                    string groupName = im.Message;
                    int found = groupName.IndexOf("Group:");
                    if (found > 0) groupName = groupName.Substring(found + 6);
                    found = groupName.IndexOf(":");
                    if (found>0)
                    {
                        groupName = groupName.Substring(0, found).Trim();
                        ExecuteCommand("joingroup " + groupName);
                    }
                 
                }
            }

            bool groupIM = im.GroupIM && GroupMembers != null && GroupMembers.ContainsKey(im.FromAgentID) ? true : false;

            if (im.FromAgentID == MasterKey || (GroupCommands && groupIM) || im.FromAgentName==MasterName)
            {
                // Received an IM from someone that is authenticated
                WriteLine(String.Format("<{0} ({1})> {2}: {3} (@{4}:{5})", im.GroupIM ? "GroupIM" : "IM", im.Dialog,
                                        im.FromAgentName, im.Message, im.RegionID, im.Position));

                if (im.Dialog == InstantMessageDialog.RequestTeleport)
                {
                    WriteLine("Accepting teleport lure.");
                    Self.TeleportLureRespond(im.FromAgentID, true);
                }
                else if (im.Dialog == InstantMessageDialog.FriendshipOffered)
                {
                    WriteLine("Accepting Friendship.");
                    Friends.AcceptFriendship(im.FromAgentID,im.IMSessionID);
                }
                else if (im.Dialog == InstantMessageDialog.MessageFromAgent ||
                    im.Dialog == InstantMessageDialog.MessageFromObject)
                {
                 //   ClientManager.DoCommandAll(im.Message, im.FromAgentID, WriteLine);
                }
            }
            else
            {
                // Received an IM from someone that is not the bot's master, ignore
                WriteLine(String.Format("<{0} ({1})> {2} (not master): {3} (@{4}:{5})", im.GroupIM ? "GroupIM" : "IM",
                                        im.Dialog, im.FromAgentName, im.Message,
                                        im.RegionID, im.Position));
                return;
            }
        }


        private bool Inventory_OnInventoryObjectReceived(InstantMessage offer, AssetType type,
                                                         UUID objectID, bool fromTask)
        {
            if (true) return true; // accept everything
            if (MasterKey != UUID.Zero)
            {
                if (offer.FromAgentID != MasterKey)
                    return false;
            }
            else if (GroupMembers != null && !GroupMembers.ContainsKey(offer.FromAgentID))
            {
                return false;
            }

            return true;
        }

        // EVENT CALLBACK SECTION
        void Network_OnDisconnected(NetworkManager.DisconnectType reason, string message)
        {
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
            catch (Exception e)
            {
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

            }
            catch (Exception e)
            {
            }
        }


        void Network_OnSimDisconnected(Simulator simulator, NetworkManager.DisconnectType reason)
        {
            SendNetworkEvent("On-Sim-Disconnected", this, simulator, reason);
        }

        void client_OnLogMessage(object message, Helpers.LogLevel level)
        {
            string msg = "" + level + " " + message;

            if (msg.Contains("esend")) return;
            if (msg.Contains("resent packet")) return;
            if (msg.Contains("Rate limit")) return;
            if (msg.Contains("ecoded image with unhandled number of compo")) return;
            if (debugLevel < 3 && msg.Contains("Array index is out of range")) return;
            if (debugLevel < 3 && (msg.Contains("nloadi") || msg.Contains("ransfer"))) return;
            if (level == Helpers.LogLevel.Warning || level == Helpers.LogLevel.Debug)
            {
                if (Settings.LOG_LEVEL == Helpers.LogLevel.Info)
                    Console.WriteLine(msg);
                return;
            }
            SendNetworkEvent("On-Log-Message", message, level);
        }

        void Network_OnEventQueueRunning(Simulator simulator)
        {
            SendNetworkEvent("On-Event-Queue-Running", simulator);
        }

        void Network_OnSimConnected(Simulator simulator)
        {
            SendNetworkEvent("On-Simulator-Connected", simulator);
            //            SendNewEvent("on-simulator-connected",simulator);
        }

        bool Network_OnSimConnecting(Simulator simulator)
        {
            SendNetworkEvent("On-Simulator-Connecing", simulator);
            return true;
        }

        void Network_OnLogoutReply(List<UUID> inventoryItems)
        {
            SendNetworkEvent("On-Logout-Reply", inventoryItems);
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

                Appearance.WearOutfit(folderID, false);
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
                str = str.Replace("\r\n", "\n").Replace("\r", "\n").Replace("\n", "\r\n");
                string SelfName = String.Format("{0}", GetName());
                str = str.Replace("$bot", SelfName);
                if (str.StartsWith(SelfName)) str = str.Substring(SelfName.Length + 1);
                ClientManager.WriteLine(SelfName + ": " + str);
            }
            catch (Exception)
            {
            }
            
        }
        public void WriteLine(string str, params object[] args)
        {
            try
            {
                if (str == null) return;
                if (args != null && args.Length > 0) str = String.Format(str, args);
                str = str.Trim();
                if (str == "") return;
                if (str.StartsWith("$bot")) str = str.Substring(4);
                str = str.Replace("\r\n", "\n").Replace("\r", "\n").Replace("\n", "\r\n");
                string SelfName = String.Format("{0}", GetName());
                str = str.Replace("$bot", SelfName);
                if (str.StartsWith(SelfName)) str = str.Substring(SelfName.Length + 1);
                ClientManager.WriteLine(SelfName + ": " + str);
            }
            catch (Exception)
            {
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

        //------------------------------------ 
        // External XML socket server
        //------------------------------------

        //public void msgClient(string serverMessage)
        //{
        //    if (debugLevel>1) {
        //        WriteLine(serverMessage);             
        //    }
        //    lock (lBotMsgSubscribers)
        //    {
        //        foreach (BotMessageSubscriber ms in lBotMsgSubscribers)
        //        {
        //            ms.msgClient(serverMessage);
        //        }
        //    }
        //}

        public void overwrite2Hash(Hashtable hashTable, string key, string value)
        {
            if (hashTable.ContainsKey(key)) hashTable.Remove(key);
            hashTable.Add(key, value);
            //WriteLine("  +Hash :('" + key + "' , " + value + ")");
        }

        public string getWithDefault(Hashtable hashTable, string key, string defaultValue)
        {
            if (hashTable.ContainsKey(key)) return hashTable[key].ToString();
            return defaultValue;
        }

        public string genActReport(string planID, string seqID, string act, string status)
        {
            DateTime dt = DateTime.Now;

            string actReport = "  <pet-signal pet-name='" + GetName()
                               + "' pet-id='" + Self.AgentID.ToString()
                               + "' timestamp='" + dt.ToString()
                               + "' action-plan-id='" + planID
                               + "' sequence='" + seqID
                               + "' name='" + act
                               + "' status='" + status + "'/>";
            WriteLine("actReport:" + actReport);
            return actReport;
        }

        /// <summary>
        /// (thisClient.XML2Lisp2 "http://myserver/myservice/?q=" chatstring) 
        /// </summary>
        /// <param name="URL"></param>
        /// <param name="args"></param>
        /// <returns></returns>
        public string XML2Lisp2(string URL, string args)
        {
            args = args.Replace("\\", "");
            args = args.Replace("\"", "");
            string xcmd = URL + args;
            return XML2Lisp(xcmd);
        } // method: XML2Lisp2


        public string XML2Lisp(string xcmd)
        {
            String lispCodeString = "";

            try
            {
                XmlTextReader reader;
                StringReader stringReader;

                if (xcmd.Contains("http:") || xcmd.Contains(".xml") || xcmd.Contains(".xlsp"))
                {
                    // assuming its a file
                    xcmd = xcmd.Trim();
                    reader = new XmlTextReader(xcmd);
                }
                else
                {
                    // otherwise just use the string
                    stringReader = new System.IO.StringReader(xcmd);
                    reader = new XmlTextReader(stringReader);
                }

                Hashtable[] attributeStack = new Hashtable[64];

                for (int i = 0; i < 64; i++)
                {
                    attributeStack[i] = new Hashtable();
                }
                int depth = 0;

                while (reader.Read())
                {
                    depth = reader.Depth + 1;
                    if (attributeStack[depth] == null)
                    {
                        attributeStack[depth] = new Hashtable();
                    }
                    string tagname = reader.Name;
                    switch (reader.NodeType)
                    {

                        case XmlNodeType.Element:
                            if (reader.HasAttributes)
                            {
                                for (int i = 0; i < reader.AttributeCount; i++)
                                {
                                    reader.MoveToAttribute(i);
                                    string attributeName = reader.Name;
                                    string attributeValue = reader.Value;

                                    overwrite2Hash(attributeStack[depth], attributeName, attributeValue);
                                }
                            }
                            // WriteLine(" X2L Begin(" + depth.ToString() + ") " + attributeStack[depth]["name"].ToString());
                            if (tagname == "op")
                            {
                                lispCodeString += "(" + getWithDefault(attributeStack[depth], "name", " ");
                            }
                            if (tagname == "opq")
                            {
                                lispCodeString += "'(" + getWithDefault(attributeStack[depth], "name", " ");
                            }

                            break;
                        //
                        //you can handle other cases here
                        //

                        case XmlNodeType.Text:
                            //WriteLine(" X2L TEXT(" + depth.ToString() + ") " + reader.Name);

                            // Todo
                            lispCodeString += " " + reader.Value.ToString();
                            break;

                        case XmlNodeType.EndElement:

                            if (tagname == "op")
                            {
                                lispCodeString += " )";
                            }
                            if (tagname == "opq")
                            {
                                lispCodeString += " )";
                            }

                            // Todo
                            //depth--;
                            break;

                        default:
                            break;
                    } //switch
                } //while
                WriteLine("XML2Lisp =>'" + lispCodeString + "'");
                //string results = evalLispString(lispCodeString);
                //string results = "'(enqueued)";
                return evalLispString(lispCodeString);
                //return results;
            } //try
            catch (Exception e)
            {
                WriteLine("error occured: " + e.Message);
                WriteLine("        Stack: " + e.StackTrace.ToString());
                WriteLine("        lispCodeString: " + lispCodeString);
                return "()";
            }


        }


        public ScriptInterpreter lispTaskInterperter;
        readonly private List<Type> registeredTypes = new List<Type>();

        private void initTaskInterperter()
        {
            try

            {
                if (lispTaskInterperter != null)
                {
                    return;
                }
                WriteLine("Start Loading TaskInterperter ... '" + taskInterperterType + "' \n");
                lispTaskInterperter = ScriptEngines.ScriptManager.LoadScriptInterpreter(taskInterperterType);
                lispTaskInterperter.LoadFile("boot.lisp");
                lispTaskInterperter.LoadFile("extra.lisp");
                lispTaskInterperter.LoadFile("cogbot.lisp");
                lispTaskInterperter.Intern("clientManager", ClientManager);
                scriptEventListener = new ScriptEventListener(lispTaskInterperter, this);
                botPipeline.AddSubscriber(scriptEventListener);

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
                Object r = null;
                //lispCode = "(load-assembly \"libsecondlife\")\r\n" + lispCode;                
                StringReader stringCodeReader = new StringReader(lispCode);
                r = lispTaskInterperter.Read("evalLispString", stringCodeReader);
                if (lispTaskInterperter.Eof(r))
                    return r.ToString();
                return lispTaskInterperter.Str(evalLispCode(r));
            }
            catch (Exception e)
            {
                WriteLine("!Exception: " + e.GetBaseException().Message);
                WriteLine("error occured: " + e.Message);
                WriteLine("        Stack: " + e.StackTrace.ToString());
                throw e;
            }
        }

        public Object evalLispCode(Object lispCode)
        {
            try
            {
                if (lispCode == null) return null;
                if (lispTaskInterperter == null)
                {
                    initTaskInterperter();
                }
                WriteLine("Eval> " + lispCode);
                if (lispTaskInterperter.Eof(lispCode))
                    return lispCode.ToString();
                return lispTaskInterperter.Eval(lispCode);
            }
            catch (Exception e)
            {
                WriteLine("!Exception: " + e.GetBaseException().Message);
                WriteLine("error occured: " + e.Message);
                WriteLine("        Stack: " + e.StackTrace.ToString());
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
        public void Network_OnLogin(LoginStatus login, string message)
        {
            if (login == LoginStatus.Success)
            {
                // Start in the inventory root folder.
                CurrentDirectory = Inventory.Store.RootFolder;//.RootFolder;
            }
            //            WriteLine("TextForm Network_OnLogin : [" + login.ToString() + "] " + message);
            //SendNewEvent("On-Login", login, message);

            if (login == LoginStatus.Failed)
            {
                WriteLine("Not able to login");
                // SendNewEvent("on-login-fail",login,message);
                SendNetworkEvent("On-Login-Fail", login, message);
                if (LoginRetries-- >= 0) Login();
            }
            else if (login == LoginStatus.Success)
            {
                WriteLine("Logged in successfully");
                SendNetworkEvent("On-Login-Success", login, message);
                //                SendNewEvent("on-login-success",login,message);
            }
            else
            {
                SendNetworkEvent("On-Login", login, message);
            }

        }

        public void RegisterAllCommands(Assembly assembly)
        {
            foreach (Type t in assembly.GetTypes())
            {
                try
                {
                    if (t.IsSubclassOf(typeof(Command)))
                    {
                        if (typeof(SystemApplicationCommand).IsAssignableFrom(t)) continue;
                        ConstructorInfo info = t.GetConstructor(new Type[] { typeof(BotClient) });
                        try
                        {
                            Command command = (Command)info.Invoke(new object[] { this });
                            RegisterCommand(command);
                        }
                        catch (Exception e)
                        {
                            WriteLine("RegisterAllCommands: " + e.ToString() + "\n" + e.InnerException + "\n In " + t.Name);
                        }
                    }
                }
                catch (Exception e)
                {
                    WriteLine(e.ToString());
                }
            }
        }


        /// <summary>
        /// Initialize everything that needs to be initialized once we're logged in.
        /// </summary>
        /// <param name="login">The status of the login</param>
        /// <param name="message">Error message on failure, MOTD on success.</param>
        private void RegisterCommand(string name, cogbot.Actions.Action command)
        {
            name = name.ToLower();

            if (!Commands.ContainsKey(name))
            {
                Commands.Add(name, command);
                command.Name = name;
                command.TheBotClient = this;
            }
            else
            {
                RegisterCommand("!" + name, command);
            }
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


        internal void ShutDown()
        {
            //scriptEventListener.
            logout();
            //botPipeline.Shut
            //thrJobQueue.Abort();
            //lock (lBotMsgSubscribers)
            //{
            //  foreach (BotMessageSubscriber ms in lBotMsgSubscribers)
            //{
            //  ms.ShuttingDown();
            // }
            // }
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

        public void SendNetworkEvent(string eventName, params object[] args)
        {
            SendPersonalEvent(SimEventType.NETWORK,eventName,args);
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
            SimObjectEvent evt = botPipeline.CreateEvent(type, eventName, args);
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

        public string ExecuteCommand(string text)
        {
            return ExecuteCommand(text, WriteLine);
        }

        public string ExecuteCommand(string text, OutputDelegate WriteLine)
        {
            String res = ExecuteBotCommand(text, WriteLine);
            if (!String.IsNullOrEmpty(res)) return res;
            return ClientManager.ExecuteSystemCommand(text, WriteLine);
        }


        public string ExecuteBotCommand(string text, OutputDelegate WriteLine)
        {
            if (text == null)
            {
                return String.Empty;
            }
            text = text.TrimStart();
            if (text.Length == 0)
            {
                return String.Empty;
            }
            try
            {
                if (text.StartsWith("("))
                {
                    return evalLispString(text);
                }
                //            Settings.LOG_LEVEL = Helpers.LogLevel.Debug;
                //text = text.Replace("\"", "");
                string verb = text.Split(null)[0];
                if (Commands != null && Commands.ContainsKey(verb))
                {
                    Action act = Commands[verb];
                    if (act is GridMasterCommand)
                    {
                        if (!WorldSystem.IsGridMaster)
                        {
                            return String.Empty;
                        }
                    }
                    if (act is RegionMasterCommand)
                    {
                        if (!WorldSystem.IsRegionMaster)
                        {
                            return String.Empty;
                        }
                    }
                    string res;
                    if (text.Length > verb.Length)
                        res = act.acceptInputWrapper(verb, text.Substring(verb.Length + 1), WriteLine);
                    else
                        res = act.acceptInputWrapper(verb, "", WriteLine);
                    if (String.IsNullOrEmpty(res))
                    {
                        return string.Format("{0} Completed: {1}", GetName(), text);
                    }
                    return res.Replace("$bot", GetName());
                }
                else
                {
                    return String.Empty;
                }
            }
            catch (Exception e)
            {
                WriteLine("" + e);
                return String.Empty;
            }
        }

        public string GetName()
        {
            string n = Self.Name;
            if (!String.IsNullOrEmpty(n)) return n;
            return BotLoginParams.FirstName + " " + BotLoginParams.LastName;
        }


        void SimEventSubscriber.OnEvent(SimObjectEvent evt)
        {
            if (evt.GetVerb() == "On-Execute-Command")
            {
                ExecuteCommand(evt.GetArgs()[0].ToString(), WriteLine);
            }
        }

        void SimEventSubscriber.ShuttingDown()
        {
            ((BotClient)this).ShutDown();
        }


        public void Talk(string str)
        {
            Self.Chat(str, 0, ChatType.Normal);
        }

        internal void Intern(string n, object v)
        {
            if (lispTaskInterperter == null)
            {
                initTaskInterperter();
            }
            lispTaskInterperter.Intern(n, v);
        }


        private void RegisterListener(Listener listener)
        {
           // listeners[listener.GetModuleName()] = listener;
            listener.StartupListener();
        }

        internal void RegisterType(Type t)
        {
            if (registeredTypes.Contains(t)) return;
            registeredTypes.Add(t);
            try
            {
                if (t.IsSubclassOf(typeof(Command)))
                {
                    if (typeof(SystemApplicationCommand).IsAssignableFrom(t)) return;
                    ConstructorInfo info = t.GetConstructor(new Type[] { typeof(BotClient) });
                    try
                    {
                        Command command = (Command)info.Invoke(new object[] { this });
                        RegisterCommand(command);
                    }
                    catch (Exception e)
                    {
                        WriteLine("RegisterCommand: " + e.ToString() + "\n" + e.InnerException + "\n In " + t.Name);
                    }
                }
            }
            catch (Exception e)
            {
                WriteLine(e.ToString());
            }
            try
            {
                if (t.IsSubclassOf(typeof(WorldObjectsModule)))
                {
                    ConstructorInfo info = t.GetConstructor(new Type[] { typeof(BotClient) });
                    try
                    {
                        Listener command = (Listener)info.Invoke(new object[] { this }); 
                        RegisterListener(command);
                    }
                    catch (Exception e)
                    {
                        WriteLine("RegisterListener: " + e.ToString() + "\n" + e.InnerException + "\n In " + t.Name);
                    }
                }
            }
            catch (Exception e)
            {
                WriteLine(e.ToString());
            }
        }

        internal object GetAvatar()
        {
            if (gridClient.Self.AgentID != UUID.Zero) return WorldSystem.TheSimAvatar;
            return this;
        }

    }

}

