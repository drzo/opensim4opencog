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

namespace cogbot
{
	public class BotClient : GridClient {
		public static int nextTcpPort = 5555;
		int thisTcpPort;
		public OpenMetaverse.LoginParams BotLoginParams;// = null;
		public UUID GroupID = UUID.Zero;
		public Dictionary<UUID, GroupMember> GroupMembers = null; // intialized from a callback
		public Dictionary<UUID, AvatarAppearancePacket> Appearances = new Dictionary<UUID, AvatarAppearancePacket>();
		///public Dictionary<string, Command> Commands = new Dictionary<string, Command>();
		public bool Running = true;
		public bool GroupCommands = false;
		public string MasterName = String.Empty;
		public UUID MasterKey = UUID.Zero;
		public bool AllowObjectMaster = false;
		public ClientManager ClientManager;
		public VoiceManager VoiceManager;
		// Shell-like inventory commands need to be aware of the 'current' inventory folder.
		public InventoryFolder CurrentDirectory = null;

		private Quaternion bodyRotation = Quaternion.Identity;
		private Vector3 forward = new Vector3(0, 0.9999f, 0);
		private Vector3 left = new Vector3(0.9999f, 0, 0);
		private Vector3 up = new Vector3(0, 0, 0.9999f);
		private System.Timers.Timer updateTimer;

		public Listeners.WorldObjects WorldSystem;
        //   static public TextForm SingleInstance = null;
        public static int debugLevel = 2;
        public bool GetTextures = true;

        //  public cogbot.TextForm ClientManager;
        //  public VoiceManager VoiceManager;
        // Shell-like inventory commands need to be aware of the 'current' inventory folder.

        //  public GridClient this = null;
        //  public OutputDelegate outputDelegate;
        ///public DotCYC.CycConnectionForm cycConnection;
        public Dictionary<string, DescribeDelegate> describers;

        //public Dictionary<string, Listeners.Listener> listeners;
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

        public Queue taskQueue = null;// new Queue();
        public Thread thrJobQueue = null;
        InventoryEval searcher = null; // new InventoryEval(this);
        //public Inventory Inventory;
        //public InventoryManager Manager;
        // public Configuration config;
        public String taskInterperterType = "DotLispInterpreter";// DotLispInterpreter,CycInterpreter or ABCLInterpreter
        public TextForm parentTextForm = null;
        public List<string> muteList;
        public bool muted = false;

		/// <summary>
		/// 
		/// </summary>
		public BotClient(ClientManager manager)
		{
			ClientManager = manager;
			manager.lastBotClient = this;


			updateTimer = new System.Timers.Timer(500);
			updateTimer.Elapsed += new System.Timers.ElapsedEventHandler(updateTimer_Elapsed);

//            manager.AddTextFormCommands(this);
			//          RegisterAllCommands(Assembly.GetExecutingAssembly());

			Settings.LOG_LEVEL = Helpers.LogLevel.Debug;
			Settings.LOG_RESENDS = false;
			Settings.STORE_LAND_PATCHES = true;
			Settings.ALWAYS_DECODE_OBJECTS = true;
			Settings.ALWAYS_REQUEST_OBJECTS = true;
			Settings.SEND_AGENT_UPDATES = true;
			Settings.USE_TEXTURE_CACHE = false;	//was true
			// Optimize the throttle
			Throttle.Wind = 0;
			Throttle.Cloud = 0;
			Throttle.Land = 1000000;
			Throttle.Task = 1000000;

			VoiceManager = new VoiceManager(this);
			//manager.AddBotClientToTextForm(this);
			SetDefaultLoginDetails(TextForm.SingleInstance.config);
		}

        public void TextFormClient(TextForm parent)
        {
            // SingleInstance = this;
            ///this = this;// new GridClient();
            parentTextForm = parent;


            //            Appearances = new Dictionary<UUID, AvatarAppearancePacket>();

            Settings.ALWAYS_DECODE_OBJECTS = true;
            Settings.ALWAYS_REQUEST_OBJECTS = true;
            Settings.OBJECT_TRACKING = true;

            //  Manager = Inventory;
            //  Inventory = Manager.Store;

            // config = new Configuration();
            // config.loadConfig();
            /// Settings.LOGIN_SERVER = config.simURL;
            // Opensim recommends 250k total
            Throttle.Total = 250000;
            Settings.CAPS_TIMEOUT = 5 * 1000;
            Settings.RESEND_TIMEOUT = 4 * 1000;
            Settings.LOGIN_TIMEOUT = 16 * 1000;
            Settings.LOGOUT_TIMEOUT = 16 * 1000;
            Settings.SIMULATOR_TIMEOUT = 90 * 1000;
            Settings.SEND_PINGS = true;
            Settings.MULTIPLE_SIMS = false;
            Settings.ENABLE_CAPS = true;
            Self.Movement.Camera.Far = 32;
            Settings.LOG_ALL_CAPS_ERRORS = true;
            Settings.FETCH_MISSING_INVENTORY = true;
            Settings.SEND_AGENT_THROTTLE = true;

            muteList = new List<string>();

            // outputDelegate = new OutputDelegate(doOutput);

            describers = new Dictionary<string, DescribeDelegate>();
            describers["location"] = new DescribeDelegate(describeLocation);
            describers["people"] = new DescribeDelegate(describePeople);
            describers["objects"] = new DescribeDelegate(describeObjects);
            describers["buildings"] = new DescribeDelegate(describeBuildings);

            describePos = 0;

            //listeners = new Dictionary<string, cogbot.Listeners.Listener>();
            //listeners["chat"] = new Listeners.Chat(this);
            WorldSystem = new Listeners.WorldObjects(this);
            //listeners["teleport"] = new Listeners.Teleport(this);
            //listeners["whisper"] = new Listeners.Whisper(this);
            //ObjectSystem = new Listeners.Objects(this);
            //listeners["bump"] = new Listeners.Bump(this);
            //listeners["sound"] = new Listeners.Sound(this);

            Commands = new SortedDictionary<string, cogbot.Actions.Action>();
            Commands["login"] = new Actions.Login(this);
            Commands["logout"] = new Actions.Logout(this);
            Commands["stop"] = new Actions.Stop(this);
            Commands["teleport"] = new Actions.Teleport(this);
            Commands["describe"] = new Actions.Describe(this);
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

            RegisterAllCommands(Assembly.GetExecutingAssembly());



            tutorials = new Dictionary<string, cogbot.Tutorials.Tutorial>();
            tutorials["tutorial1"] = new Tutorials.Tutorial1(parent, this);


            describeNext = true;


            // Start the server
            thisTcpPort = nextTcpPort;
            Utilities.TcpServer UtilitiesTcpServer = new Utilities.TcpServer(nextTcpPort, this);
            nextTcpPort++;
            UtilitiesTcpServer.startSocketListener();


            Network.RegisterCallback(PacketType.AgentDataUpdate, new NetworkManager.PacketCallback(AgentDataUpdateHandler));
            Network.RegisterCallback(PacketType.AlertMessage, new NetworkManager.PacketCallback(AlertMessageHandler));
            Network.RegisterCallback(PacketType.AvatarAppearance, new NetworkManager.PacketCallback(AvatarAppearanceHandler));

            Appearance.OnAppearanceUpdated += new AppearanceManager.AppearanceUpdatedCallback(Appearance_OnAppearanceUpdated);

            Inventory.OnObjectOffered += new InventoryManager.ObjectOfferedCallback(Inventory_OnInventoryObjectReceived);
            Groups.OnGroupMembers += new GroupManager.GroupMembersCallback(GroupMembersHandler);
            Inventory.OnObjectOffered += new InventoryManager.ObjectOfferedCallback(Inventory_OnInventoryObjectReceived);
            Logger.OnLogMessage += new Logger.LogCallback(client_OnLogMessage);
            Network.OnEventQueueRunning += new NetworkManager.EventQueueRunningCallback(Network_OnEventQueueRunning);
            Network.OnLogin += new NetworkManager.LoginCallback(Network_OnLogin);
            Network.OnLogoutReply += new NetworkManager.LogoutCallback(Network_OnLogoutReply);
            Network.OnSimConnected += new NetworkManager.SimConnectedCallback(Network_OnSimConnected);
            Network.OnSimDisconnected += new NetworkManager.SimDisconnectedCallback(Network_OnSimDisconnected);
            Network.OnConnected += new NetworkManager.ConnectedCallback(Network_OnConnected);
            Network.OnDisconnected += new NetworkManager.DisconnectedCallback(Network_OnDisconnected);
            Self.OnInstantMessage += new AgentManager.InstantMessageCallback(Self_OnInstantMessage);
            Self.OnScriptDialog += new AgentManager.ScriptDialogCallback(Self_OnScriptDialog);
            Self.OnScriptQuestion += new AgentManager.ScriptQuestionCallback(Self_OnScriptQuestion);
            Self.OnTeleport += new AgentManager.TeleportCallback(Self_OnTeleport);
            Self.OnChat += new AgentManager.ChatCallback(Self_OnChat);

            updateTimer.Start();
            searcher = new InventoryEval(this);
            thrJobQueue = new Thread(jobManager);
            thrJobQueue.Start();
        }


		private void SetDefaultLoginDetails(Configuration configuration)
		{

			BotLoginParams.FirstName = configuration.firstName;
			BotLoginParams.LastName = configuration.lastName;
			BotLoginParams.Password = configuration.password;
			BotLoginParams.URI = configuration.simURL;
		}


		//breaks up large responses to deal with the max IM size
		private void SendResponseIM(GridClient client, UUID fromAgentID, string data)
		{
			for (int i = 0; i < data.Length; i += 1024) {
				int y;
				if ((i + 1023) > data.Length) {
					y = data.Length - i;
				} else {
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
			if (p.AgentData.AgentID == sim.Client.Self.AgentID) {
				output("Got the group ID for " + sim.Client.ToString() + ", requesting group members...");
				GroupID = p.AgentData.ActiveGroupID;

				sim.Client.Groups.RequestGroupMembers(GroupID);
			}
		}

		private void GroupMembersHandler(Dictionary<UUID, GroupMember> members)
		{
			output("Got " + members.Count + " group members.");
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

			Logger.Log("[AlertMessage] " + Utils.BytesToString(message.AlertData.Message), Helpers.LogLevel.Info, this);
		}


        void Self_OnTeleport(string message, AgentManager.TeleportStatus status, AgentManager.TeleportFlags flags)
        {
            if (status == AgentManager.TeleportStatus.Failed)
            {
                output("Teleport failed.");
                describeSituation();
            }
            else if (status == AgentManager.TeleportStatus.Finished)
            {
                output("Teleport succeeded.");
                describeSituation();
            }
        }


        void Self_OnChat(string message, ChatAudibleLevel audible, ChatType type, ChatSourceType sourceType, string fromName, UUID id, UUID ownerid, Vector3 position)
        {
            if (message.Length > 0 && sourceType == ChatSourceType.Agent && !muteList.Contains(fromName))
            {
                output(fromName + " says, \"" + message + "\".");
                //botenqueueLispTask("(thisClient.msgClient \"(heard (" + fromName + ") '" + message + "' )\" )");
                SendEvent("on-chat", fromName, message);
            }
        }

		private void Self_OnInstantMessage(InstantMessage im, Simulator simulator)
		{
            if (im.Message.Length > 0 && im.Dialog == InstantMessageDialog.MessageFromAgent)
            {
                output(im.FromAgentName + " whispers, \"" + im.Message + "\".");
                SendEvent("on-instantmessage", im.FromAgentName, im.Message);

                Actions.Whisper whisper = (Actions.Whisper)Commands["whisper"];
                whisper.currentAvatar = im.FromAgentID;
                whisper.currentSession = im.IMSessionID;
            }

			bool groupIM = im.GroupIM && GroupMembers != null && GroupMembers.ContainsKey(im.FromAgentID) ? true : false;

			if (im.FromAgentID == MasterKey || (GroupCommands && groupIM)) {
				// Received an IM from someone that is authenticated
				output(String.Format("<{0} ({1})> {2}: {3} (@{4}:{5})", im.GroupIM ? "GroupIM" : "IM", im.Dialog, im.FromAgentName, im.Message, im.RegionID, im.Position));

				if (im.Dialog == InstantMessageDialog.RequestTeleport) {
					output("Accepting teleport lure.");
					Self.TeleportLureRespond(im.FromAgentID, true);
				} else if (
						  im.Dialog == InstantMessageDialog.MessageFromAgent ||
						  im.Dialog == InstantMessageDialog.MessageFromObject) {
					ClientManagerRef.ClientManager.DoCommandAll(im.Message, im.FromAgentID);
				}
			} else {
				// Received an IM from someone that is not the bot's master, ignore
				output(String.Format( "<{0} ({1})> {2} (not master): {3} (@{4}:{5})", im.GroupIM ? "GroupIM" : "IM", im.Dialog, im.FromAgentName, im.Message,
									  im.RegionID, im.Position));
				return;
			}
		}

        void Self_OnScriptQuestion(Simulator simulator, UUID taskID, UUID itemID, string objectName, string objectOwner, ScriptPermission questions)
        {
            SendEvent("On-Script-Question", simulator, taskID, itemID, objectName, objectOwner, questions);
            //throw new NotImplementedException();
        }

        void Self_OnScriptDialog(string message, string objectName, UUID imageID, UUID objectID, string firstName, string lastName, int chatChannel, List<string> buttons)
        {
            SendEvent("On-Script-Dialog", message, objectName, imageID, objectID, firstName, lastName, chatChannel, buttons);
            //throw new NotImplementedException();
        }

        private bool Inventory_OnInventoryObjectReceived(InstantMessage offer, AssetType type,
														 UUID objectID, bool fromTask)
		{
			if (MasterKey != UUID.Zero) {
				if (offer.FromAgentID != MasterKey)
					return false;
			} else if (GroupMembers != null && !GroupMembers.ContainsKey(offer.FromAgentID)) {
				return false;
			}

			return true;
		}

		// EVENT CALLBACK SECTION
		void Network_OnDisconnected(NetworkManager.DisconnectType reason, string message)
		{
			try {
				if (message.Length > 0)
					output("Disconnected from server. Reason is " + message + ". " + reason);
				else
					output("Disconnected from server. " + reason);

				SendEvent("on-network-disconnected",reason,message);

				output("Bad Names: " + BoringNamesCount);
				output("Good Names: " + GoodNamesCount);
			} catch (Exception e) {
			}
		}

		void Network_OnConnected(object sender)
		{
			try {

				System.Threading.Thread.Sleep(3000);

				//  describeAll();
				//  describeSituation();
				SendEvent("on-network-connected");

			} catch (Exception e) {
			}
		}



		public void SendEvent(string eventName, params object[] args)
		{
			eventName = eventName.ToLower();
			String msg = "(" + eventName.ToLower();
			int start = 0;
			if (args.Length > 1) {
				if (args[0] is Simulator) {
					start = 1;
				}
			}
			for (int i = start; i < args.Length; i++) {
				msg += " ";
				msg += argString(args[i]);
			}

			msg += ")";
			if (taskInterperter.DefinedFunction(eventName)) {
				enqueueLispTask(msg);
			} else {
                msgClient(msg);
			}
		}
		public string argsListString(Array args)
		{
			switch (args.Length) {
			case 0:
				return "";
			case 1:
				return argString(args.GetValue(0));
			default:
				String msg = argString(args.GetValue(0));
				for (int i = 1; i < args.Length; i++) {
					msg += " ";
					msg += argString(args.GetValue(i));
				}
				return msg;
			}
		}

		public string argString(object arg)
		{
			if (arg == null) return "NIL";
			Type type = arg.GetType();
			if (arg is Simulator) {
				return argString(((Simulator)arg).Name);
			}
			if (arg is Avatar) {
				Avatar prim = (Avatar)arg;
                arg = "'(avatar"; //+ argString(prim.ID.ToString());
				if (prim.Name != null) {
					arg = arg + " " + argString(prim.Name);
				}
				return arg + ")";
			}

			if (arg is Primitive) {
				Primitive prim = (Primitive)arg;
                arg = "'(prim " + argString(prim.ID.ToString());
				if (prim.Properties != null) {
					arg = arg + " " + argString(prim.Properties.Name);
				}
				return arg + ")";
			}
			if (type.IsEnum) {
				return argString(arg.ToString());
			}
			//InternalDictionary
			if (arg is IList) {
				String dictname = "'(list " + type.Name;
				IList list = (IList)arg;
				foreach (object key in list)
				{
					dictname += " " + argString(key);
				}
				return dictname + ")";


			}
			if (arg is IDictionary) {
				String dictname = "'(dict " + type.Name;
				IDictionary dict = (IDictionary)arg;
                lock (dict)
                {
                    foreach (object key in dict.Keys)
                    {
                        Object o = dict[key];
                        dictname += " " + argString(key) + "=" + argString(o);
                    }
                    return dictname + ")";
                }

			}
			if (arg is UUID) {
                object found = WorldSystem.GetObject((UUID)arg);
                if (found == null || found == arg)
                {
                    return argString(arg.ToString());
                }
                return argString(found);
			} else
				if (arg is Vector3) {
				Vector3 vect = (Vector3)arg;
				return "'(Vector3 " + vect.X + " " + vect.Y + " " + vect.Z + ")";
			} else
                if (arg is Vector2)
                {
                    Vector2 vect = (Vector2)arg;
                    return "'(Vector2 " + vect.X + " " + vect.Y + ")";
                }
                else
                    if (arg is Vector3d)
                    {
                        Vector3d vect = (Vector3d)arg;
                        return "'(Vector3d " + vect.X + " " + vect.Y + " " + vect.Z + ")";
                    }
                    else
                        if (arg is Quaternion)
                        {
                            Quaternion vect = (Quaternion)arg;
                            return "'(Quaternion " + vect.X + " " + vect.Y + " " + vect.Z + " " + vect.W +")";
                        }

			if (type.IsArray) {
				Array a = (Array)arg;
				return "#{/*"+type+"*/"+ argsListString(a) + "}";
			}
			if (arg is String) {
				return "\"" + arg.ToString().Replace("\"", "\\\"") + "\"";
			}
			if (type.Namespace.StartsWith("System")) {
				return "" + arg;
			}
			if (type.IsValueType) {
				String tostr = "{" + arg + "";
				foreach (FieldInfo fi in type.GetFields())
				{
					if (!fi.IsStatic) {
						tostr += ",";
						tostr += fi.Name + "=";
						tostr += argString(fi.GetValue(arg));
					}
				}
				return argString(tostr + "}");
			}
			if (!type.IsValueType) {
				String tostr = "{" + arg + "";
				foreach (FieldInfo fi in type.GetFields())
				{
					if (!fi.IsStatic) {
						tostr += ",";
						tostr += fi.Name + "=";
						tostr += fi.GetValue(arg);
					}
				}
				return argString(tostr + "}");
			}
			return "" + arg;
		}

		void Appearance_OnAppearanceUpdated(Primitive.TextureEntry te)
		{
			SendEvent("On-Appearance-Updated", te);
		}

		void Network_OnSimDisconnected(Simulator simulator, NetworkManager.DisconnectType reason)
		{
			SendEvent("On-Sim-Disconnected", simulator, reason);
		}

		void client_OnLogMessage(object message, Helpers.LogLevel level)
		{
			string msg = "" + level + " " + message;
			if (msg.Contains("esend")) return;
			if (msg.Contains("resent packet")) return;
			if (msg.Contains("Rate limit"))	return;
			if (debugLevel < 3 && msg.Contains("Array index is out of range")) return;
			if (debugLevel < 3 && (msg.Contains("nloadi") || msg.Contains("ransfer"))) return;
			SendEvent("On-Log-Message", message, level);
		}

		void Network_OnEventQueueRunning(Simulator simulator)
		{
			SendEvent("On-Event-Queue-Running", simulator);
		}

		void Network_OnSimConnected(Simulator simulator)
		{
			SendEvent("On-Simulator-Connected", simulator);
//            SendEvent("on-simulator-connected",simulator);
		}

		bool Network_OnSimConnecting(Simulator simulator)
		{
			SendEvent("On-simulator-Connecing", simulator);
			return true;
		}

		void Network_OnLogoutReply(List<UUID> inventoryItems)
		{
			SendEvent("On-Logout-Reply", inventoryItems);
		}

		public bool ExecuteCommand(string text)
		{
			text = text.Replace("\"", "");
			string verb = text.Split(null)[0];
			if (Commands.ContainsKey(verb)) {
				if (text.Length > verb.Length)
					Commands[verb].acceptInputWrapper(verb, text.Substring(verb.Length + 1));
				else
					Commands[verb].acceptInputWrapper(verb, "");
				return true;
			} else {
				return false;
			}
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
		public void UseInventoryItem(string usage,string itemName)
		{

			InventoryFolder rootFolder = Inventory.Store.RootFolder;
			//InventoryEval searcher = new InventoryEval(this);
			searcher.evalOnFolders(rootFolder, usage, itemName);
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
			InventoryEval searcher = new InventoryEval(this);

			folderID = searcher.findInFolders(rootFolder, folderName);

			if (folderID != UUID.Zero) {
				Self.Chat("Wearing folder \"" + folderName + "\"", 0, ChatType.Normal);
				output("Wearing folder \"" + folderName + "\"");

				Appearance.WearOutfit(folderID,false);
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
							output("Attaching item: " + i.Name);
						}
					);
				}
				*/
			} else {
				Self.Chat("Can't find folder \"" + folderName + "\" to wear", 0, ChatType.Normal);
				output(   "Can't find folder \""    +    folderName    +    "\" to wear");
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
			InventoryFolder rootFolder = Inventory.Store.RootFolder ;  //  .Inventory.InventorySkeleton.Folders;// .RootUUID;
			//InventoryEval searcher = new InventoryEval(this);

			return searcher.findInFolders(rootFolder, name);

		}

		public class InventoryEval {
			// recursive evaluator
			public string current_operation = "";
			public string current_itemName = "";
			//   protected TextForm botclient;
			protected BotClient botclient;
			public Hashtable hooked = new Hashtable();
			//private Inventory Inventory;
			//private InventoryManager Manager;

			public InventoryEval(BotClient _c)
			{
				//  botclient = _c.botclient;
				botclient = _c;// botclient.CurrentClient;

			}

			public void regFolderHook(InventoryFolder folder)
			{
				if (!hooked.ContainsKey(folder.UUID)) {
					hooked.Add(folder.UUID, folder.Name);
					botclient.output("  regFolderHook " + folder.Name);
				}

			}

			public void appendFolderHook(InventoryFolder folder)
			{
				if (!hooked.ContainsKey(folder.UUID)) {
					hooked.Add(folder.UUID, folder.Name);
					// folder.OnContentsRetrieved += new InventoryFolder.ContentsRetrieved(myfolder_OnContentsRetrieved);
					botclient.output("  appendFolderHook " + folder.Name);
				}

			}

			public void myfolder_OnContentsRetrieved(InventoryFolder folder)
			{
				regFolderHook(folder);
				// folder.DownloadContentsOpenSim(TimeSpan.FromSeconds(60));
				// botclient.output("    myfolder_OnContentsRetrieved [" + folder.Name + "] = " + folder.UUID.ToString()+ " with count="+folder.Contents.Count.ToString());
				List<InventoryBase> folderContents = botclient.Inventory.FolderContents(folder.UUID, botclient.Self.AgentID,
																						true, true, InventorySortOrder.ByName, 3000);
				if (folderContents != null) {

					foreach (InventoryBase ib in folderContents)
					{
						if (ib is InventoryItem) {
							InventoryItem ii = ib as InventoryItem;
							if (current_operation == "print") {
								//int indent = 1;
								//StringBuilder result = new StringBuilder();
								//result.AppendFormat("{0}{1} ({2})\n", new String(' ', indent * 2), ii.Name, ii.UUID.ToString());
								//output(result.ToString());
								botclient.output("   [Inventory Item] Name: " + ii.Name + " <==> " + ii.UUID.ToString() + " in folder[" + folder.Name + "]");
							}


							if (ii.Name == current_itemName) {
								// we found a matcher so lets do our ops
								if (current_operation == "wear") botclient.Appearance.WearOutfit(ii.UUID, false);
								if (current_operation == "animationStart") botclient.Self.AnimationStart(ii.UUID, false);
								if (current_operation == "animationStop") botclient.Self.AnimationStop(ii.UUID, false);
								if (current_operation == "attach") botclient.Appearance.Attach(ii, AttachmentPoint.Default);
							}
						}
					}
					// now run any subfolders
					foreach (InventoryBase ib in folderContents)
					{
						if (ib is InventoryFolder) {
							InventoryFolder fld = (InventoryFolder)ib;

							botclient.output(" [Folder] Name: " + ib.Name + " <==> " + ib.UUID.ToString() + " in folder[" + folder.Name + "]");

							//evalOnFolders(ib as InventoryFolder, operation, itemName);
							appendFolderHook(fld);
							//fld.RequestContents();

						}
					}
				}

			}

			public void evalOnFolders(InventoryFolder folder, string operation, string itemName)
			{

				current_itemName = itemName;
				current_operation = operation;

				try {
					/*
				 //   botclient.output("examining folder [" + folder.Name + "] = " + folder.UUID.ToString());
					bool success = false;
					if (folder.IsStale)
					{
						for (int wait = 5; ((wait < 10) && (!success)&&(folder.Contents.Count==0)); wait += 5)
						{
							success = folder.DownloadContentsOpenSim(TimeSpan.FromSeconds(wait));
							//success = folder.DownloadContents(TimeSpan.FromSeconds(wait));
							botclient.output(" DownloadContentets returned " + success.ToString());
							botclient.output(" Contents.count = " + folder.Contents.Count.ToString());
						}
					//appendFolderHook(folder);
					//folder.RequestContents();
					}
					//else
					//{
					//    output(" Claim is folder is fresh...");
					//}
					*/

					List<InventoryBase> folderContents = botclient.Inventory.FolderContents(folder.UUID, botclient.Self.AgentID,
																							true, true, InventorySortOrder.ByName, 3000);
					if (folderContents != null) {

						// first scan this folder for objects

						foreach (InventoryBase ib in folderContents)
						{
							//botclient.output(" [InvAll] Name: " + ib.Name + " <==> " + ib.ToString());
							if (ib is InventoryItem) {
								InventoryItem ii = ib as InventoryItem;
								if (operation == "print") {
									//int indent = 1;
									//StringBuilder result = new StringBuilder();
									//result.AppendFormat("{0}{1} ({2})\n", new String(' ', indent * 2), ii.Name, ii.UUID.ToString());
									//output(result.ToString());
									botclient.output(" [Inventory Item] Name: " + ii.Name + " <==> " + ii.UUID.ToString());
								}


								if (String.Compare(ii.Name,itemName,true)==0) {
									// we found a matcher so lets do our ops
									if (operation == "wear") botclient.Appearance.WearOutfit(ii.UUID, false);
									if (operation == "animationStart") botclient.Self.AnimationStart(ii.UUID, false);
									if (operation == "animationStop") botclient.Self.AnimationStop(ii.UUID, false);
									if (operation == "attach") botclient.Appearance.Attach(ii, AttachmentPoint.Default);
									return;
								}
							}
						}
						// now run any subfolders
						foreach (InventoryBase ib in folderContents)
						{

							if (ib is InventoryFolder) {
								botclient.output(" [Folder] Name: " + ib.Name + " <==> " + ib.UUID.ToString());
								InventoryFolder fld = (InventoryFolder)ib;
								//appendFolderHook(fld);
								//fld.RequestContents();
								if ((operation == "wear") && (ib.Name == itemName)) {
									botclient.Appearance.WearOutfit(ib.UUID, false);
									botclient.output(" [WEAR] Name: " + ib.Name + " <==> " + ib.UUID.ToString());
									return;
								}
								evalOnFolders(ib as InventoryFolder, operation, itemName);
							}
						}
					}
				} catch (Exception e) {
					botclient.output("Search Exception :" + e.StackTrace);
					botclient.output("  msg:" + e.Message);

				}
			}



			// Recursively generates lispTasks for items that match itemName in the inventory
			// like evalLispOnFolders(root,"(Console.Write 'OBJNAME is OBJUUID')","Shoes")
			public void evalLispOnFolders(InventoryFolder folder, string lispOperation, string itemName)
			{
				try {
					//if (folder.IsStale)
					//    folder.DownloadContentsOpenSim(TimeSpan.FromSeconds(10));
					// first scan this folder for text
					List<InventoryBase> folderContents = botclient.Inventory.FolderContents(folder.UUID, botclient.Self.AgentID,
																							true, true, InventorySortOrder.ByName, 3000);
					if (folderContents != null) {

						foreach (InventoryBase ib in folderContents)
						{
							if (ib is InventoryItem) {
								InventoryItem ii = ib as InventoryItem;
								if (ii.Name == itemName) {
									String lispCode = lispOperation;
									lispCode.Replace("OBJUUID", ii.UUID.ToString());
									lispCode.Replace("OBJNAME", ii.Name);
									botclient.enqueueLispTask(lispCode);
								}
							}
						}
						// now run any subfolders
						foreach (InventoryBase ib in folderContents)
						{
							if (ib is InventoryFolder)
								evalLispOnFolders(ib as InventoryFolder, lispOperation, itemName);
						}
					}
				} catch (Exception e) {
				}
			}


			// recursive finder
			public UUID findInFolders(InventoryFolder folder, string itemName)
			{
				try {

					List<InventoryBase> folderContents = botclient.Inventory.FolderContents(folder.UUID, botclient.Self.AgentID,
																							true, true, InventorySortOrder.ByName, 3000);
					//if (folder.IsStale)
					//    folder.DownloadContentsOpenSim(TimeSpan.FromSeconds(10));
					// first scan this folder for text
					if (folderContents != null) {
						foreach (InventoryBase ib in folderContents)
						{
							if (ib is InventoryItem) {
								InventoryItem ii = ib as InventoryItem;
								if (ii.Name == itemName) {
									return ii.UUID;
								}
							}
						}
						// now run any subfolders
						foreach (InventoryBase ib in folderContents)
						{
							if (ib is InventoryFolder) {
								UUID ANS = findInFolders(ib as InventoryFolder, itemName);
								if (ANS != UUID.Zero) return ANS;
							}
						}
					}

				} catch (Exception e) {
				}
				return UUID.Zero;

			}
		}

		//================

		public void logout()
		{
			if (Network.Connected)
				Network.Logout();
		}

		public void output(string str)
		{
			try {
                string toprint = str.Replace("\r\n", "\n").Replace("\r", "\n").Replace("\n", "\r\n");
                toprint = toprint.Replace("$bot", Self.Name);
                toprint = toprint.Replace("You", Self.Name);
                toprint = toprint.Replace("you", Self.Name);
                parentTextForm.output(Self.Name + ": " + toprint);
			} catch (Exception) {
			}
		}

		public void describeAll()
		{
			foreach (string dname in describers.Keys)
			describers[dname].Invoke(true);
		}

		public void describeSituation()
		{
			int i = 0;
			string name = "";
			foreach (string dname in describers.Keys)
			if (i++ == describePos)
				name = dname;
			describePos = (describePos + 1) % describers.Count;
			describers[name].Invoke(false);
		}

		public void describeLocation(bool detailed)
		{
			output("You are in " + Network.CurrentSim.Name + ".");
		}

		public void describePeople(bool detailed)
		{
			if (detailed) {
				List<Avatar> avatarList = WorldSystem.getAvatarsNear(Self.RelativePosition, 8);
				if (avatarList.Count > 1) {
					string str = "You see the people ";
					for (int i = 0; i < avatarList.Count - 1; ++i)
						str += WorldSystem.getAvatarName(avatarList[i]) + ", ";
					str += "and " + WorldSystem.getAvatarName(avatarList[avatarList.Count - 1]) + ".";
					output(str);
				} else if (avatarList.Count == 1) {
					output("You see one person: " + WorldSystem.getAvatarName(avatarList[0]) + ".");
				} else
					output("You don't see anyone around.");
			} else {
				output("You see " + WorldSystem.numAvatars() + " people.");
			}
		}

		public void describeObjects(bool detailed)
		{
			List<Primitive> prims = WorldSystem.getPrimitives(16);
			if (prims.Count > 1) {
				string str = "You see the objects ";
				for (int i = 0; i < prims.Count - 1; ++i)
					str += WorldSystem.getObjectName(prims[i]) + ", ";
				str += "and " + WorldSystem.getObjectName(prims[prims.Count - 1]) + ".";
				output(str);
			} else if (prims.Count == 1) {
				output("You see one object: " + WorldSystem.getObjectName(prims[0]));
			} else {
				output("You don't see any objects around.");
			}
		}

		public void describeBuildings(bool detailed)
		{
			List<Vector3> buildings = WorldSystem.getBuildings(8);
			output("You see " + buildings.Count + " buildings.");
		}

		//------------------------------------ 
		// External XML socket server
		//------------------------------------

		public void msgClient(string serverMessage)
		{
			if (debugLevel>1) {
				output(serverMessage);             
			}
            lock (lBotMsgSubscribers)
            {
                foreach (BotMessageSubscriber ms in lBotMsgSubscribers)
                {
                    ms.msgClient(serverMessage);
                }
            }
        }

		public void overwrite2Hash(Hashtable hashTable, string key, string value)
		{
			if (hashTable.ContainsKey(key))	hashTable.Remove(key);
			hashTable.Add(key, value);
			//output("  +Hash :('" + key + "' , " + value + ")");
		}

		public string getWithDefault(Hashtable hashTable, string key, string defaultValue)
		{
			if (hashTable.ContainsKey(key))	return hashTable[key].ToString();
			return defaultValue;
		}

		public string genActReport(string planID, string seqID, string act, string status)
		{
			DateTime dt = DateTime.Now;

			string actReport = "  <pet-signal pet-name='" + Self.Name.ToString()
							   + "' pet-id='" + Self.AgentID.ToString()
							   + "' timestamp='" + dt.ToString()
							   + "' action-plan-id='" + planID
							   + "' sequence='" + seqID
							   + "' name='" + act
							   + "' status='" + status + "'/>";
			output("actReport:" + actReport);
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
			string xcmd = URL + args;
			return XML2Lisp(xcmd);             
		} // method: XML2Lisp2


		public string XML2Lisp(string xcmd)
		{
			try {
				XmlDocument xdoc = new XmlDocument();
				XmlTextReader reader;
				StringReader stringReader;
				if (xcmd.Contains("http:") || xcmd.Contains(".xml") || xcmd.Contains(".xlsp")) {
					// assuming its a file
					xcmd = xcmd.Trim();
					reader = new XmlTextReader(xcmd);
					xdoc.Load(xcmd);
				} else {
					// otherwise just use the string
					stringReader = new System.IO.StringReader(xcmd);
					reader = new XmlTextReader(stringReader);
					xdoc.LoadXml(xcmd);
				}

				Hashtable[] attributeStack = new Hashtable[64];
				String lispCodeString = "";

				for (int i = 0; i < 64; i++) {
					attributeStack[i] = new Hashtable();
				}
				int depth = 0;

				while (reader.Read()) {
					depth = reader.Depth + 1;
					if (attributeStack[depth] == null) {
						attributeStack[depth] = new Hashtable();
					}
					string tagname = reader.Name;
					switch (reader.NodeType) {
					
					case XmlNodeType.Element:
						if (reader.HasAttributes) {
							for (int i = 0; i < reader.AttributeCount; i++) {
								reader.MoveToAttribute(i);
								string attributeName = reader.Name;
								string attributeValue = reader.Value;

								overwrite2Hash(attributeStack[depth], attributeName, attributeValue);
							}
						}
						// output(" X2L Begin(" + depth.ToString() + ") " + attributeStack[depth]["name"].ToString());
						if (tagname == "op") {
							lispCodeString += "(" + getWithDefault(attributeStack[depth], "name", " ");
						}
						if (tagname == "opq") {
							lispCodeString += "'(" + getWithDefault(attributeStack[depth], "name", " ");
						}

						break;
						//
						//you can handle other cases here
						//

					case XmlNodeType.Text:
						//output(" X2L TEXT(" + depth.ToString() + ") " + reader.Name);

						// Todo
						lispCodeString += " " + reader.Value.ToString();
						break;

					case XmlNodeType.EndElement:

						if (tagname == "op") {
							lispCodeString += " )";
						}
						if (tagname == "opq") {
							lispCodeString += " )";
						}

						// Todo
						//depth--;
						break;

					default:
						break;
					} //switch
				} //while
				output("XML2Lisp =>'" + lispCodeString + "'");
				//string results = evalLispString(lispCodeString);
				string results = "'(enqueued)";
				enqueueLispTask(lispCodeString);
				return results;
			} //try
			catch (Exception e) {
				output("error occured: " + e.Message);
				output("        Stack: " + e.StackTrace.ToString());
				return "()";
			}


		}


		ScriptInterpreter taskInterperter;

		public void initTaskInterperter()
		{
			try {
				taskQueue = new Queue();
				output("Start Loading TaskInterperter ... '" + taskInterperterType + "' \n");
				taskInterperter = ScriptEngines.ScriptManager.LoadScriptInterpreter(taskInterperterType);
				taskInterperter.LoadFile("boot.lisp");
				taskInterperter.LoadFile("extra.lisp");
				taskInterperter.LoadFile("cogbot.lisp");
				// load the initialization string
				if (TextForm.SingleInstance.config.startupClientLisp.Length > 1) {
					enqueueLispTask("(progn "+TextForm.SingleInstance.config.startupClientLisp+")");
				}
                taskInterperter.Intern("clientManager", ClientManager);
                taskInterperter.Intern("Client", this);
                taskInterperter.Intern("thisClient", this);
				output("Completed Loading TaskInterperter '" + taskInterperterType + "'\n");
			} catch (Exception e) {
				output("!Exception: " + e.GetBaseException().Message);
				output("error occured: " + e.Message);
				output("        Stack: " + e.StackTrace.ToString());
			}

		}

		public Object genLispCodeTree(string lispCode)
		{
			Object codeTree = null;
			try {
				StringReader stringCodeReader = new System.IO.StringReader(lispCode);
				codeTree = taskInterperter.Read("console", stringCodeReader);
				if (taskInterperter.Eof(codeTree))
					return null;
			} catch {
				throw;
			}
			return codeTree;
		}

		public void enqueueLispTask(string lispCode)
		{
			output(":: " + lispCode);
			try {
				subtask thisTask = new subtask();
				thisTask.requeue = false;
				thisTask.code = lispCode;
				thisTask.results = "";
				thisTask.codeTree = genLispCodeTree(thisTask.code);
				lock (taskQueue)
				{
					taskQueue.Enqueue(thisTask);
				}
			} catch (Exception e) {
				output("!Exception: " + e.GetBaseException().Message);
				output("error occured: " + e.Message);
				output("        Stack: " + e.StackTrace.ToString());
				output("     LispCode: " + lispCode);
			}
		}

		public void jobManager()
		{
			try {
				initTaskInterperter();
				while (true) {
					while (taskQueue.Count > 0) {
						taskTick();
						Thread.Sleep(1);
					}
					Thread.Sleep(50);
				}
			} catch (Exception e) {
				output("!Exception: " + e.GetBaseException().Message);
				output("error occured: " + e.Message);
				output("        Stack: " + e.StackTrace.ToString());
			}
		}

		public void taskTick()
		{
			string lastcode="";
			try {
				// see if there is anything to process
				if (taskQueue.Count == 0) return;

				// if so then process it
				//Interpreter lispInterperter = new Interpreter();
				subtask thisTask;
				lock (taskQueue)
				{
					thisTask = (subtask)taskQueue.Dequeue();
				}
				// setup the local context
				lastcode = thisTask.code;
				string serverMessage = "";
				thisTask.results = "'(unevaluated)";
				taskInterperter.Intern("thisTask", thisTask);
				//should make the following safer ...
				//taskInterperter.Intern("tcpReader", tcpStreamReader);
				//taskInterperter.Intern("tcpWriter", tcpStreamWriter);
				//a safer way is to have a serverMessage string that is sent to the Client
				// in a more thread safe async way
				taskInterperter.Intern("serverMessage", serverMessage);
				//interpreter.Intern("Client",Command.Client);

				// EVALUATE !!!
				Object x = taskInterperter.Eval(thisTask.codeTree);
				thisTask.results = taskInterperter.Str(x);
                lock (lBotMsgSubscribers)
                {
                    foreach (BotMessageSubscriber ms in lBotMsgSubscribers)
                    {
                        if (ms is Utilities.TcpServer)
                        {
                            ((Utilities.TcpServer)ms).taskTick(thisTask.results);
                        }
                    }
                }
				if (false) {
					output(" taskcode: " + lastcode + " --> " + thisTask.results);
					//output(" taskTick Results>" + thisTask.results);
					//output(" taskTick continueTask=" + thisTask.requeue.ToString());
				}

				// Should we do again ?
				if (thisTask.requeue == true) {
					if (!lastcode.Equals(thisTask.code)) {
						// not the same so must "re-compile"
						thisTask.codeTree = genLispCodeTree(thisTask.code);
					}
					lock (taskQueue)
					{
						taskQueue.Enqueue(thisTask);
					}
				}
				return;
			} catch (Exception e) {
				output("!Exception: " + e.GetBaseException().Message);
				output("error occured: " + e.Message);
				output("        Stack: " + e.StackTrace.ToString());
				output("     LispCode: " + lastcode);
			}

		}

        public string evalLispString(string lispCode)
        {
            try
            {
                if (lispCode == null || lispCode.Length == 0) return null;
                if (taskInterperter == null)
                {
                    output("runTaskInterperter ... '" + taskInterperterType + "'");
                    taskInterperter = ScriptEngines.ScriptManager.LoadScriptInterpreter(taskInterperterType);
                    taskInterperter.LoadFile("boot.lisp");
                    taskInterperter.LoadFile("extra.lisp");
                    taskInterperter.LoadFile("cogbot.lisp");
                    taskInterperter.Intern("thisClient", this);
                    taskInterperter.Intern("clientManager", ClientManager);

                }
                //lispCode = "(load-assembly \"libsecondlife\")\r\n" + lispCode;                
                output("Eval> " + lispCode);
                Object r = null;
                StringReader stringCodeReader = new StringReader(lispCode);
                r = taskInterperter.Read("console", stringCodeReader);
                if (taskInterperter.Eof(r))
                    return r.ToString();
                return taskInterperter.Str(taskInterperter.Eval(r));
            }
            catch (Exception e)
            {
                output("!Exception: " + e.GetBaseException().Message);
                output("error occured: " + e.Message);
                output("        Stack: " + e.StackTrace.ToString());
                return null;
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



		/// <summary>
		/// Initialize everything that needs to be initialized once we're logged in.
		/// </summary>
		/// <param name="login">The status of the login</param>
		/// <param name="message">Error message on failure, MOTD on success.</param>
		public void Network_OnLogin(LoginStatus login, string message)
		{
			if (login == LoginStatus.Success) {
				// Start in the inventory root folder.
				CurrentDirectory = Inventory.Store.RootFolder;//.RootFolder;
			}
//            output("TextForm Network_OnLogin : [" + login.ToString() + "] " + message);
			//SendEvent("On-Login", login, message);

			if (login == LoginStatus.Failed) {
				output("Not able to login");
				// SendEvent("on-login-fail",login,message);
				SendEvent("On-Login-Fail", login, message);
			} else if (login == LoginStatus.Success) {
				output("Logged in successfully");
				SendEvent("On-Login-Success", login, message);
//                SendEvent("on-login-success",login,message);
			} else {
				SendEvent("On-Login", login, message);
			}

		}

		public void RegisterAllCommands(Assembly assembly)
		{
			foreach (Type t in assembly.GetTypes())
			{
				try {
					if (t.IsSubclassOf(typeof(Command))) {
						ConstructorInfo info = t.GetConstructor(new Type[]{ typeof(BotClient)});
						try {
							Command command = (Command)info.Invoke(new object[]{ this});
							RegisterCommand(command);
						} catch (Exception e) {
							output("RegisterAllCommands: " + e.ToString() + "\n" + e.InnerException + "\n In " + t.Name);
						}
					}
				} catch (Exception e) {
					output(e.ToString());
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

			if (!Commands.ContainsKey(name)) {
				Commands.Add(name, command);
				command.Name = name;
				command.Client = this;
			} else {
				RegisterCommand("!" + name, command);
			}
		}

		public void RegisterCommand(Command command)
		{
			RegisterCommand(command.Name, command);
		}

		internal void DoCommandAll(string line, UUID uUID)
		{
			ExecuteCommand(line);
		}

		internal void LogOut(GridClient Client)
		{
			Client.Network.Logout();
		}

		internal OpenMetaverse.Utilities.VoiceManager GetVoiceManager()
		{
			return VoiceManager;
		}


		internal void ShutDown()
		{
			logout();
			thrJobQueue.Abort();
            lock (lBotMsgSubscribers)
            {
                foreach (BotMessageSubscriber ms in lBotMsgSubscribers)
                {
                    ms.ShuttingDown();
                }
            }
		}

        List<BotMessageSubscriber> lBotMsgSubscribers = new List<BotMessageSubscriber>();
        public interface BotMessageSubscriber
        {
            void msgClient(string serverMessage);
            void ShuttingDown();
        }
        internal void AddBotMessageSubscriber(BotMessageSubscriber tcpServer)
        {
            lock (lBotMsgSubscribers)
                lBotMsgSubscribers.Add(tcpServer);
        }

    }

}

