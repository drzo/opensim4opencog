using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using OpenMetaverse; //using libsecondlife;
//using OpenMetaverseTypes;

using System.Reflection;
using System.Xml;
using System.Xml.XPath;
using System.Xml.Serialization;
using System.Net;
using System.Net.Sockets;
using System.Threading;
using System.IO;
using System.Collections;
using DotLisp;


namespace cogbot
{
#pragma warning disable 0168

    public delegate void OutputDelegate(string str);
    public delegate void DescribeDelegate(bool detailed);
    enum Modes { normal, tutorial };

    public partial class TextForm : Form
    {
        public GridClient client;
        public OutputDelegate outputDelegate;
        public DotCYC.CycConnectionForm cycConnection;
        public Dictionary<string, DescribeDelegate> describers;

        public Dictionary<string, Listeners.Listener> listeners;
        public Dictionary<string, Actions.Action> actions;
        public Dictionary<string, Tutorials.Tutorial> tutorials;

        public bool describeNext;
        private int describePos;
        private string currTutorial;

        public int BoringNamesCount = 0;
        public int GoodNamesCount = 0;
        public int RunningMode = (int)Modes.normal;
        public UUID AnimationFolder = UUID.Zero;

        public Thread thrSvr;
        public Queue taskQueue;
        public Thread thrJobQueue;
        StreamReader tcpStreamReader = null;// = new StreamReader(ns);
        StreamWriter tcpStreamWriter = null;// = new StreamWriter(ns);
        NetworkStream ns = null;
        InventoryEval searcher =null; // new InventoryEval(this);
        public Inventory Inventory;
        public InventoryManager Manager;
        public Configuration config;

        public TextForm()
        {
            client = new GridClient();

            client.Settings.ALWAYS_DECODE_OBJECTS = true;
            client.Settings.ALWAYS_REQUEST_OBJECTS = true;
            client.Settings.OBJECT_TRACKING = true;

            Manager = client.Inventory;
            Inventory = Manager.Store;

            config = new Configuration();
            config.loadConfig();
            client.Settings.LOGIN_SERVER = config.simURL;
            extraSettings();


            client.Network.OnConnected += new NetworkManager.ConnectedCallback(Network_OnConnected);
            client.Network.OnDisconnected += new NetworkManager.DisconnectedCallback(Network_OnDisconnected);
            client.Network.OnLogin += new NetworkManager.LoginCallback(Network_OnLogin);

            outputDelegate = new OutputDelegate(doOutput);

            describers = new Dictionary<string, DescribeDelegate>();
            describers["location"] = new DescribeDelegate(describeLocation);
            describers["people"] = new DescribeDelegate(describePeople);
            describers["objects"] = new DescribeDelegate(describeObjects);
            describers["buildings"] = new DescribeDelegate(describeBuildings);

            describePos = 0;

            listeners = new Dictionary<string, cogbot.Listeners.Listener>();
            listeners["chat"] = new Listeners.Chat(this);
            listeners["avatars"] = new Listeners.Avatars(this);
            listeners["teleport"] = new Listeners.Teleport(this);
            listeners["whisper"] = new Listeners.Whisper(this);
            listeners["objects"] = new Listeners.Objects(this);
            listeners["bump"] = new Listeners.Bump(this);
            listeners["sound"] = new Listeners.Sound(this);

            actions = new Dictionary<string, cogbot.Actions.Action>();
            actions["login"] = new Actions.Login(this);
            actions["logout"] = new Actions.Logout(this);
            actions["stop"] = new Actions.Stop(this);
            actions["teleport"] = new Actions.Teleport(this);
            actions["describe"] = new Actions.Describe(this);
            actions["say"] = new Actions.Say(this);
            actions["whisper"] = new Actions.Whisper(this);
            actions["help"] = new Actions.Help(this);
            actions["sit"] = new Actions.Sit(this);
            actions["stand"] = new Actions.Stand(this);
            actions["jump"] = new Actions.Jump(this);
            actions["crouch"] = new Actions.Crouch(this);
            actions["mute"] = new Actions.Mute(this);
            actions["move"] = new Actions.Move(this);
            actions["use"] = new Actions.Use(this);
            actions["eval"] = new Actions.Eval(this);

            actions["fly"] = new Actions.Fly(this);
            actions["stop-flying"] = new Actions.StopFlying(this);
            actions["where"] = new Actions.Where(this);
            actions["locate"] = new Actions.Locate(this);
            Actions.Follow follow = new Actions.Follow(this);
            actions["follow"] = follow;
            actions["wear"] = new Actions.Wear(this);
            actions["stop following"] = follow;
            actions["stop-following"] = follow;
            tutorials = new Dictionary<string, cogbot.Tutorials.Tutorial>();
            tutorials["tutorial1"] = new Tutorials.Tutorial1(this);

            describeNext = true;

            InitializeComponent();

            consoleInputText.Enabled = true;
            consoleInputText.Focus();
            // Start the server
            startSocketListener();
            extraHooks();


        }

        void startSocketListener()
        {
            // The thread that accepts the client and awaits messages

            thrSvr = new Thread(tcpSrv);

            // The thread calls the tcpSvr() method

            thrSvr.Start();

            // Thread that runs the Job Queue
            taskQueue = new Queue();
            thrJobQueue = new Thread(jobManager);
            thrJobQueue.Start();

        }
        void extraSettings()
        {
            // Opensim recommends 250k total
            client.Throttle.Total = 250000;
            client.Settings.CAPS_TIMEOUT   = 5 * 1000;
            client.Settings.RESEND_TIMEOUT = 4 * 1000;
            client.Settings.LOGIN_TIMEOUT  = 16 * 1000;
            client.Settings.LOGOUT_TIMEOUT = 16 * 1000;
            client.Settings.SIMULATOR_TIMEOUT = 90 * 1000;
            client.Settings.SEND_PINGS = true;
            client.Settings.MULTIPLE_SIMS = false;
            client.Settings.ENABLE_CAPS = true;
            client.Self.Movement.Camera.Far = 32;
            client.Settings.LOG_ALL_CAPS_ERRORS = true;
            client.Settings.FETCH_MISSING_INVENTORY = true;
            client.Settings.SEND_AGENT_THROTTLE = true;
            //client.Settings.
            
            
        }

        void extraHooks()
        {

            Logger.OnLogMessage += new Logger.LogCallback(client_OnLogMessage);

            client.Network.OnLogin += new NetworkManager.LoginCallback(Network_OnLogin);
            client.Network.OnLogoutReply += new NetworkManager.LogoutCallback(Network_OnLogoutReply);
            client.Network.OnSimConnected += new NetworkManager.SimConnectedCallback(Network_OnSimConnected);
            client.Network.OnSimDisconnected += new NetworkManager.SimDisconnectedCallback(Network_OnSimDisconnected);
            client.Network.OnEventQueueRunning += new NetworkManager.EventQueueRunningCallback(Network_OnEventQueueRunning);

            //client.Appearance.OnAppearanceUpdated += new AppearanceManager.AppearanceUpdatedCallback(Appearance_OnAppearanceUpdated);
            
            client.Avatars.OnLookAt += new AvatarManager.LookAtCallback(Avatars_OnLookAt);
            client.Avatars.OnPointAt += new AvatarManager.PointAtCallback(Avatars_OnPointAt);
            client.Avatars.OnAvatarProperties += new AvatarManager.AvatarPropertiesCallback(Avatars_OnAvatarProperties);
            
            client.Objects.OnNewAvatar += new ObjectManager.NewAvatarCallback(Objects_OnNewAvatar);
            client.Objects.OnNewFoliage += new ObjectManager.NewFoliageCallback(Objects_OnNewFoliage);
            client.Objects.OnNewPrim += new ObjectManager.NewPrimCallback(Objects_OnNewPrim);
            client.Objects.OnObjectKilled += new ObjectManager.KillObjectCallback(Objects_OnObjectKilled);
            client.Objects.OnObjectUpdated += new ObjectManager.ObjectUpdatedCallback(Objects_OnObjectUpdated);
            client.Objects.OnObjectProperties += new ObjectManager.ObjectPropertiesCallback(Objects_OnObjectProperties);
            
            client.Self.OnAnimationsChanged += new AgentManager.AnimationsChangedCallback(Self_OnAnimationsChanged);
            client.Self.OnScriptDialog += new AgentManager.ScriptDialogCallback(Self_OnScriptDialog);
            client.Self.OnScriptQuestion += new AgentManager.ScriptQuestionCallback(Self_OnScriptQuestion);
            
            client.Terrain.OnLandPatch += new TerrainManager.LandPatchCallback(Terrain_OnLandPatch);
            
            client.Sound.OnSoundTrigger += new SoundManager.SoundTriggerCallback(Sound_OnSoundTrigger);
            searcher = new InventoryEval(this);

        }
// EVENT CALLBACK SECTION
        void Network_OnDisconnected(NetworkManager.DisconnectType reason, string message)
        {
            try
            {
            if (message.Length > 0)
                output("Disconnected from server. Reason is " + message + ". " + reason);
            else
                output("Disconnected from server. " + reason);
            
            enqueueLispTask("(on-network-disconnected (@\"" + reason + "\") (@\"" + message + "\") )");

            output("Bad Names: " + BoringNamesCount);
            output("Good Names: " + GoodNamesCount);
                loginToolStripMenuItem.Enabled = true;
                logoutToolStripMenuItem.Enabled = false;
                consoleInputText.Enabled = false;
                submitButton.Enabled = false;
            }
            catch (Exception e)
            {
            }
         }

        void Network_OnConnected(object sender)
        {
            try
            {
                consoleInputText.Enabled = true;
                loginToolStripMenuItem.Enabled = false;
                logoutToolStripMenuItem.Enabled = true;
                submitButton.Enabled = true;

                consoleInputText.Focus();

                System.Threading.Thread.Sleep(3000);

                describeAll();
                describeSituation();
                enqueueLispTask("(on-network-connected )");

            }
            catch (Exception e)
            {
            }

        }
        public void Network_OnLogin(LoginStatus login, string message)
        {
            output("TextForm Network_OnLogin : [" + login.ToString() + "] " + message);
            EnableIt();
            if (login == LoginStatus.Failed)
            {
                output("Not able to login");
                enqueueLispTask("(on-login-fail (@\"" + login.ToString() + "\") (@\"" + message + "\") )");
            }
            else if (login == LoginStatus.Success)
            {
                output("Logged in successfully");
                enqueueLispTask("(on-login-success (@\"" + login.ToString() + "\") (@\"" + message + "\") )");
            }
        }


        void Sound_OnSoundTrigger(UUID soundID, UUID ownerID, UUID objectID, UUID parentID, float gain, ulong regionHandle, Vector3 position)
        {
            //throw new NotImplementedException();
            output("TextForm Sound_OnSoundTrigger: ");
        }

        void Terrain_OnLandPatch(Simulator simulator, int x, int y, int width, float[] data)
        {
            //throw new NotImplementedException();
//            output("TextForm Terrain_OnLandPatch: "+simulator.ToString()+"/"+x.ToString()+"/"+y.ToString()+" w="+width.ToString());
        }

        void Self_OnScriptQuestion(Simulator simulator, UUID taskID, UUID itemID, string objectName, string objectOwner, ScriptPermission questions)
        {
            //throw new NotImplementedException();
            output("TextForm Self_OnScriptQuestion: "+objectName);
        }

        void Self_OnScriptDialog(string message, string objectName, UUID imageID, UUID objectID, string firstName, string lastName, int chatChannel, List<string> buttons)
        {
            //throw new NotImplementedException();
            output("TextForm Self_OnScriptDialog: "+message);
        }

        void Self_OnAnimationsChanged(InternalDictionary<UUID, int> agentAnimations)
        {
            //throw new NotImplementedException();
 //           output("TextForm Self_OnAnimationsChanged: ");
        }

        void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties properties)
        {
            // Handled by Object Listener
            //throw new NotImplementedException();
 //           output("TextForm Objects_OnObjectProperties: ");
        }

        void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            //throw new NotImplementedException();
//            output("TextForm Objects_OnObjectUpdated: ");
        }

        void Objects_OnObjectKilled(Simulator simulator, uint objectID)
        {
            //throw new NotImplementedException();
            output("TextForm Objects_OnObjectKilled: ");
        }

        void Objects_OnNewPrim(Simulator simulator, Primitive prim, ulong regionHandle, ushort timeDilation)
        {
            // Handled by Object Listener
            //throw new NotImplementedException();
//            output("TextForm Objects_OnNewPrim: "+simulator.ToString()+" "+prim.ToString());
            if (prim.Properties.Name != null)
                enqueueLispTask("(on-new-prim (@\"" + prim.Properties.Name + "\") (@\"" + prim.Properties.ObjectID.ToString() + "\") (@\"" + prim.Properties.Description + "\") )");
            
        }

        void Objects_OnNewFoliage(Simulator simulator, Primitive foliage, ulong regionHandle, ushort timeDilation)
        {
            //throw new NotImplementedException();
//            output("TextForm Objects_OnNewFoliage: ");
            if (foliage.Properties.Name != null)
                enqueueLispTask("(on-new-foliage (@\"" + foliage.Properties.Name + "\") (@\"" + foliage.Properties.ObjectID.ToString() + "\") (@\"" + foliage.Properties.Description + "\") )");
        }

        void Objects_OnNewAvatar(Simulator simulator, Avatar avatar, ulong regionHandle, ushort timeDilation)
        {
            //throw new NotImplementedException();
            output("TextForm Objects_OnNewAvatar: ");
            enqueueLispTask("(on-new-avatar (@\"" + avatar.Name + "\") (@\"" + avatar.ID.ToString() + "\") )");
        }

        void Avatars_OnAvatarProperties(UUID avatarID, Avatar.AvatarProperties properties)
        {
            //throw new NotImplementedException();
            output("TextForm Avatars_OnAvatarProperties: ");
        }

        void Avatars_OnPointAt(UUID sourceID, UUID targetID, Vector3d targetPos, PointAtType pointType, float duration, UUID id)
        {
            output("TextForm Avatars_OnPointAt: " + sourceID.ToString() + " to " + targetID.ToString() + " at " + targetID.ToString() + " with type " + pointType.ToString()+" duration "+duration.ToString());
            if (targetID == client.Self.AgentID)
            {
                output("  (TARGET IS SELF)");
                enqueueLispTask("(on-self-point-target (@\"" + sourceID.ToString() + "\") (@\"" + pointType.ToString() + "\") )");
            }
            enqueueLispTask("(on-avatar-point (@\"" + sourceID.ToString() + "\") (@\"" + targetID.ToString() + "\") (@\"" + pointType.ToString() + "\") )");
        }

        void Avatars_OnLookAt(UUID sourceID, UUID targetID, Vector3d targetPos, LookAtType lookType, float duration, UUID id)
        {
            if (lookType== LookAtType.Idle) return;
            output("TextForm Avatars_OnLookAt: " + sourceID.ToString() + " to " + targetID.ToString() + " at " + targetID.ToString() + " with type " + lookType.ToString() + " duration " + duration.ToString());
            if (targetID == client.Self.AgentID)
            {
                output("  (TARGET IS SELF)");
                enqueueLispTask("(on-self-look-target (@\"" + sourceID.ToString() + "\") (@\"" + lookType.ToString() + "\") )");
            }
            enqueueLispTask("(on-avatar-look (@\"" + sourceID.ToString() + "\") (@\""+targetID.ToString() + "\") (@\"" + lookType.ToString() + "\") )");
        }

        void Appearance_OnAppearanceUpdated(Primitive.TextureEntry te)
        {
            output("TextForm Appearance_OnAppearanceUpdated: " + te.ToString());
        }

        void Network_OnSimDisconnected(Simulator simulator, NetworkManager.DisconnectType reason)
        {
            output("TextForm Network_OnSimDisconnected: " + simulator.ToString()+" reason="+reason.ToString());
        }

        void client_OnLogMessage(object message, Helpers.LogLevel level)
        {
            if (message.ToString().Contains("esend")) return;
            if (message.ToString().Contains("resent packet")) return;
            if (message.ToString().Contains("Rate limit")) return;
            output("TextForm client_OnLogMessage: " + level.ToString() + " " + message.ToString());
        }

        void Network_OnEventQueueRunning(Simulator simulator)
        {
            output("TextForm Network_OnEventQueueRunning: " + simulator.ToString());

        }

        void Network_OnSimConnected(Simulator simulator)
        {
            output("TextForm Network_OnSimConnected: " + simulator.ToString());
            enqueueLispTask("(on-simulator-connected (@\"" + simulator.ToString() + "\") )");

        }

        bool Network_OnSimConnecting(Simulator simulator)
        {
            output("TextForm Network_OnSimConnecting: " + simulator.ToString());
            return true;
        }

        void Network_OnLogoutReply(List<UUID> inventoryItems)
        {
            output("TextForm Network_OnLogoutReply");

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

                    describeSituation();

                }
            }
            catch (Exception e)
            {

            }
        }
        public bool ExecuteCommand(string text)
        {
            string verb = text.Split(null)[0];

            if (actions.ContainsKey(verb))
            {
                if (text.Length > verb.Length)
                    actions[verb].acceptInputWrapper(verb, text.Substring(verb.Length + 1));
                else
                    actions[verb].acceptInputWrapper(verb, "");
                return true;
            }
            else
            {
                output("I don't understand the verb " + verb + ".");
                output("Type \"help\" for help.");
                describeNext = true;

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

            InventoryFolder rootFolder = client.Inventory.Store.RootFolder;
            //InventoryEval searcher = new InventoryEval(this);
            searcher.evalOnFolders(rootFolder, usage, itemName);
        }



        public void ListObjectsFolder()
        {
            // should be just the objects folder 
            InventoryFolder rootFolder = client.Inventory.Store.RootFolder;
            //InventoryEval searcher = new InventoryEval(this);
            searcher.evalOnFolders(rootFolder, "print", "");
        }

        public void wearFolder(string folderName)
        {
            // what we simply want
            //    client.Appearance.WearOutfit(folderName.Split('/'), false);

            UUID folderID;
            InventoryFolder rootFolder = client.Inventory.Store.RootFolder;
            //List<FolderData> folderContents;
            // List<ItemData> folderItems;
            InventoryEval searcher = new InventoryEval(this);

            folderID = searcher.findInFolders(rootFolder, folderName);
            
            if (folderID != UUID.Zero)
            {
                client.Self.Chat("Wearing folder \"" + folderName + "\"", 0, ChatType.Normal);
                output("Wearing folder \"" + folderName + "\"");

                client.Appearance.WearOutfit(folderID,false);
                /*
                List<InventoryBase> folderContents=  client.Inventory.FolderContents(folderID, client.Self.AgentID,
                                                                false, true,
                                                                InventorySortOrder.ByDate, new TimeSpan(0, 0, 0, 0, 10000));

                if (folderContents != null)
                {
                    folderContents.ForEach(
                        delegate(ItemData i)
                        {
                            client.Appearance.Attach(i, AttachmentPoint.Default);
                            client.Self.Chat("Attaching item: " + i.Name, 0, ChatType.Normal);
                            output("Attaching item: " + i.Name);
                        }
                    );
                }
                */
            }
            else
            {
                client.Self.Chat("Can't find folder \"" + folderName + "\" to wear", 0, ChatType.Normal);
                output("Can't find folder \"" + folderName + "\" to wear");
            }

        }

        public void PrintInventoryAll()
        {
            InventoryFolder rootFolder = client.Inventory.Store.RootFolder;
            //InventoryEval searcher = new InventoryEval(this);
            searcher.evalOnFolders(rootFolder, "print", "");

        }

 
        public UUID findInventoryItem(string name)
        {
            InventoryFolder rootFolder = client.Inventory.Store.RootFolder ;  //  .Inventory.InventorySkeleton.Folders;// .RootUUID;
            //InventoryEval searcher = new InventoryEval(this);

            return searcher.findInFolders(rootFolder, name);

        }

        public class InventoryEval
        {
            // recursive evaluator
            public string current_operation = "";
            public string current_itemName = "";
            protected TextForm parent;
            protected GridClient client;
            public Hashtable hooked = new Hashtable();
            //private Inventory Inventory;
            //private InventoryManager Manager;

            public InventoryEval(TextForm _parent)
            {
                parent = _parent;
                client = parent.client;

            }

            public void regFolderHook(InventoryFolder folder)
            {
                if (!hooked.ContainsKey(folder.UUID))
                {
                    hooked.Add(folder.UUID, folder.Name);
                    parent.output("  regFolderHook " + folder.Name);
                }
                    
            }

            public void appendFolderHook(InventoryFolder folder)
            {
                if (!hooked.ContainsKey(folder.UUID))
                {
                    hooked.Add(folder.UUID, folder.Name);
                   // folder.OnContentsRetrieved += new InventoryFolder.ContentsRetrieved(myfolder_OnContentsRetrieved);
                    parent.output("  appendFolderHook " + folder.Name);
                }

            }

            public void myfolder_OnContentsRetrieved(InventoryFolder folder)
            {
                regFolderHook(folder);
               // folder.DownloadContentsOpenSim(TimeSpan.FromSeconds(60));
               // parent.output("    myfolder_OnContentsRetrieved [" + folder.Name + "] = " + folder.UUID.ToString()+ " with count="+folder.Contents.Count.ToString());
                     List<InventoryBase> folderContents = client.Inventory.FolderContents(folder.UUID, client.Self.AgentID,
                                    true, true, InventorySortOrder.ByName, 3000);
                     if (folderContents != null)
                     {

                         foreach (InventoryBase ib in folderContents)
                         {
                             if (ib is InventoryItem)
                             {
                                 InventoryItem ii = ib as InventoryItem;
                                 if (current_operation == "print")
                                 {
                                     //int indent = 1;
                                     //StringBuilder result = new StringBuilder();
                                     //result.AppendFormat("{0}{1} ({2})\n", new String(' ', indent * 2), ii.Name, ii.UUID.ToString());
                                     //output(result.ToString());
                                     parent.output("   [Inventory Item] Name: " + ii.Name + " <==> " + ii.UUID.ToString() + " in folder[" + folder.Name + "]");
                                 }


                                 if (ii.Name == current_itemName)
                                 {
                                     // we found a matcher so lets do our ops
                                     if (current_operation == "wear") client.Appearance.WearOutfit(ii.UUID, false);
                                     if (current_operation == "animationStart") client.Self.AnimationStart(ii.UUID, false);
                                     if (current_operation == "animationStop") client.Self.AnimationStop(ii.UUID, false);
                                     if (current_operation == "attach") client.Appearance.Attach(ii, AttachmentPoint.Default);
                                 }
                             }
                         }
                         // now run any subfolders
                         foreach (InventoryBase ib in folderContents)
                         {
                             if (ib is InventoryFolder)
                             {
                                 InventoryFolder fld = (InventoryFolder)ib;

                                 parent.output(" [Folder] Name: " + ib.Name + " <==> " + ib.UUID.ToString() + " in folder[" + folder.Name + "]");

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

                try
                {
                    /*
                 //   parent.output("examining folder [" + folder.Name + "] = " + folder.UUID.ToString());
                    bool success = false;
                    if (folder.IsStale)
                    {
                        for (int wait = 5; ((wait < 10) && (!success)&&(folder.Contents.Count==0)); wait += 5)
                        {
                            success = folder.DownloadContentsOpenSim(TimeSpan.FromSeconds(wait));
                            //success = folder.DownloadContents(TimeSpan.FromSeconds(wait));
                            parent.output(" DownloadContentets returned " + success.ToString());
                            parent.output(" Contents.count = " + folder.Contents.Count.ToString());
                        }
                    //appendFolderHook(folder);
                    //folder.RequestContents();
                    }
                    //else
                    //{
                    //    output(" Claim is folder is fresh...");
                    //}
                    */

                     List<InventoryBase> folderContents = client.Inventory.FolderContents(folder.UUID, client.Self.AgentID,
                                    true, true, InventorySortOrder.ByName, 3000);
                     if (folderContents != null)
                     {

                         // first scan this folder for objects

                         foreach (InventoryBase ib in folderContents)
                         {
                             //parent.output(" [InvAll] Name: " + ib.Name + " <==> " + ib.ToString());
                             if (ib is InventoryItem)
                             {
                                 InventoryItem ii = ib as InventoryItem;
                                 if (operation == "print")
                                 {
                                     //int indent = 1;
                                     //StringBuilder result = new StringBuilder();
                                     //result.AppendFormat("{0}{1} ({2})\n", new String(' ', indent * 2), ii.Name, ii.UUID.ToString());
                                     //output(result.ToString());
                                     parent.output(" [Inventory Item] Name: " + ii.Name + " <==> " + ii.UUID.ToString());
                                 }


                                 if (ii.Name == itemName)
                                 {
                                     // we found a matcher so lets do our ops
                                     if (operation == "wear") client.Appearance.WearOutfit(ii.UUID, false);
                                     if (operation == "animationStart") client.Self.AnimationStart(ii.UUID, false);
                                     if (operation == "animationStop") client.Self.AnimationStop(ii.UUID, false);
                                     if (operation == "attach") client.Appearance.Attach(ii, AttachmentPoint.Default);
                                     return;
                                 }
                             }
                         }
                         // now run any subfolders
                         foreach (InventoryBase ib in folderContents)
                         {

                             if (ib is InventoryFolder)
                             {
                                 parent.output(" [Folder] Name: " + ib.Name + " <==> " + ib.UUID.ToString());
                                 InventoryFolder fld = (InventoryFolder)ib;
                                 //appendFolderHook(fld);
                                 //fld.RequestContents();
                                 if ((operation == "wear") && (ib.Name == itemName))
                                 {
                                     client.Appearance.WearOutfit(ib.UUID, false);
                                     parent.output(" [WEAR] Name: " + ib.Name + " <==> " + ib.UUID.ToString());
                                     return;
                                 }
                                 evalOnFolders(ib as InventoryFolder, operation, itemName);
                             }
                         }
                     }
                }
                catch (Exception e)
                {
                    parent.output("Search Exception :" + e.StackTrace);
                    parent.output("  msg:" + e.Message);

                }
            }



            // Recursively generates lispTasks for items that match itemName in the inventory
            // like evalLispOnFolders(root,"(Console.Write 'OBJNAME is OBJUUID')","Shoes")
            public void evalLispOnFolders(InventoryFolder folder, string lispOperation, string itemName)
            {
                try
                {
                    //if (folder.IsStale)
                    //    folder.DownloadContentsOpenSim(TimeSpan.FromSeconds(10));
                    // first scan this folder for text
                    List<InventoryBase> folderContents = client.Inventory.FolderContents(folder.UUID, client.Self.AgentID,
                                    true, true, InventorySortOrder.ByName, 3000);
                    if (folderContents != null)
                    {

                        foreach (InventoryBase ib in folderContents)
                        {
                            if (ib is InventoryItem)
                            {
                                InventoryItem ii = ib as InventoryItem;
                                if (ii.Name == itemName)
                                {
                                    String lispCode = lispOperation;
                                    lispCode.Replace("OBJUUID", ii.UUID.ToString());
                                    lispCode.Replace("OBJNAME", ii.Name);
                                    parent.enqueueLispTask(lispCode);
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
                }
                catch (Exception e)
                {
                }
            }


            // recursive finder
            public UUID findInFolders(InventoryFolder folder, string itemName)
            {
                try
                {

                    List<InventoryBase> folderContents = client.Inventory.FolderContents(folder.UUID, client.Self.AgentID,
                                    true, true, InventorySortOrder.ByName, 3000);
                    //if (folder.IsStale)
                    //    folder.DownloadContentsOpenSim(TimeSpan.FromSeconds(10));
                    // first scan this folder for text
                    if (folderContents != null)
                    {
                        foreach (InventoryBase ib in folderContents)
                        {
                            if (ib is InventoryItem)
                            {
                                InventoryItem ii = ib as InventoryItem;
                                if (ii.Name == itemName)
                                {
                                    return ii.UUID;
                                }
                            }
                        }
                        // now run any subfolders
                        foreach (InventoryBase ib in folderContents)
                        {
                            if (ib is InventoryFolder)
                            {
                                UUID ANS = findInFolders(ib as InventoryFolder, itemName);
                                if (ANS != UUID.Zero) return ANS;
                            }
                        }
                    }

                }
                catch (Exception e)
                {
                }
                return UUID.Zero;

            }
        }

 //================


        private void TextForm_FormClosed(object sender, FormClosedEventArgs e)
        {
            logout();
            closeTcpListener();
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            logout();
            closeTcpListener();

            Close();
        }

        private void loginToolStripMenuItem_Click(object sender, EventArgs e)
        {
             // LoginForm loginForm = new LoginForm(client);
            LoginForm loginForm = new LoginForm(client,this);
            loginForm.ShowDialog();
        }

        private void logoutToolStripMenuItem_Click(object sender, EventArgs e)
        {
            logout();
        }

        public void logout()
        {
            if (client.Network.Connected)
                client.Network.Logout();
            config.saveConfig();
        }

        public void output(string str)
        {
            try
            {
                this.Invoke(outputDelegate, str);
            }
            catch (Exception e)
            {
            }
        }

        public void doOutput(string str)
        {
            try
            {
                lock (consoleText)
                {
                    consoleText.AppendText(str + "\r\n");
                }
            }
            catch (Exception e)
            {
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

        public void acceptConsoleInput()
        {
            string text = consoleInputText.Text;
            if (text.Length > 0)
            {
                //output(text);
                consoleInputText.Text = "";

                string verb = text.Split(null)[0];
                if (actions.ContainsKey(verb))
                {
                    if (text.Length > verb.Length)
                        actions[verb].acceptInputWrapper(verb, text.Substring(verb.Length + 1));
                    else
                        actions[verb].acceptInputWrapper(verb, "");
                } 
                else {
                    output("I don't understand the verb " + verb + ".");
                    output("Type \"help\" for help.");
                    describeNext = true;
                }

                if (describeNext)
                {
                    describeNext = false;
                    describeSituation();
                }
            }
        }

        private void consoleInputText_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (Convert.ToInt32(e.KeyChar) == 13)
                acceptConsoleInput();
        }
        public void describeAll()
        {
            foreach (string dname in describers.Keys)
                describers[dname].Invoke(false);
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
            output("You are in " + client.Network.CurrentSim.Name + ".");
        }

        public void describePeople(bool detailed)
        {
            Listeners.Avatars avatars = (Listeners.Avatars)listeners["avatars"];
            if (detailed)
            {
                List<Avatar> avatarList = avatars.getAvatarsNear(client.Self.RelativePosition, 8);
                if (avatarList.Count > 1)
                {
                    string str = "You see the people ";
                    for (int i = 0; i < avatarList.Count - 1; ++i)
                        str += avatars.getAvatarName(avatarList[i]) + ", ";
                    str += "and " + avatars.getAvatarName(avatarList[avatarList.Count - 1]) + ".";
                    output(str);
                }
                else if (avatarList.Count == 1)
                {
                    output("You see one person: " + avatars.getAvatarName(avatarList[0]) + ".");
                }
                else
                    output("You don't see anyone around.");
            }
            else
            {
                output("You see " + avatars.numAvatars() + " people.");
            }
        }

        public void describeObjects(bool detailed)
        {
            Listeners.Objects objects = (Listeners.Objects)listeners["objects"];
            List<Primitive> prims = objects.getPrimitives(16);
            if (prims.Count > 1)
            {
                string str = "You see the objects ";
                for (int i = 0; i < prims.Count - 1; ++i)
                    str += objects.getObjectName(prims[i]) + ", ";
                str += "and " + objects.getObjectName(prims[prims.Count - 1]) + ".";
                output(str);
            }
            else if (prims.Count == 1)
            {
                output("You see one object: " + objects.getObjectName(prims[0]));
            }
            else
            {
                output("You don't see any objects around.");
            }
        }

        public void describeBuildings(bool detailed)
        {
            List<Vector3> buildings = ((Listeners.Objects)listeners["objects"]).getBuildings(8);
            output("You see " + buildings.Count + " buildings.");
        }

        //------------------------------------ 
        // External XML socket server
        //------------------------------------
        TcpListener tcp_socket = null;
        TcpClient tcp_client = null;
        private void tcpSrv()
        {

            try
            {
                bool _quitRequested = false;
                string clientMessage = string.Empty;
                string serverMessage = string.Empty;

                //int receivedDataLength;
                byte[] data = new byte[1024];

                int PortNumber = config.tcpPort; // 5555;
                tcp_socket = new TcpListener(IPAddress.Parse(config.tcpIPAddress), PortNumber);
                output("About to initialize port.");
                tcp_socket.Start();
                output("Listening for a connection... port=" + PortNumber);

                tcp_client = tcp_socket.AcceptTcpClient();
                ns = tcp_client.GetStream();
                tcpStreamReader = new StreamReader(ns);
                tcpStreamWriter = new StreamWriter(ns);

                string welcome = "<comment>Welcome to Cogbot</comment>";
                data = Encoding.ASCII.GetBytes(welcome);
                ns.Write(data, 0, data.Length);

                // Start loop and handle commands:
                while (!_quitRequested)
                {
                    clientMessage = tcpStreamReader.ReadLine();
                    output("SockClient:" + clientMessage);
                    tcpStreamWriter.WriteLine();
                    if (clientMessage.Contains("xml") || clientMessage.Contains("http:"))
                    {
                        serverMessage = EvaluateXmlCommand(clientMessage);
                    }
                    else
                    {
                        serverMessage = EvaluateCommand(clientMessage);
                    }
                    lock (tcpStreamWriter)
                    {
                        if (serverMessage != "")
                            tcpStreamWriter.WriteLine(serverMessage);
                        tcpStreamWriter.WriteLine();
                        ns.Write(Encoding.ASCII.GetBytes(serverMessage.ToCharArray()), 0, serverMessage.Length);
                    }
                    if (clientMessage == "quit") _quitRequested = true;
                }

                //data = new byte[1024];
                //receivedDataLength = ns.Read(data, 0, data.Length);
                //Console.WriteLine(Encoding.ASCII.GetString(data, 0, receivedDataLength));
                //ns.Write(data, 0, receivedDataLength);
                ns.Close();
                tcp_client.Close();
                tcp_socket.Stop();
                thrSvr.Abort();
            }
            catch (Exception e)
            {
            }
        }

        public void closeTcpListener()
        {
            if (ns!=null) ns.Close();
            if (tcp_client != null) tcp_client.Close();
            if (tcp_socket != null) tcp_socket.Stop();
            if (thrSvr != null) thrSvr.Abort();
            if (thrJobQueue !=null ) thrJobQueue.Abort();

        }

        public void msgClient(string serverMessage)
        {
         //   System.Console.Out.WriteLine("msgClient: " + serverMessage);
            if ((ns!=null)&&(tcpStreamWriter!=null))
            {
                lock (tcpStreamWriter)
                {
                    if (serverMessage != "")
                        tcpStreamWriter.WriteLine(serverMessage);

                    tcpStreamWriter.WriteLine();
                    ns.Write(Encoding.ASCII.GetBytes(serverMessage.ToCharArray()), 
                             0, serverMessage.Length);
                }
            }

        }
        public void overwrite2Hash(Hashtable hashTable, string key, string value)
        {
            if (hashTable.ContainsKey(key)) hashTable.Remove(key);
            hashTable.Add(key, value);
            //output("  +Hash :('" + key + "' , " + value + ")");
        }

        public string getWithDefault(Hashtable hashTable, string key, string defaultValue)
        {
            if (hashTable.ContainsKey(key)) return hashTable[key].ToString();
            return defaultValue;
        }
        public string EvaluateCommand(string cmd)
        {
            return "";
        }
        public string EvaluateXmlCommand(string xcmd)
        {
            output("EvaluateXmlCommand :" + xcmd);

            string response = "<request>\r\n <cmd>" + xcmd + "</cmd>\r\n <response>null</response>\r\n</request>";
            try
            {
                if (xcmd.Contains(".xlsp"))
                {
                    return XML2Lisp(xcmd);
                }


                int depth = 0;
                XmlDocument xdoc = new XmlDocument();
                XmlTextReader reader;
                StringReader stringReader;
                if (xcmd.Contains("http:") || xcmd.Contains(".xml"))
                {
                    // assuming its a file
                    xcmd = xcmd.Trim();
                    reader = new XmlTextReader(xcmd);
                    xdoc.Load(xcmd);
                }
                else
                {
                    // otherwise just use the string
                    stringReader = new System.IO.StringReader(xcmd);
                    reader = new XmlTextReader(stringReader);
                    xdoc.LoadXml(xcmd);
                }

                Hashtable[] attributeStack = new Hashtable[16];


                string[] strURI = new String[16];
                string[] strName = new String[16];
                string[] strPath = new String[16];

                string totalResponse = "";
                for (int i = 0; i < 16; i++) { attributeStack[i] = new Hashtable(); }

                while (reader.Read())
                {
                    depth = reader.Depth + 1;
                    switch (reader.NodeType)
                    {

                        case XmlNodeType.Element:
                            //Hashtable attributes = new Hashtable();
                            strURI[depth] = reader.NamespaceURI;
                            strName[depth] = reader.Name;
                            strPath[depth] = strPath[depth - 1] + "." + strName[depth];
                            if (reader.HasAttributes)
                            {
                                for (int i = 0; i < reader.AttributeCount; i++)
                                {
                                    reader.MoveToAttribute(i);
                                    string attributeName = reader.Name;
                                    string attributeValue = reader.Value;
                                    string attributePath = "";
                                    if ((attributeName == "name") && ((strName[depth] == "param") || (strName[depth] == "feeling")))
                                    {
                                        // so you can have multiple named params
                                        strPath[depth] = strPath[depth] + "." + attributeValue;
                                    }
                                    if (depth > 1)
                                    {
                                        attributePath = strPath[depth] + "." + attributeName;
                                    }
                                    else
                                    {
                                        attributePath = attributeName;
                                    }
                                    overwrite2Hash(attributeStack[depth], attributeName, attributeValue);
                                    // zero depth contains the fully qualified nested dotted value
                                    // i.e. pet-action-plan.action.param.vector.x
                                    // i.e. pet-action-plan.action.param.entity.value
                                    overwrite2Hash(attributeStack[0], attributePath, attributeValue);
                                }
                            }
                            overwrite2Hash(attributeStack[depth], "ElementName", strName[depth]);
                            overwrite2Hash(attributeStack[depth], "Path", strPath[depth]);
                            xStartElement(strURI[depth], strName[depth], attributeStack[depth], depth, attributeStack);
                            if (reader.IsEmptyElement)
                            {
                                // do whatever EndElement would do
                                response = xEndElement(strURI[depth], strName[depth], attributeStack[depth], depth, attributeStack);
                                totalResponse += response + "\r\n";

                            }
                            break;
                        //
                        //you can handle other cases here
                        //

                        case XmlNodeType.Text:
                            // Todo
                            output(" TextNode: depth=" + depth.ToString() + "  path = " + strPath[depth - 1]); ;
                            if (reader.Name == "param")
                            {
                                overwrite2Hash(attributeStack[depth], strPath[depth - 1] + ".param." + strName[depth] + ".InnerText", reader.Value);
                                overwrite2Hash(attributeStack[0], strPath[depth - 1] + ".param." + strName[depth] + ".InnerText", reader.Value);
                            }
                            else
                            {

                                overwrite2Hash(attributeStack[depth], strPath[depth - 1] + ".InnerText", reader.Value);
                                overwrite2Hash(attributeStack[0], strPath[depth - 1] + ".InnerText", reader.Value);
                            }
                            break;

                        case XmlNodeType.EndElement:
                            response = xEndElement(strURI[depth], strName[depth], attributeStack[depth], depth, attributeStack);
                            totalResponse += response + "\r\n";
                            // Todo
                            //depth--;
                            break;
                        default:
                            break;
                    } //switch
                } //while
                string finalResponse = "<pet-petaverse-msg>\r\n" + totalResponse + "</pet-petaverse-msg>\r\n";
                return finalResponse;
            } //try
            catch (Exception e)
            {
                output("error occured: " + e.Message);
                output("        Stack: " + e.StackTrace.ToString());
                return "<error><response>" + response + "</response><errormsg>" + e.Message.ToString() + "</errormsg> </error>";
            }
        }

        public void xStartElement(string strURI, string strName, Hashtable attributes, int depth, Hashtable[] attributeStack)
        {
            output("   xStartElement: strURI =(" + strURI + ") strName=(" + strName + ") depth=(" + depth + ")");
        }

        public string xEndElement(string strURI, string strName, Hashtable attributes, int depth, Hashtable[] attributeStack)
        {
            try
            {
                output("   xEndElement: strURI =(" + strURI + ") strName=(" + strName + ") depth=(" + depth + ")");
                if (strName == "action")
                {
                    string act = attributes["name"].ToString();
                    string seqid = attributes["sequence"].ToString();
                    string planID = getWithDefault(attributeStack[1], "id", "unknown");

                    if (act == "say")
                    {
                        string actCmd = act + " " + getWithDefault(attributeStack[0], ".pet-action-plan.action.InnerText", "");
                        string evalReply = EvaluateCommand(actCmd);
                        string actSignal = genActReport(planID, seqid, act, "done");
                        return actSignal;
                    }
                    if (act == "wear")
                    {
                        string actCmd = act + " " + getWithDefault(attributeStack[0], ".pet-action-plan.action.InnerText", "");
                        string evalReply = EvaluateCommand(actCmd);
                        string actSignal = genActReport(planID, seqid, act, "done");
                        return actSignal;
                    }
                    if (act == "follow")
                    {
                        string TargetName = getWithDefault(attributeStack[0], ".pet-action-plan.action.param.target.entity.value", "");

                        string actCmd = act + " " + TargetName;
                        string evalReply = EvaluateCommand(actCmd);
                        string actSignal = genActReport(planID, seqid, act, "done");
                        return actSignal;
                    }

                }
                /*
                 * if (strName == "param")
                {
                    string paramName = attributes["name"].ToString();
                    string paramType = attributes["type"].ToString();
                    string paramValue = attributes["value"].ToString();
                    string paramText = attributes["InnerText"].ToString();
                }
                 */

                return "<response>null</response>";
            }
            catch (Exception e)
            {
                output("error occured: " + e.Message);
                output("        Stack: " + e.StackTrace.ToString());
                return "<error>" + e.Message + "</error>";
            }
        }

        public string genActReport(string planID, string seqID, string act, string status)
        {
            DateTime dt = DateTime.Now;
            
            string actReport = "  <pet-signal pet-name='" + client.Self.Name.ToString()
                                       + "' pet-id='" + client.Self.AgentID.ToString()
                                       + "' timestamp='" + dt.ToString()
                                       + "' action-plan-id='" + planID
                                       + "' sequence='" + seqID
                                       + "' name='" + act
                                       + "' status='" + status + "'/>";
            output("actReport:" + actReport);
            return actReport;
        }

        public string XML2Lisp(string xcmd)
        {
            try
            {
                XmlDocument xdoc = new XmlDocument();
                XmlTextReader reader;
                StringReader stringReader;
                if (xcmd.Contains("http:") || xcmd.Contains(".xml") || xcmd.Contains(".xlsp"))
                {
                    // assuming its a file
                    xcmd = xcmd.Trim();
                    reader = new XmlTextReader(xcmd);
                    xdoc.Load(xcmd);
                }
                else
                {
                    // otherwise just use the string
                    stringReader = new System.IO.StringReader(xcmd);
                    reader = new XmlTextReader(stringReader);
                    xdoc.LoadXml(xcmd);
                }

                Hashtable[] attributeStack = new Hashtable[64];
                String lispCodeString = "";

                for (int i = 0; i < 64; i++) { attributeStack[i] = new Hashtable(); }
                int depth = 0;

                while (reader.Read())
                {
                    depth = reader.Depth + 1;
                    if (attributeStack[depth] == null) { attributeStack[depth] = new Hashtable(); }
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
                           // output(" X2L Begin(" + depth.ToString() + ") " + attributeStack[depth]["name"].ToString());
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
                            //output(" X2L TEXT(" + depth.ToString() + ") " + reader.Name);

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
                output("XML2Lisp =>'" + lispCodeString + "'");
                //string results = evalLispString(lispCodeString);
                string results = "'(enqueued)";
                enqueueLispTask(lispCodeString);
                return results;
            } //try
            catch (Exception e)
            {
                output("error occured: " + e.Message);
                output("        Stack: " + e.StackTrace.ToString());
                return "()";
            }


        }

        //------------------------------------ 
        // OUR NANO LISP JOB QUEUE SYSTEM
        //------------------------------------
        public class subtask
        {
            public Boolean requeue; // should we be re-entered to the queue
            public String code;    // the lisp code as a string
            public String results; // the evaluation results as a string
            public Object codeTree; // the lisp code as an evaluatable object

        }
        Interpreter taskInterperter;

        public void initTaskInterperter()
        {
            try
            {
                taskQueue = new Queue();
                taskInterperter = new Interpreter();
                taskInterperter.LoadFile("boot.lisp");
                taskInterperter.LoadFile("extra.lisp");
                taskInterperter.LoadFile("cogbot.lisp");
                // load the initialization string
                if (config.startupLisp.Length > 1)
                {
                    enqueueLispTask(config.startupLisp);
                }
            }
            catch (Exception e)
            {
                output("!Exception: " + e.GetBaseException().Message);
                output("error occured: " + e.Message);
                output("        Stack: " + e.StackTrace.ToString());
            }

        }

        public Object genLispCodeTree(string lispCode)
        {
            Object codeTree = null;
            try
            {
                StringReader stringCodeReader = new System.IO.StringReader(lispCode);
                codeTree = taskInterperter.Read("console", stringCodeReader);
                if (taskInterperter.Eof(codeTree))
                    return null;
            }
            catch
            {
                throw;
            }
            return codeTree;
        }

        public void enqueueLispTask(string lispCode)
        {
            try
            {
                subtask thisTask = new subtask();
                thisTask.requeue = false;
                thisTask.code = lispCode;
                thisTask.results = "";
                thisTask.codeTree = genLispCodeTree(thisTask.code);
                lock (taskQueue)
                {
                    taskQueue.Enqueue(thisTask);
                }
            }
            catch (Exception e)
            {
                output("!Exception: " + e.GetBaseException().Message);
                output("error occured: " + e.Message);
                output("        Stack: " + e.StackTrace.ToString());
                output("     LispCode: " + lispCode);
            }
        }

        public void jobManager()
        {
            try
            {
                initTaskInterperter();
                while (true)
                {
                    taskTick();
                    Thread.Sleep(50);
                }
            }
            catch (Exception e)
            {
                output("!Exception: " + e.GetBaseException().Message);
                output("error occured: " + e.Message);
                output("        Stack: " + e.StackTrace.ToString());
            }
        }

        public void taskTick()
        {
            string lastcode="";
            try
            {
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
                taskInterperter.Intern("thisClient", this);
                taskInterperter.Intern("Client", this);
                taskInterperter.Intern("thisTask", thisTask);
                //should make the following safer ...
                //taskInterperter.Intern("tcpReader", tcpStreamReader);
                //taskInterperter.Intern("tcpWriter", tcpStreamWriter);
                //a safer way is to have a serverMessage string that is sent to the client
                // in a more thread safe async way
                taskInterperter.Intern("serverMessage", serverMessage);
                //interpreter.Intern("Client",Command.Client);

                // EVALUATE !!!
                Object x = taskInterperter.Eval(thisTask.codeTree);
                thisTask.results = taskInterperter.Str(x);
                if ((serverMessage != "") && (ns != null) && (tcpStreamWriter != null))
                {
                    // lock it only if we need to and its there to use
                    lock (tcpStreamWriter)
                    {

                        tcpStreamWriter.WriteLine(serverMessage);
                        tcpStreamWriter.WriteLine();
                        ns.Write(Encoding.ASCII.GetBytes(serverMessage.ToCharArray()), 0, serverMessage.Length);
                    }
                }

                if (false)
                {
                    output(" taskcode: " + lastcode + " --> " + thisTask.results);
                    //output(" taskTick Results>" + thisTask.results);
                    //output(" taskTick continueTask=" + thisTask.requeue.ToString());
                }

                // Should we do again ?
                if (thisTask.requeue == true)
                {
                    if (!lastcode.Equals(thisTask.code))
                    {
                        // not the same so must "re-compile"
                        thisTask.codeTree = genLispCodeTree(thisTask.code);
                    }
                    lock (taskQueue)
                    {
                        taskQueue.Enqueue(thisTask);
                    }
                }
                return;
            }
            catch (Exception e)
            {
                output("!Exception: " + e.GetBaseException().Message);
                output("error occured: " + e.Message);
                output("        Stack: " + e.StackTrace.ToString());
                output("     LispCode: " + lastcode);
            }

        }

        public string evalLispString(string lispCode)
        {
            Interpreter interpreter = new Interpreter();
            StringReader stringCodeReader = new System.IO.StringReader(lispCode);
            string results = "'(unevaluated)";
            subtask thisTask = new subtask();
            thisTask.requeue = false;

            try
            {
                output("Load Boot:" + interpreter.LoadFile("boot.lisp").ToString());
                output("Load Boot:" + interpreter.LoadFile("extra.lisp").ToString());
                interpreter.Intern("thisClient", this);
                interpreter.Intern("thisTask", thisTask);

            }
            catch (Exception e)
            {
                Console.Error.WriteLine(e.ToString());
            }
            try
            {
                //lispCode = "(load-assembly \"libsecondlife\")\r\n" + lispCode;

                output("Eval> " + lispCode);
                Object r = null;
                try
                {
                    r = interpreter.Read("console", stringCodeReader);
                    if (interpreter.Eof(r))
                        return results;
                }
                catch
                {
                    throw;
                }
                Object x = interpreter.Eval(r);
                results = interpreter.Str(x);
                output("Results>" + results);
                output("continueTask=" + thisTask.requeue.ToString());
                return results;
            }
            catch (Exception e)
            {
                output("!Exception: " + e.GetBaseException().Message);
                output("error occured: " + e.Message);
                output("        Stack: " + e.StackTrace.ToString());
                return results;
            }
// TODO's
        // Play Animations
        // private static UUID type_anim_uuid = new UUID("c541c47f-e0c0-058b-ad1a-d6ae3a4584d9");
        // client.Self.AnimationStart(type_anim_uuid,false);
        // client.Self.AnimationStop(type_anim_uuid,false);

            // animationFolder = client.Inventory.FindFolderForType(AssetType.Animation);
            // animationUUID = client.Inventory.FindObjectByPath(animationFolder, client.Self.AgentID, AnimationPath, 500);
            // client.Self.AnimationStart(animationLLUUID,false);
            // client.Self.AnimationStop(animationLLUUID,false);

        
        // Reflect events into lisp
        // 

        }

        private void TextForm_Load(object sender, EventArgs e)
        {
            cycConnection = new DotCYC.CycConnectionForm();
        }

        private void consoleText_TextChanged(object sender, EventArgs e)
        {

        }

        private void cycConnectionToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (cycConnection == null || cycConnection.IsDisposed) cycConnection = new DotCYC.CycConnectionForm();           
            cycConnection.Reactivate();
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
        string _TcpIpAddress;
        string _FirstName;
        string _LastName;
        string _Password;
        string _SimURL;
        string _startupLisp;

        public Configuration()
        {
            _Version = 1;
            _FirstName = "";
            _LastName = "";
            _Password = "";
            _SimURL = "http://127.0.0.1:8002/"; // The sim server we talk to
            _TcpPort = 5555; // The TCP port WE provide
            _TcpIpAddress = "127.0.0.1"; // The IPAddress WE Listen on
            _startupLisp = "";
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

            }
            catch (Exception e)
            {
            }

        }

        public void saveConfig()
        {
            try
            {
                 Configuration.Serialize("botconfig.xml",this);
            }
            catch(Exception e)
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
    }
    #endregion

#pragma warning restore 0168

}
