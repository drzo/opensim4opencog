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
using cogbot.ScriptEngines;
using OpenMetaverse.Packets;
using cogbot.Actions;
//using DotLisp;


namespace cogbot
{
#pragma warning disable 0168

    public delegate void OutputDelegate(string str);
    public delegate void DescribeDelegate(bool detailed);
    enum Modes { normal, tutorial };

    public partial class TextForm : Form
    {

        static public TextForm SingleInstance = null;
        public static int debugLevel = 2;
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
        public DotCYC.CycConnectionForm cycConnection;
        ///public Dictionary<string, DescribeDelegate> describers;

       // public Dictionary<string, Listeners.Listener> listeners;
        public Dictionary<string, Actions.Action> groupActions;
        public Dictionary<string, Tutorials.Tutorial> tutorials;

        public bool describeNext;
        private int describePos;
        private string currTutorial;

        public int BoringNamesCount = 0;
        public int GoodNamesCount = 0;
        public int RunningMode = (int)Modes.normal;
        public UUID AnimationFolder = UUID.Zero;

        public Queue taskQueue;
        public Thread thrJobQueue;
        //InventoryEval searcher =null; // new InventoryEval(this);
        //public Inventory Inventory;
        //public InventoryManager Manager;
        public Configuration config;
        //Utilities.TcpServer UtilitiesTcpServer;
        public String taskInterperterType = "DotLispInterpreter";// DotLispInterpreter,CycInterpreter or ABCLInterpreter
        static List<LoginDetails> accounts = new List<LoginDetails>();
        public static ClientManager clientManager = new ClientManager(accounts, false);
        public TextForm()
        {
            SingleInstance = this;
            config = new Configuration();
            config.loadConfig();
            BotClient.nextTcpPort = config.tcpPort;
            //LoginDetails details = GetDetailsFromConfig(config);
          //  CurrentClient = new BotClient(clientManager);// clientManager.Login(details);
          //  CurrentClient.TextFormClient(this);
            //GroupMembers = new Dictionary<UUID, GroupMember>();
            //Appearances = new Dictionary<UUID, AvatarAppearancePacket>();

           // CurrentClient.Settings.ALWAYS_DECODE_OBJECTS = true;
           // CurrentClient.Settings.ALWAYS_REQUEST_OBJECTS = true;
          //  CurrentClient.Settings.OBJECT_TRACKING = true;

            //Manager = CurrentClient.Inventory;
            //Inventory = Manager.Store;
            groupActions = new Dictionary<string, Action>();
            groupActions["login"] = new Login(null);

         //   CurrentClient.Settings.LOGIN_SERVER = config.simURL;
           // extraSettings();


           // CurrentClient.Network.OnConnected += new NetworkManager.ConnectedCallback(Network_OnConnected);
           // CurrentClient.Network.OnDisconnected += new NetworkManager.DisconnectedCallback(Network_OnDisconnected);
           // CurrentClient.Network.OnLogin += new NetworkManager.LoginCallback(Network_OnLogin);

            outputDelegate = new OutputDelegate(doOutput);

            //listeners = new Dictionary<string, cogbot.Listeners.Listener>();
            //listeners["chat"] = new Listeners.Chat(this);
            //listeners["avatars"] = new Listeners.Avatars(this);
            //listeners["teleport"] = new Listeners.Teleport(this);
            //listeners["whisper"] = new Listeners.Whisper(this);
            //ObjectSystem = new Listeners.Objects(this);
            //listeners["bump"] = new Listeners.Bump(this);
            //listeners["sound"] = new Listeners.Sound(this);


            tutorials = new Dictionary<string, cogbot.Tutorials.Tutorial>();
          //  tutorials["tutorial1"] = new Tutorials.Tutorial1(this);
    
            describeNext = true;

            InitializeComponent();
            Show();
            consoleInputText.Enabled = true;
            consoleInputText.Focus();
         //   RegisterAllCommands(Assembly.GetExecutingAssembly());

         //   UtilitiesTcpServer = new cogbot.Utilities.TcpServer(this);
            // Start the server
            ///UtilitiesTcpServer.startSocketListener();
            taskQueue = new Queue();
            thrJobQueue = new Thread(jobManager);
            thrJobQueue.Start();

            //Client.Network.RegisterCallback(PacketType.AgentDataUpdate, new NetworkManager.PacketCallback(AgentDataUpdateHandler));
          //  CurrentClient.Network.OnLogin += new NetworkManager.LoginCallback(LoginHandler);
            //Client.Self.OnInstantMessage += new AgentManager.InstantMessageCallback(Self_OnInstantMessage);
            //Client.Groups.OnGroupMembers += new GroupManager.GroupMembersCallback(GroupMembersHandler);
            //Client.Inventory.OnObjectOffered += new InventoryManager.ObjectOfferedCallback(Inventory_OnInventoryObjectReceived);

            //Client.Network.RegisterCallback(PacketType.AvatarAppearance, new NetworkManager.PacketCallback(AvatarAppearanceHandler));
            //Client.Network.RegisterCallback(PacketType.AlertMessage, new NetworkManager.PacketCallback(AlertMessageHandler));


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
        public bool ExecuteCommand(string text)
        {
           text = text.Replace("\"", "");
           output("textform> " + text);
           string verb = text.Split(null)[0];
            if (groupActions.ContainsKey(verb))
            {
                if (text.Length > verb.Length)
                    groupActions[verb].acceptInputWrapper(verb, text.Substring(verb.Length + 1));
                else
                    groupActions[verb].acceptInputWrapper(verb, "");
                return true;
            }
            if (clientManager.BotByName.Count == 0) return clientManager.lastBotClient.ExecuteCommand(text);
            bool handled = false;
            foreach (BotClient CurrentClient in clientManager.BotByName.Values)
            if (CurrentClient!=null)
            {
                if (CurrentClient.ExecuteCommand(text)) handled = true;
            }
            
            if (!handled) {
                output("I don't understand the verb " + verb + ".");
                output("Type \"help\" for help.");
            }
            return handled;
        }


        private void TextForm_FormClosed(object sender, FormClosedEventArgs e)
        {
            ShutDown();
            //UtilitiesTcpServer.closeTcpListener();
        }

        private void ShutDown()
        {
            logout();
            foreach (BotClient CurrentClient in clientManager.BotByName.Values)
            {
                CurrentClient.ShutDown();
            }
            Application.DoEvents();
            thrJobQueue.Abort();
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
            foreach (BotClient CurrentClient in clientManager.Clients.Values)
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
            foreach (BotClient CurrentClient in clientManager.Clients.Values)
                if (CurrentClient.Network.Connected)
                CurrentClient.Network.Logout();
            config.saveConfig();
        }

        public void output(string str)
        {
            try
            {
                str = str.Replace("\r\n", "\n").Replace("\r", "\n").Replace("\n", "\r\n");
                if (str.ToLower().Contains("look")) return;
                if (IsDisposed) return; // for (un)clean exits
                this.Invoke(outputDelegate,str);
            }
            catch (Exception e)
            {
                if (IsDisposed)
                {
                    logout();
                    Application.Exit();
                }
            }
        }

        public void doOutput(string str)
        {
            try
            {
                lock (consoleText)
                {
                    if (consoleText.IsDisposed) return; // for (un)clean exits
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

                describeNext = !ExecuteCommand(text);

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
        //public void describeAll()
        //{
        //    CurrentClient.describeAll();
        //}

        //public void describeSituation()
        //{
        //    CurrentClient.describeSituation();
        //}
    
    
        //public void msgClient(string serverMessage)
        //{
        //    if (debugLevel>1)
        //    {
        //        output("msgClient: " + serverMessage);                
        //    } // if
              
           
        // //   System.Console.Out.WriteLine("msgClient: " + serverMessage);
        //    UtilitiesTcpServer.msgClient(serverMessage);

        //}
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
            public Object results; // the evaluation results as a string
            public Object codeTree; // the lisp code as an evaluatable object

        }
        ScriptInterpreter taskInterperter;

        public void initTaskInterperter()
        {
            try
            {
                taskQueue = new Queue();
                output("Start Loading TaskInterperter ... '" + taskInterperterType + "' \n");
                taskInterperter = ScriptEngines.ScriptManager.LoadScriptInterpreter(taskInterperterType);
                taskInterperter.LoadFile("boot.lisp");
                taskInterperter.LoadFile("extra.lisp");
                taskInterperter.LoadFile("cogbot.lisp");
                // load the initialization string
                if (config.startupLisp.Length > 1)
                {
                    enqueueLispTask("(progn " + config.startupLisp + ")");
                }
                output("Completed Loading TaskInterperter '" + taskInterperterType + "'\n");
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
                    while (taskQueue.Count > 0)
                    {
                        taskTickForTCP();
                        Thread.Sleep(1);
                    }
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

        public void taskTickForTCP()
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
                taskInterperter.Intern("textForm", this);
                taskInterperter.Intern("clientManager", clientManager);
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
               // UtilitiesTcpServer.taskTick(serverMessage);

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
            ScriptInterpreter interpreter = taskInterperter.newInterpreter();
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
        // Client.Self.AnimationStart(type_anim_uuid,false);
        // Client.Self.AnimationStop(type_anim_uuid,false);

            // animationFolder = Client.Inventory.FindFolderForType(AssetType.Animation);
            // animationUUID = Client.Inventory.FindObjectByPath(animationFolder, Client.Self.AgentID, AnimationPath, 500);
            // Client.Self.AnimationStart(animationLLUUID,false);
            // Client.Self.AnimationStop(animationLLUUID,false);

        
        // Reflect events into lisp
        // 

        }

        private void TextForm_Load(object sender, EventArgs e)
        {           
            //cycConnection = new DotCYC.CycConnectionForm();
        }

        private void consoleText_TextChanged(object sender, EventArgs e)
        {

        }

        private void cycConnectionToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (cycConnection == null || cycConnection.IsDisposed) cycConnection = new DotCYC.CycConnectionForm();           
            cycConnection.Reactivate();
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
