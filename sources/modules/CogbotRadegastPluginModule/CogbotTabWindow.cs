// 
// Radegast Metaverse Client
// Copyright (c) 2009, Radegast Development Team
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright notice,
//       this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//     * Neither the name of the application "Radegast", nor the names of its
//       contributors may be used to endorse or promote products derived from
//       this software without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
// $Id: ConferenceIMTabWindow.cs 104 2009-07-14 20:14:54Z latifer $
//
using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text.RegularExpressions;
using System.Windows.Forms;
using System.Threading;
using cogbot;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using Radegast;
using Radegast.Netcom;
using OpenMetaverse;
using cogbot.Utilities;
//using RadegastTab = Radegast.SleekTab;
//using RadegastMovement = Radegast.SleekMovement;


namespace CogbotRadegastPluginModule
{
    public partial class CogbotTabWindow : UserControl
    {
        private RadegastInstance instance;
        private RadegastNetcom netcom
        {
            get { return instance.Netcom; }
        }
        private GridClient client
        {
            get { return instance.Client; }
        }
        private TabsConsole tabConsole
        {
            get { return instance.TabConsole; }
        }
        private CogbotRadegastPlugin chatManager;
        public CogbotRadegastPlugin ChatManager
        {
            get { return chatManager; }
        }
        public BotClient TheBot
        {
            get { return ChatManager.TheBot; }
        }

        private Avatar currentAvatar;
        private RadegastMovement  movement;
        //    private AIMLbot.Bot Alice;
        private Hashtable AliceUsers = new Hashtable();
        private Avatar.AvatarProperties myProfile;
        private Regex chatRegex = new Regex(@"^/(\d+)\s*(.*)", RegexOptions.Compiled);
        private Dictionary<uint, Avatar> avatars = new Dictionary<uint, Avatar>();
        private Dictionary<uint, bool> bots = new Dictionary<uint, bool>();
        private RichTextBoxPrinter printer;
        private SimObjectSorterClass simObjectSorterClass;
        private WorldObjects GridMaster
        {
            get { return WorldObjects.GridMaster; }
        }


        public RadegastContextMenuStrip PluginExtraContextMenu { get { return ExtraContextMenu; } }

        public CogbotTabWindow(RadegastInstance instance, CogbotRadegastPlugin man)
        {
            InitializeComponent();
            this.instance = instance;
            Disposed += new EventHandler(ChatConsole_Disposed);

            if (!instance.advancedDebugging)
            {
//                tbtnAnim.Visible = false;
  //              tbtnTextures.Visible = false;

                ctxAnim.Visible = false;
                ctxTextures.Visible = false;
            }

            ctxAnim.Visible = true;
            ctxTextures.Visible = true;



            // Callbacks
            netcom.ClientLoginStatus += new EventHandler<LoginProgressEventArgs>(netcom_ClientLoginStatus);
            //netcom.ClientLoggedOut += new EventHandler(netcom_ClientLoggedOut);
            //  netcom.ChatReceived += new EventHandler<ChatEventArgs>(netcom_ChatReceived);
            //netcom.InstantMessageReceived += new EventHandler<InstantMessageEventArgs>(netcom_InstantMessageReceived);
            //this.instance.Config.ConfigApplied += new EventHandler<ConfigAppliedEventArgs>(Config_ConfigApplied);
            // client.Grid.OnCoarseLocationUpdate += new GridManager.CoarseLocationUpdateCallback(Grid_OnCoarseLocationUpdate);
            //client.Avatars.OnAvatarProperties += new AvatarManager.AvatarPropertiesCallback(Avatars_OnAvatarProperties);

            // movement = new SleekMovement(client);
            printer = new RichTextBoxPrinter(rtbChat);
            chatManager = man;
            //chatManager.PrintStartupMessage();

            this.instance.MainForm.Load += new EventHandler(MainForm_Load);
            this.VisibleChanged += Form_VisibleChanged;

            simObjectSorterClass = new SimObjectSorterClass();
            lvwObjects.ListViewItemSorter = simObjectSorterClass;

            //Alice = new AIMLbot.Bot();
            //Alice.isAcceptingUserInput = false;
            
            //try {
            //    Alice.loadSettings();
            //    AIMLbot.Utils.AIMLLoader loader = new AIMLbot.Utils.AIMLLoader(Alice);
            //    Alice.isAcceptingUserInput = false;
            //    loader.loadAIML(Alice.PathToAIML);
            //    Alice.isAcceptingUserInput = true;
            //} catch (Exception ex) {
            //    System.Console.WriteLine("Failed loading ALICE: " + ex.Message);
            //}
            instance.Client.Self.Movement.UseOnlyThreads.Add(Thread.CurrentThread);
           // ApplyConfig(this.instance.Config.CurrentConfig);
            //ClientManager.SingleInstance.Clients[]
            //BotClien.OnlyOneCurrentBotClient
            this.cbxInput.Enabled = true;
        }

        private void Form_VisibleChanged(object sender, EventArgs e)
        {

        }

        public void StartWriter()
        {
            if (!writeLock.IsRunning) writeLock.Start();
        }

        void ChatConsole_Disposed(object sender, EventArgs e)
        {
            netcom.ClientLoginStatus -= new EventHandler<LoginProgressEventArgs>(netcom_ClientLoginStatus);
            netcom.ClientLoggedOut -= new EventHandler(netcom_ClientLoggedOut);
            //   netcom.ChatReceived -= new EventHandler<ChatEventArgs>(netcom_ChatReceived);
            netcom.InstantMessageReceived -= new EventHandler<InstantMessageEventArgs>(netcom_InstantMessageReceived);
          //  this.instance.Config.ConfigApplied -= new EventHandler<ConfigAppliedEventArgs>(Config_ConfigApplied);
            //client.Grid.CoarseLocationUpdate -= new GridManager.CoarseLocationUpdateCallback(Grid_OnCoarseLocationUpdate);
            client.Avatars.AvatarPropertiesReply -= new EventHandler<AvatarPropertiesReplyEventArgs>(Avatars_OnAvatarProperties);
            writeLock.Dispose();
        }

        void Avatars_OnAvatarProperties(object sender, AvatarPropertiesReplyEventArgs e)
        {
            //if (avatarID == client.Self.AgentID)
            //{
            //    myProfile = properties;
            //    Alice.GlobalSettings.updateSetting("birthday", myProfile.BornOn);
            //    DateTime bd;
            //    if (DateTime.TryParse(myProfile.BornOn, Utils.EnUsCulture, System.Globalization.DateTimeStyles.None, out bd))
            //    {
            //        DateTime now = DateTime.Today;
            //        int age = now.Year - bd.Year;
            //        if (now.Month < bd.Month || (now.Month == bd.Month && now.Day < bd.Day))
            //        {
            //            --age;
            //        }
            //        Alice.GlobalSettings.updateSetting("age", age.ToString());
            //        Alice.GlobalSettings.updateSetting("birthday", bd.ToLongDateString());

            //    }
            //}
        }

        void Grid_OnCoarseLocationUpdate(Simulator sim, List<UUID> newEntries, List<UUID> removedEntries)
        {
            //if (client.Network.CurrentSim.Handle != sim.Handle)
            //{
            //    return;
            //}

            //if (InvokeRequired)
            //{

            //    BeginInvoke(new MethodInvoker(delegate()
            //    {
            //        Grid_OnCoarseLocationUpdate(sim, newEntries, removedEntries);
            //    }));
            //    return;
            //}

            //lvwObjects.BeginUpdate();
            //try
            //{
            //    Vector3 mypos = sim.AvatarPositions.ContainsKey(client.Self.AgentID)
            //                        ? sim.AvatarPositions[client.Self.AgentID]
            //                        : client.Self.SimPosition;

            //    List<UUID> existing = new List<UUID>();
            //    List<UUID> removed = new List<UUID>();

            //    sim.AvatarPositions.ForEach(delegate(KeyValuePair<UUID, Vector3> avi)
            //    {
            //        existing.Add(avi.Key);
            //        if (!lvwObjects.Items.ContainsKey(avi.Key.ToString()))
            //        {
            //            string name = instance.getAvatarName(avi.Key);
            //            ListViewItem item = lvwObjects.Items.Add(avi.Key.ToString(), name, string.Empty);
            //            if (avi.Key == client.Self.AgentID) item.Font = new Font(item.Font, FontStyle.Bold);
            //            item.Tag = avi.Key;
            //        }
            //    });

            //    foreach (ListViewItem item in lvwObjects.Items)
            //    {
            //        UUID key = (UUID)item.Tag;
            //        if (!existing.Contains(key))
            //        {
            //            removed.Add(key);
            //            continue;
            //        }
            //        item.Text = instance.getAvatarName(key);
            //        if (key == client.Self.AgentID)
            //        {
            //            continue;
            //        }
            //        int d = (int)Vector3.Distance(sim.AvatarPositions[key], mypos);
            //        item.Text = instance.getAvatarName(key) + " (" + d + "m)";
            //    }

            //    foreach (UUID key in removed)
            //    {
            //        lvwObjects.Items.RemoveByKey(key.ToString());
            //    }

            //    lvwObjects.Sort();
            //}
            //catch (Exception)
            //{
            //}
            //lvwObjects.EndUpdate();
        }

        private void MainForm_Load(object sender, EventArgs e)
        {
            StartWriter();
            //tabConsole = instance.TabConsole;
        }

        private void StartWriter(object sender, EventArgs e)
        {
            //StartWriter();
            //tabConsole = instance.TabConsole;
        }
        //private void Config_ConfigApplied(object sender, ConfigAppliedEventArgs e)
        //{
        //    ApplyConfig(e.AppliedConfig);
        //}

        //private void ApplyConfig(Config config)
        //{
        //    if (config.InterfaceStyle == 0) //System
        //        toolStrip1.RenderMode = ToolStripRenderMode.System;
        //    else if (config.InterfaceStyle == 1) //Office 2003
        //        toolStrip1.RenderMode = ToolStripRenderMode.ManagerRenderMode;
        //}

        void netcom_InstantMessageReceived(object sender, InstantMessageEventArgs e)
        {
            //if (e.IM.Dialog == InstantMessageDialog.MessageFromAgent 
            //    && Alice.isAcceptingUserInput
            //    && instance.Config.CurrentConfig.UseAlice
            //    && !instance.Groups.ContainsKey(e.IM.IMSessionID)
            //    && e.IM.BinaryBucket.Length < 2
            //    && e.IM.FromAgentName != "Second Life") 
            //{
            //    Alice.GlobalSettings.updateSetting("location", "region " + client.Network.CurrentSim.Name);
            //    AIMLbot.User user;
            //    if (AliceUsers.ContainsKey(e.IM.FromAgentName)) {
            //        user = (AIMLbot.User)AliceUsers[e.IM.FromAgentName];
            //    } else {
            //        user = new User(e.IM.FromAgentName, Alice);
            //        user.Predicates.removeSetting("name");
            //        user.Predicates.addSetting("name", firstName(e.IM.FromAgentName));
            //        AliceUsers[e.IM.FromAgentName] = user;
            //    }
            //    AIMLbot.Request req = new Request(e.IM.Message, user, Alice);
            //    AIMLbot.Result res = Alice.Chat(req);
            //    string msg = res.Output;
            //    if (msg.Length > 1000) {
            //        msg = msg.Substring(0, 1000);
            //    }
            //    netcom.SendInstantMessage(msg, e.IM.FromAgentID, e.IM.IMSessionID);
            //}
        }

        private void netcom_ClientLoginStatus(object sender, LoginProgressEventArgs e)
        {
            if (e.Status != LoginStatus.Success) return;

            cbxInput.Enabled = true;
            btnSay.Enabled = true;
            //btnShout.Enabled = true;
            return;
            GridMaster.OnAddSimObject += WorldSystem_OnAddSimObject;
          //  simObjectSorterClass.Origin = GridMaster.TheSimAvatar;
            //  Alice.GlobalSettings.updateSetting("name", firstName(client.Self.Name));
        }

        private void WorldSystem_OnAddSimObject(SimObject obj)
        {
            if (lvwObjects.InvokeRequired)
            {
                lvwObjects.Invoke(new OnAddSimObjectCallback(WorldSystem_OnAddSimObject), obj);
                return;
            }
            //lvwObjects.Items.Add(new SimObjectListViewItem(obj));
        }

        private void netcom_ClientLoggedOut(object sender, EventArgs e)
        {
           cbxInput.Enabled = true;
            //btnSay.Enabled = false;
            //btnShout.Enabled = false;

            lvwObjects.Items.Clear();
        }

        private string firstName(string name)
        {
            return name.Split(' ')[0];
        }

        //private void netcom_ChatReceived(object sender, ChatEventArgs e)
        //{
        //    if (e.SourceType != ChatSourceType.Agent) return;
        //    if (e.FromName == netcom.LoginOptions.FullName) return;

        //    if (Alice.isAcceptingUserInput && e.Message.ToLower().Contains(firstName(client.Self.Name).ToLower()) && instance.Config.CurrentConfig.UseAlice)
        //    {
        //        Alice.GlobalSettings.updateSetting("location", "region " + client.Network.CurrentSim.Name);
        //        string msg = e.Message.ToLower();
        //        msg = msg.Replace(firstName(client.Self.Name).ToLower(), "");
        //        AIMLbot.User user;
        //        if (AliceUsers.ContainsKey(e.FromName))
        //        {
        //            user = (AIMLbot.User)AliceUsers[e.FromName];
        //        }
        //        else
        //        {
        //            user = new User(e.FromName, Alice);
        //            user.Predicates.removeSetting("name");
        //            user.Predicates.addSetting("name", firstName(e.FromName));
        //            AliceUsers[e.FromName] = user;
        //        }
        //        client.Self.Movement.TurnToward(e.Position);
        //        if (!instance.State.IsTyping)
        //        {
        //            instance.State.SetTyping(true);
        //        }
        //        System.Threading.Thread.Sleep(1000);
        //        instance.State.SetTyping(false);
        //        AIMLbot.Request req = new Request(msg, user, Alice);
        //        AIMLbot.Result res = Alice.Chat(req);
        //        string outp = res.Output;
        //        if (outp.Length > 1000)
        //        {
        //            outp = outp.Substring(0, 1000);
        //        }
        //        ProcessChatInput(outp, ChatType.Normal);
        //    }
        //}

        private void cbxInput_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.KeyCode != Keys.Enter) return;
            e.SuppressKeyPress = true;
            btnSay_Click(sender, e);
        }

        private void ProcessChatInput(string input, ChatType type)
        {
            if (string.IsNullOrEmpty(input)) return;

            string msg;

            if (input.Length >= 1000)
            {
                msg = input.Substring(0, 1000);
            }
            else
            {
                msg = input;
            }

            int ch = 0;
            Match m = chatRegex.Match(msg);

            if (m.Groups.Count > 2)
            {
                ch = int.Parse(m.Groups[1].Value);
                msg = m.Groups[2].Value;
            }

            netcom.ChatOut(msg, type, ch);
            ClearChatInput();
        }

        private void ClearChatInput()
        {
            if (string.IsNullOrEmpty(cbxInput.Text)) return;
            cbxInput.Items.Add(cbxInput.Text);
            cbxInput.Text = string.Empty;
        }

        private void btnSay_Click(object sender, EventArgs e)
        {
            string s = cbxInput.Text;
            BotClient bc = ClientManager.SingleInstance.BotClientFor(this.instance);
            if (bc != null)
            {
                WriteLine("cogbot> " + s);
                WriteLine("" + bc.ExecuteCommand(s, e, WriteLine));
                ClearChatInput();
                return;
            }
            WorldObjects gm = GridMaster;
            if (gm != null)
            {
                WriteLine("gridmaster> " + s);
                WriteLine("" + gm.client.ExecuteCommand(s, e, WriteLine));
                ClearChatInput();
                return;
            }
            WriteLine("Too early: Could not do " + s);
        }

        private void btnShout_Click(object sender, EventArgs e)
        {
            ProcessChatInput(cbxInput.Text, ChatType.Shout);
        }

        private void cbxInput_TextChanged(object sender, EventArgs e)
        {
            return; //no typing animations in cogbot indow
            if (cbxInput.Text.Length > 0)
            {
               // btnSay.Enabled = btnShout.Enabled = true;

                if (!cbxInput.Text.StartsWith("/"))
                {
                    if (!instance.State.IsTyping)
                        instance.State.SetTyping(true);
                }
            }
            else
            {
                //btnSay.Enabled = btnShout.Enabled = false;
                instance.State.SetTyping(false);
            }
        }

        private void tbtnStartIM_Click(object sender, EventArgs e)
        {
            if (lvwObjects.SelectedItems.Count == 0) return;
            UUID av = (UUID)lvwObjects.SelectedItems[0].Tag;
            string name = instance.getAvatarName(av);

            if (tabConsole.TabExists((client.Self.AgentID ^ av).ToString()))
            {
                tabConsole.SelectTab((client.Self.AgentID ^ av).ToString());
                return;
            }

            tabConsole.AddIMTab(av, client.Self.AgentID ^ av, name);
        }

        private void tbtnFollow_Click(object sender, EventArgs e)
        {
            Avatar av = currentAvatar;
            if (av == null) return;

            if (instance.State.FollowName != av.Name)
                instance.State.Follow(av.Name, av.ID);
            else
                instance.State.Follow(string.Empty, UUID.Zero);
        }

        private void ctxPoint_Click(object sender, EventArgs e)
        {
            if (!instance.State.IsPointing)
            {
                Avatar av = currentAvatar;
                if (av == null) return;
                instance.State.SetPointing(av, 5);
                ctxPoint.Text = "Unpoint";
            }
            else
            {
                ctxPoint.Text = "Point at";
                instance.State.UnSetPointing();
            }
        }


        private void lvwObjects_SelectedIndexChanged(object sender, EventArgs e)
        {
            return;
            if (lvwObjects.SelectedItems.Count == 0)
            {
                currentAvatar = null;
//                tbtnStartIM.Enabled = tbtnFollow.Enabled = tbtnProfile.Enabled = tbtnTextures.Enabled = tbtnMaster.Enabled = tbtnAttach.Enabled = tbtnAnim.Enabled = false;
                ctxPay.Enabled = ctxSource.Enabled = ctxPoint.Enabled = ctxStartIM.Enabled = ctxFollow.Enabled = ctxProfile.Enabled = ctxTextures.Enabled = ctxMaster.Enabled = ctxAttach.Enabled = ctxAnim.Enabled = false;
            }
            else
            {
                currentAvatar = client.Network.CurrentSim.ObjectsAvatars.Find(delegate(Avatar a)
                {
                    return a.ID == (UUID)lvwObjects.SelectedItems[0].Tag;
                });

            //    tbtnStartIM.Enabled = tbtnProfile.Enabled = true;
            //    tbtnFollow.Enabled = tbtnTextures.Enabled = tbtnMaster.Enabled = tbtnAttach.Enabled = tbtnAnim.Enabled = currentAvatar != null;

                ctxPay.Enabled = ctxSource.Enabled = ctxStartIM.Enabled = ctxProfile.Enabled = true;
                ctxPoint.Enabled = ctxFollow.Enabled = ctxTextures.Enabled = ctxMaster.Enabled = ctxAttach.Enabled = ctxAnim.Enabled = currentAvatar != null;

                if ((UUID)lvwObjects.SelectedItems[0].Tag == client.Self.AgentID)
                {
              //      tbtnFollow.Enabled = tbtnStartIM.Enabled = false;
                    ctxPay.Enabled = ctxFollow.Enabled = ctxStartIM.Enabled = false;
                }
            }
            if (instance.State.IsPointing)
            {
                ctxPoint.Enabled = true;
            }
        }

        private void rtbChat_LinkClicked(object sender, LinkClickedEventArgs e)
        {
            instance.MainForm.ProcessLink(e.LinkText);
        }

        private void tbtnProfile_Click(object sender, EventArgs e)
        {
            if (lvwObjects.SelectedItems.Count == 0) return;
            UUID av = (UUID)lvwObjects.SelectedItems[0].Tag;
            string name = instance.getAvatarName(av);

            (new frmProfile(instance, name, av)).Show();
        }

        private void cbxInput_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter) e.SuppressKeyPress = true;
        }

        private void dumpOufitBtn_Click(object sender, EventArgs e)
        {
            Avatar av = currentAvatar;
            if (av == null) return;

            if (!instance.TabConsole.TabExists("OT: " + av.Name))
            {
                instance.TabConsole.AddOTTab(av);
            }
            instance.TabConsole.SelectTab("OT: " + av.Name);
        }

        private void tbtnMaster_Click(object sender, EventArgs e)
        {
            Avatar av = currentAvatar;
            if (av == null) return;

            if (!instance.TabConsole.TabExists("MS: " + av.Name))
            {
                instance.TabConsole.AddMSTab(av);
            }
            instance.TabConsole.SelectTab("MS: " + av.Name);
        }

        private void tbtnAttach_Click(object sender, EventArgs e)
        {
            Avatar av = currentAvatar;
            if (av == null) return;
            string tabName = "AT: " + av.Name;
            if (!instance.TabConsole.TabExists(tabName))
            {
                AttachmentTab atTab = new AttachmentTab(instance, av);
                instance.TabConsole.AddTab(tabName, tabName, atTab);
            }
            instance.TabConsole.SelectTab(tabName);
        }

        private void tbtnAnim_Click(object sender, EventArgs e)
        {
            Avatar av = currentAvatar;
            if (av == null) return;

            if (!instance.TabConsole.TabExists("Anim: " + av.Name))
            {
                instance.TabConsole.AddAnimTab(av);
            }
            instance.TabConsole.SelectTab("Anim: " + av.Name);


        }

        private void btnTurnLeft_MouseDown(object sender, MouseEventArgs e)
        {
            movement.TurningLeft = true;
        }

        private void btnTurnLeft_MouseUp(object sender, MouseEventArgs e)
        {
            movement.TurningLeft = false;
        }

        private void btnTurnRight_MouseDown(object sender, MouseEventArgs e)
        {
            movement.TurningRight = true;
        }

        private void btnTurnRight_MouseUp(object sender, MouseEventArgs e)
        {
            movement.TurningRight = false;
        }

        private void btnFwd_MouseDown(object sender, MouseEventArgs e)
        {
            movement.MovingForward = true;
        }

        private void btnFwd_MouseUp(object sender, MouseEventArgs e)
        {
            movement.MovingForward = false;
        }

        private void btnMoveBack_MouseDown(object sender, MouseEventArgs e)
        {
            movement.MovingBackward = true;
        }

        private void btnMoveBack_MouseUp(object sender, MouseEventArgs e)
        {
            movement.MovingBackward = false;
        }

        private void pnlMovement_Click(object sender, EventArgs e)
        {
            client.Self.Jump(true);
            System.Threading.Thread.Sleep(500);
            client.Self.Jump(false);
        }

        private void lvwObjects_DragDrop(object sender, DragEventArgs e)
        {
            Point local = lvwObjects.PointToClient(new Point(e.X, e.Y));
            ListViewItem litem = lvwObjects.GetItemAt(local.X, local.Y);
            if (litem == null) return;
            TreeNode node = e.Data.GetData(typeof(TreeNode)) as TreeNode;
            if (node == null) return;

            if (node.Tag is InventoryItem)
            {
                InventoryItem item = node.Tag as InventoryItem;
                client.Inventory.GiveItem(item.UUID, item.Name, item.AssetType, (UUID)litem.Tag, true);
                chatManager.DisplayNotificationInChat("Offered item " + item.Name + " to " + instance.getAvatarName((UUID)litem.Tag) + ".");
            }
            else if (node.Tag is InventoryFolder)
            {
                InventoryFolder folder = node.Tag as InventoryFolder;
                client.Inventory.GiveFolder(folder.UUID, folder.Name, AssetType.Folder, (UUID)litem.Tag, true);
                chatManager.DisplayNotificationInChat("Offered folder " + folder.Name + " to " + instance.getAvatarName((UUID)litem.Tag) + ".");
            }
        }

        private void lvwObjects_DragOver(object sender, DragEventArgs e)
        {
            Point local = lvwObjects.PointToClient(new Point(e.X, e.Y));
            ListViewItem litem = lvwObjects.GetItemAt(local.X, local.Y);
            if (litem == null) return;

            if (!e.Data.GetDataPresent(typeof(TreeNode))) return;

            e.Effect = DragDropEffects.Copy;
        }

        private void avatarContext_Opening(object sender, CancelEventArgs e)
        {
            if (lvwObjects.SelectedItems.Count == 0 && !instance.State.IsPointing)
            {
                e.Cancel = true;
            }
            else if (instance.State.IsPointing)
            {
                ctxPoint.Enabled = true;
                ctxPoint.Text = "Unpoint";
            }
        }

        private void ctxSource_Click(object sender, EventArgs e)
        {
            if (lvwObjects.SelectedItems.Count != 1) return;

            // instance.State.EffectSource = (UUID)lvwObjects.SelectedItems[0].Tag;
        }

        private void ctxPay_Click(object sender, EventArgs e)
        {
            if (lvwObjects.SelectedItems.Count != 1) return;
            (new frmPay(instance, (UUID)lvwObjects.SelectedItems[0].Tag, instance.getAvatarName((UUID)lvwObjects.SelectedItems[0].Tag), false)).ShowDialog();
        }


        private readonly TaskQueueHandler writeLock = new TaskQueueHandler("FormWriter", TimeSpan.Zero, false);        
        public void WriteLine(string str, params object[] args)
        {

            if (args == null || args.Length == 0)
            {
                args = new object[] { str };
                str = "{0}";
            }
            try
            {
                if (str == null) return;
                str = String.Format(str, args);
                str = str.Trim();
                if (str == "") return;

                str = str.Replace("\r\n", "\n").Replace("\r", "\n").Replace("\n", "\r\n");
                //if (str.ToLower().Contains("look")) return;
                if (IsDisposed) return; // for (un)clean exits
                //if (printer.InvokeRequired)
                //{
                //    //new Thread(() =>
                //    printer.Invoke(new OutputDelegate(doOutput), str, new object[0]);
                //    //).Start();

                //}
                //else
                {
                    // new Thread(() => this.Invoke(new OutputDelegate(doOutput), str, new object[0])).Start();//  new Thread(() => doOutput(str)).Start();
                    doOutput(str);
                }
            }
            catch (Exception ex)
            {
                Logger.Log("CogbotTabWindow exception " + ex, Helpers.LogLevel.Error, ex);
            }
        }

        private void doOutput(string str, params object[] args)
        {
            if (str == null) return;
            if (args == null || args.Length == 0)
            {
                args = new object[] { str };
                str = "{0}";
            }
            if (args.Length > 0) str = DLRConsole.SafeFormat(str, args);
            str = str.Trim();
            if (str == "") return;
            if (!writeLock.IsRunning)
            {
                DLRConsole.SYSTEM_ERR_WRITELINE_REAL("early " + str);
            }
            writeLock.Enqueue(str, () => DLRConsole.InvokeControl(rtbChat, new MethodInvoker(() =>
                                                                         {
                                                                             try
                                                                             {
                                                                                 string s = rtbChat.Text;
                                                                                 int sLength = s.Length;
                                                                                 if (sLength > 20000)
                                                                                 {
                                                                                     rtbChat.Text =
                                                                                         s.Substring(sLength - 20000);
                                                                                 }
                                                                                 printer.PrintTextLine(str);
                                                                             }
                                                                             catch (Exception e)
                                                                             {
                                                                                 DLRConsole.SYSTEM_ERR_WRITELINE_REAL("" + e);
                                                                                 // probably dead anyway ...
                                                                             }
                                                                         })));
        }

        private void setCameraToolStripMenuItem_Click(object sender, EventArgs e)
        {

        }

        private void ctxPoint_Click555(object sender, EventArgs e)
        {
            sender = FindContextArg(sender,typeof(Primitive));
            Primitive currentPrim = sender as Primitive;
            if (currentPrim!=null) instance.State.SetPointing(currentPrim, 3);
        }

        private object FindContextArg(object sender, Type type)
        {
            if (type.IsInstanceOfType(sender)) return sender;
            if (sender is ToolStripItem)
            {
                object o = ((ToolStripItem)sender).Tag;
                if (o is AspectContextAction)
                {
                    return ((AspectContextAction) o).GetValue(type);
                }
            }
            return null;
        }

        private void ctxAnim_Click(object sender, EventArgs e)
        {

        }

        private void cbxInput_SelectedIndexChanged(object sender, EventArgs e)
        {

        }

    }
}