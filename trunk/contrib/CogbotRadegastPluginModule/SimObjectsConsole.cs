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
// $Id: SimObjectsConsole.cs 271 2009-09-27 11:45:39Z latifer@gmail.com $
//
using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Timers;
using System.Windows.Forms;
using cogbot;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using cogbot.Utilities;
using MushDLR223.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;
using Radegast;
using System.Reflection;
//MAYBE: Radegast.ToolStripCheckBox?
using GenericSearchFilter = System.Windows.Forms.CheckBox;

namespace CogbotRadegastPluginModule
{
    public partial class SimObjectsConsole : UserControl, IContextMenuProvider
    {
        private TaskQueueHandler addObjects = new TaskQueueHandler("SimObjectsConsole", TimeSpan.Zero, false);
        private RadegastInstance instance;
        public readonly CogbotRadegastPlugin Plugin;

        private GridClient client { get { return instance.Client;} }
        private Primitive currentPrim = new Primitive();
        private ListViewItem currentItem = new ListViewItem();
        private float searchRadius = 15.0f;
        public bool IsDisposing;
        //PropertiesQueue propRequester;

        public SimObjectsConsole(RadegastInstance instance, CogbotRadegastPlugin plugin)
        {
            this.instance = instance;
            InitializeComponent();
            Plugin = plugin;
            Disposed += new EventHandler(frmObjects_Disposed);

            //propRequester = new PropertiesQueue(instance);
            //propRequester.OnTick += new PropertiesQueue.TickCallback(propRequester_OnTick);

            nudRadius.Value = (decimal)searchRadius;
            nudRadius.ValueChanged += nudRadius_ValueChanged;
            nudRadius.KeyUp += nudRadius_KeyUp;
            nudRadius.KeyDown += nudRadius_KeyDown;

            lstPrims.ListViewItemSorter = new SimObjectSorter(client.Self);
            lstPrims.MouseUp += lstPrims_MouseUp;

            AddObjectType(typeof (SimObjectImpl));
            AddObjectType(typeof(Primitive));
            AddObjectType(typeof(Primitive.ObjectProperties));
            // Callbacks
            client.Network.Disconnected += Network_OnDisconnected;
            instance.Netcom.ClientConnected += Network_OnConnected;
            if (instance.Netcom.IsLoggedIn)
            {
                RegisterWorldSystemEvents();
            }
            client.Objects.KillObject +=  Objects_OnObjectKilled;
            //client.Objects.OnObjectProperties += new ObjectManager.ObjectPropertiesCallback(Objects_OnObjectProperties);
            client.Network.SimChanged +=  Network_OnCurrentSimChanged;
            client.Avatars.UUIDNameReply += Avatars_OnAvatarNames;
            instance.State.OnWalkStateCanged += new StateManager.WalkStateCanged(State_OnWalkStateCanged);
        }

        private void AddObjectType(Type type)
        {

            foreach (var c in type.GetProperties(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance))
            {
                //AddMember(c);
                if (!c.CanRead) continue;
                if (c.PropertyType == typeof(bool))
                {
                    AddChecks(c.Name);
                }
            }
            foreach (var c in type.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance))
            {
                AddMember(c);
            }

        }

        private void Network_OnConnected(object sender, EventArgs e1)
        {
            RegisterWorldSystemEvents();
        }

        private bool DidRegisterWorldSystemEvents = false;
        static object RegisterWorldSystemEventsLock = new object();
        private void RegisterWorldSystemEvents()
        {
            var TheWorldSystem = this.TheWorldSystem;
            lock (RegisterWorldSystemEventsLock)
            {
                if (DidRegisterWorldSystemEvents) return;
                if (TheWorldSystem == null)
                {
                    ClientManager.GlobalWriteLine("Cannot really RegisterWorldSystemEvents yet for " + instance.Client +
                                                  " not BotClient ready");
                    return;
                }
                ClientManager.GlobalWriteLine("DID Really RegisterWorldSystemEvents yet for " + instance.Client +
                              " BECAUSE BotClient ready");
                DidRegisterWorldSystemEvents = true;
            }
            TheWorldSystem.OnAddSimObject += Objects_OnAddSimObject;
            TheWorldSystem.OnUpdateDataAspect += Objects_OnUpdateSimObject;
            TheWorldSystem.AddObjectGroup("SelectedObjects", () => SelectedItems);
        }

        private void Network_OnDisconnected(object sender, DisconnectedEventArgs e)
        {
            var TheWorldSystem = this._TheWorldSystem;
            if (TheWorldSystem == null) return;
            lock (RegisterWorldSystemEventsLock)
            {
                TheWorldSystem.OnAddSimObject -= Objects_OnAddSimObject;
                TheWorldSystem.OnUpdateDataAspect -= Objects_OnUpdateSimObject;
                DidRegisterWorldSystemEvents = false;
            }
        }

        void frmObjects_Disposed(object sender, EventArgs e)
        {
            var TheWorldSystem = this._TheWorldSystem;
            if (TheWorldSystem != null)
            {
                TheWorldSystem.OnAddSimObject -= Objects_OnAddSimObject;
                TheWorldSystem.OnUpdateDataAspect -= Objects_OnUpdateSimObject;
            }
            IsDisposing = true;
            addObjects.Dispose();
            //client.Network.OnDisconnected -= new NetworkManager.DisconnectedCallback(Network_OnDisconnected);
            client.Objects.KillObject -= Objects_OnObjectKilled;
            //client.Objects.OnObjectProperties -= new ObjectManager.ObjectPropertiesCallback(Objects_OnObjectProperties);
            client.Network.SimChanged -= Network_OnCurrentSimChanged;
            client.Avatars.UUIDNameReply -= Avatars_OnAvatarNames;
            instance.State.OnWalkStateCanged -= new StateManager.WalkStateCanged(State_OnWalkStateCanged);
        }

        private readonly object RefreshObjectListLock = new object();
        private bool RefreshObjectListWorking;
        public void RefreshObjectList()
        {
            if (IsDisposing || RefreshObjectListWorking) return;
            if (InvokeRequired)
            {
                Invoke(new MethodInvoker(RefreshObjectList));
                return;
            }
            if (IsDisposing || RefreshObjectListWorking) return;
            if (!Monitor.TryEnter(RefreshObjectListLock)) return;
            RefreshObjectListWorking = true;
            lstPrims.BeginUpdate();
            Cursor.Current = Cursors.WaitCursor;
            lock (lstPrims.Items)
            {
                lstPrims.Items.Clear();
            }
            Vector3d location = client.Self.GlobalPosition;
            List<ListViewItem> items = new List<ListViewItem>();

            foreach (var prim in WorldObjects.SimObjects.CopyOf())
            {
                if (IsSkipped(location, prim)) continue;
                items.Add(new ListViewItem
                {
                    Text = GetObjectName(prim),
                    Tag = prim,
                    Name = prim.ID.ToString()
                });
            }
            lock (lstPrims.Items)
            {
                lstPrims.Items.AddRange(items.ToArray());
            }
            Cursor.Current = Cursors.Default;
            lstPrims.EndUpdate();
            RefreshObjectListWorking = false;
            Monitor.Exit(RefreshObjectListLock);
        }

        void propRequester_OnTick(int remaining)
        {
            if (InvokeRequired)
            {
                Invoke(new MethodInvoker(delegate()
                    {
                        propRequester_OnTick(remaining);
                    }
                ));
                return;
            }

            StringBuilder sb = new StringBuilder();
            sb.AppendFormat("Tracking {0} objects", lstPrims.Items.Count);

            if (remaining > 10)
            {
                sb.AppendFormat(", fetching {0} object names.", remaining);
            }
            else
            {
                sb.Append(".");
            }

            lblStatus.Text = sb.ToString();
        }

        void Network_OnCurrentSimChanged(object sender, SimChangedEventArgs e)
        {
            RegisterWorldSystemEvents();
            RefreshObjectList();
        }

        void Avatars_OnAvatarNames(object sender, UUIDNameReplyEventArgs e)
        {
            if (IsDisposing) return;
            var names = e.Names;
            if (InvokeRequired)
            {
                Invoke(new MethodInvoker(delegate() { Avatars_OnAvatarNames(sender, e); }));
                return;
            }

            lstPrims.BeginUpdate();
            lock (lstPrims.Items)
            {
                foreach (ListViewItem item in lstPrims.Items)
                {
                    SimObject prim = item.Tag as SimObject;
                    if (prim.Properties != null && names.ContainsKey(prim.Properties.OwnerID))
                    {
                        item.Text = GetObjectName(prim);
                    }
                }
            }
            lstPrims.EndUpdate();
            RegisterWorldSystemEvents();
        }

        private void Objects_OnUpdateSimObject(BotMentalAspect bma, string property, object value, object o)
        {
            return;
            SimObject ea = bma as SimObject;
            if (ea==null) return;
            string id = ea.ID.ToString();
            addObjects.Start();
            addObjects.Enqueue(new ThreadStart(() =>
            {
                Invoke(
                    new MethodInvoker(
                        delegate()
                        {
                            lock (lstPrims.Items)
                            {
                                if (lstPrims.Items.ContainsKey(id))
                                {
                                    SimObject prim = lstPrims.Items[id].Tag as SimObject;
                                    lstPrims.Items[id].Text = GetObjectName(prim);
                                }
                            }
                            if (currentPrim != null && ea.ID == currentPrim.ID)
                            {
                                UpdateCurrentObject();
                            }
                        }));
                return;

            }));

        }

        void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties props)
        {
            string id = props.ObjectID.ToString();
            addObjects.Start();
            addObjects.Enqueue(new ThreadStart(() =>
            {
                Invoke(
                    new MethodInvoker(
                        delegate()
                        {
                            lock (lstPrims.Items)
                            {
                                if (lstPrims.Items.ContainsKey(id))
                                {
                                    SimObject prim = lstPrims.Items[id].Tag as SimObject;
                                    lstPrims.Items[id].Text = GetObjectName(prim);
                                }
                            }
                            if (currentPrim != null && props.ObjectID == currentPrim.ID)
                            {
                                UpdateCurrentObject();
                            }
                        }));
                return;

            }));
        }
        

        void UpdateCurrentObject()
        {
            if (currentPrim==null || currentPrim.Properties == null) return;

            if (InvokeRequired)
            {
                BeginInvoke(new MethodInvoker(() => UpdateCurrentObject()));
                return;
            }

            currentItem.Text = GetObjectName(GetSimObject(currentPrim));

            txtObjectName.Text = currentPrim.Properties.Name;
            txtDescription.Text = currentPrim.Properties.Description;
            txtHover.Text = currentPrim.Text;
            txtOwner.AgentID = currentPrim.Properties.OwnerID;
            txtCreator.AgentID = currentPrim.Properties.CreatorID;

            Permissions p = currentPrim.Properties.Permissions;
            cbOwnerModify.Checked = (p.OwnerMask & PermissionMask.Modify) != 0;
            cbOwnerCopy.Checked = (p.OwnerMask & PermissionMask.Copy) != 0;
            cbOwnerTransfer.Checked = (p.OwnerMask & PermissionMask.Transfer) != 0;
            cbNextOwnModify.Checked = (p.NextOwnerMask & PermissionMask.Modify) != 0;
            cbNextOwnCopy.Checked = (p.NextOwnerMask & PermissionMask.Copy) != 0;
            cbNextOwnTransfer.Checked = (p.NextOwnerMask & PermissionMask.Transfer) != 0;

            txtPrims.Text = GetSimObject(currentPrim).Children.Count.ToString();
            tabSimObjeks.SelectTab(0);
            //if ((currentPrim.Flags & PrimFlags.Money) != 0)
            //{
            //    btnPay.Enabled = true;
            //}
            //else
            //{
            //    btnPay.Enabled = false;
            //}

            //if (currentPrim.Properties.SaleType != SaleType.Not)
            //{
            //    btnBuy.Text = string.Format("Buy $L{0}", currentPrim.Properties.SalePrice);
            //    btnBuy.Enabled = true;
            //}
            //else
            //{
            //    btnBuy.Text = "Buy";
            //    btnBuy.Enabled = false;
            //}
        }

        static SimObject GetSimObject(Primitive primitive)
        {
            SimObject O = WorldObjects.GridMaster.GetSimObject(primitive);
            return O;
        }

        private string GetObjectName(SimObject prim, int distance)
        {
            return String.Format("{0} ({1}m)", prim, distance);
        }

        private string GetObjectName(SimObject prim)
        {
            int distance = (int)Vector3d.Distance(client.Self.GlobalPosition,WorldPosition(prim));
            return GetObjectName(prim, distance);
        }

        static public Vector3d WorldPosition(SimObject O)
        {
            return O.IsRegionAttached ? O.GlobalPosition : new Vector3d(0,0,0);
        }

        private void AddPrim(SimObject prim)
        {
            if (IsDisposing) return;
            string pName = prim.ID.ToString();
            Invoke(new MethodInvoker(() =>
                                         {
                                             if (!Focused) return;
                                             ListViewItem item = null;

                                             lock (lstPrims.Items)
                                             {
                                                 if (lstPrims.Items.ContainsKey(pName))
                                                 {
                                                     item = lstPrims.Items[pName];
                                                 }
                                             }

                                             if (item == null)
                                             {
                                                 if (!IsSkipped(client.Self.GlobalPosition,prim))
                                                 {
                                                     item = new ListViewItem
                                                                {
                                                                    Text = GetObjectName(prim),
                                                                    Tag = prim,
                                                                    Name = pName
                                                                };
                                                     lock (lstPrims.Items)
                                                     {
                                                         lstPrims.Items.Add(item);
                                                     }
                                                 }
                                             }
                                             else
                                             {
                                                 item.Text = GetObjectName(prim);
                                             }
                                         }
                       ));
        }



        private void Objects_OnAddSimObject(SimObject prim)
        {
            if (IsDisposing || !IsHandleCreated) return;
            addObjects.Start();
            addObjects.Enqueue(() => AddPrim(prim));
            if (currentPrim!=null && prim.ID == currentPrim.ID)
            {
                if (currentPrim.Properties != null)
                {
                    UpdateCurrentObject();
                }
            }
        }

        private void Objects_OnObjectKilled(object sender, KillObjectEventArgs e)
        {
            return;
            var simulator = e.Simulator;
            var objectID = e.ObjectLocalID;
            if (InvokeRequired)
            {
                Invoke(new MethodInvoker(delegate() { Objects_OnObjectKilled(sender,e); }));
                return;
            }

            Primitive prim;

            if (client.Network.CurrentSim.ObjectsPrimitives.TryGetValue(objectID, out prim))
            {
                lock (lstPrims.Items)
                {
                    if (lstPrims.Items.ContainsKey(prim.ID.ToString()))
                    {
                        lstPrims.Items[prim.ID.ToString()].Remove();
                    }
                }
            }
        }

        private Dictionary<GenericSearchFilter, PropertyInfo> Boxs = new Dictionary<GenericSearchFilter, PropertyInfo>();
        Dictionary<PropertyInfo, Object> uBoxs = new Dictionary<PropertyInfo, Object>();
        public List<SimObject> SelectedItems = new List<SimObject>();
        private WorldObjects _TheWorldSystem = null;
        private WorldObjects TheWorldSystem
        {
            get
            {
                if (_TheWorldSystem != null) return _TheWorldSystem;
                try
                {

                    BotClient bc = Plugin.TheBot;
                    if (bc == null) bc = ClientManager.GetBotByGridClient(client);
                    if (bc == null) bc = ClientManager.SingleInstance.LastBotClient;
                    if (bc == null) return null;
                    _TheWorldSystem = bc.WorldSystem;
                }
                catch (Exception e)
                {
                    DLRConsole.DebugWriteLine(e);
                }
                return _TheWorldSystem;
            }
        }

        void AddChecks(string name)
        {
            GenericSearchFilter IsRoot = new GenericSearchFilter();
            IsRoot.Checked = false;
            IsRoot.CheckState = System.Windows.Forms.CheckState.Indeterminate;
            IsRoot.Name = "object_" + name;
            IsRoot.Size = new System.Drawing.Size(80, 22);
            IsRoot.Text = name.StartsWith("Is") ? name.Substring(2) /*: name.StartsWith("Has") ? name.Substring(3)*/ : name;
            IsRoot.ThreeState = true;
            IsRoot.Click += new System.EventHandler(this.IsRoot_Click);
            this.searchOptions.Controls.Add(IsRoot);
            Boxs[IsRoot] = typeof (SimObjectImpl).GetProperty(name);
        }


        private bool IsSkipped(Vector3d location, SimObject prim)
        {
            int distance = (int) Vector3d.Distance(location, WorldPosition(prim));
            if (!((distance < searchRadius)
                  && (txtSearch.Text.Length == 0 ||
                      (prim.ToString().ToLower().Contains(txtSearch.Text.ToLower()))))) return true;

            foreach (KeyValuePair<PropertyInfo, Object> box in uBoxs)
            {
                object v = GetObjectProp(prim,box.Key.DeclaringType);
                if (v==null) continue;
                if (!box.Value.Equals(box.Key.GetValue(v, null)))
                {
                    return true;
                }
            }
            return false;
        }

        private object GetObjectProp(Object prim, Type type)
        {
            if (type.IsInstanceOfType(prim)) return prim;
            foreach (PropertyInfo o in prim.GetType().GetProperties(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance))
            {
                if (o.CanRead)
                {
                    if (type.IsAssignableFrom(o.PropertyType)) return o.GetValue(prim, null);
                }
            }
            foreach (FieldInfo o in prim.GetType().GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance))
            {
                if (!o.IsStatic)
                {
                    if (type.IsAssignableFrom(o.FieldType)) return o.GetValue(prim);
                }
            }
            return null;

        }


        private void txtSearch_TextChanged(object sender, EventArgs e)
        {
            RefreshObjectList();
        }

        private void btnClear_Click(object sender, EventArgs e)
        {
            txtSearch.Clear();
            txtSearch.Select();
            RefreshObjectList();
        }

        private void btnRefresh_Click(object sender, EventArgs e)
        {
            RefreshObjectList();
        }


        private void lstPrims_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (lstPrims.SelectedItems.Count == 1)
            {
                //gbxInworld.Enabled = true;
                currentItem = lstPrims.SelectedItems[0];
                SimObject currentSim = currentItem.Tag as SimObject;
                if (currentSim != null)
                {
                    if (currentSim.Prim == null) return;
                    currentPrim = currentSim.Prim;
                }
                else
                {
                    return;
                }
          
                //btnBuy.Tag = currentPrim;

                if (currentPrim.Properties == null)
                {
                    client.Objects.SelectObject(client.Network.CurrentSim, currentPrim.LocalID);
                }

                UpdateCurrentObject();
            }
            else
            {
                //gbxInworld.Enabled = false;
            }
        }

        private void btnPay_Click(object sender, EventArgs e)
        {
            (new frmPay(instance, currentPrim.ID, currentPrim.Properties.Name, true)).ShowDialog();
        }

        private void btnView_Click(object sender, EventArgs e)
        {
            //List<SimObject> prims = new List<SimObject>();

            //WorldObjects.SimObjects.ForEach(delegate( SimObject kvp)
            //{
            //    if (kvp.Key == currentPrim.LocalID || kvp.Value.ParentID == currentPrim.LocalID)
            //    {
            //        prims.Add(kvp.Value);
            //    }
            //});dang cant grab the URL off that page


            //frmPrimWorkshop pw = new frmPrimWorkshop(instance);
            //pw.loadPrims(prims);
            //pw.Show();
        }

        private void nudRadius_ValueChanged(object sender, EventArgs e)
        {
            searchRadius = (float)nudRadius.Value;
            RefreshObjectList();
        }


        private void rbDistance_CheckedChanged(object sender, EventArgs e)
        {
            if (rbDistance.Checked)
            {
                lstPrims.BeginUpdate();
                ((SimObjectSorter)lstPrims.ListViewItemSorter).SortByName = false;
                lstPrims.Sort();
                lstPrims.EndUpdate();
            }
        }

        private void rbName_CheckedChanged(object sender, EventArgs e)
        {
            if (rbName.Checked)
            {
                lstPrims.BeginUpdate();
                ((SimObjectSorter)lstPrims.ListViewItemSorter).SortByName = true;
                lstPrims.Sort();
                lstPrims.EndUpdate();
            }
        }

        private void btnTurnTo_Click(object sender, EventArgs e)
        {
            if (lstPrims.SelectedItems.Count != 1) return;
            client.Self.Movement.TurnToward(currentPrim.Position);
        }

        private void btnWalkTo_Click(object sender, EventArgs e)
        {
            if (lstPrims.SelectedItems.Count != 1) return;

            if (instance.State.IsWalking)
            {
                instance.State.EndWalking();
            }
            else
            {
                instance.State.WalkTo(currentPrim);
            }
        }

        void State_OnWalkStateCanged(bool walking)
        {
            if (InvokeRequired)
            {
                Invoke(new MethodInvoker(delegate() { State_OnWalkStateCanged(walking); }));
                return;
            }
        }

        private void nudRadius_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
            {
                e.SuppressKeyPress = true;
            }
        }

        private void nudRadius_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
            {
                e.SuppressKeyPress = true;
            }
        }
        private void lstPrims_MouseUp(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Right)
            {
                ListView box = (ListView)sender;
                if (box.SelectedItems.Count > 0)
                {
                    ctxMenuObjects.Selection = box.SelectedItems[0];
                    ctxMenuObjects.HasSelection = true;
                    ctxMenuObjects.Show(lstPrims, new System.Drawing.Point(e.X, e.Y));
                } else
                {
                    ctxMenuObjects.Selection = null;
                    ctxMenuObjects.HasSelection = false;                    
                }
            }
        }
        private void ctxMenuObjects_Opening(object sender, System.ComponentModel.CancelEventArgs e)
        {
            if (lstPrims.SelectedItems.Count == 0)
            {
                ctxMenuObjects.Selection = null;
                ctxMenuObjects.HasSelection = false;
                e.Cancel = true;
                return;
            }
            instance.ContextActionManager.AddContributions(ctxMenuObjects, typeof(SimObject), lstPrims.SelectedItems[0].Tag);//, btnWalkTo.Parent);
        }

        public RadegastContextMenuStrip GetContextMenu()
        {
            return ctxMenuObjects;
        }

        private void IsRoot_Click(object sender, EventArgs e)
        {
            uBoxs.Clear();
            foreach (KeyValuePair<GenericSearchFilter, PropertyInfo> box in Boxs)
            {
                if (box.Key.CheckState != CheckState.Indeterminate)
                {
                    uBoxs.Add(box.Value, box.Key.Checked);
                }
            }
            RefreshObjectList();
        }


        private void txtDescription_TextChanged(object sender, EventArgs e)
        {
            txtDescription.ToString();
        }

        private void AddMember(MemberInfo info)
        {
            if (info.Name.StartsWith("_") || info.Name.StartsWith("<")) return;
            MemberInfoControl lblPrimType = MemberInfoControl.GetPropertyController(info);
            if (lblPrimType != null)
            {
                this.grpPrimInfo.Controls.Add(lblPrimType.Control);
            }
        }

        private void textBox1_TextChanged(object sender, EventArgs e)
        {

        }

        private void flowLayoutPanel1_Paint(object sender, PaintEventArgs e)
        {

        }

        private void lstPrims_ItemSelectionChanged(object sender, ListViewItemSelectionChangedEventArgs e)
        {
            UpdateCurrentObject();
            lock (SelectedItems)
                lock (lstPrims.SelectedItems)
                {
                    SelectedItems.Clear();
                    foreach (var v in lstPrims.SelectedItems)
                    {
                        ListViewItem item = v as ListViewItem;
                        if (item != null)
                        {
                            SimObject o = item.Tag as SimObject;
                            if (o != null)
                            {
                                SelectedItems.Add(o);
                            }

                        }
                    }
                }
        }

    }

    public class SimObjectSorter : IComparer
    {
        private AgentManager me;
        private bool sortByName = false;

        public bool SortByName { get { return sortByName; } set { sortByName = value; } }

        public SimObjectSorter(AgentManager me)
        {
            this.me = me;
        }


        //this routine should return -1 if xy and 0 if x==y.
        // for our sample we'll just use string comparison
        public int Compare(object x, object y)
        {
            ListViewItem item1 = (ListViewItem)x;
            ListViewItem item2 = (ListViewItem)y;

            if (sortByName)
            {
                return string.Compare(item1.Text, item2.Text);
            }

            double dist1 = Vector3d.Distance(me.GlobalPosition, SimObjectsConsole.WorldPosition(((SimObject)item1.Tag)));
            double dist2 = Vector3d.Distance(me.GlobalPosition, SimObjectsConsole.WorldPosition(((SimObject)item2.Tag)));

            if (dist1 == dist2)
            {
                return String.Compare(item1.Text, item2.Text);
            }
            else
            {
                if (dist1 < dist2)
                {
                    return -1;
                }
                return 1;
            }
       }
    }

}