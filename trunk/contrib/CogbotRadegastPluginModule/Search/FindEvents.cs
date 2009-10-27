using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using OpenMetaverse;
//using SLNetworkComm;
using Radegast;
using METAboltInstance = Radegast.RadegastInstance;
using SLNetCom = Radegast.Netcom.RadegastNetcom;

namespace METAbolt
{
    public partial class FindEvents : UserControl
    {
        private METAboltInstance instance;
        private SLNetCom netcom;
        private GridClient client;
        private float fX;
        private float fY;
        private float fZ;

        private ListViewItemComparer lvcompare;

        private UUID queryID;
        private Dictionary<string, uint> findEventsResults;

        public event EventHandler SelectedIndexChanged;

        public FindEvents(METAboltInstance instance, UUID queryID)
        {
            InitializeComponent();

            findEventsResults = new Dictionary<string, uint>();
            this.queryID = queryID;

            this.instance = instance;
            netcom = this.instance.Netcom;
            client = this.instance.Client;
            AddClientEvents();
        }

        private void AddClientEvents()
        {
            client.Directory.DirEventsReply += Directory_OnEventsReply;
            client.Directory.EventInfoReply += eventsconsole_OnEventInfo;
            
        }

        //Separate thread
        private void Directory_OnEventsReply(object sender, DirEventsReplyEventArgs e)
        {
            BeginInvoke(new MethodInvoker(() =>
            {
                EventsReply(e.QueryID, e.MatchedEvents);
            }));
        }

        // Separate thread
        private void eventsconsole_OnEventInfo(object sender, EventInfoReplyEventArgs e)
        {
            BeginInvoke(new MethodInvoker(() =>
            {
                EventInf(e.MatchedEvent);
            }));
        }

        // UI thread
        private void EventInf(DirectoryManager.EventInfo matchedEvent)
        {
            textBox7.Text = matchedEvent.Creator.ToString();
            textBox2.Text = matchedEvent.Name.ToString();
            textBox3.Text = DirectoryManager.EventCategories.All.ToString();   // matchedEvent.Category.ToString();

            if (matchedEvent.Duration > 59)
            {
                uint dur = matchedEvent.Duration/60;
                textBox5.Text = dur.ToString() + " hours"; 
            }
            else
            {
                textBox5.Text = matchedEvent.Duration.ToString() + " minutes";
            }
            textBox6.Text = matchedEvent.Date.ToString();

            // Get region handle
            //ulong regionhand =Helpers.UIntsToLong((uint)(matchedEvent.GlobalPos.X - (matchedEvent.GlobalPos.X % 256)), (uint)(matchedEvent.GlobalPos.Y - (matchedEvent.GlobalPos.Y % 256)));
            
            // Convert Global pos to local
            float locX = (float)matchedEvent.GlobalPos.X; ;
            float locY = (float)matchedEvent.GlobalPos.Y;
            float locX1;
            float locY1;
            Helpers.GlobalPosToRegionHandle(locX, locY, out locX1, out locY1); 

            fX = locX1;
            fY = locY1;
            fZ = (float)matchedEvent.GlobalPos.Z;

            textBox8.Text = matchedEvent.SimName.ToString() + "/" + fX.ToString() + "/" + fY.ToString() + "/" + fZ.ToString();

            if (matchedEvent.Cover == 0)
            {
                textBox9.Text = "none";
            }
            else
            {
                textBox9.Text = "L$ " + matchedEvent.Cover.ToString();
            }

            textBox1.Text = matchedEvent.Desc.ToString();
        }

        //UI thread
        private void EventsReply(UUID queryID, List<DirectoryManager.EventsSearchData> matchedEvents)
        {
            if (queryID != this.queryID) return;

            lvwFindEvents.BeginUpdate();

            foreach (DirectoryManager.EventsSearchData  events in matchedEvents)
            {
                try
                {
                    if (!findEventsResults.ContainsValue(events.ID))
                    {
                        string fullName = events.Name;
                        findEventsResults.Add(fullName, events.ID);

                        ListViewItem item = lvwFindEvents.Items.Add(fullName);
                        item.SubItems.Add(events.Date);   // + "-" + events.Time);
                    }
                }
                catch (Exception exc)
                {
                    string exp = exc.Message; 
                }
            }
 
            lvwFindEvents.Sort();
            lvwFindEvents.EndUpdate();
            pEvents.Visible = false; 
        }

        public void ClearResults()
        {
            findEventsResults.Clear();
            lvwFindEvents.Items.Clear();
            button1.Enabled = false;
            button2.Enabled = false;
        }

        private void lvwFindEvents_SelectedIndexChanged(object sender, EventArgs e)
        {
            OnSelectedIndexChanged(e);

            button1.Enabled = true;
            button2.Enabled = true;  
        }

        protected virtual void OnSelectedIndexChanged(EventArgs e)
        {
            if (SelectedIndexChanged != null) SelectedIndexChanged(this, e);
        }

        public Dictionary<string, uint> LLUUIDs
        {
            get { return findEventsResults; }
        }

        public UUID QueryID
        {
            get { return queryID; }
            set { queryID = value; }
        }

        public int SelectedIndex
        {
            get
            {
                if (lvwFindEvents.SelectedItems == null) return -1;
                if (lvwFindEvents.SelectedItems.Count == 0) return -1;

                return lvwFindEvents.SelectedIndices[0];
            }
        }

        public string SelectedName
        {
            get
            {
                if (lvwFindEvents.SelectedItems == null) return string.Empty;
                if (lvwFindEvents.SelectedItems.Count == 0) return string.Empty;

                return lvwFindEvents.SelectedItems[0].Text;
            }
        }

        public string SelectedTime
        {
            get
            {
                if (lvwFindEvents.SelectedItems == null) return "";
                if (lvwFindEvents.SelectedItems.Count == 0) return "";

                string sTime = lvwFindEvents.SelectedItems[0].SubItems[0].Text;

                return sTime;
            }
        }

        public uint SelectedEventUUID
        {
            get
            {
                if (lvwFindEvents.SelectedItems == null) return 0;
                if (lvwFindEvents.SelectedItems.Count == 0) return 0;

                string name = lvwFindEvents.SelectedItems[0].Text;
                return findEventsResults[name];
            }
        }

        private void FindEvents_Load(object sender, EventArgs e)
        {

        }

        private void button2_Click(object sender, EventArgs e)
        {
            System.Diagnostics.Process.Start(@"http://secondlife.com/events/");
        }

        private void button1_Click(object sender, EventArgs e)
        {
            string sLoc = textBox8.Text;

            char[] deli = "/".ToCharArray();
            string[] iDets = sLoc.Split(deli);

            //netcom.Teleport(iDets[0],
            //client.Self.Teleport(   

            var frmTeleport = new frmTeleport(instance);
            frmTeleport.txtRegion.Text = iDets[0].ToString();
            frmTeleport.nudX.Value = (int)fX;
            frmTeleport.nudY.Value = (int)fY;
            frmTeleport.nudZ.Value = (int)fZ;
            frmTeleport.ShowDialog();
        }

        private void lvwFindEvents_ColumnClick(object sender, ColumnClickEventArgs e)
        {
            this.lvwFindEvents.ListViewItemSorter = new ListViewItemComparer(e.Column);
            // Call the sort method to manually sort.
            lvwFindEvents.Sort();
        }    
    }
}