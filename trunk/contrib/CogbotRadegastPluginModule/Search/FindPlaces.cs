using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using OpenMetaverse;
using Radegast;
using METAboltInstance = Radegast.RadegastInstance;
using SLNetCom = Radegast.Netcom.RadegastNetcom;
//using SLNetworkComm;

namespace METAbolt
{
    public partial class FindPlaces : UserControl
    {
        private METAboltInstance instance;
        private SLNetCom netcom;
        private GridClient client;
        private float fX;
        private float fY;
        private float fZ;
        private string sSIM;

        //private ListViewItemComparer lvcompare;

        private UUID queryID;
        private Dictionary<string, DirectoryManager.PlacesSearchData> findPlacesResults;
        private DirectoryManager.PlacesSearchData EmptyPlace;

        public event EventHandler SelectedIndexChanged;

        public FindPlaces(METAboltInstance instance, UUID queryID)
        {
            InitializeComponent();

            findPlacesResults = new Dictionary<string, DirectoryManager.PlacesSearchData>();
            this.queryID = queryID;

            this.instance = instance;
            netcom = this.instance.Netcom;
            client = this.instance.Client;
            AddClientEvents();
        }

        private void AddClientEvents()
        {
            client.Directory.PlacesReply += Directory_OnPlacesReply;
        }

        //Separate thread
        private void Directory_OnPlacesReply(object sender, PlacesReplyEventArgs e)
        {
            BeginInvoke(new MethodInvoker(() =>
            {
                PlacesReply(e.QueryID, e.MatchedPlaces);
            }));
        }

        private void PlacesReply(UUID queryID, List<DirectoryManager.PlacesSearchData> matchedPlaces)
        {
            if (queryID != this.queryID) return;

            lvwFindPlaces.BeginUpdate();

            foreach (DirectoryManager.PlacesSearchData places in matchedPlaces)
            {
                int icnt = 0;
                try
                {
                    if (!findPlacesResults.ContainsValue(places))
                    {
                        string fullName = places.Name;
                        findPlacesResults.Add(fullName, places);
                        
                        ListViewItem item = lvwFindPlaces.Items.Add(fullName);
                        item.SubItems.Add(places.Dwell.ToString());   // + "-" + events.Time);
                    }

                    icnt += 1;
                }
                catch (Exception exc)
                {
                    string exp = exc.Message;
                }
            }

            lvwFindPlaces.Sort();
            lvwFindPlaces.EndUpdate();
            pPlaces.Visible = false; 
        }

        // UI thread
        public void DisplayPlace(DirectoryManager.PlacesSearchData place)
        {
            if (place.Name == null)
                return;

            string sForSale = "";

            if (place.Price > 0)
            {
                sForSale = "For Sale for L$" + place.Price.ToString();   
            }

            txtName.Text = place.Name;
            txtDescription.Text = place.Desc;
            txtInformation.Text = "Traffic: " + place.Dwell + " Area: " + place.ActualArea.ToString() + " sq. m. " + sForSale;


            // Can't figure out how this is done so I am commenting
            // this feature out. If you know how it's done
            // go ahead and implement it

            //string LandFlag = "";

            //if ((place.Flags & Parcel.ParcelFlags.MaturePublish) == Parcel.ParcelFlags.MaturePublish)
            //{
            //    byte sflag = place.Flags;
            //    LandFlag = " (Mature)";
            //}

            //txtLocation.Text += LandFlag;

            // Convert Global pos to local
            float locX = (float)place.GlobalX; ;
            float locY = (float)place.GlobalY;
            float locX1;
            float locY1;
            Helpers.GlobalPosToRegionHandle(locX, locY, out locX1, out locY1);

            fX = locX1;
            fY = locY1;
            fZ = (float)place.GlobalZ;
            sSIM = place.SimName;  

            txtLocation.Text = place.SimName.ToString() + " " + fX.ToString() + ", " + fY.ToString() + ", " + fZ.ToString();
        }

        public void ClearResults()
        {
            findPlacesResults.Clear();
            lvwFindPlaces.Items.Clear();
            button1.Enabled = false;
        }

        private void lvwFindPlaces_SelectedIndexChanged(object sender, EventArgs e)
        {
            OnSelectedIndexChanged(e);

            button1.Enabled = true;
        }

        protected virtual void OnSelectedIndexChanged(EventArgs e)
        {
            if (SelectedIndexChanged != null) SelectedIndexChanged(this, e);
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
                if (lvwFindPlaces.SelectedItems == null) return -1;
                if (lvwFindPlaces.SelectedItems.Count == 0) return -1;

                return lvwFindPlaces.SelectedIndices[0];
            }
        }

        public DirectoryManager.PlacesSearchData SelectedName
        {
            get
            {
                if (lvwFindPlaces.SelectedItems == null) return EmptyPlace;
                if (lvwFindPlaces.SelectedItems.Count == 0) return EmptyPlace;

                string name = lvwFindPlaces.SelectedItems[0].Text;
                return findPlacesResults[name];
            }
        }

        private void FindPlaces_Load(object sender, EventArgs e)
        {

        }

        private void lvwFindPlaces_ColumnClick(object sender, ColumnClickEventArgs e)
        {
            this.lvwFindPlaces.ListViewItemSorter = new ListViewItemComparer(e.Column);
            // Call the sort method to manually sort.
            lvwFindPlaces.Sort();
        }

        private void txtName_TextChanged(object sender, EventArgs e)
        {

        }

        private void button1_Click(object sender, EventArgs e)
        {
            var frmTeleport = new frmTeleport(instance);
            frmTeleport.txtRegion.Text = sSIM;
            frmTeleport.nudX.Value = (int)fX;
            frmTeleport.nudY.Value = (int)fY;
            frmTeleport.nudZ.Value = (int)fZ;
            frmTeleport.ShowDialog();
        }
    }
}
