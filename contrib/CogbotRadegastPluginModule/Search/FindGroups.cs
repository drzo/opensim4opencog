using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using OpenMetaverse;
//using SLNetworkComm;
using METAboltInstance = Radegast.RadegastInstance;
using SLNetCom = Radegast.Netcom.RadegastNetcom;


namespace METAbolt
{
    public partial class FindGroups : UserControl
    {
        private METAboltInstance instance;
        private SLNetCom netcom;
        private GridClient client;

        private UUID queryID = UUID.Zero;
        private Dictionary<string, UUID> findGroupsResults;

        public event EventHandler SelectedIndexChanged;

        public FindGroups(METAboltInstance instance, UUID queryID)
        {
            InitializeComponent();

            findGroupsResults = new Dictionary<string, UUID>();
            this.queryID = queryID;

            this.instance = instance;
            netcom = this.instance.Netcom;
            client = this.instance.Client;
            AddClientEvents();
        }

        private void AddClientEvents()
        {
            client.Directory.DirGroupsReply += Directory_OnDirGroupsReply;   
        }

        private void Directory_OnDirGroupsReply(object sender, DirGroupsReplyEventArgs e)
        {
            BeginInvoke(new MethodInvoker(() =>
            {
                GroupsReply(e.QueryID, e.MatchedGroups);
            }));
        }


        private void GroupsReply(UUID queryID, List<DirectoryManager.GroupSearchData> matchedGroups)
        {
            if (queryID != this.queryID) return;

            lvwFindGroups.BeginUpdate();

            foreach (DirectoryManager.GroupSearchData group in matchedGroups)
            {
                findGroupsResults.Add(group.GroupName, group.GroupID);

                ListViewItem item = lvwFindGroups.Items.Add(group.GroupName);
                item.Tag = group.GroupID;
                item.SubItems.Add("UUID: " + group.GroupID.ToString() + ", Total " + group.Members.ToString() + " members");
            }

            lvwFindGroups.Sort();
            lvwFindGroups.EndUpdate();
            pGroups.Visible = false;
        }

        public void ClearResults()
        {
            findGroupsResults.Clear();
            lvwFindGroups.Items.Clear();
        }

        protected virtual void OnSelectedIndexChanged(EventArgs e)
        {
            if (SelectedIndexChanged != null) SelectedIndexChanged(this, e);
        }

        private void lvwFindGroups_SelectedIndexChanged(object sender, EventArgs e)
        {
            OnSelectedIndexChanged(e);
        }

        public Dictionary<string, UUID> LLUUIDs
        {
            get { return findGroupsResults; }
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
                if (lvwFindGroups.SelectedItems == null) return -1;
                if (lvwFindGroups.SelectedItems.Count == 0) return -1;

                return lvwFindGroups.SelectedIndices[0];
            }
        }

        public string SelectedName
        {
            get
            {
                if (lvwFindGroups.SelectedItems == null) return string.Empty;
                if (lvwFindGroups.SelectedItems.Count == 0) return string.Empty;

                return lvwFindGroups.SelectedItems[0].Text;
            }
        }

        public UUID SelectedGroupUUID
        {
            get
            {
                if (lvwFindGroups.SelectedItems == null) return UUID.Zero;
                if (lvwFindGroups.SelectedItems.Count == 0) return UUID.Zero;

                return (UUID)lvwFindGroups.SelectedItems[0].Tag;
            }
        }
    }
}
