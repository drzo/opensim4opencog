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
// $Id: SearchConsole.cs 152 2009-08-24 14:19:58Z latifer@gmail.com $
//
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
    public partial class SearchConsole : UserControl
    {
        private METAboltInstance instance;
        private SLNetCom netcom;
        private GridClient client;

        private TabsConsole tabConsole;
        private FindPeopleConsole console;
        private FindEvents eventsconsole;
        private FindPlaces placesconsole;
        private FindGroups groupsconsole; 

        private string lastQuery = string.Empty;
        private int startResult = 0;

        private int totalResults = 0;
        GroupManager.GroupJoinedCallback gcallback;


        public SearchConsole(METAboltInstance instance)
        {
            InitializeComponent();

            this.instance = instance;
            netcom = this.instance.Netcom;
            client = this.instance.Client;
            AddClientEvents();

            tabConsole = this.instance.TabConsole;

            console = new FindPeopleConsole(instance, UUID.Random());
            console.Dock = DockStyle.Fill;
            console.SelectedIndexChanged += new EventHandler(console_SelectedIndexChanged);
            pnlFindPeople.Controls.Add(console);

            eventsconsole = new FindEvents(instance, UUID.Random());
            eventsconsole.Dock = DockStyle.Fill;
            eventsconsole.SelectedIndexChanged += new EventHandler(eventsconsole_SelectedIndexChanged);
            pnlFindEvents.Controls.Add(eventsconsole);

            placesconsole = new FindPlaces(instance, UUID.Random());
            placesconsole.Dock = DockStyle.Fill;
            placesconsole.SelectedIndexChanged += new EventHandler(placesconsole_SelectedIndexChanged);
            pnlFindPlaces.Controls.Add(placesconsole);

            groupsconsole = new FindGroups(instance, UUID.Random());
            groupsconsole.Dock = DockStyle.Fill;
            groupsconsole.SelectedIndexChanged += new EventHandler(groupsconsole_SelectedIndexChanged);
            pnlFindGroups.Controls.Add(groupsconsole);
        }

        private void console_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (console.SelectedName == client.Self.Name)
            {
                btnNewIM.Enabled = btnNewIM.Enabled = button10.Enabled = btnFriend.Enabled = false;
                btnProfile.Enabled = true; 
            }
            else
            {
                btnNewIM.Enabled = btnNewIM.Enabled = button10.Enabled = btnFriend.Enabled = btnProfile.Enabled = (console.SelectedName != null);
            }
        }

        private void eventsconsole_SelectedIndexChanged(object sender, EventArgs e)
        {
            uint eid = eventsconsole.SelectedEventUUID;
            client.Directory.EventInfoRequest(eid);

        }

        private void placesconsole_SelectedIndexChanged(object sender, EventArgs e)
        {
            //DirectoryManager.PlacesSearchData eid = placesconsole.SelectedPlaceUUID;
            DirectoryManager.PlacesSearchData iplc = placesconsole.SelectedName; 
            ////client.Directory.EventInfoRequest(eid);
            placesconsole.DisplayPlace(iplc);  

        }

        private void groupsconsole_SelectedIndexChanged(object sender, EventArgs e)
        {
            //DirectoryManager.PlacesSearchData igrp = groupsconsole.SelectedName;
            //////client.Directory.EventInfoRequest(eid);
            //groupsconsole.DisplayPlace(igrp); 

            btnJoin.Enabled = (groupsconsole.SelectedName != null);
        }

        private void AddClientEvents()
        {
            client.Directory.DirPeopleReply += Directory_OnDirPeopleReply;
            client.Directory.DirEventsReply += Directory_OnEventsReply;
            client.Directory.PlacesReply += Directory_OnDirPlacesReply;
            client.Directory.DirGroupsReply += Directory_OnDirGroupsReply;   
            //client.Directory.OnEventInfo += new DirectoryManager.EventInfoCallback(eventsconsole_OnEventInfo);
        }

        //Separate thread
        private void Directory_OnDirPeopleReply(object sender, DirPeopleReplyEventArgs e)
        {
            BeginInvoke(new MethodInvoker(() =>
            {
                PeopleReply(e.QueryID, e.MatchedPeople);
            }));
        }

        private void Directory_OnEventsReply(object sender, DirEventsReplyEventArgs e)
        {
            BeginInvoke(new MethodInvoker(() =>
            {
                EventsReply(e.QueryID, e.MatchedEvents);
            }));
        }

        private void Directory_OnDirPlacesReply(object sender, PlacesReplyEventArgs e)
        {
            BeginInvoke(new MethodInvoker(() =>
            {
                PlacesReply(e.QueryID, e.MatchedPlaces);
            }));
        }

        private void Directory_OnDirGroupsReply(object sender, DirGroupsReplyEventArgs e)
        {
            BeginInvoke(new MethodInvoker(() =>
            {
                GroupsReply(e.QueryID,e.MatchedGroups);
            }));
        }

        private void GroupsReply(UUID queryID, List<DirectoryManager.GroupSearchData> matchedGroups)
        {
            if (groupsconsole.QueryID != queryID) return;

            totalResults += matchedGroups.Count;
            lblGroupsFound.Text = totalResults.ToString() + " groups found";

            txtGroups.Enabled = true;
            btnFindGroups.Enabled = true;

            btnNextGroups.Enabled = (totalResults > 100);
            btnPrevGroups.Enabled = (startResult > 0);
        }

        //UI thread
        private void PeopleReply(UUID queryID, List<DirectoryManager.AgentSearchData> matchedPeople)
        {
            if (console.QueryID != queryID) return;

            totalResults += matchedPeople.Count;
            lblResultCount.Text = totalResults.ToString() + " people found";

            txtPersonName.Enabled = true;
            btnFind.Enabled = true;

            btnNext.Enabled = (totalResults > 100);
            btnPrevious.Enabled = (startResult > 0);
        }

        private void EventsReply(UUID queryID, List<DirectoryManager.EventsSearchData> matchedEvents)
        {
            if (eventsconsole.QueryID != queryID) return;

            totalResults += matchedEvents.Count;
            lblEventsCount.Text = totalResults.ToString() + " events found";

            txtEvents.Enabled = true;
            btnFindEvents.Enabled = true;

            btnNextEvents.Enabled = (totalResults > 100);
            btnPrevEvents.Enabled = (startResult > 0);
        }

        private void PlacesReply(UUID queryID, List<DirectoryManager.PlacesSearchData> matchedPlaces)
        {
            if (placesconsole.QueryID != queryID) return;

            totalResults += matchedPlaces.Count;
            lblPlacesCount.Text = totalResults.ToString() + " places found";

            txtPlaces.Enabled = true;
            btnFindPlaces.Enabled = true;

            btnNextPlaces.Enabled = false; // (totalResults > 100);
            btnPrevPlaces.Enabled = false; // (startResult > 0);
        }

        private void txtPersonName_TextChanged(object sender, EventArgs e)
        {
            btnFind.Enabled = (txtPersonName.Text.Trim().Length > 2);
        }

        private void txtEvents_TextChanged(object sender, EventArgs e)
        {
            btnFindEvents.Enabled = (txtEvents.Text.Trim().Length > 2);
        }

        private void txtPlaces_TextChanged(object sender, EventArgs e)
        {
            btnFindPlaces.Enabled = (txtPlaces.Text.Trim().Length > 2);
        }

        private void txtGroups_TextChanged(object sender, EventArgs e)
        {
            btnFindGroups.Enabled = (txtGroups.Text.Trim().Length > 2);
        }

        private void btnNewIM_Click(object sender, EventArgs e)
        {
            // V 0.9.1.6 change
            if (console.SelectedName == client.Self.Name)
                return;
            // end

            if (tabConsole.TabExists(console.SelectedName))
            {
                tabConsole.SelectTab(console.SelectedName);
                return;
            }

            tabConsole.AddIMTab(console.SelectedAgentUUID, client.Self.SessionID ^ console.SelectedAgentUUID, console.SelectedName/*, false*/);
            tabConsole.SelectTab(console.SelectedName);
        }

        private void btnFind_Click(object sender, EventArgs e)
        {
            lastQuery = txtPersonName.Text;
            startResult = 0;
            StartFinding();
        }

        private void btnFindEvents_Click(object sender, EventArgs e)
        {
            lastQuery = txtEvents.Text;
            startResult = 0;
            //totalResults = 0;
            StartFindingEvents();
        }

        private void btnFindPlaces_Click(object sender, EventArgs e)
        {
            lastQuery = txtPlaces.Text;
            startResult = 0;
            //totalResults = 0;
            StartFindingPlaces();
        }

        private void btnFindGroups_Click(object sender, EventArgs e)
        {
            lastQuery = txtGroups.Text;
            startResult = 0;
            //totalResults = 0;
            StartFindingGroups();
        }

        private void txtPersonName_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.KeyCode != Keys.Enter) return;
            e.SuppressKeyPress = true;
            if (txtPersonName.Text.Trim().Length < 3) return;

            lastQuery = txtPersonName.Text;
            startResult = 0;
            StartFinding();
        }

        private void StartFinding()
        {
            console.Controls["pPeople"].Visible = true;

            totalResults = 0;
            lblResultCount.Text = "Searching for " + lastQuery;

            txtPersonName.Enabled = false;
            btnFind.Enabled = false;
            btnNewIM.Enabled = false;
            btnProfile.Enabled = false;
            btnFriend.Enabled = false;
            btnPrevious.Enabled = false;
            btnNext.Enabled = false;

            try
            {
                console.ClearResults();
                console.QueryID = client.Directory.StartPeopleSearch(
                   // DirectoryManager.DirFindFlags.NameSort |
                   // DirectoryManager.DirFindFlags.SortAsc |
                   // DirectoryManager.DirFindFlags.People,
                    lastQuery, startResult);
            }
            catch (Exception exc)
            {
                string exp = exc.Message;
            }
        }

        private void StartFindingEvents()
        {
            //eventsconsole = new FindEvents(instance, UUID.Random());
            eventsconsole.Controls["pEvents"].Visible = true;
   
            totalResults = 0;
            lblEventsCount.Text = "Searching for " + lastQuery;

            txtEvents.Enabled = false;
            btnFindEvents.Enabled = false;
            btnPrevEvents.Enabled = false;
            btnNextEvents.Enabled = false;

            try
            {
                eventsconsole.ClearResults();
                eventsconsole.QueryID = client.Directory.StartEventsSearch(
                    lastQuery,
                   // true,
                   // "u",
                    (uint)startResult//,
                  //  DirectoryManager.EventCategories.All,
                //    UUID.Random()
                );
            }
            catch (Exception exc)
            {
                string exp = exc.Message;
            }
        }

        private void StartFindingPlaces()
        {
            //placesconsole = new FindPlaces(instance, UUID.Random());
            placesconsole.Controls["pPlaces"].Visible = true;

            totalResults = 0;
            lblPlacesCount.Text = "Searching for " + lastQuery;

            txtPlaces.Enabled = false;
            btnFindPlaces.Enabled = false;
            btnPrevPlaces.Enabled = false;
            btnNextPlaces.Enabled = false;

            try
            {
                placesconsole.ClearResults();
                placesconsole.QueryID = client.Directory.StartPlacesSearch(
                    DirectoryManager.DirFindFlags.NameSort,
                    ParcelCategory.Any,
                    lastQuery,
                    String.Empty,
                    UUID.Zero,
                    UUID.Random());
            }
            catch (Exception exc)
            {
                string exp = exc.Message;
            }
        }

        private void StartFindingGroups()
        {
            groupsconsole.Controls["pGroups"].Visible = true;
            totalResults = 0;

            lblGroupsFound.Text = "Searching for " + lastQuery;

            txtGroups.Enabled = false;
            btnFindGroups.Enabled = false;
            btnNextGroups.Enabled = false;
            btnPrevGroups.Enabled = false;

            try
            {
                groupsconsole.ClearResults();
                groupsconsole.QueryID = client.Directory.StartGroupSearch(
                 //   DirectoryManager.DirFindFlags.NameSort |
                //    DirectoryManager.DirFindFlags.SortAsc |
                //    DirectoryManager.DirFindFlags.Groups,
                    lastQuery,
                    startResult//,
                   /// UUID.Random()
                    );
            }
            catch (Exception exc)
            {
                string exp = exc.Message;
            }

        }

        private void btnNext_Click(object sender, EventArgs e)
        {
            startResult += 100;
            StartFinding();
        }

        private void btnPrevious_Click(object sender, EventArgs e)
        {
            startResult -= 100;
            StartFinding();
        }

        private void btnNextEvents_Click(object sender, EventArgs e)
        {
            startResult += 100;
            StartFindingEvents();
        }

        private void btnPrevEvents_Click(object sender, EventArgs e)
        {
            startResult -= 100;
            StartFindingEvents();
        }

        private void btnPrevPlaces_Click(object sender, EventArgs e)
        {
            startResult -= 100;
            StartFindingPlaces();
        }

        private void btnNextPlaces_Click(object sender, EventArgs e)
        {
            startResult += 100;
            StartFindingPlaces();
        }

        private void btnNextGroups_Click(object sender, EventArgs e)
        {
            startResult += 100;
            StartFindingGroups();
        }

        private void btnPrevGroups_Click(object sender, EventArgs e)
        {
            startResult -= 100;
            StartFindingGroups();
        }

        private void btnProfile_Click(object sender, EventArgs e)
        {
            (new frmProfile(instance, console.SelectedName, console.SelectedAgentUUID)).Show();
        }

        private void txtPersonName_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter) e.SuppressKeyPress = true;
        }

        private void txtEvents_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter) e.SuppressKeyPress = true;
        }

        private void btnFriend_Click(object sender, EventArgs e)
        {
            // V 0.9.1.6 change
            if (console.SelectedName == client.Self.Name)
                return;
            // end
 
            string  sAvName = console.SelectedName;
            if (console.SelectedAgentUUID == null) return;

            //List<FriendInfo> friends = client.Friends.FriendsList();
            //client.Friends.FriendList

            Boolean fFound = true;

            client.Friends.FriendList.ForEach(delegate(FriendInfo friend)
            {
                if (friend.Name == sAvName)
                {
                    fFound = false;
                }

                if (fFound)
                {
                    client.Friends.OfferFriendship(console.SelectedAgentUUID);
                }
            });

            //foreach (FriendInfo friend in friends)
            //    if (friend.Name == sAvName)
            //    {
            //        fFound = false; 
            //    }

            //if (fFound)
            //{
            //    client.Friends.OfferFriendship(console.SelectedAgentUUID);
            //}
        }

        private void txtEvents_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.KeyCode != Keys.Enter) return;
            e.SuppressKeyPress = true;
            if (txtEvents.Text.Trim().Length < 3) return;

            lastQuery = txtEvents.Text;
            startResult = 0;
            StartFindingEvents();
        }

        private void txtPlaces_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter) e.SuppressKeyPress = true;
        }
 
        private void txtPlaces_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.KeyCode != Keys.Enter) return;
            e.SuppressKeyPress = true;
            if (txtPlaces.Text.Trim().Length < 3) return;

            lastQuery = txtPlaces.Text;
            startResult = 0;
            StartFindingPlaces();
        }

        private void txtGroups_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter) e.SuppressKeyPress = true;
        }

        private void txtGroups_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.KeyCode != Keys.Enter) return;
            e.SuppressKeyPress = true;
            if (txtGroups.Text.Trim().Length < 3) return;

            lastQuery = txtGroups.Text;
            startResult = 0;
            StartFindingGroups();
        }


        private void pnlFindEvents_Paint(object sender, PaintEventArgs e)
        {

        }

        private void pnlFindPlaces_Paint(object sender, PaintEventArgs e)
        {

        }

        private void button10_Click(object sender, EventArgs e)
        {
            // V 0.9.1.6 change
            if (console.SelectedName == client.Self.Name)
                return;
            // end

            if (console.SelectedAgentUUID == null) return;

            //Avatar av = ((ListViewItem)lvwObjects.SelectedItems[0]).Tag as Avatar;
            //if (av == null) return;

            client.Parcels.EjectUser(console.SelectedAgentUUID, true);
        }

        private void btnJoin_Click(object sender, EventArgs e)
        {
            if (groupsconsole.SelectedName == null) return;

            gcallback = new GroupManager.GroupJoinedCallback(Groups_OnGroupJoined);
            client.Groups.OnGroupJoined += gcallback;
            client.Groups.RequestJoinGroup(groupsconsole.SelectedGroupUUID);
        }

        void Groups_OnGroupJoined(UUID groupID, bool success)
        {
            //string msg = string.Empty;
  
            //if (success)
            //{
            //    msg = "You have joined group: " + groupID.ToString();  
            //}
            //else
            //{
            //    msg = "Failed to join group: " + groupID.ToString();
            //}
            //MessageBox.Show(msg);

            client.Groups.OnGroupJoined -= gcallback;
        }

        private void pnlFindPeople_Paint(object sender, PaintEventArgs e)
        {

        }
    }
}
