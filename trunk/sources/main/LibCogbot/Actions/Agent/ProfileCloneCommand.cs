/******************************************************************************************
  Cogbot -- Copyright (c) 2008-2012, Douglas Miles, Kino Coursey, Daxtron Labs, Logicmoo
      and the Cogbot Development Team.
   
  Major contributions from (and special thanks to):
      Latif Kalif, Anne Ogborn and Openmeteverse Foundation

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**************************************************************************************************/

using System;
using System.Collections.Generic;
using System.Threading;
using Cogbot.World;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Agent
{
    public class ProfileCloneCommand : Command, BotPersonalCommand, BotStatefullCommand
    {
        Avatar.AvatarProperties Properties;
        Avatar.Interests Interests;
        List<UUID> Groups = new List<UUID>();
        bool ReceivedProperties = false;
        bool ReceivedInterests = false;
        bool ReceivedGroups = false;
        ManualResetEvent ReceivedProfileEvent = new ManualResetEvent(false);
        UUID targetID = UUID.Zero;

        bool registeredCallbacks = false;

        public ProfileCloneCommand(BotClient testClient)
            : base(testClient)
        {

            Name = "Profile Clone";
            Description = "Copies another avatars profile as closely as possible onto your existing profile. WARNING: This command will destroy your existing profile!";
            Category = CommandCategory.Other;
            AddVersion(CreateParams("agent", typeof(UUID), "agent you are going to " + Name),
                       "copies the profile specified by agent's uuid");
            ResultMap = CreateParams(
                "reason", typeof(string), "if success was false, the reason why",
                "success", typeof(bool), "true if command was successful");
        }

        #region Implementation of IDisposable

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {
            if (registeredCallbacks)
            {
                registeredCallbacks = false;
                Client.Avatars.AvatarInterestsReply -= new EventHandler<AvatarInterestsReplyEventArgs>(Avatars_AvatarInterestsReply);
                Client.Avatars.AvatarPropertiesReply -= new EventHandler<AvatarPropertiesReplyEventArgs>(Avatars_AvatarPropertiesReply);
                Client.Avatars.AvatarGroupsReply -= new EventHandler<AvatarGroupsReplyEventArgs>(Avatars_AvatarGroupsReply);
                Client.Groups.GroupJoinedReply -= new EventHandler<GroupOperationEventArgs>(Groups_OnGroupJoined);
                Client.Avatars.AvatarPicksReply -= new EventHandler<AvatarPicksReplyEventArgs>(Avatars_AvatarPicksReply);
                Client.Avatars.PickInfoReply -= new EventHandler<PickInfoReplyEventArgs>(Avatars_PickInfoReply);
            }
        }

        #endregion

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
                return ShowUsage();

            ReceivedProperties = false;
            ReceivedInterests = false;
            ReceivedGroups = false;

            if (!registeredCallbacks)
            {
                registeredCallbacks = true;
                Client.Avatars.AvatarInterestsReply += new EventHandler<AvatarInterestsReplyEventArgs>(Avatars_AvatarInterestsReply);
                Client.Avatars.AvatarPropertiesReply += new EventHandler<AvatarPropertiesReplyEventArgs>(Avatars_AvatarPropertiesReply);
                Client.Avatars.AvatarGroupsReply += new EventHandler<AvatarGroupsReplyEventArgs>(Avatars_AvatarGroupsReply);
                Client.Groups.GroupJoinedReply += new EventHandler<GroupOperationEventArgs>(Groups_OnGroupJoined);
                Client.Avatars.AvatarPicksReply += new EventHandler<AvatarPicksReplyEventArgs>(Avatars_AvatarPicksReply);
                Client.Avatars.PickInfoReply += new EventHandler<PickInfoReplyEventArgs>(Avatars_PickInfoReply);
            }

            if (!args.TryGetValue("agent", out targetID)) return ShowUsage();

            // Request all of the packets that make up an avatar profile
            Client.Avatars.RequestAvatarProperties(targetID);

            //Request all of the avatars pics
            Client.Avatars.RequestAvatarPicks(Client.Self.AgentID);
            Client.Avatars.RequestAvatarPicks(targetID);

            // Wait for all the packets to arrive
            ReceivedProfileEvent.Reset();
            ReceivedProfileEvent.WaitOne(5000, false);

            // Check if everything showed up
            if (!ReceivedInterests || !ReceivedProperties || !ReceivedGroups)
                return Failure("Failed to retrieve a complete profile for that UUID");

            // Synchronize our profile
            Client.Self.UpdateInterests(Interests);
            Client.Self.UpdateProfile(Properties);

            // TODO: Leave all the groups we're currently a member of? This could
            // break BotClient connectivity that might be relying on group authentication

            // Attempt to join all the groups
            foreach (UUID groupID in Groups)
            {
                Client.Groups.RequestJoinGroup(groupID);
            }

            return Success("Synchronized our profile to the profile of " + targetID.ToString());
        }                           

        void Groups_OnGroupJoined(object sender, GroupOperationEventArgs e)
        {
            WriteLine(Client.ToString() + (e.Success ? " joined " : " failed to join ") +
                e.GroupID.ToString());

            if (e.Success)
            {
                WriteLine(Client.ToString() + " setting " + e.GroupID.ToString() +
                    " as the active group");
                Client.Groups.ActivateGroup(e.GroupID);
            }
        }

        void Avatars_PickInfoReply(object sender, PickInfoReplyEventArgs e)
        {
            Client.Self.PickInfoUpdate(e.PickID, e.Pick.TopPick, e.Pick.ParcelID, e.Pick.Name, e.Pick.PosGlobal, e.Pick.SnapshotID, e.Pick.Desc);
        }

        void Avatars_AvatarPicksReply(object sender, AvatarPicksReplyEventArgs e)
        {
            if (e.AvatarID != targetID) return;
            foreach (KeyValuePair<UUID, string> kvp in e.Picks)
            {
                if (e.AvatarID == Client.Self.AgentID)
                {
                    Client.Self.PickDelete(kvp.Key);
                }
                else
                {
                    Client.Avatars.RequestPickInfo(e.AvatarID, kvp.Key);
                }
            }
        }

        void Avatars_AvatarGroupsReply(object sender, AvatarGroupsReplyEventArgs e)
        {
            if (e.AvatarID != targetID) return;
            lock (ReceivedProfileEvent)
        {
                foreach (AvatarGroup group in e.Groups)
                {
                    Groups.Add(group.GroupID);
        }

                ReceivedGroups = true;

                if (ReceivedInterests && ReceivedProperties && ReceivedGroups)
                    ReceivedProfileEvent.Set();
            }
        }

        void Avatars_AvatarPropertiesReply(object sender, AvatarPropertiesReplyEventArgs e)
        {
            if (e.AvatarID != targetID) return;
            lock (ReceivedProfileEvent)
            {
                Properties = e.Properties;
                ReceivedProperties = true;

                if (ReceivedInterests && ReceivedProperties && ReceivedGroups)
                    ReceivedProfileEvent.Set();
            }
        }

        void Avatars_AvatarInterestsReply(object sender, AvatarInterestsReplyEventArgs e)
        {
            if (e.AvatarID != targetID) return;
            lock (ReceivedProfileEvent)
            {
                Interests = e.Interests;
                ReceivedInterests = true;

                if (ReceivedInterests && ReceivedProperties && ReceivedGroups)
                    ReceivedProfileEvent.Set();
            }
        }
    }
}
