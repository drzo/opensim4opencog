using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class CloneProfileCommand : Command, BotPersonalCommand
    {
        Avatar.AvatarProperties Properties;
        Avatar.Interests Interests;
        List<UUID> Groups = new List<UUID>();
        bool ReceivedProperties = false;
        bool ReceivedInterests = false;
        bool ReceivedGroups = false;
        ManualResetEvent ReceivedProfileEvent = new ManualResetEvent(false);

        bool registeredCallbacks = false;

        public CloneProfileCommand(BotClient testClient)
        {

            Name = "cloneprofile";
            Description = "Clones another avatars profile as closely as possible. WARNING: This command will " +
                "destroy your existing profile! Usage: cloneprofile [targetuuid]";
            Category = CommandCategory.Other;
            Parameters = new [] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();

            UUID targetID;
            ReceivedProperties = false;
            ReceivedInterests = false;
            ReceivedGroups = false;

            if (!registeredCallbacks)
            {
                registeredCallbacks = true;
                Client.Avatars.OnAvatarInterests += new AvatarManager.AvatarInterestsCallback(Avatars_OnAvatarInterests);
                Client.Avatars.OnAvatarProperties += new AvatarManager.AvatarPropertiesCallback(Avatars_OnAvatarProperties);
                Client.Avatars.OnAvatarGroups += new AvatarManager.AvatarGroupsCallback(Avatars_OnAvatarGroups);
                Client.Groups.OnGroupJoined += new GroupManager.GroupJoinedCallback(Groups_OnGroupJoined);
                Client.Avatars.OnAvatarPicks += new AvatarManager.AvatarPicksCallback(Avatars_OnAvatarPicks);
                Client.Avatars.OnPickInfo += new AvatarManager.PickInfoCallback(Avatars_OnPickInfo);
            }

            int argsUsed;
            if (!UUIDTryParse(args,0, out targetID, out argsUsed))
				return ShowUsage();

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

        void Avatars_OnAvatarPicks(UUID avatarid, Dictionary<UUID, string> picks)
        {
            foreach (KeyValuePair<UUID, string> kvp in picks)
            {
                if (avatarid == Client.Self.AgentID)
                {
                    Client.Self.PickDelete(kvp.Key);
                }
                else
                {
                    Client.Avatars.RequestPickInfo(avatarid, kvp.Key);
                }
            }
        }

        void Avatars_OnPickInfo(UUID pickid, ProfilePick pick)
        {
            Client.Self.PickInfoUpdate(pickid, pick.TopPick, pick.ParcelID, pick.Name, pick.PosGlobal, pick.SnapshotID, pick.Desc);
        }

        void Avatars_OnAvatarProperties(UUID avatarID, Avatar.AvatarProperties properties)
        {
            lock (ReceivedProfileEvent)
            {
                Properties = properties;
                ReceivedProperties = true;

                if (ReceivedInterests && ReceivedProperties && ReceivedGroups)
                    ReceivedProfileEvent.Set();
            }
        }

        void Avatars_OnAvatarInterests(UUID avatarID, Avatar.Interests interests)
        {
            lock (ReceivedProfileEvent)
            {
                Interests = interests;
                ReceivedInterests = true;

                if (ReceivedInterests && ReceivedProperties && ReceivedGroups)
                    ReceivedProfileEvent.Set();
            }
        }

        void Avatars_OnAvatarGroups(UUID avatarID, List<AvatarGroup> groups)
        {
            lock (ReceivedProfileEvent)
            {
                foreach (AvatarGroup group in groups)
                {
                    Groups.Add(group.GroupID);
                }

                ReceivedGroups = true;

                if (ReceivedInterests && ReceivedProperties && ReceivedGroups)
                    ReceivedProfileEvent.Set();
            }
        }

        void Groups_OnGroupJoined(UUID groupID, bool success)
        {
            WriteLine(Client.ToString() + (success ? " joined " : " failed to join ") +
                groupID.ToString());

            if (success)
            {
                WriteLine(Client.ToString() + " setting " + groupID.ToString() +
                    " as the active group");
                Client.Groups.ActivateGroup(groupID);
            }
        }
    }
}