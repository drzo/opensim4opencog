using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.Listeners
{
    partial class WorldObjects
    {


        public override void Self_OnChat(string message, ChatAudibleLevel audible, ChatType type,
                                         ChatSourceType sourceType, string fromName, UUID id, UUID ownerid,
                                         Vector3 position)
        {
            SimObject source = AsObject(fromName, id);
            object s1 = source;
            object s2 = source;
            if (source == null)
            {
                s1 = id;
                s2 = id;
            }
            if (!string.IsNullOrEmpty(fromName))
            {
                s2 = fromName;
            }
            object location = AsLocation(client.Network.CurrentSim, position, source);
            if (ownerid != id)
            {
                SendNewEvent("Bug", "id!=ownerID?", "on-chat", 
                    message, audible, type, sourceType, fromName,id,ownerid, location);

            }
            if (type == ChatType.StartTyping || type == ChatType.StopTyping)
            {

                lock (UpdateQueue)
                    UpdateQueue.Enqueue(
                        () =>
                        SendNewEvent(type.ToString(), audible, sourceType, s1, location));
                return;

            }            
            lock (UpdateQueue)
                UpdateQueue.Enqueue(() => SendNewEvent("on-chat", s2, message, 
                    audible, type, sourceType, location));
        }

        public override void Self_OnInstantMessage(InstantMessage im, Simulator simulator)
        {

            if (im.FromAgentID != UUID.Zero)
            {
                lock (Name2Key) Name2Key[im.FromAgentName.ToLower()] = im.FromAgentID;
                if (im.RegionID != UUID.Zero)
                {
                    AvatarRegion[im.FromAgentID] = im.RegionID;
                    Debug("Avatar region " + im.FromAgentName + " " + im.RegionID);
                }
            }
            lock (UpdateQueue)
                UpdateQueue.Enqueue(() => SendNewEvent("on-instantmessage", im.FromAgentName, im.Message, im.ToAgentID,
                                                       im.Offline, im.IMSessionID, im.GroupIM, AsLocation(im.RegionID, im.Position), im.Dialog,
                                                       im.ParentEstateID));
        }

        public override void Self_OnAlertMessage(string msg)
        {
            lock (UpdateQueue) UpdateQueue.Enqueue(() => SendNewEvent("On-Alert-Message", client.gridClient, msg));
        }


        public static Dictionary<string, UUID> Name2Key = new Dictionary<string, UUID>();
        public static Dictionary<UUID, UUID> AvatarRegion = new Dictionary<UUID, UUID>();


        public override void Groups_OnCurrentGroups(Dictionary<UUID, Group> groups)
        {
            base.Groups_OnCurrentGroups(groups);
            foreach (UUID key in groups.Keys)
            {
                Group g = groups[key];
                RegisterUUID(key, g);
            }
            //OnEvent("On-Current-Groups", paramNamesOnCurrentGroups, paramTypesOnCurrentGroups, groups);
        }

        public override void Avatars_OnAvatarNameSearch(UUID queryID, Dictionary<UUID, string> avatars)
        {
            foreach (KeyValuePair<UUID, string> kvp in avatars)
            {
                lock (Name2Key) Name2Key[kvp.Value.ToLower()] = kvp.Key;
            }
        }

        public override void Avatars_OnAvatarNames(Dictionary<UUID, string> names)
        {
            foreach (KeyValuePair<UUID, string> kvp in names)
            {
                lock (Name2Key) Name2Key[kvp.Value.ToLower()] = kvp.Key;
            }
        }

        public override void Avatars_OnAvatarPicks(UUID avatarid, Dictionary<UUID, string> picks)
        {
            foreach (KeyValuePair<UUID, string> kvp in picks)
            {
                lock (Name2Key) Name2Key[kvp.Value.ToLower()] = kvp.Key;
            }
        }

        public override void Friends_OnFriendOnline(FriendInfo friend)
        {
            if (friend.IsOnline && !string.IsNullOrEmpty(friend.Name) && friend.Name.ToLower() == client.MasterName.ToLower())
            {
                lock (Name2Key) Name2Key[friend.Name.ToLower()] = friend.UUID;
                client.Self.InstantMessage(friend.UUID, "Hello Master");
            }
            //base.Friends_OnFriendOnline(friend);
        }

        public override void Friends_OnFriendNamesReceived(Dictionary<UUID, string> names)
        {
            base.Friends_OnFriendNamesReceived(names);
            bool masterFound = false;
            string clientMasterNameToLower = client.MasterName.ToLower();
            foreach (KeyValuePair<UUID, string> kvp in names)
            {
                string kvpValueToLower = kvp.Value.ToLower();
                lock (Name2Key) Name2Key[kvpValueToLower] = kvp.Key;
                if (clientMasterNameToLower == kvpValueToLower)
                {
                    masterFound = true;
                    client.MasterKey = kvp.Key;
                }
                else if (kvp.Key == client.MasterKey)
                {
                    client.MasterName = kvp.Value;
                    masterFound = true;
                }
            }
            if (!masterFound)
            {
                if (!string.IsNullOrEmpty(clientMasterNameToLower))
                {
                    {
                        lock (Name2Key)
                        {
                            if (Name2Key.ContainsKey(clientMasterNameToLower))
                            {
                                client.Friends.OfferFriendship(Name2Key[clientMasterNameToLower]);
                                masterFound = true;
                            }
                        }
                    }
                }
            }
            if (!masterFound)
            {
                if (client.MasterKey != UUID.Zero)
                {
                    client.Friends.OfferFriendship(client.MasterKey);
                    masterFound = true;
                }
            }
            if (!masterFound)
            {
                if (!string.IsNullOrEmpty(clientMasterNameToLower))
                {
                    client.Friends.OfferFriendship(GetUserID(client.MasterName));
                }
            }
        }

        public override void Directory_OnDirPeopleReply(UUID queryID, List<DirectoryManager.AgentSearchData> matchedPeople)
        {
            matchedPeople.ForEach(data =>
            {
                lock (Name2Key)
                    Name2Key[data.FirstName.ToLower() + " " + data.LastName.ToLower()] = data.AgentID;
            });
        }

        public override void Directory_OnDirGroupsReply(UUID queryID, List<DirectoryManager.GroupSearchData> matchedGroups)
        {
            matchedGroups.ForEach(data =>
            {
                lock (Name2Key)
                    Name2Key[data.GroupName] = data.GroupID;
            });
        }

        public override void Friends_OnFriendRights(FriendInfo friend)
        {
            lock (Name2Key) Name2Key[friend.Name.ToLower()] = friend.UUID;
            base.Friends_OnFriendRights(friend);
        }

        internal UUID GetUserID(string ToAvatarName)
        {
            lock (Name2Key)
                if (Name2Key.ContainsKey(ToAvatarName.ToLower()))
                {
                    return Name2Key[ToAvatarName.ToLower()];
                }
            UUID found = UUID.Zero;

            ManualResetEvent NameSearchEvent = new ManualResetEvent(false);
            AvatarManager.AvatarNameSearchCallback callback =
                new AvatarManager.AvatarNameSearchCallback((queryid, avatars) =>
                {
                    foreach (KeyValuePair<UUID, string> kvp in avatars)
                    {
                        if (kvp.Value.ToLower() == ToAvatarName.ToLower())
                        {
                            lock (Name2Key)
                                Name2Key[ToAvatarName.ToLower()] =
                                    kvp.Key;
                            NameSearchEvent.Set();
                            return;
                        }
                    }
                });

            bool indict;

            lock (Name2Key) indict = Name2Key.ContainsKey(ToAvatarName.ToLower());

            if (!indict)
            {
                try
                {
                    client.Avatars.OnAvatarNameSearch += callback;
                    // Send the Query
                    client.Avatars.RequestAvatarNameSearch(ToAvatarName, UUID.Random());

                    NameSearchEvent.WaitOne(6000, false);
                }
                finally
                {
                    client.Avatars.OnAvatarNameSearch -= callback;
                }
            }


            lock (Name2Key)
                if (Name2Key.ContainsKey(ToAvatarName.ToLower()))
                {
                    found = Name2Key[ToAvatarName.ToLower()];

                }

            return found;
        }

    }
}
