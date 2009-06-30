using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Packets;
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
                AddName2Key(im.FromAgentName, im.FromAgentID);
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
                AddName2Key(g.Name, key);
                RegisterUUID(key, g);
            }
            //OnEvent("On-Current-Groups", paramNamesOnCurrentGroups, paramTypesOnCurrentGroups, groups);
        }

        private void AvatarAppearanceHandler(Packet packet, Simulator sim)
        {
            if (sim != client.Network.CurrentSim) { Debug("from a differnt sim than current " + sim); }
            AvatarAppearancePacket appearance = (AvatarAppearancePacket)packet;
            Avatar found = sim.ObjectsAvatars.Find(delegate(Avatar av)
            {
                if (av.ID == appearance.Sender.ID)
                {
                    List<byte> visualParams = new List<byte>();
                    foreach (
                        AvatarAppearancePacket.VisualParamBlock block in
                            appearance.VisualParam)
                    {
                        visualParams.Add(block.ParamValue);
                    }

                    Primitive.TextureEntry textureEntry =
                        new Primitive.TextureEntry(
                            appearance.ObjectData.TextureEntry, 0,
                            appearance.ObjectData.TextureEntry.Length);

                    Primitive.TextureEntryFace defaultTexture =
                        textureEntry.DefaultTexture;
                    Primitive.TextureEntryFace[] faceTextures =
                        textureEntry.FaceTextures;

                    av.Textures = textureEntry;

                    //if (OnAvatarAppearance != null)
                    //{
                    //    try { OnAvatarAppearance(appearance.Sender.ID, appearance.Sender.IsTrial, defaultTexture, faceTextures, visualParams); }
                    //    catch (Exception e) { Logger.Log(e.Message, Helpers.LogLevel.Error, Client, e); }
                    //}
                    return true;
                }
                return false;
            });
            if (found != null) return;
            UUID id = appearance.Sender.ID;
            if (GetSimObjectFromUUID(id) == null)
                CreateSimAvatar(id, this, sim);
        }

        public override void Avatars_OnAvatarProperties(UUID avatarID, Avatar.AvatarProperties properties)
        {
            //TODO SendNewEvent("On-Avatar-Properties", GetAvatar(avatarID, null), properties);
        }

        public override void Avatars_OnAvatarInterests(UUID avatarID, Avatar.Interests interests)
        {
            //TODO SendNewEvent("On-Avatar-Properties", GetAvatar(avatarID, null), interests);
        }

        public override void Avatars_OnAvatarGroups(UUID avatarID, List<AvatarGroup> avatarGroups)
        {
            foreach (var grp in avatarGroups)
            {
                //TODO SendNewEvent("On-Avatar-Properties", GetAvatar(avatarID, null), grp);                
            }
        }
        public override void Groups_OnGroupMembers(UUID requestID, UUID groupID, int totalCount, Dictionary<UUID, GroupMember> members)
        {
           // base.Groups_OnGroupMembers(requestID, totalCount, members);
        }

        public override void Groups_OnGroupNames(Dictionary<UUID, string> groupNames)
        {
            foreach (KeyValuePair<UUID, string> kvp in groupNames)
            {
                AddName2Key(kvp.Value, kvp.Key);
            }
            ///base.Groups_OnGroupNames(groupNames);
        }



        public override void Avatars_OnAvatarNameSearch(UUID queryID, Dictionary<UUID, string> avatars)
        {
            foreach (KeyValuePair<UUID, string> kvp in avatars)
            {
                AddName2Key(kvp.Value, kvp.Key);
            }
        }

        public override void Avatars_OnAvatarNames(Dictionary<UUID, string> names)
        {
            foreach (KeyValuePair<UUID, string> kvp in names)
            {
                AddName2Key(kvp.Value, kvp.Key);
            }
        }

        private void AddName2Key(string value, UUID id)
        {
            if (value == null)
            {
                Debug("AddName2Key: NULL " + id);
                return;
            }
            if (id == UUID.Zero)
            {
                Debug("AddName2Key: UUID.Zero " + value);
                return;
            }
            if (value.Contains("?"))
            {
                SimObject O = GetSimObjectFromUUID(id);
                if (O != null)
                {
                    if (O is SimAvatar)
                    {
                        Debug("AVATAR?!?!? " + O);
                    }
                    return;
                }
                CreateSimObject(id, this, null);
                Debug("AddName2Key: " + value + " " + id);
                return;
            }
            string n = value.Trim();
            if (n.Length<3)
            {
                Debug("AddName2Key: " + value + " " + id);
                return;
            }
            lock (Name2Key)
            {
                Name2Key[value] = id;
            }
        }

        public override void Avatars_OnAvatarPicks(UUID avatarid, Dictionary<UUID, string> picks)
        {
            foreach (KeyValuePair<UUID, string> kvp in picks)
            {
                AddName2Key(kvp.Value, kvp.Key);
            }
        }

        public override void Friends_OnFriendOnline(FriendInfo friend)
        {
            if (friend.IsOnline && !string.IsNullOrEmpty(friend.Name) && friend.Name == client.MasterName)
            {
                client.Self.InstantMessage(friend.UUID, "Hello Master");
            }
            AddName2Key(friend.Name, friend.UUID);
            //base.Friends_OnFriendOnline(friend);
        }

        public override void Friends_OnFriendNamesReceived(Dictionary<UUID, string> names)
        {
            base.Friends_OnFriendNamesReceived(names);
            bool masterFound = false;
            string clientMasterNameToLower = client.MasterName;
            foreach (KeyValuePair<UUID, string> kvp in names)
            {
                string kvpValueToLower = kvp.Value;
                AddName2Key(kvpValueToLower,kvp.Key);
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
                   AddName2Key(data.FirstName + " " + data.LastName, data.AgentID);
            });
        }

        public override void Directory_OnDirGroupsReply(UUID queryID, List<DirectoryManager.GroupSearchData> matchedGroups)
        {
            matchedGroups.ForEach(data =>
            {
                AddName2Key(data.GroupName,data.GroupID);
            });
        }

        public override void Friends_OnFriendRights(FriendInfo friend)
        {
            AddName2Key(friend.Name, friend.UUID);
            base.Friends_OnFriendRights(friend);
        }

        internal UUID GetUserID(string ToAvatarName)
        {
            UUID found;
            // case sensitive
            lock (Name2Key) if (Name2Key.TryGetValue(ToAvatarName, out found)) return found;
            // case insensitive
            lock (Name2Key)
                foreach (KeyValuePair<string, UUID> kvp in Name2Key)
                {
                    if (kvp.Key.ToLower() == ToAvatarName.ToLower())
                    {
                        return kvp.Value;

                    }
                }
            {
                ManualResetEvent NameSearchEvent = new ManualResetEvent(false);
                AvatarManager.AvatarNameSearchCallback callback =
                    new AvatarManager.AvatarNameSearchCallback((queryid, avatars) =>
                    {
                        foreach (KeyValuePair<UUID, string> kvp in avatars)
                        {
                            AddName2Key(kvp.Value, kvp.Key);
                            if (kvp.Value.ToLower() == ToAvatarName.ToLower())
                            {
                                NameSearchEvent.Set();
                                return;
                            }
                        }
                    });
                try
                {
                    client.Avatars.OnAvatarNameSearch += callback;
                    // Send the Query
                    client.Avatars.RequestAvatarNameSearch(ToAvatarName, UUID.Random());

                    NameSearchEvent.WaitOne(10000, false);
                }
                finally
                {
                    client.Avatars.OnAvatarNameSearch -= callback;
                }
            }

            lock (Name2Key) if (Name2Key.TryGetValue(ToAvatarName, out found)) return found;

            return found;
        }

    }
}
