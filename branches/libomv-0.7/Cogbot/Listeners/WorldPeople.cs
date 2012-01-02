﻿using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Interfaces;
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
            PCode pCode = PCode.None;
            if (sourceType == ChatSourceType.Agent)
            {
                pCode = PCode.Avatar;
            }
            if (sourceType == ChatSourceType.Object)
            {
                pCode = PCode.Prim;
            }
            SimObject source = AsObject(fromName, id, pCode);
            object s1 = source;
            if (source == null)
            {
                s1 = id;
            }
            if (!string.IsNullOrEmpty(fromName))
            {
                s1 = fromName;
            }
            if (source != null) s1 = source;
            object location = AsLocation(client.Network.CurrentSim, position, source);
            if (ownerid != id)
            {
                SendNewRegionEvent(SimEventType.NETWORK, "Bug", "id!=ownerID?", "on-chat",
                                   message, audible, type, sourceType, fromName, id, ownerid, location);

            }

            if (type == ChatType.Normal || type == ChatType.Shout || type == ChatType.Whisper)
            {
                EventQueue.Enqueue(() =>
                                   SendNewRegionEvent(SimEventType.SOCIAL, "ChatType-" + type.ToString(),
                                                      ToParameter("senderOfInfo", s1),
                                                      ToParameter("infoTransferred-NLString", message),
                                                      audible,
                                                      type,
                                                      sourceType,
                                                      ToParameter("eventPrimarilyOccursAt", location)));
                return;

            }

            if (string.IsNullOrEmpty(message))
            {

                EventQueue.Enqueue(
                    () =>
                    SendNewRegionEvent(SimEventType.SOCIAL, "ChatType-" + type.ToString(),
                                       audible,
                                       sourceType,
                                       type,
                                       ToParameter("senderOfInfo", s1),
                                       ToParameter("eventPrimarilyOccursAt", location)));
                return;
            }
            EventQueue.Enqueue(() =>
                               SendNewRegionEvent(SimEventType.SOCIAL, "ChatType-" + type.ToString(),
                                                  ToParameter("senderOfInfo", s1),
                                                  ToParameter("infoTransferred-NLString", message),
                                                  audible,
                                                  type,
                                                  sourceType,
                                                  ToParameter("eventPrimarilyOccursAt", location)));

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
            bool Conference = false;
            bool GroupIM = im.GroupIM || client.Groups.GroupName2KeyCache.ContainsKey(im.IMSessionID);
            if (GroupIM)
            {
                DeclareGroup(im.IMSessionID);
            }
            if (im.Dialog == InstantMessageDialog.SessionSend)
            {
                if (!GroupIM) Conference = true;
            }

            PCode pcode = PCode.None;
            object s = im.FromAgentName;
            if (string.IsNullOrEmpty(im.FromAgentName) || im.FromAgentName == "Object" || !im.FromAgentName.Contains(" "))
            {
                s = im.FromAgentID;
                pcode = PCode.Prim;
            }
            else
            {
                pcode = PCode.Avatar;
            }
            InstantMessageDialog d = im.Dialog;
            if (d == InstantMessageDialog.StartTyping || d == InstantMessageDialog.StopTyping)
            {
                pcode = PCode.Avatar;
            }
            if (!Conference && GroupIM)
            {

                //"recipientOfInfo-Intended";

            }
            SimObject source = AsObject(im.FromAgentName, im.FromAgentID, pcode);
            if (source != null) s = source;
            object location = AsLocation(im.RegionID, im.Position);
            EventQueue.Enqueue(() =>
                                client.SendPersonalEvent(SimEventType.SOCIAL, "InstantMessageDialog-" + im.Dialog.ToString() + (GroupIM ? "-Group" : ""),
                                             ToParameter("senderOfInfo", s),
                                             ToParameter("infoTransferred-NLString", im.Message),
                                             ToParameter("recipientOfInfo-Intended", im.ToAgentID),
                                             im.Offline,
                                             im.IMSessionID,
                                             ToParameter("eventPrimarilyOccursAt", location),
                                    //(im.GroupIM ? "GroupIM" : ""),
                                    //im.Dialog,
                                             im.ParentEstateID));
        }

        public override void Self_OnAlertMessage(string msg)
        {
            EventQueue.Enqueue(() => SendNewRegionEvent(SimEventType.SCRIPT, "On-Alert-Message", client.gridClient, msg));
        }

        public override void Self_OnAgentDataUpdated(string firstName, string lastName, UUID activeGroupID, string groupTitle, GroupPowers groupPowers, string groupName)
        {
            //OnEvent("On-Agent-Data-Updated", firstName, lastName, activeGroupID, groupTitle, groupPowers, groupName);
        }


        public static Dictionary<string, UUID> Name2Key = new Dictionary<string, UUID>();
        public static Dictionary<UUID, UUID> AvatarRegion = new Dictionary<UUID, UUID>();
        private Dictionary<UUID, object> uuid2Group = new Dictionary<UUID, object>();


        public override void Groups_OnCurrentGroups(Dictionary<UUID, Group> groups)
        {
            foreach (UUID key in groups.Keys)
            {
                Group g = groups[key];
                AddGroup2Key(g.Name, key);
                RegisterGroup(key, g);
            }
            //base.Groups_OnCurrentGroups(groups);
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
            SimAvatar A = CreateSimAvatar(avatarID, this, null);
            if (!MaintainAvatarMetaData) return;
            A.ProfileProperties = properties;
            UUID propertiesPartner = properties.Partner;
            if (propertiesPartner != UUID.Zero)
            {
                SimAvatarImpl AA = CreateSimAvatar(propertiesPartner, this, null);
                //if (AA.GetName() == null)
                //{
                //    String s = GetUserName(propertiesPartner);
                //    AA.AspectName = s;
                //}
            }
            if (properties.ProfileImage != UUID.Zero)
            {
               // RequestAsset(properties.ProfileImage, AssetType.Texture, true);
            }
            //TODO SendNewEvent("On-Avatar-Properties", GetAvatar(avatarID, null), properties);
        }

        public override void Grid_OnCoarseLocationUpdate(Simulator sim, List<UUID> newEntries, List<UUID> removedEntries)
        {
            if (!MaintainAvatarMetaData) return;

            foreach (UUID uuid in newEntries)
            {
                SimAvatarImpl A = CreateSimAvatar(uuid,this,sim);
                A.RegionHandle = sim.Handle;
                Vector3 pos;
                if (sim.AvatarPositions.TryGetValue(uuid, out pos))
                    A.SimPosition = pos;

            }
            //for (int i = 0; i < coarse.Location.Length; i++)
            //{
            //    if (i == coarse.Index.$bot)
            //    {
            //        simulator.positionIndexYou = i;
            //    }
            //    else if (i == coarse.Index.Prey)
            //    {
            //        simulator.positionIndexPrey = i;
            //    }
            //    simulator.avatarPositions.Add(new Vector3(coarse.Location[i].X, coarse.Location[i].Y,
            //        coarse.Location[i].Z * 4));
            //}

            if (newEntries.Count == 0 && removedEntries.Count == 0) return;


            //OnEvent("On-Coarse-Location-Update", paramNamesOnCoarseLocationUpdate, paramTypesOnCoarseLocationUpdate, sim, newEntries , removedEntries);
        }


        public override void Avatars_OnAvatarInterests(UUID avatarID, Avatar.Interests properties)
        {
            SimAvatar A = CreateSimAvatar(avatarID, this, null);
            if (!MaintainAvatarMetaData) return;
            A.AvatarInterests = properties;
        }


        private void RegisterGroup(UUID uuid, Group g)
        {
            lock (uuid2Group)
            {
                SimGroup v = DeclareGroup(uuid);
                v.Group = g;
            }
        }


        public override void Avatars_OnAvatarGroups(UUID avatarID, List<AvatarGroup> avatarGroups)
        {
            SimAvatar A = CreateSimAvatar(avatarID, this, null);
            foreach (AvatarGroup grp in avatarGroups)
            {
                AddGroup2Key(grp.GroupName,grp.GroupID);
                //TODO SendNewEvent("On-Avatar-Properties", GetAvatar(avatarID, null), grp);                
            }
        }

        public override void Groups_OnGroupRoles(UUID requestID, UUID groupID, Dictionary<UUID, GroupRole> roles)
        {
            DeclareGroup(groupID);
            if (!MaintainGroupMetaData) return;
            MetaDataQueue.Enqueue(() =>
                                      {
                                          foreach (var list in roles)
                                          {
                                              GroupRole value = list.Value;
                                              SimGeneric declareGeneric = DeclareGroupRole(groupID, list.Key);
                                              declareGeneric.Value = value;
                                              SendOnUpdateDataAspect(declareGeneric, "GroupRole", null, value);
                                              //a.AddInfoMap(new NamedParam("memberRole"))
                                          }
                                      });

        }

        private SimGeneric DeclareGroupRole(UUID groupID, UUID key)
        {
            // Create a non UUID.Zero for the "Everyone" GroupRole
            if (key == UUID.Zero)
            {
                byte[] i = groupID.GetBytes();
                for (int index = 0; index < i.Length; index++)
                {
                    i[index] ^= 0xff;
                }
                key = new UUID(i, 0);
            }
            return DeclareGeneric("GroupRole", key);
        }

        public override void Groups_OnGroupRolesMembers(UUID requestID, UUID groupID, List<KeyValuePair<UUID, UUID>> rolesMembers)
        {
            SimGroup g = DeclareGroup(groupID);
            if (MaintainGroupMetaData)
            {
                MetaDataQueue.Enqueue(() =>
                                          {
                                              foreach (var list in rolesMembers)
                                              {
                                                  SimGeneric declareGeneric = DeclareGroupRole(groupID, list.Key);
                                                  if (list.Value != UUID.Zero)
                                                  {
                                                      SimAvatarImpl a = CreateSimAvatar(list.Value, this,
                                                                                        null);

                                                      a.AddInfoMap(new NamedParam("simMemberRole",
                                                                                  declareGeneric));
                                                      SendOnUpdateDataAspect(a, "MemberRole", null, null);
                                                  }
                                              }
                                              if (g != null) SendOnUpdateDataAspect(g, "Members", null, null);
                                          });
            }
           // base.Groups_OnGroupRolesMembers(requestID, groupID, rolesMembers);
        }

        public override void Groups_OnGroupMembers(UUID requestID, UUID groupID, Dictionary<UUID, GroupMember> members)
        {
            SimGroup g = DeclareGroup(groupID);
            if (MaintainGroupMetaData)
                MetaDataQueue.Enqueue(() =>
                                          {
                                              foreach (var member in members)
                                              {
                                                  var v = member.Value;
                                                  if (member.Key == UUID.Zero) continue;
                                                  SimAvatarImpl A = CreateSimAvatar(member.Key, this, null);
                                                  //A.AddInfoMap(new NamedParam("GroupMember",groupID));               
                                              }
                                              if (g != null) SendOnUpdateDataAspect(g, "Members", null, null);
                                          });
            // base.Groups_OnGroupMembers(requestID, totalCount, members);
        }

        public override void Groups_OnGroupNames(Dictionary<UUID, string> groupNames)
        {
            foreach (KeyValuePair<UUID, string> kvp in groupNames)
            {
                AddGroup2Key(kvp.Value, kvp.Key);
            }
            ///base.Groups_OnGroupNames(groupNames);
        }

        public override void Groups_OnGroupAccountSummary(UUID groupID, GroupAccountSummary summary)
        {
            DeclareGroup(groupID);
            base.Groups_OnGroupAccountSummary(groupID, summary);
        }

        public override void Groups_OnGroupProfile(Group group)
        {
            if (group.ID == UUID.Zero) return;
            SimGroup v = DeclareGroup(group.ID);
            v.Group = group;
            MetaDataQueue.Enqueue(() =>
                                      {
                                          //RegisterUUIDMaybe(group.ID, v);
                                          SendOnUpdateDataAspect(v, "Group", null, group);
                                      });
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


        private void AddGroup2Key(string name, UUID uuid)
        {
            DeclareGroup(uuid);
            lock (Name2Key)
            {
                Name2Key[name] = uuid;
            }
        }

        public static SimGeneric DeclareGeneric(string genericName, UUID uuid)
        {
            if (uuid == UUID.Zero) return null;
            object g;
            if (uuidTypeObject.TryGetValue(uuid, out g))
                if (g is SimGeneric) return g as SimGeneric;

            lock (uuidTypeObject)
            {
                if (uuidTypeObject.TryGetValue(uuid, out g))
                {
                    if (g is SimGeneric) return g as SimGeneric;

                    if (g is BotMentalAspect)
                    {
                        throw new AbandonedMutexException("" + genericName + " for " + g);
                        return null;
                    }
                    return (SimGeneric) (uuidTypeObject[uuid] = new SimGeneric(genericName, uuid) {Value = g});
                }
                return (SimGeneric) (uuidTypeObject[uuid] = new SimGeneric(genericName, uuid));
            }
        }

        private SimGroup DeclareGroup(UUID uuid)
        {
            if (uuid == UUID.Zero) return null;
            object g;
            if (uuid2Group.TryGetValue(uuid, out g))
                if (g is SimGroup) return g as SimGroup;
            lock (uuid2Group)
            {
                if (uuid2Group.TryGetValue(uuid, out g))
                {
                    if (g is SimGroup) return g as SimGroup;
                    if (g is Group)
                    {
                        return (SimGroup)(uuid2Group[uuid] = new SimGroup(uuid) { Group = (Group)g });
                    }
                    if (g is BotMentalAspect)
                    {
                        throw new AbandonedMutexException("Group for " + g);
                        return null;
                    }
                    return null;
                }
                try
                {
                    return (SimGroup)(uuid2Group[uuid] = new SimGroup(uuid));
                }
                finally
                {
                    RequestGroupInfo(uuid);

                }
            }
        }

        static HashSet<UUID> GroupsRequested = new HashSet<UUID>();
        static HashSet<UUID> GroupsRequested2 = new HashSet<UUID>();
        private void RequestGroupInfo(UUID uuid)
        {
            lock (GroupsRequested) if (!GroupsRequested.Add(uuid)) return;
            GridMaster.client.Groups.RequestGroupProfile(uuid);
            if (!MaintainGroupMetaData) return;
            // WriteLine("Requesting groupInfo " + uuid);
            GridMaster.client.Groups.RequestGroupRoles(uuid);
            GridMaster.client.Groups.RequestGroupMembers(uuid);
            GridMaster.client.Groups.RequestGroupRoleMembers(uuid);
        }

        // like avatar picks or role names
        private void AddOther2Key(string name, UUID uuid)
        {
            lock (Name2Key)
            {
                Name2Key[name] = uuid;
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
            if (n.Length < 3)
            {
                Debug("AddName2Key: " + value + " " + id);
                return;
            }
            SimAvatarImpl A = CreateSimAvatar(id, this, null);
            A.AspectName = value;
            SendOnUpdateDataAspect(A, "simProperties-Name", null, value);
            
            lock (Name2Key)
            {
                Name2Key[value] = id;
            }
        }

        public override void Avatars_OnAvatarPicks(UUID avatarid, Dictionary<UUID, string> picks)
        {
            foreach (KeyValuePair<UUID, string> kvp in picks)
            {
                AddOther2Key(kvp.Value, kvp.Key);
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
                AddName2Key(kvpValueToLower, kvp.Key);
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

        public override void Directory_OnDirPeopleReply(object sender, DirPeopleReplyEventArgs e)
        {
            e.MatchedPeople.ForEach(data =>
            {
                AddName2Key(data.FirstName + " " + data.LastName, data.AgentID);
            });
        }

        public override void Directory_OnDirGroupsReply(object sender, DirGroupsReplyEventArgs e)
        {
            e.MatchedGroups.ForEach(data =>
            {
                AddGroup2Key(data.GroupName, data.GroupID);
            });
        }

        public override void Friends_OnFriendRights(FriendInfo friend)
        {
            AddName2Key(friend.Name, friend.UUID);
            base.Friends_OnFriendRights(friend);
        }

        public UUID GetUserID(string ToAvatarName)
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
            if (UUID.TryParse(ToAvatarName, out found)) return found;
            {
                client.Directory.StartPeopleSearch(/*DirectoryManager.DirFindFlags.People,*/ ToAvatarName, 0/*, UUID.Random()*/);

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

        public String GetUserName(UUID found)
        {

            SimObject obj0 = GetSimObjectFromUUID(found);
            if (obj0 != null)
            {
                string s = obj0.GetName();
                if (!string.IsNullOrEmpty(s))
                {
                    if (s.Trim() != "") return s;
                }
            }
            SimAvatarImpl AA = CreateSimAvatar(found, this, null);
            // case insensitive
            lock (Name2Key)
                foreach (KeyValuePair<string, UUID> kvp in Name2Key)
                {
                    if (kvp.Value == found)
                    {
                        return kvp.Key;

                    }
                }
            {
                ManualResetEvent NameSearchEvent = new ManualResetEvent(false);
                AvatarManager.AvatarNamesCallback callback =
                    new AvatarManager.AvatarNamesCallback((avatars) =>
                    {
                        foreach (KeyValuePair<UUID, string> kvp in avatars)
                        {
                            AddName2Key(kvp.Value, kvp.Key);
                            if (kvp.Key == found)
                            {
                                AA.AspectName = kvp.Value;
                                NameSearchEvent.Set();
                                return;
                            }
                        }
                    });
                try
                {
                    client.Avatars.OnAvatarNames += callback;
                    // Send the Query
                    client.Avatars.RequestAvatarName(found);
                    NameSearchEvent.WaitOne(10000, false);
                }
                finally
                {
                    client.Avatars.OnAvatarNames -= callback;
                }
            }

            lock (Name2Key)
                foreach (KeyValuePair<string, UUID> kvp in Name2Key)
                {
                    if (kvp.Value == found)
                    {
                        AA.AspectName = kvp.Key;
                        return kvp.Key;

                    }
                }

            return AA.GetName();
        }

        void InformMaster(string masterName)
        {
            UUID resolvedMasterKey = UUID.Zero;
            ManualResetEvent keyResolution = new ManualResetEvent(false);
            UUID query = UUID.Zero;
            BotClient Client = client;
            UUID masterUUID;
            if (UUID.TryParse(masterName, out masterUUID))
            {
                Client.MasterKey = masterUUID;
                if (String.IsNullOrEmpty(Client.MasterName))
                {
                    Client.MasterName = GetUserName(masterUUID);
                }
                Client.Self.InstantMessage(
                    Client.MasterKey, "You are now my master.  IM me with \"help\" for a command list.");
                WriteLine("Set master UUID with name = " + Client.MasterName);
            }
            masterUUID = GetUserID(masterName);
            //if (String.IsNullOrEmpty(Client.MasterName))                 
            Client.MasterName = masterName;
            if (masterUUID != UUID.Zero)
            {
                Client.MasterName = masterName;
                Client.MasterKey = masterUUID;
                Client.Self.InstantMessage(
                    Client.MasterKey, "You are now my master.  IM me with \"help\" for a command list.");

                WriteLine("Set master UUID with name = " + Client.MasterName);
            }

            EventHandler<DirPeopleReplyEventArgs> callback = delegate(object sender, DirPeopleReplyEventArgs e)
                                                                 {
                                                                     if (query != e.QueryID)
                                                                         return;

                                                                     resolvedMasterKey = e.MatchedPeople[0].AgentID;
                                                                     keyResolution.Set();
                                                                     query = UUID.Zero;
                                                                 };
            Client.Directory.DirPeopleReply += callback;

            query = Client.Directory.StartPeopleSearch(masterName, 0);

            if (keyResolution.WaitOne(TimeSpan.FromMinutes(1), false))
            {
                Client.MasterKey = resolvedMasterKey;
                Client.MasterName = masterName;
                keyResolution.Reset();
                Client.Directory.DirPeopleReply -= callback;
            }
            else
            {
                keyResolution.Reset();
                Client.Directory.DirPeopleReply -= callback;
                WriteLine("Unable to obtain UUID for \"" + masterName + "\". Master unchanged.");
                return;
            }

            // Send an Online-only IM to the new master
            Client.Self.InstantMessage(
                Client.MasterKey, "You are now my master.  IM me with \"help\" for a command list.");

        }

        private void AgentGroupDataUpdateHandler(string capskey, IMessage message, Simulator simulator)
        {
            //throw new NotImplementedException();
        }

        private void AgentGroupDataUpdatePT(Packet packet, Simulator simulator)
        {
            //throw new NotImplementedException();
        }
    }
}