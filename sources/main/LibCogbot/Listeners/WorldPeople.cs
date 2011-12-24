using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Interfaces;
using OpenMetaverse.Packets;
using PathSystem3D.Navigation;
using System.Diagnostics;

namespace cogbot.Listeners
{
    partial class WorldObjects
    {
        public bool MasterIsFriendYet = false;
               
        public override void Self_OnChat(object sender, ChatEventArgs e)
        {
            var type = e.Type;
            var message = e.Message;
            var id = e.SourceID;
            var fromName = e.FromName;
            var sourceType = e.SourceType;
            var position = e.Position;
            var ownerid = e.OwnerID;
            var audible = e.AudibleLevel;


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
            object location = AsLocation(e.Simulator, position, source);
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

        public override void Self_OnMuteListUpdated(object sender, EventArgs e)
        {
            client.DisplayNotificationInChat("MuteList count = " + client.Self.MuteList.Count);
        }
        public override void Self_OnInstantMessage(object sender, InstantMessageEventArgs e)
        {
            var im = e.IM;
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

        public override void Self_OnAlertMessage(object sender, AlertMessageEventArgs e)
        {
            EventQueue.Enqueue(() => SendNewRegionEvent(SimEventType.SCRIPT, "On-Alert-Message", client.gridClient, e.Message));
        }

        public override void Self_OnAgentDataUpdated(object sender, AgentDataReplyEventArgs e)
        {
            //OnEvent("On-Agent-Data-Updated", firstName, lastName, activeGroupID, groupTitle, groupPowers, groupName);
        }


        public static Dictionary<string, UUID> Name2Key = new Dictionary<string, UUID>();
        public static Dictionary<UUID, UUID> AvatarRegion = new Dictionary<UUID, UUID>();
        private Dictionary<UUID, object> uuid2Group = new Dictionary<UUID, object>();


        public override void Groups_OnCurrentGroups(object sender, CurrentGroupsEventArgs e)
        {
            var groups = e.Groups;
            foreach (UUID key in groups.Keys)
            {
                Group g = groups[key];
                AddGroup2Key(g.Name, key);
                RegisterGroup(key, g);
            }
            //base.Groups_OnCurrentGroups(groups);
            //OnEvent("On-Current-Groups", paramNamesOnCurrentGroups, paramTypesOnCurrentGroups, groups);
        }

        private void AvatarAppearanceHandler(object sender, PacketReceivedEventArgs e)
        {
            Simulator sim = e.Simulator;
            var packet = e.Packet;
            //if (sim != client.Network.CurrentSim) { Debug("AvatarAppearance: from a differnt sim than current " + sim); }
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
            //if (GetSimObjectFromUUID(id) == null)
            CreateSimAvatar(id, this, sim);
        }

        public override void Avatars_OnAvatarProperties(object sender, AvatarPropertiesReplyEventArgs e)
        {
            SimAvatar A = DeclareAvatarProfile(e.AvatarID);
            if (!MaintainAvatarMetaData) return;
            A.ProfileProperties = e.Properties;
            UUID propertiesPartner = e.Properties.Partner;
            if (propertiesPartner != UUID.Zero)
            {
                SimAvatarImpl AA = DeclareAvatarProfile(propertiesPartner);
                //if (AA.GetName() == null)
                //{
                //    String s = GetUserName(propertiesPartner);
                //    AA.AspectName = s;
                //}
            }
            if (e.Properties.ProfileImage != UUID.Zero)
            {
               // RequestAsset(properties.ProfileImage, AssetType.Texture, true);
            }
            //TODO SendNewEvent("On-Avatar-Properties", GetAvatar(avatarID, null), properties);
        }
        public override void Friends_OnFriendFound(object sender, FriendFoundReplyEventArgs e)
        {
            SimAvatar A = DeclareAvatarProfile(e.AgentID);
            A.UpdatePosition(e.RegionHandle, e.Location);
        }
        public override void Grid_OnCoarseLocationUpdate(object sender, CoarseLocationUpdateEventArgs e)
        {
            Simulator sim = e.Simulator;
            var newEntries = e.NewEntries;
            var removedEntries = e.RemovedEntries;
            if (newEntries.Count == 0 && removedEntries.Count == 0) return;
            foreach (UUID uuid in newEntries)
            {
                SimObject A = CreateSimAvatar(uuid, this, sim);
                Vector3 pos;
                if (sim.AvatarPositions.TryGetValue(uuid, out pos))
                    A.UpdatePosition(sim.Handle, pos);
            }
            foreach (UUID uuid in removedEntries)
            {
                DeclareAvatar(uuid);
            }
            if (!MaintainAvatarMetaData) return;
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

            //OnEvent("On-Coarse-Location-Update", paramNamesOnCoarseLocationUpdate, paramTypesOnCoarseLocationUpdate, sim, newEntries , removedEntries);
        }

        private SimAvatarImpl DeclareAvatar(UUID uuid)
        {
            return CreateSimAvatar(uuid, this, null);
        }

        private SimAvatarImpl DeclareAvatarProfile(UUID uuid)
        {
            SimAvatarImpl A = CreateSimAvatar(uuid, this, null);
            A.IsProfile = true;
            return A;
        }

        private SimObject DeclareTask(UUID uuid, Simulator simulator)
        {
            if (DiscoverTaskUUIDs)
            {
                lock (UUIDTypeObject)
                {
                    object g;
                    if (!UUIDTypeObjectTryGetValue(uuid, out g))
                    {
                        var o = CreateSimObject(uuid, this, simulator);
                        // SimGeneric o = DeclareGeneric("Task", uuid, "DeclareTask");
                        if (o != null)
                        {
                            // client.Objects.RequestObjectPropertiesFamily(simulator, uuid);
                            ///  client.Objects.RequestObjectMedia(uuid, simulator, OnMediaCallback);
                            Debug("Made Task: " + o);
                        }
                        return o;
                    }
                    return g as SimObject;
                }
            }
            return null;
        }

        private void OnMediaCallback(bool success, string version, MediaEntry[] facemedia)
        {
            
        }

        public override void Avatars_OnAvatarInterests(object sender, AvatarInterestsReplyEventArgs e)
        {
            SimAvatar A = DeclareAvatarProfile(e.AvatarID);
            if (!MaintainAvatarMetaData) return;
            A.AvatarInterests = e.Interests;
        }


        private void RegisterGroup(UUID uuid, Group g)
        {
            if (uuid == UUID.Zero) return;
            lock (uuid2Group)
            {
                SimGroup v = DeclareGroup(uuid);
                v.Group = g;
            }
        }


        public override void Avatars_OnAvatarGroups(object sender, AvatarGroupsReplyEventArgs e)
        {
            SimAvatar A = DeclareAvatarProfile(e.AvatarID);
            A.AddGoupRoles(e.Groups);
            foreach (AvatarGroup grp in e.Groups)
            {
                AddGroup2Key(grp.GroupName,grp.GroupID);
                //TODO SendNewEvent("On-Avatar-Properties", GetAvatar(avatarID, null), grp);                
            }
        }

        public override void Groups_OnGroupRoles(object sender, GroupRolesDataReplyEventArgs e)
        {
            var groupID = e.GroupID;
            DeclareGroup(groupID);
            if (!MaintainGroupMetaData) return;
            MetaDataQueue.Enqueue(() =>
                                      {
                                          foreach (var list in e.Roles)
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
                key = UUIDFactory.GetUUID(i, 0);
            }
            return DeclareGeneric("GroupRole", key, "DeclareGroupRole" );
        }

        public override void Groups_OnGroupRolesMembers(object sender, GroupRolesMembersReplyEventArgs e)
        {
            var groupID = e.GroupID;
            SimGroup g = DeclareGroup(groupID);
            if (MaintainGroupMetaData)
            {
                MetaDataQueue.Enqueue(() =>
                                          {
                                              foreach (var list in e.RolesMembers)
                                              {
                                                  SimGeneric declareGeneric = DeclareGroupRole(groupID, list.Key);
                                                  if (list.Value != UUID.Zero)
                                                  {
                                                      SimAvatarImpl a = DeclareAvatarProfile(list.Value);

                                                      a.AddInfoMapItem(new NamedParam("simMemberRole",
                                                                                  declareGeneric));
                                                      SendOnUpdateDataAspect(a, "MemberRole", null, null);
                                                  }
                                              }
                                              if (g != null) SendOnUpdateDataAspect(g, "Members", null, null);
                                          });
            }
           // base.Groups_OnGroupRolesMembers(requestID, groupID, rolesMembers);
        }

        public override void Groups_OnGroupMembers(object sender, GroupMembersReplyEventArgs e)
        {
            var groupID = e.GroupID;
            SimGroup g = DeclareGroup(groupID);
            if (MaintainGroupMetaData)
                MetaDataQueue.Enqueue(() =>
                                          {
                                              foreach (var member in e.Members)
                                              {
                                                  var v = member.Value;
                                                  if (member.Key == UUID.Zero) continue;
                                                  SimAvatarImpl A = DeclareAvatarProfile(member.Key);
                                                  //A.AddInfoMap(new NamedParam("GroupMember",groupID));               
                                              }
                                              if (g != null) SendOnUpdateDataAspect(g, "Members", null, null);
                                          });
            // base.Groups_OnGroupMembers(requestID, totalCount, members);
        }

        public override void Groups_OnGroupNames(object sender, GroupNamesEventArgs e)
        {
            foreach (KeyValuePair<UUID, string> kvp in e.GroupNames)
            {
                AddGroup2Key(kvp.Value, kvp.Key);
            }
            ///base.Groups_OnGroupNames(groupNames);
        }

        public override void Groups_OnGroupAccountSummary(object sender, GroupAccountSummaryReplyEventArgs e)
        {
            var groupID = e.GroupID;
            SimGroup g = DeclareGroup(groupID);
            g.Summary = e.Summary; 
            base.Groups_OnGroupAccountSummary(sender, e);
        }

        public override void Groups_OnGroupProfile(object sender, GroupProfileEventArgs e)
        {
            Group group = e.Group;
            if (group.ID == UUID.Zero) return;
            SimGroup v = DeclareGroup(group.ID);
            v.Group = group;
            MetaDataQueue.Enqueue(() =>
                                      {
                                          //RegisterUUIDMaybe(group.ID, v);
                                          SendOnUpdateDataAspect(v, "Group", null, group);
                                      });
        }

        public override void Avatars_OnAvatarNameSearch(object sender, AvatarPickerReplyEventArgs e1)
        {
            foreach (KeyValuePair<UUID, string> kvp in e1.Avatars)
            {
                AddName2Key(kvp.Value, kvp.Key);
            }
        }

        public override void Avatars_OnAvatarNames(object sender, UUIDNameReplyEventArgs e1)
        {
            foreach (KeyValuePair<UUID, string> kvp in e1.Names)
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

        public static SimGeneric DeclareGeneric(string genericName, UUID uuid, string moreInfo)
        {
            if (uuid == UUID.Zero) return null;
            object g;
            SimGeneric sg = null;
            if (UUIDTypeObjectTryGetValue(uuid, out g))
                if (g is SimGeneric) return g as SimGeneric;

            lock (UUIDTypeObject)
            {
                if (UUIDTypeObjectTryGetValue(uuid, out g))
                {
                    if (g is SimGeneric) return g as SimGeneric;

                    if (g != null)
                    {
                        string issue = moreInfo + " " + genericName + " " + uuid + " is really a  " + g.GetType().Name + ": " + g;
                        //throw new AbandonedMutexException(issue);
                        if (!(g is Type))
                        {
                            Debug(issue);
                            return null;
                        }
                        sg = (SimGeneric) (UUIDTypeObjectSetValue(uuid, new SimGeneric(genericName, uuid) {Value = g}));
                        return sg;
                    }
                }
                sg = (SimGeneric) (UUIDTypeObjectSetValue(uuid, new SimGeneric(genericName, uuid)));
                return sg;
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
                        SimGroup sg = new SimGroup(uuid) {Group = (Group) g};
                        RegisterUUID(uuid, sg);
                        return (SimGroup)(uuid2Group[uuid] = sg);
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
                    SimGroup sg = new SimGroup(uuid);
                    RegisterUUID(uuid, sg);
                    return (SimGroup)(uuid2Group[uuid] = sg);
                }
                finally
                {
                    RequestGroupInfo(uuid);

                }
            }
        }

        static HashSet<UUID> GroupsRequested = new HashSet<UUID>();
        static HashSet<UUID> MetadataRequested = new HashSet<UUID>();
        private void RequestGroupInfo(UUID uuid)
        {
            lock (GroupsRequested) if (!GroupsRequested.Add(uuid)) return;
            GridMaster.client.Groups.RequestGroupProfile(uuid);
            if (!RequestGroupMetaData) return;
            RequestGroupMetadata(uuid);
        }


        private void RequestAvatarMetadata(UUID uuid)
        {
            lock (MetadataRequested) if (!MetadataRequested.Add(uuid)) return;
            DeclareAvatarProfile(uuid);
            NeedRequestAvatarName(uuid);
            OnConnectedQueue.Enqueue(() =>
            {
                client.Avatars.RequestAvatarName(uuid);
                if (MaintainAvatarMetaData)
                {
                    client.Avatars.RequestAvatarProperties(uuid);
                    //client.Avatars.RequestAvatarClassified(uuid);
                    //client.Avatars.RequestAvatarPicks(uuid);
                }
            });
        }

        private void RequestGroupMetadata(UUID uuid)
        {
            lock (MetadataRequested) if (!MetadataRequested.Add(uuid)) return;
            OnConnectedQueue.Enqueue(() =>
                                         {
                                             // should not loop
                                             RequestGroupInfo(uuid);
                                             // WriteLine("Requesting groupInfo " + uuid);
                                             GridMaster.client.Groups.RequestGroupRoles(uuid);
                                             GridMaster.client.Groups.RequestGroupMembers(uuid);
                                             GridMaster.client.Groups.RequestGroupRolesMembers(uuid);
                                         });
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
            if (string.IsNullOrEmpty(value))
            {
                Debug("AddName2Key: ERROR name='" + value + "' " + id);
                return;
            }
            if (id == UUID.Zero)
            {
                Debug("AddName2Key: ERROR UUID.Zero " + value);
                return;
            }
            if (value.Contains("?"))
            {
                SimObject O = GetSimObjectFromUUID(id);
                if (O != null)
                {
                    if (O is SimAvatar)
                    {
                        Debug("ERROR AVATAR?!?!? " + value + " " + O);
                    }
                    return;
                }
                object obj = GetObject(id);
                if (obj is UUID || obj is string)
                {
                    lock (RequestedAvatarNames)
                    {
                        object stackTrace;
                        if (RequestedAvatarNames.TryGetValue(id, out stackTrace))
                        {
                           // Debug("AddName2Key: ERROR requested Name for ID=" + id + " " + stackTrace.ToString());
                            RequestedAvatarNames.Remove(id);
                        }
                        else
                        {
                            Debug("AddName2Key: ERROR NOT requested Name for ID=" + id);
                        }
                    }
                    var Obj = CreateSimObject(id, this, null);
                    Obj.ConfirmedObject = false;
                    Debug("AddName2Key: INFO Discovered new SimObject from UUID? " + value + " " + id + " " + Obj);
                    return;
                }
                else
                {
                    Debug("AddName2Key: ERROR UUID? " + value + " " + id + " " + obj);
                }
            }
            string n = value.Trim();
            if (n.Length < 3)
            {
                Debug("AddName2Key: INFO " + value + " " + id);
                return;
            }
            SimAvatarImpl A = DeclareAvatarProfile(id);
            A.AspectName = value;
            SendOnUpdateDataAspect(A, "simProperties-Name", null, value);
            
            lock (Name2Key)
            {
                Name2Key[value] = id;
            }
        }

        public override void Avatars_OnAvatarPicks(object sender, AvatarPicksReplyEventArgs e)
        {
            var picks = e.Picks;
            foreach (KeyValuePair<UUID, string> kvp in picks)
            {
                AddOther2Key(kvp.Value, kvp.Key);
            }
        }

        public override void Friends_OnFriendOnline(object sender, FriendInfoEventArgs e)
        {
            var friend = e.Friend;
            UUID id = friend.UUID;
            if (friend.IsOnline && !string.IsNullOrEmpty(friend.Name) && friend.Name == client.MasterName)
            {
                client.Self.InstantMessage(id, "Hello Master");
            }
            if (!string.IsNullOrEmpty(friend.Name))
                AddName2Key(friend.Name, id);
            DeclareAvatarProfile(id);
            client.Friends.TrackFriend(id);
            //base.Friends_OnFriendOnline(friend);
        }


        public override void Friends_OnFriendNamesReceived(object sender, FriendNamesEventArgs e)
        {
            IEnumerable names = e.Names;
            base.Friends_OnFriendNamesReceived(sender, e);
            string clientMasterNameToLower = client.MasterName;
            foreach (KeyValuePair<UUID, string> kvp in names)
            {
                string kvpValueToLower = kvp.Value;
                AddName2Key(kvpValueToLower, kvp.Key);
                if (clientMasterNameToLower == kvpValueToLower)
                {
                    MasterIsFriendYet = true;
                    client.MasterKey = kvp.Key;
                }
                else if (kvp.Key == client.MasterKey)
                {
                    client.MasterName = kvp.Value;
                    MasterIsFriendYet = true;
                }
            }
            if (!MasterIsFriendYet)
            {
                if (!string.IsNullOrEmpty(clientMasterNameToLower))
                {
                    {
                        lock (Name2Key)
                        {
                            if (Name2Key.ContainsKey(clientMasterNameToLower))
                            {
                                client.Friends.OfferFriendship(Name2Key[clientMasterNameToLower]);
                                MasterIsFriendYet = true;
                            }
                        }
                    }
                }
            }
            if (!MasterIsFriendYet)
            {
                if (client.MasterKey != UUID.Zero)
                {
                    client.Friends.OfferFriendship(client.MasterKey);
                    MasterIsFriendYet = true;
                }
            }
            if (!MasterIsFriendYet)
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

        public override void Friends_OnFriendRights(object sender, FriendInfoEventArgs e)
        {
            var friend = e.Friend;
            AddName2Key(friend.Name, friend.UUID);
            base.Friends_OnFriendRights(sender, e);
        }

        public UUID GetUserID(string ToAvatarName)
        {
            if (ToAvatarName.StartsWith("$"))
            {
                int usedArgs;
                var col = ResolveCollection(ToAvatarName.ToLower(), out usedArgs, null);
                if (col.Count>0)
                {
                    foreach (var prim in col)
                    {
                        if (prim is SimObject) return ((SimObject)prim).ID;
                        if (prim is string)
                        {
                            string s = (string) prim;
                            if (s != ToAvatarName) return GetUserID(s);
                        }
                    }
                }
            }
            UUID found = UUID.Zero;
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
                UUID queryID = UUID.Random();
                EventHandler<AvatarPickerReplyEventArgs> callback =
                    new EventHandler<AvatarPickerReplyEventArgs>((s, e) =>
                    {
                        foreach (KeyValuePair<UUID, string> kvp in e.Avatars)
                        {
                            AddName2Key(kvp.Value, kvp.Key);
                        }
                        if (queryID == e.QueryID) NameSearchEvent.Set();
                    });
                try
                {
                    client.Avatars.AvatarPickerReply += callback;
                    // Send the Query
                    client.Avatars.RequestAvatarNameSearch(ToAvatarName, queryID);

                    NameSearchEvent.WaitOne(10000, false);
                }
                finally
                {
                    client.Avatars.AvatarPickerReply -= callback;
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
            SimAvatarImpl AA = DeclareAvatarProfile(found);
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
                UUID queryID = UUID.Random();
                ManualResetEvent NameSearchEvent = new ManualResetEvent(false);
                EventHandler<UUIDNameReplyEventArgs> callback =
                    new EventHandler<UUIDNameReplyEventArgs>((s, e) =>
                    {
                        foreach (KeyValuePair<UUID, string> kvp in e.Names)
                        {
                            AddName2Key(kvp.Value, kvp.Key);
                            if (kvp.Key == found)
                            {
                                AA.AspectName = kvp.Value;
                                NameSearchEvent.Set();
                            }
                        }
                    });
                try
                {
                    client.Avatars.UUIDNameReply += callback;
                    // Send the Query
                    RequestAvatarName(found);
                    NameSearchEvent.WaitOne(10000, false);
                }
                finally
                {
                    client.Avatars.UUIDNameReply -= callback;
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

        private Dictionary<UUID, object> RequestedAvatarNames = new Dictionary<UUID, object>();
        private void RequestAvatarName(UUID uuid)
        {
            if (NeedRequestAvatarName(uuid)) client.Avatars.RequestAvatarName(uuid);
        }

        private bool NeedRequestAvatarName(UUID uuid)
        {
            lock (RequestedAvatarNames)
            {
                if (!RequestedAvatarNames.ContainsKey(uuid))
                {
                    object st = null;
                    if (DLRConsole.SkipStackTraces) st = new object();
                    st = st ?? new StackTrace(true);
                    RequestedAvatarNames.Add(uuid, st);
                    return true;
                }
            }
            return false;
        }
        void InformMaster(string masterName)
        {
            UUID resolvedMasterKey = UUID.Zero;
            ManualResetEvent keyResolution = new ManualResetEvent(false);
            UUID query = UUID.Zero;
            BotClient Client = client;
            UUID masterUUID = UUID.Zero;
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

        private void AgentGroupDataUpdatePT(object sender, PacketReceivedEventArgs e)
        {
            //throw new NotImplementedException();
        }

        public GroupPowers GetGroupPowers(UUID uuid, UUID groupid)
        {
            DeclareGroup(groupid);
            RequestGroupInfo(groupid);
            RequestGroupMetadata(groupid);
            var A = DeclareAvatarProfile(uuid);
            var R = A.GroupRoles;
            if (R==null)
            {
                RequestAvatarMetadata(uuid);
                // not known yet
                return GroupPowers.None;
            }
            lock (R)
            {
                AvatarGroup agr;
                if (R.TryGetValue(groupid, out agr)) return agr.GroupPowers;
                // not in group
                return GroupPowers.None;

            }
            if (uuid == client.Self.AgentID)
            {
                Group g;
                if (GridMaster.client.GroupsCache.TryGetValue(groupid, out g))
                {
                    return g.Powers;
                }
                // not in group
                return GroupPowers.None;
            }
            // not known yet
            return GroupPowers.None;
        }
        public static bool HasValue<T>(T properties)
        {
            if (Object.ReferenceEquals(properties, null)) return false;
            var dt = default(T);
            return !Equals(dt, properties);
        }
    }

    internal static class UUIDFactory
    {
        public static UUID GetUUID(byte[] bytes, int pos)
        {
            return new UUID(bytes,pos);
        }
    }
}
