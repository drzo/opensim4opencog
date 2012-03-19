using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Threading;
using cogbot.Actions;
using cogbot.Listeners;
using cogbot.Utilities;
using MushDLR223.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;
using UUID=OpenMetaverse.UUID;
using System.Drawing;
using Math=System.Math;
using String=System.String;

/// Complex outcomes may be a result of simple causes, or they may just be complex by nature. 
/// Those complexities that turn out to have simple causes can be simulated and studied, 
/// thus increasing our knowledge without needing direct observation.

namespace cogbot.TheOpenSims
{
    public partial class SimAvatarImpl : SimObjectImpl, SimAvatar
    {
        protected BotAction _currentAction;
        public BotAction LastAction { get; set; }
        public string AspectName { get; set; }
        protected double ApproachDistance = 2f;

        //public SimPosition ApproachPosition { get; set; }
        public Vector3d ApproachVector3D { get; set; }

        /// <summary>
        ///  Action template stubs 
        /// </summary>
        public IEnumerable<SimTypeUsage> KnownTypeUsages
        {
            get { return _knownTypeUsages; }
        }

        private string PostureType;
        private SimObjectEvent LastPostureEvent;
        readonly private object postureLock = new object();

        public bool IsProfile;
        public Dictionary<UUID, AvatarGroup> GroupRoles { get; set; }

        public override string DebugInfo()
        {

            string s = String.Format("{0} {1} {2}", GetName(), ID, GetHeading()) + "\n " + base.ToString();
            if (ActionEventQueue != null) lock (ActionEventQueue)
                {
                    if (ActionEventQueue.Count == 0) return s + " -NoActions- ";
                    foreach (SimObjectEvent s1 in ActionEventQueue)
                    {
                        s += "\n " + s1;

                    }
                }
            return s;

        }
        //public override bool OnEffect(string effectType, object t, object p, float duration, UUID id)
        //{
        //    bool noteable = LogEvent(new SimObjectEvent(effectType, SimEventType.EFFECT, SimEventStatus.Once, this, t, p, duration, id));
        //    //todo
        //    if (noteable) WorldSystem.SendNewEvent("on-effect", effectType, this, t, p, duration, id);
        //    return noteable;
        //    //throw new NotImplementedException();
        //}

        public override bool LogEvent(SimObjectEvent SE)
        {
            string typeUse = SE.Verb;
            object[] args1_N = SE.GetArgs();
            // does this backwards to the first argument is the most reliavant object
            for (int i = args1_N.Length - 1; i >= 0; i--)
            {
                object o = args1_N[i];
                if (o == this) continue; //skip self
                if (o is SimObject) KnownSimObjects.Add((SimObject)o);
            }
            _knownTypeUsages.AddTo(SimTypeSystem.CreateTypeUsage(typeUse));
            bool noteable = base.LogEvent(SE);
            if (noteable)
            //if (theAvatar.Name.Contains("rael"))
            {
                //DLRConsole.DebugWriteLine(SE);
            }
            return noteable;
        }
        /// <summary>
        ///  Action template stubs 
        /// </summary>
        internal ListAsSet<SimTypeUsage> _knownTypeUsages;

        public override bool IsPassable
        {
            get
            {
                return true || base.IsPassable;
            }
            set
            {
                base.IsPassable = value;
            }
        }

        public override bool IsPhantom
        {
            get
            {
                return true || base.IsPhantom;
            }
            set
            {
                base.IsPhantom = value;
            }
        }
        public override bool IsSolid
        {
            get
            {
                return false && base.IsSolid;
            }
            set
            {
                base.IsSolid = value;
            }
        }

        public override bool IsRoot
        {
            get { Avatar theAvatar = this.theAvatar; return theAvatar == null || theAvatar.ParentID == 0; }
        }

        ///  public override ISimObject Parent {  get { return this; }   }

        public virtual bool IsSitting
        {
            get
            {
                if (SimAssetStore.Matches(GetCurrentAnims(), "sit").Count > 0) return true;
                var theAvatar = this.theAvatar;
                if (theAvatar == null) return false;
                return theAvatar.ParentID != 0;
            }
            set
            {
                if (IsSitting == value) return;
                Debug("Uncontroled IsStting=" + value);
            }
        }

        protected bool noticedOnline;
        public virtual bool IsOnline
        {
            get
            {
                var fi = FriendshipInfo;
                if (fi != null && fi.CanSeeThemOnline)
                {
                    return fi.IsOnline;
                }
                return HasPrim || noticedOnline;
            }
            set
            {
                noticedOnline = value;
            }
        }

        public override void UpdatePosition(ulong handle, Vector3 pos)
        {
            noticedOnline = !handle.Equals(0);
            base.UpdatePosition(handle, pos);
        }

        public override bool HasPrim
        {
            get
            {
                if (base.HasPrim && _Prim0 is Avatar) return true;
                if (RegionHandle != 0)
                {
                    Simulator S = WorldSystem.GetSimulator(RegionHandle);
                    Primitive prim = WorldSystem.GetLibOMVHostedPrim(ID, S, true);
                    if (prim == null) return false;
                    SetFirstPrim(prim);
                    return _Prim0 is Avatar;
                }
                return false;
                return LastKnownSimPos != Vector3.Zero;
            }
        }
        public override bool IsControllable
        {
            get
            {
                return false;
            }
        }


        public override void AddCanBeTargetOf(int argN, SimObjectEvent evt)
        {
            base.AddCanBeTargetOf(argN, evt);
        }

        public Avatar theAvatar
        {
            get
            {
                Primitive P = Prim;
                if (P is Avatar) return (Avatar)P;
                return null;
                uint l = P.LocalID;
                UUID id = P.ID;
                var simulator = GetSimulator();
                Avatar found = simulator.ObjectsAvatars.Find(prim0 =>
                                                                 {
                                                                     if ((prim0.ID == id || prim0.LocalID == l))
                                                                         return true;
                                                                     return false;
                                                                 });
                if (found != null)
                {
                    SetFirstPrim(found);
                    return found;
                }
                return null;
            }
        }

        //public SimAvatar InDialogWith { get; set; }


        private double _SightRange = 260.0;

        public double SightRange
        {
            get { return _SightRange; }
            set
            {
                if (value > _SightRange)
                {
                    GetNearByObjects(value, false);
                }
                _SightRange = value;
            }
        }


        /// <summary>
        /// things the bot cycles through mentally
        /// </summary>
        public readonly ListAsSet<SimObject> KnownSimObjects = new ListAsSet<SimObject>();

        /// <summary>
        /// Returns hopefully at least three objects sorted by distance
        /// </summary>
        /// <returns></returns>
        public ListAsSet<SimObject> GetKnownObjects()
        {
            ScanNewObjects(3, SightRange, false);
            //lock (KnownSimObjects)
            //   SortByDistance(KnownSimObjects.RealListT);
            // or 
            KnownSimObjects.Sort(CompareDistance);
            return KnownSimObjects;
        }

        public override List<SimObject> GetNearByObjects(double maxDistance, bool rootOnly)
        {
            List<SimObject> near = base.GetNearByObjects(maxDistance, rootOnly);
            AddKnowns(near);
            return near;
        }

        public SimObject FindSimObject(SimObjectType pUse, double maxXYDistance, double maxZDist)
        {
            double myZ = GlobalPosition.Z;
            IList<SimObject> objects = GetKnownObjects();
            lock (objects) foreach (SimObject O in objects)
                {
                    if (O.Distance(this) > maxXYDistance) continue;
                    if (Math.Abs(O.GlobalPosition.Z - myZ) > maxZDist) continue;
                    if (O.Affordances.IsTypeOf(pUse) != null) return O;
                }
            return null;
        }
        public override void UpdateObject(ObjectMovementUpdate objectUpdate, ObjectMovementUpdate objectUpdateDiff)
        {
        }

        public void RemoveObject(SimObject O)
        {
            KnownSimObjects.Remove(O);
        }

        public SimAvatarImpl(UUID id, WorldObjects objectSystem, Simulator sim)
            : base(id, objectSystem, sim)
        {
            Affordances.ObjectType.SuperType.Add(SimTypeSystem.GetObjectType("Avatar"));
            _knownTypeUsages = new ListAsSet<SimTypeUsage>();
            WorldObjects.SimAvatars.Add(this);
        }

        public FriendInfo FriendshipInfo
        {
            get
            {
                FriendInfo info;
                var Client = WorldSystem.client;
                if (Client == null) return null;
                if (!Client.Friends.FriendList.TryGetValue(ID, out info)) return null;
                return info;
            }
        }

        private Avatar.AvatarProperties _profileProperties;
        public Avatar.AvatarProperties ProfileProperties
        {
            get
            {
                lock (HasPrimLock) if (HasPrim)
                    {
                        var theAvatar = this.theAvatar;
                        if (theAvatar != null && WorldObjects.HasValue(theAvatar.ProfileProperties) && !string.IsNullOrEmpty(theAvatar.ProfileProperties.BornOn))
                        {
                            _profileProperties = theAvatar.ProfileProperties;
                        }
                    }
                return _profileProperties;
            }
            set
            {
                lock (HasPrimLock) if (HasPrim)
                    {
                        var theAvatar = this.theAvatar;
                        if (theAvatar != null && !WorldObjects.HasValue(theAvatar.ProfileProperties) || string.IsNullOrEmpty(theAvatar.ProfileProperties.BornOn))
                        {
                            theAvatar.ProfileProperties = value;
                        }
                    }
                _profileProperties = value;
                AddInfoMap(value, "ProfileProperties");
            }
        }

        private Avatar.Interests _AvatarInterests;
        public Avatar.Interests AvatarInterests
        {
            get
            {
                lock (HasPrimLock) if (HasPrim)
                    {
                        var theAvatar = this.theAvatar;
                        if (theAvatar != null && !string.IsNullOrEmpty(theAvatar.ProfileInterests.LanguagesText))
                        {
                            _AvatarInterests = theAvatar.ProfileInterests;
                        }
                    }
                return _AvatarInterests;
            }
            set
            {
                lock (HasPrimLock) if (HasPrim)
                    {
                        var theAvatar = this.theAvatar;
                        if (theAvatar != null && string.IsNullOrEmpty(theAvatar.ProfileInterests.LanguagesText))
                        {
                            theAvatar.ProfileInterests = value;
                        }
                    }
                _AvatarInterests = value;
                AddInfoMap(value, "AvatarInterests");
            }
        }
        private List<UUID> _AvatarGroups;
        public List<UUID> AvatarGroups
        {
            get
            {
                lock (HasPrimLock) if (HasPrim)
                    {
                        if (theAvatar.Groups != null)
                        {
                            _AvatarGroups = theAvatar.Groups;
                        }
                    }
                return _AvatarGroups;
            }
            set
            {
                lock (HasPrimLock) if (HasPrim)
                    {
                        if (theAvatar.Groups == null)
                        {
                            theAvatar.Groups = value;
                        }
                    }
                _AvatarGroups = value;
            }
        }
        public override bool IsKilled
        {
            ///  get { return WasKilled; }
            set
            {
                if (WasKilled == value) return;
                lock (HasPrimLock) if (!WasKilled) /// already
                    {
                        IEnumerable<SimObject> AttachedChildren0 = Children;
                        lock (AttachedChildren0)
                            foreach (SimObject C in AttachedChildren0)
                            {
                                C.IsKilled = true;
                            }
                        ///  RemoveCollisions();
                    }
                WasKilled = value;
            }
        }


        public override Simulator GetSimulator()
        {
            // if (Client!=null && Client.Self.AgentID == Prim.ID) return Client.Network.CurrentSim;
            var R = GetSimRegion();
            if (R == null)
            {
                return null;
            }
            return R.TheSimulator;
        }


        public override Vector3d GlobalPosition
        {
            get
            {
                if (RegionHandle != 0 && SimPosition != default(Vector3))
                {
                    return ToGlobal(RegionHandle, SimPosition);
                }
                var Client = WorldSystem.client;
                var fi = FriendshipInfo;
                if (fi != null && fi.CanSeeThemOnMap)
                {
                    Client.Friends.MapFriend(ID);
                    Client.Friends.TrackFriend(ID);
                    //return fi.IsOnline;
                }

                /// if (Client.Self.AgentID == Prim.ID)
                /// {
                ///     if (Client.Settings.OBJECT_TRACKING)
                ///         return Client.Self.GlobalPosition;

                /// }
                return default(Vector3d);// base.GlobalPosition;
                //return GetSimRegion().LocalToGlobal(GetSimPosition());
            }
        }

        public override SimRegion GetSimRegion()
        {
            return base.GetSimRegion();
            //if (Prim == null)
            //{
            //    if (_CurrentRegion == null)
            //        if (IsControllable)
            //        {
            //            _CurrentRegion = SimRegion.GetRegion(Client.Network.CurrentSim);
            //        }
            //    return _CurrentRegion;
            //}

            //if (Prim.RegionHandle == 0)
            //{
            //    DLRConsole.WriteLine("Don't know the region for " + this);
            //}

            //if (_CurrentRegion == null)
            //{
            //    if (Prim.RegionHandle==0)
            //    {
            //        DLRConsole.WriteLine("Dont know the region for " + this);
            //    }
            //    _CurrentRegion = SimRegion.GetRegion(Prim.RegionHandle);
            //    Debug("out of date _CurrentRegion ");
            //}
            //if (theAvatar.RegionHandle != _CurrentRegion.RegionHandle)
            //{
            //    /// Debug("out of date RegionHandle ");
            //    /// _CurrentRegion = null;
            //}
            //return _CurrentRegion;
        }


        public override Quaternion SimRotation
        {
            get
            {
                //if (IsControllable)
                //{
                //    if (Client.Settings.OBJECT_TRACKING)
                //        return Client.Self.SimRotation;
                //}
                /// lock (Prim)
                {
                    //if (theAvatar.RegionHandle != _CurrentRegion.RegionHandle)
                    //{
                    //    Debug("out of date RegionHandle ");
                    //}
                    return base.SimRotation;
                }
            }
        }

        /// <summary>
        ///  
        /// </summary>
        /// <param name="minimum"></param>
        /// <param name="sightRange"></param>
        public void ScanNewObjects(int minimum, double sightRange, bool rootOnly)
        {
            List<SimObject> objects = GetNearByObjects(sightRange, rootOnly);
            ///  ill do this for us: AddKnowns(objects);
            if (KnownSimObjects.Count < minimum)
            {
                if (sightRange < 255)
                    ScanNewObjects(minimum, sightRange + 10, false);
            }
        }

        /// <summary>
        ///  
        /// </summary>
        /// <param name="objects"></param>
        private void AddKnowns(IEnumerable<SimObject> objects)
        {
            lock (objects)
            {
                foreach (SimObject obj in objects)
                {
                    if (obj != this)
                        if (obj.IsRoot || obj.Affordances.IsTyped)
                        {
                            //lock (KnownSimObjects)
                            if (!KnownSimObjects.Contains(obj))
                            {
                                KnownSimObjects.Add(obj);
                                IList<SimTypeUsage> uses = obj.Affordances.GetTypeUsages();
                                foreach (SimTypeUsage use in uses)
                                {
                                    lock (_knownTypeUsages)
                                        if (!_knownTypeUsages.Contains(use))
                                        {
                                            _knownTypeUsages.Add(use);
                                        }
                                }
                            }
                        }
                }
            }
        }

        /// <summary>
        ///  
        /// </summary>
        /// <param name="regionHandle"></param>
        public override void ResetRegion(ulong regionHandle)
        {
            bool changed = (regionHandle != RegionHandle);
            base.ResetRegion(regionHandle);
            if (changed && IsRegionAttached)
            {
                //lock (KnownSimObjects) 
                KnownSimObjects.Clear();
                GetKnownObjects();
            }
        }


        /// <summary>
        ///   Avatars approach distance
        /// </summary>
        /// <returns></returns>
        public override float GetSizeDistance()
        {
            return 2f;
        }
        public void AddGoupRoles(List<AvatarGroup> groups)
        {
            if (theAvatar != null)
            {
                foreach (AvatarGroup avatarGroup in groups)
                {
                    AddGroup(avatarGroup.GroupID);
                }
            }
            GroupRoles = GroupRoles ?? new Dictionary<UUID, AvatarGroup>();
            lock (GroupRoles)
                foreach (var avatarGroup in groups)
                {
                    var id = avatarGroup.GroupID;
                    AvatarGroup prev;
                    if (GroupRoles.TryGetValue(id, out prev))
                    {
                        if (false && prev.GroupPowers != avatarGroup.GroupPowers)
                        {
                            Debug("GroupPowers changed = " + prev + " -> " + avatarGroup);
                        }
                    }
                    GroupRoles[id] = avatarGroup;
                }
        }
        public bool InGroup(UUID uuid)
        {
            if (CogbotHelpers.IsNullOrZero(uuid)) return false;
            if (GroupRoles != null) if (GroupRoles.ContainsKey(uuid)) return true;
            var ag = AvatarGroups;
            return ag != null && ag.Contains(uuid);
        }
        public void AddGroup(UUID groupID)
        {
            if (theAvatar != null)
            {
                List<UUID> agroups = theAvatar.Groups;
                if (agroups == null)
                {
                    agroups = theAvatar.Groups = new List<UUID>();
                }
                if (!agroups.Contains(groupID)) agroups.Add(groupID);
            }
        }

        public void AddGroupRole(UUID group, UUID role)
        {
            AddGroup(group);
        }

        public PermissionWho EffectivePermissionWho(SimObject exportPrim)
        {
            Primitive.ObjectProperties permProps = exportPrim.Properties;
            Primitive prim = exportPrim.Prim;
            UUID objectGroupID = UUID.Zero;
            UUID ownerID = UUID.Zero;
            PrimFlags flag = PrimFlags.None;
            if (prim != null)
            {
                objectGroupID = prim.GroupID;
                ownerID = prim.OwnerID;
                flag = prim.Flags;
            } else if (permProps != null)
            {
                objectGroupID = permProps.GroupID;
                ownerID = permProps.OwnerID;
            }
            bool groupOwned = (flag & PrimFlags.ObjectGroupOwned) != 0;
          //  bool groupOwned = (flag & PrimFlags.) != 0;
            return EffectivePermissionWho(ownerID, objectGroupID, groupOwned);
        }

        public PermissionWho EffectivePermissionWho(UUID ownerID, UUID objectGroupID, bool groupOwned)
        {
            if (ownerID == ID) return PermissionWho.Owner;
            if (!CogbotHelpers.IsNullOrZero(objectGroupID) && InGroup(objectGroupID))
            {
                if (groupOwned) return PermissionWho.Owner;
                return PermissionWho.Owner;
                return PermissionWho.Group;
            }
            return PermissionWho.Everyone;
        }

        public PermissionMask EffectivePermissionsMask(SimObject exportPrim)
        {
            Permissions perms = exportPrim.Properties.Permissions;
            return NonEffectivePermissionsMask(exportPrim) & perms.BaseMask;
        }
        public PermissionMask NonEffectivePermissionsMask(SimObject exportPrim)
        {
            Permissions perms = exportPrim.Properties.Permissions;
            return CogbotHelpers.PermMaskForWho(EffectivePermissionWho(exportPrim), perms);
        }

        public override bool KilledPrim(Primitive prim, Simulator simulator)
        {
            //Debug("AVATAR KilledPrim " + prim);
            return base.KilledPrim(prim, simulator);
        }

        public override void ResetPrim(Primitive prim, BotClient bc, Simulator sim)
        {
            //if (!ReferenceEquals(_Prim0,prim))
            //{
            //    Debug("AVATAR ResetPrim " + prim);
            //}
            base.ResetPrim(prim, bc, sim);
        }

        public override void SetFirstPrim(Primitive prim)
        {
            if (prim is Avatar)
            {
                Avatar av = (Avatar)prim;
                if (!string.IsNullOrEmpty(av.Name))
                {
                    AspectName = av.Name;
                }
            }
            base.SetFirstPrim(prim);
            //  Debug("AVATAR SetFirstPrim " + prim);
        }
        public override string GetName()
        {
            if (!string.IsNullOrEmpty(AspectName)) return AspectName;
            lock (HasPrimLock)
            {
                if (!HasPrim)
                {
                    return null;
                }
                if (!string.IsNullOrEmpty(theAvatar.Name))
                    return AspectName = theAvatar.Name;
                return _Prim0.ToString();
            }
        }

        public override string ToString()
        {
            string s = GetName();
            if (s == null)
            {
                if (ID != UUID.Zero) return "Avatar " + ID;
            }
            return s;
        }
        public override bool Matches(string name)
        {
            String n = GetName();
            if (string.IsNullOrEmpty(n)) return string.IsNullOrEmpty(name);
            if (string.IsNullOrEmpty(name)) return false;
            if (n.ToLower().Replace("_", " ").Trim() == (name.ToLower().Replace("_", " ").Trim())) return true;
            return SimTypeSystem.MatchString(base.ToString(), name)
                   || SimTypeSystem.MatchString(ToString(), name);
        }
























        protected bool IsDrivingVehical
        {
            get
            {
                Primitive Prim = this.Prim;
                if (Prim == null) return false;
                return Prim.ParentID != 0;
            }
        }

        #region SimAvatar Members

        public bool IsWalking
        {
            get
            {
                return Overlaps(GetCurrentAnims(), SimAssetStore.MeaningUUIDs("LocomotionProcess-Animal"));
            }

        }
        public bool IsFlying
        {
            get
            {
                return Overlaps(GetCurrentAnims(), SimAssetStore.MeaningUUIDs("Flying"));
            }
        }

        public bool IsStanding
        {
            get
            {
                return Overlaps(GetCurrentAnims(), SimAssetStore.MeaningUUIDs("StandingStill"));
            }
        }

        public bool IsSleeping
        {
            get
            {
                return Overlaps(GetCurrentAnims(), SimAssetStore.MeaningUUIDs("Sleeping"));
            }
        }

        public int DebugLevel
        {
            get { return _debugLevel; }
            set { _debugLevel = value; }
        }


        public ICollection<UUID> GetCurrentAnims()
        {
            return ExpectedCurrentAnims.Dictionary.Keys;
        }

        internal static List<UUID> GetAnimUUIDs(List<Animation> anims)
        {
            var c = new List<UUID>();
            lock (anims)
                anims.ForEach((a) => c.Add(a.AnimationID));
            return c;
        }
        private static List<UUID> GetBeforeUUIDs(List<Animation> anims, int seq)
        {
            var c = new List<UUID>();
            lock (anims)
                anims.ForEach((a) =>
                {
                    if (a.AnimationSequence < seq) c.Add(a.AnimationID);
                });
            return c;
        }
        private static List<UUID> GetAfterUUIDs(List<Animation> anims, int seq)
        {
            var c = new List<UUID>();
            lock (anims)
                anims.ForEach((a) =>
                {
                    if (a.AnimationSequence > seq) c.Add(a.AnimationID);
                });
            return c;
        }
        private static List<UUID> GetDurringUUIDs(List<Animation> anims, int seq)
        {
            var c = new List<UUID>();
            lock (anims)
                anims.ForEach((a) =>
                {
                    if (a.AnimationSequence == seq) c.Add(a.AnimationID);
                });
            return c;
        }
        public Dictionary<UUID, int> GetCurrentAnimDict()
        {
            return ExpectedCurrentAnims.Dictionary;
        }

        private InternalDictionary<UUID, int> ExpectedCurrentAnims = new InternalDictionary<UUID, int>();



        public static bool mergeEvents = true;
        private int CurrentAnimSequenceNumber = 0;
        /// public UUID CurrentAmin = UUID.Zero;
        /// <summary>
        ///  Nephrael Rae: [on-object-animation '(avatar "Candie Brooks") "TALK"][on-object-animation '(avatar "Candie Brooks") "STAND_1"][on-object-animation '(avatar "Candie Brooks") "e45fbdc9-af8f-9408-f742-fcb8c341d2c8"]
        /// </summary>
        /// <param name="anims"></param>
        public void OnAvatarAnimations(List<Animation> anims0)
        {
            var anims = new InternalDictionary<UUID, int>();
            var animDict = anims.Dictionary;
            foreach (var list in anims0)
            {
                animDict[list.AnimationID] = list.AnimationSequence;
            }
            Dictionary<UUID, int> RemovedAnims = new Dictionary<UUID, int>();
            Dictionary<UUID, int> AddedAnims = new Dictionary<UUID, int>();
            bool SendAnimEvent = !WorldObjects.UseNewEventSystem;
            //if (!theAvatar.Name.Contains("rael")) return; 
            lock (ExpectedCurrentAnims)
            {
                int mostCurrentSequence = int.MinValue;
                int leastCurrentSequence = int.MaxValue;
                Dictionary<UUID, int> dictionary = ExpectedCurrentAnims.Dictionary;
                GetSequenceNumbers(dictionary, ref leastCurrentSequence, ref mostCurrentSequence);

                ///  first time so find the lowest number 
                int mostCurrentSequence1 = int.MinValue;
                int leastCurrentSequence1 = int.MaxValue;
                GetSequenceNumbers(animDict, ref leastCurrentSequence1, ref mostCurrentSequence1);



                //UUID mostCurrentAnim = UUID.Zero;// = UUID.Zero; 
                ///  List<String> names = new List<String>(); 
                Dictionary<UUID, int> RemovedThisEvent = new Dictionary<UUID, int>(dictionary);
                anims.ForEach(delegate(UUID key)
                {
                    RemovedThisEvent.Remove(key);
                    int newAnimNumber;
                    anims.TryGetValue(key, out newAnimNumber);
                    if (newAnimNumber >= mostCurrentSequence)
                    {
                        mostCurrentSequence = newAnimNumber;
                        WorldObjects.GridMaster.EnqueueRequestAsset(key, AssetType.Animation, true);
                        //mostCurrentAnim = key; 
                    }
                    if (ExpectedCurrentAnims.ContainsKey(key))
                    {
                        int oldAnimNumber;
                        ExpectedCurrentAnims.TryGetValue(key, out oldAnimNumber);
                        if (oldAnimNumber == newAnimNumber)
                        ///  still the same 
                        {
                            AddedAnims.Remove(key);
                            RemovedAnims.Remove(key);
                            return;
                        }
                        if (oldAnimNumber > newAnimNumber)
                        {
                            //  Debug("error oldAnimNumber > newAnimNumber"); 
                        }
                        ///      else 
                        {
                            if (oldAnimNumber + 1 != newAnimNumber)
                            {
                                RemovedAnims[key] = oldAnimNumber + 1;
                                AddedAnims[key] = newAnimNumber;
                            }
                        }
                        return;
                    }
                    AddedAnims[key] = newAnimNumber; /// AddedAnims.Add(key, newAnimNumber); 
                    RemovedAnims.Remove(key);
                    /// int whenSeq = newAnimNumber + 1; 
                    /// if (!RemovedAnimsWhen.ContainsKey(whenSeq)) 
                    /// { 
                    ///     RemovedAnimsWhen[whenSeq] = new List<UUID>(); 
                    /// } 
                    /// RemovedAnimsWhen[whenSeq].Add(key); 
                });
                List<UUID> shownRemoved = new List<UUID>();
                List<UUID> showAdded = new List<UUID>();

                Dictionary<UUID, int> AddedThisEvent = new Dictionary<UUID, int>(AddedAnims);

                foreach (UUID key in RemovedThisEvent.Keys)
                {
                    dictionary.Remove(key);
                }

                foreach (UUID list in RemovedAnims.Keys)
                {
                    if (RemovedThisEvent.ContainsKey(list))
                    {
                        RemovedThisEvent.Remove(list);
                    }
                }


                List<SimObjectEvent> startStops = new List<SimObjectEvent>();

                //if (SimAnimationStore.IsSitAnim(RemovedThisEvent)) { 
                //    if (!SimAnimationStore.IsSitAnim(AddedThisEvent)) 
                //    { 
                //        LogEvent(new SimObjectEvent("StandUp", SimEventType.ANIM, this, ZHeading * SimPathStore.RAD2DEG, GetSimulator().Name, GetSimPosition())); 
                //    } 
                //} 
                //start or stop moving 
                Dictionary<UUID, int> RemovedThisEventUnsent = new Dictionary<UUID, int>(RemovedThisEvent);
                Dictionary<UUID, int> AddedThisEventUnsent = new Dictionary<UUID, int>(AddedThisEvent);
                if (mergeEvents)
                {
                    StartOrStopAnimEvent(RemovedThisEventUnsent, AddedThisEventUnsent, "Movement-TranslationProcess",
                                         startStops);

                    StartOrStopAnimEvent(RemovedThisEventUnsent, AddedThisEventUnsent, "Jumping", startStops);

                    StartOrStopAnimEvent(RemovedThisEventUnsent, AddedThisEventUnsent, "SittingDown", startStops);

                    StartOrStopAnimEvent(RemovedThisEventUnsent, AddedThisEventUnsent, "StandingStill", startStops);
                    // start or stop flying 
                    StartOrStopAnimEvent(RemovedThisEventUnsent, AddedThisEventUnsent, "Flying", startStops);
                    //start or stop sleeping 
                    StartOrStopAnimEvent(RemovedThisEventUnsent, AddedThisEventUnsent, "Lying-Physical", startStops);


                    //start or stop talking 
                    //StartOrStopAnimEvent(RemovedThisEvent, AddedThisEvent, SimAnimationStore.IsCommunationAnim, "Commuincation", SimEventType.ANIM, startStops); 

                    // StartOrStopAnimEvent(RemovedThisEvent, AddedThisEvent, "OtherAnim", startStops); 

                    foreach (SimObjectEvent evt in startStops)
                    {
                        if (evt.Verb == "Flying")
                        {
                            LastEventByName[evt.EventName] = evt;
                            if (evt.EventStatus != SimEventStatus.Start) continue;
                        }
                        if (evt.Verb == "StandingStill")
                        {
                            LastEventByName[evt.EventName] = evt;
                            //continue; 
                        }
                        if (evt.Verb == "SittingDown")
                        {
                            LastEventByName[evt.EventName] = evt;
                            //continue; 
                        }
                        if (evt.EventName == "Movement-TranslationProcess-Start")
                        {
                            LastEventByName[evt.EventName] = evt;
                            //lastEvent = evt; 
                            //continue; 
                        }
                        //if (evt.EventName == "MovingStop") 
                        //{ 
                        //    object old = GetLastEvent("MovingStart", 2); 
                        //    evt.Verb = "MoveTo"; 
                        //} 
                        if (SimEventStatus.Start == evt.EventStatus)
                        {
                            SetPosture(evt);
                        }
                        //  LogEvent(evt); 
                    }
                }

                for (int seq = leastCurrentSequence; seq <= mostCurrentSequence; seq++)
                {
                    if (RemovedAnims.Count > 0)
                    {
                        foreach (KeyValuePair<UUID, int> uuid in RemovedAnims)
                        {
                            if (seq == uuid.Value)
                            {
                                if (RemovedThisEventUnsent.ContainsKey(uuid.Key))
                                {
                                    var evt = AnimEvent(uuid.Key, SimEventStatus.Stop, seq);
                                    bool sent = LogEvent(evt);
                                    if (!sent && ShouldEventSource)
                                    {
                                        WorldSystem.SendPipelineEvent(evt);
                                    }
                                    shownRemoved.Add(uuid.Key);
                                }
                            }
                        }
                    }
                    if (AddedAnims.Count > 0)
                    {
                        foreach (KeyValuePair<UUID, int> uuid in AddedAnims)
                        {
                            if (seq == uuid.Value)
                            {
                                if (AddedThisEventUnsent.ContainsKey(uuid.Key))
                                {
                                    var evt = AnimEvent(uuid.Key, SimEventStatus.Start, seq);
                                    bool sent = LogEvent(evt);
                                    if (!sent && ShouldEventSource)
                                    {
                                        WorldSystem.SendPipelineEvent(evt);
                                    }
                                    showAdded.Add(uuid.Key);
                                }
                            }
                        }
                    }
                }
                leastCurrentSequence = mostCurrentSequence;

                foreach (UUID key in shownRemoved)
                {
                    RemovedThisEvent.Remove(key);
                    RemovedAnims.Remove(key);
                }
                if (SimAssetStore.IsSitAnim(showAdded))
                {

                }
                foreach (UUID key in showAdded)
                {
                    AddedAnims.Remove(key);
                }
            }

            //ExpectedCurrentAnims.Dictionary.Clear(); 
            ExpectedCurrentAnims.Dictionary = anims.Dictionary;
            /// String newName = WorldSystem.GetAnimationName(mostCurrentAnim); 
            /// { 
            ///     if (oldAnim != mostCurrentAnim) 
            ///     { 
            ///         String oldName = WorldSystem.GetAnimationName(oldAnim); 
            ///         if (oldName.Length > 4 && newName.Length > 4 && 
            ///             oldName.Substring(0, 5) == newName.Substring(0, 5)) 
            ///         { 
            ///         } 
            ///         else 
            ///             WorldSystem.SendNewEvent("On-Object-Start-Animation", this, newName); 
            ///     } 
            /// } 
            /// else 
            /// { 
            ///     WorldSystem.SendNewEvent("On-Object-Start-Animation", this, newName); 
            /// } 

            /// CurrentAmin = mostCurrentAnim; 
            /// SendNewEvent("On-Avatar-Animation", avatar, names); 
        }

        private SimObjectEvent AnimEvent(UUID uuid, SimEventStatus status, int serial)
        {
            SimAsset a = SimAssetStore.FindOrCreateAsset(uuid, AssetType.Animation);

            string headingString;
            switch (status)
            {
                case SimEventStatus.Start:
                    {
                        headingString = "eventOccursAt";
                        break;
                    }
                case SimEventStatus.Stop:
                    {
                        headingString = "toLocation";
                        break;
                    }
                default:
                    {
                        headingString = "eventOccursAt";
                        break;
                    }
            }
            object m = a.GetMeaning();
            SimObjectEvent oe = new SimObjectEvent(status, "OnAnim", SimEventType.ANIM, SimEventClass.REGIONAL,
                                      WorldObjects.ToParameter("doneBy", this),
                                      WorldObjects.ToParameter("isa", a),
                                      WorldObjects.ToParameter(headingString, GetHeading()));
            oe.Serial = serial;
            if (m != null) oe.AddParam("isa", m);
            return oe;
        }

        private void SetPosture(SimObjectEvent evt)
        {
            lock (postureLock)
            {
                if (PostureType != null)
                {
                    // was the same 
                    if (PostureType == evt.Verb) return;
                    SimObjectEvent ending = new SimObjectEvent(
                        SimEventStatus.Stop,
                        PostureType + (IsFlying ? "-Flying" : ""),
                        SimEventType.ANIM, SimEventClass.REGIONAL, evt.Parameters) { Serial = LastPostureEvent.Serial };
                    LogEvent(ending);
                    PostureType = evt.Verb;
                    SimObjectEvent starting = new SimObjectEvent(
                        SimEventStatus.Start,
                        PostureType + (IsFlying ? "-Flying" : ""),
                        SimEventType.ANIM, SimEventClass.REGIONAL, evt.Parameters) { Serial = evt.Serial };
                    LogEvent(starting);
                }
                PostureType = evt.Verb;
                LastPostureEvent = evt;
            }
        }

        private static void GetSequenceNumbers(IEnumerable<KeyValuePair<UUID, int>> anims, ref int leastCurrentSequence, ref int mostCurrentSequence)
        {
            lock (anims)
            {
                int mostCurrentSequence0 = mostCurrentSequence;
                int leastCurrentSequence0 = leastCurrentSequence;
                foreach (KeyValuePair<UUID, int> i in anims)
                {
                    int newAnimNumber = i.Value;
                    if (newAnimNumber < leastCurrentSequence0)
                    {
                        leastCurrentSequence0 = newAnimNumber;
                    }
                    if (newAnimNumber > mostCurrentSequence0)
                    {
                        mostCurrentSequence0 = newAnimNumber;
                    }
                    WorldObjects.GridMaster.EnqueueRequestAsset(i.Key, AssetType.Animation, true);

                }
                leastCurrentSequence = leastCurrentSequence0;
                mostCurrentSequence = mostCurrentSequence0;
            }
        }

        //private delegate bool AnimationTest(ICollection<UUID> thisEvent);

        private void StartOrStopAnimEvent(IDictionary<UUID, int> RemovedThisEvent, IDictionary<UUID, int> AddedThisEvent, string name, IList<SimObjectEvent> startStops)
        {
            int wasStarted = 0;
            int wasStopped = 0;
            List<UUID> e = SimAssetStore.MeaningUUIDs(name);
            //  if (e.Count==0) throw new NoSuchElementException(name);
            foreach (UUID list in e)
            {
                if (AddedThisEvent.ContainsKey(list))
                {
                    wasStarted = AddedThisEvent[list];
                    AddedThisEvent.Remove(list);
                }
            }
            foreach (UUID list in e)
            {
                if (RemovedThisEvent.ContainsKey(list))
                {
                    wasStopped = RemovedThisEvent[list];
                    RemovedThisEvent.Remove(list);
                }
            }
            if (wasStarted != 0 && wasStopped != 0) return;
            if (wasStarted != 0)
            {
                SimObjectEvent simEvent = new SimObjectEvent(SimEventStatus.Start, name, SimEventType.ANIM, SimEventClass.REGIONAL,
                                                             WorldObjects.ToParameter("doneBy", this),
                                                             WorldObjects.ToParameter("eventOccursAt", GetHeading()));
                simEvent.Serial = wasStarted;
                startStops.Add(simEvent);
            }
            if (wasStopped != 0)
            {
                SimObjectEvent simEvent = new SimObjectEvent(SimEventStatus.Stop, name, SimEventType.ANIM, SimEventClass.REGIONAL,
                                                             WorldObjects.ToParameter("doneBy", this),
                                                             WorldObjects.ToParameter("toLocation", GetHeading()));
                simEvent.Serial = wasStopped;
                startStops.Insert(0, simEvent);
            }
        }

        static bool Overlaps(IEnumerable<UUID> c1, IEnumerable<UUID> c2)
        {
            foreach (var uuid in c2)
            {
                foreach (var uuid1 in c1)
                {
                    if (uuid == uuid1) return true;
                }
            }
            return false;
        }


        public object GetLastEvent(String name, int arg)
        {
            return LastEventByName.ContainsKey(name) ? LastEventByName[name].GetArg(arg) : null;
        }

        #endregion
        private int _debugLevel = 2;

    }


    public interface SimAvatarSight
    {
        /// <summary>
        /// Returns hopefully at least three objects sorted by distance
        /// </summary>
        /// <returns></returns>
        ListAsSet<SimObject> GetKnownObjects();
        void ScanNewObjects(int minimum, double sightRange, bool rootOnly);
        double SightRange { get; set; }
        SimObject FindSimObject(SimObjectType pUse, double maxXYDistance, double maxZDist);
        void RemoveObject(SimObject O);
    }

    public interface SimAvatarActing
    {
        IEnumerable<SimTypeUsage> KnownTypeUsages { get; }
        BotAction LastAction { get; }
        //BotNeeds CurrentNeeds { get; }

    }

    public interface SimAvatar : SimObject, PathSystem3D.Navigation.SimMover, SimAvatarSight, SimAvatarActing
    {

        Avatar theAvatar { get; }
        bool IsSitting { get; }
        bool IsOnline { get; }
        float ZHeading { get; }
        Avatar.AvatarProperties ProfileProperties { get; set; }
        Avatar.Interests AvatarInterests { get; set; }
        bool IsFlying { get; }
        Dictionary<UUID, AvatarGroup> GroupRoles { get; set; }
        string AspectName { get; set; }

        void OnAvatarAnimations(List<Animation> anims);

        ICollection<UUID> GetCurrentAnims();
        Dictionary<UUID, int> GetCurrentAnimDict();


        void AddGoupRoles(List<AvatarGroup> groups);


    }
}