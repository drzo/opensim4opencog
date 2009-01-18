using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.StructuredData;
using System.Reflection;
using OpenMetaverse.Packets; //using libsecondlife;

namespace cogbot.Listeners
{
	public delegate float ObjectHeuristic(Primitive prim);

    public class WorldObjects : DebugAllEvents {
		protected Dictionary<string, Avatar> avatarCache = new Dictionary<string,Avatar>();
		public Vector3 compPos;
		public int searchStep;
		public List<string> numberedAvatars;

		public int burstSize = 100;
		public float burstTime = 1;
		public DateTime burstStartTime;
		public TimeSpan burstInterval;
		public float buildingSize = 5;
		public List<ObjectHeuristic> objectHeuristics;
		public Dictionary<UUID, List<Primitive>> primGroups;
		public Object newLock = new Object();
		public int maxShortNameLength = 0;
		public int maxNameLength;
		public Dictionary<string, string> shortNames;
		public Dictionary<string, string> reverseShortNames;
		public List<string> numberedObjects;
		public Dictionary<uint, OSDMap> lastOSD = new Dictionary<uint, OSDMap>();
		Simulator sim;

		DoubleDictionary<uint, UUID, Primitive> prims = new DoubleDictionary<uint, UUID, Primitive>();
		DoubleDictionary<uint, UUID, Avatar> avatars = new DoubleDictionary<uint, UUID, Avatar>();
		Dictionary<uint, ObjectUpdate> lastObjectUpdate = new Dictionary<uint, ObjectUpdate>();
		Dictionary<uint, ObjectUpdate> lastObjectUpdateDiff = new Dictionary<uint, ObjectUpdate>();
        Dictionary<Avatar, List<UUID>> avatarAminsSent = new Dictionary<Avatar, List<UUID>>();
        Dictionary<Avatar, UUID> avatarAminCurrent = new Dictionary<Avatar, UUID>();
        //Dictionary<uint, uint> selectedPrims = new Dictionary<uint, uint>();
		//Dictionary<UUID, UUID> texturesFinished = new Dictionary<UUID, UUID>();
		List<Primitive> primsAwaitingSelect = new List<Primitive>();
		List<Primitive> primsKnown = new List<Primitive>();

		public WorldObjects(BotClient client)
		: base(client)
		{
		//	client.Self.OnMeanCollision += new AgentManager.MeanCollisionCallback(Self_OnMeanCollision);

			// primsAwaitingSelect = new List<Primitive>();
			// prims = new Dictionary<string, Primitive>();
			//  primsByLocalID = new Dictionary<uint, Primitive>();
			// pendingPrims = new Dictionary<UUID, Primitive>();

			primGroups = new Dictionary<UUID, List<Primitive>>();
			shortNames = new Dictionary<string, string>();
			reverseShortNames = new Dictionary<string, string>();
			numberedObjects = new List<string>();

			objectHeuristics = new List<ObjectHeuristic>();
			objectHeuristics.Add(new ObjectHeuristic(distanceHeuristic));
			objectHeuristics.Add(new ObjectHeuristic(nameLengthHeuristic));
			objectHeuristics.Add(new ObjectHeuristic(boringNamesHeuristic));

			maxNameLength = -1;
/*
			client.Objects.OnNewPrim += new ObjectManager.NewPrimCallback(Objects_OnNewPrim);
            client.Objects.OnObjectProperties += new ObjectManager.ObjectPropertiesCallback(Objects_OnObjectProperties);
            client.Objects.OnNewAvatar += new ObjectManager.NewAvatarCallback(Objects_OnNewAvatar);
			client.Avatars.OnAvatarProperties += new AvatarManager.AvatarPropertiesCallback(Avatars_OnAvatarProperties);
			// botclient.Objects.OnObjectKilled += new ObjectManager.KillObjectCallback(Objects_OnObjectKilled);
			client.Objects.OnObjectUpdated += new ObjectManager.ObjectUpdatedCallback(Objects_OnObjectUpdated);
			client.Objects.OnObjectPropertiesFamily += new ObjectManager.ObjectPropertiesFamilyCallback(Objects_OnObjectPropertiesFamily);
			client.Avatars.OnAvatarAnimation += AvatarAnimationCallback;// new AgentManager.AnimationsChangedCallback();
            //replaced bu the above line Self.OnAnimationsChanged += new AgentManager.AnimationsChangedCallback(Self_OnAnimationsChanged);
			client.Sound.OnPreloadSound += new SoundManager.PreloadSoundCallback(Sound_OnPreloadSound);
            client.Sound.OnSoundTrigger += new SoundManager.SoundTriggerCallback(Sound_OnSoundTrigger);
            client.Avatars.OnLookAt += new AvatarManager.LookAtCallback(Avatars_OnLookAt);
            client.Avatars.OnPointAt += new AvatarManager.PointAtCallback(Avatars_OnPointAt);
           // client.Objects.OnNewFoliage += new ObjectManager.NewFoliageCallback(Objects_OnNewFoliage);
            client.Terrain.OnLandPatch += new TerrainManager.LandPatchCallback(Terrain_OnLandPatch);
  */          
			// Animation callback
			//            client.Network.RegisterCallback(PacketType.AvatarAnimation, new NetworkManager.PacketCallback(AvatarAnimationHandler));

			burstStartTime = DateTime.Now;
			burstInterval = new TimeSpan(0, 0, 0, 0, (int)(burstTime * 1000));
			searchStep = 1;
			avatarCache = new Dictionary<string, Avatar>();
			numberedAvatars = new List<string>();

			//parent.Objects.OnNewAvatar += new ObjectManager.NewAvatarCallback(Objects_OnNewAvatar);
			//parent.Objects.OnAvatarSitChanged += new ObjectManager.AvatarSitChanged(Objects_OnAvatarSitChanged);
			// parent.Avatars.OnAvatarAppearance += new AvatarManager.AvatarAppearanceCallback(Avatars_OnAvatarAppearance);
			//AgentManager..AvatarSitChanged(Objects_OnAvatarSitChanged);
			// new DebugAllEvents(client);
		}

		private void output(string p)
		{
			client.output(p);
		}

		private void enqueueLispTask(string p)
		{
			//client.enqueueLispTask(p);
		}


        public override void Self_OnMeanCollision(MeanCollisionType type, UUID perp, UUID victim, float magnitude, DateTime time)
		{
			Avatar perpAv, victimAv;
			if (client.WorldSystem.tryGetAvatarById(perp, out perpAv) && client.WorldSystem.tryGetAvatarById(victim, out victimAv)) {
				if (victimAv.Name == client.Self.Name)
					output(perpAv.Name + " bumped into you.");
				else if (perpAv.Name == client.Self.Name)
					output("You bumped into " + victimAv.Name + ".");

				SendEvent("on-meanCollision", perpAv.Name , victimAv.Name, magnitude);

			}
		}

        public override  void Sound_OnSoundTrigger(UUID soundID, UUID ownerID, UUID objectID, UUID parentID, float gain, ulong regionHandle, Vector3 position)
        {
            //throw new NotImplementedException();
            SendEvent("On-Sound-Trigger", soundID, soundID, ownerID, objectID, parentID, gain, regionHandle, position);
        }


		public override void Sound_OnPreloadSound(UUID soundID, UUID ownerID, UUID objectID)
		{
            base.Sound_OnPreloadSound(soundID, ownerID, objectID);
			//botoutput("preload sound " + soundID);
		}


		public void SetCurrentSimulator(Simulator simulator)
		{
			sim = simulator;
		}

		public Primitive BlockUntilProperties(Primitive prim)
		{
			if (prim.Properties != null) return prim;
			if (!primsAwaitingSelect.Contains(prim)) {
				client.Objects.SelectObject(sim, prim.LocalID);
				primsAwaitingSelect.Add(prim);
			}
			while (prim.Properties == null) { // TODO maybe add a timer
				System.Windows.Forms.Application.DoEvents();
			}
			return prim;
		}

        public override void Avatars_OnAvatarAnimation(UUID avatarID, InternalDictionary<UUID, int> anims)
		{
			Avatar avatar = GetAvatar(avatarID);
			List<UUID> currents = new List<UUID>();
			List<String> names = new List<String>();
            UUID mostCurrentAnim = UUID.Zero;
            int mostCurrentSequence = -1;
            lock (anims) 
                foreach (UUID key in anims.Dictionary.Keys)
			{
                int animNumber = (int)anims[key];
                if (animNumber >= mostCurrentSequence)
                {
                    mostCurrentSequence = animNumber;
                    mostCurrentAnim = key;
                }
				currents.Add(key);
				names.Add(GetAnimationName(key));
			}
            lock(avatarAminCurrent)
            if (avatarAminCurrent.ContainsKey(avatar))
            {
                UUID oldAnim = avatarAminCurrent[avatar];
                if (oldAnim != mostCurrentAnim)
                {
                    SendEvent("On-Object-Animation", avatar, mostCurrentAnim);
                }
            }
            else
            {
                SendEvent("On-Object-Animation", avatar, mostCurrentAnim);
            }
            avatarAminCurrent[avatar] = mostCurrentAnim;


			if (avatarAminsSent.ContainsKey(avatar)) {

			}
			//SendEvent("On-Avatar-Animation", avatar, names);
		}

        public override void Self_OnAnimationsChanged(InternalDictionary<UUID, int> agentAnimations)
        {
            Avatars_OnAvatarAnimation(client.Self.AgentID, agentAnimations);
        }
        public override void Grid_OnCoarseLocationUpdate(Simulator simulator)
        {

                //for (int i = 0; i < coarse.Location.Length; i++)
                //{
                //    if (i == coarse.Index.You)
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
           
            //OnEvent("On-Coarse-Location-Update", paramNamesOnCoarseLocationUpdate, paramTypesOnCoarseLocationUpdate, sim);
        }


		public Avatar GetAvatar(UUID avatarID)
		{
			Avatar prim;
			if (avatars.TryGetValue(avatarID, out prim)) {
				return prim;
			}
			return null;
		}

		static readonly string[] paramNamesOnObjectKilled = new string[] { "simulator", "objectID"};
		static readonly Type[] paramTypesOnObjectKilled = new Type[] { typeof(Simulator), typeof(uint)};

		//public void Objects_OnObjectKilled(Simulator simulator, uint objectID) {
		//    Primitive p = primsByLocalID[objectID];
		//    prims[p.Properties.Name] = null;
		//    primsByLocalID[objectID] = null;
		//}

		static readonly string[] paramNamesOnObjectPropertiesFamily = new string[] { "simulator", "props", "type"};
		static readonly Type[] paramTypesOnObjectPropertiesFamily = new Type[] { typeof(Simulator), typeof(Primitive.ObjectProperties), typeof(ReportType)};

		public override void Objects_OnObjectPropertiesFamily(Simulator simulator, Primitive.ObjectProperties props, ReportType type)
		{
           // Properties = new Primitive.ObjectProperties();
            //Properties.SetFamilyProperties(props);
           // GotPermissions = true;
           // GotPermissionsEvent.Set();        
			//Objects_OnObjectProperties(simulator, props);
            SendEvent("On-Object-PropertiesFamily",simulator, props, type);
		}

		static readonly string[] paramNamesOnObjectUpdated = new string[] { "simulator", "update", "regionHandle", "timeDilation"};
		static readonly Type[] paramTypesOnObjectUpdated = new Type[] { typeof(Simulator), typeof(ObjectUpdate), typeof(ulong), typeof(ushort)};


        public override  void Objects_OnNewPrim(Simulator simulator, Primitive prim, ulong regionHandle, ushort timeDilation)
		{
            lock (primsAwaitingSelect)
            {
                prims.Add(prim.LocalID, prim.ID, prim);
                primsAwaitingSelect.Add(prim);
                client.Objects.SelectObject(simulator, prim.LocalID);
            }
			//CalcStats(prim);
			//UpdateTextureQueue(prim.Textures);
		}



		object Objects_OnObjectUpdated1Sync = new Object();
        public override void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
		{
			lock (Objects_OnObjectUpdated1Sync)
			Objects_OnObjectUpdated1(simulator, update, regionHandle, timeDilation);
		}

        private void Objects_OnObjectUpdated1(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
		{
			Primitive objectUpdated = null;
			//Console.WriteLine("timeDilation " + timeDilation);
			if (update.Avatar) {
				Avatar prim;
				if (avatars.TryGetValue(update.LocalID, out prim)) {
					objectUpdated = prim;
					lock (prim)
					{

						// output("Updating state for Avatar " + prim.Name);
						//  updateToPrim(prim, update);
						CalcStats(prim);
						//   describePrimToAI(prim);
					}

				}

			} else {
				Primitive prim;

				if (prims.TryGetValue(update.LocalID, out prim)) {
					objectUpdated = prim;
					lock (prim)
					{

						output("Updating state for Prim " + prim);
						updateToPrim(prim, update);
					}
					if (prim.Properties != null) {
						CalcStats(prim);
						describePrimToAI(prim);
					} else {
						if (!primsAwaitingSelect.Contains(prim)) {
							client.Objects.SelectObject(sim, prim.LocalID);
							primsAwaitingSelect.Add(prim);
						}
					}
				} else {
					output("missing Objects_OnObjectUpdated");
				}
			}
			if (objectUpdated != null) {
				bool needsOsdDiff = false; //for debugging should be false otherwise
				if (!lastObjectUpdate.ContainsKey(update.LocalID)) {
					lastObjectUpdate[update.LocalID] = updatFromPrim(objectUpdated);
				}

				Object diffO = notifyUpdate(objectUpdated, lastObjectUpdate[update.LocalID], update, InformUpdate);
				lastObjectUpdate[update.LocalID] = update;

				if (diffO != null) {
					ObjectUpdate diff = (ObjectUpdate)diffO;
					//if (lastObjectUpdateDiff.ContainsKey(update.LocalID))
					//{
					//    notifyUpdate(objectUpdated, lastObjectUpdateDiff[update.LocalID], diff, InformUpdateDiff);
					//}
					lastObjectUpdateDiff[update.LocalID] = diff;
				} else {
					someThingElseNeedsUpdate(objectUpdated);
					needsOsdDiff = true;
				}
				// BlockUntilProperties(objectUpdated);
				if (objectUpdated.Properties != null) {
					OSDMap after = (OSDMap)objectUpdated.GetOSD();
					//  Console.WriteLine(OSDParser.SerializeLLSDXmlString(after));
					if (lastOSD.ContainsKey(update.LocalID)) {
						if (needsOsdDiff) {
							//Primitive before = lastDeepCopy[update.LocalID];
							OSDMap before = lastOSD[update.LocalID];
							String osdDiff = OSDDiff(before, after);
							if (osdDiff.Length > 0) {
								Console.WriteLine(osdDiff);
							}
						}

					}

					lastOSD[update.LocalID] = after;
				}

				//Primitive deep = (Primitive)deepCopy(objectUpdated);
				//lastDeepCopy[update.LocalID] = deep;


			} else {
				lastObjectUpdate[update.LocalID] = update;
			}
		}
		//removes from taintable
		public String OSDDiff(OSDMap before, OSDMap after)
		{
			string xmlBefore = OpenMetaverse.StructuredData.OSDParser.SerializeLLSDXmlString(before);
			string xmlAfter = OpenMetaverse.StructuredData.OSDParser.SerializeLLSDXmlString(after);
			if (xmlBefore.Equals(xmlAfter))	return "";
			return "\n\n\n\nBEFORE\n" + before + "\n\nAFTER\n" + after;
		}

		class SomeChange {
			Object before;
			Object after;
			String named;
			SomeChange(String _named, Object _before, Object _after)
			{
				named = _named;
				before = _before;
				after = _after;
			}
			public override string ToString()
			{
				return "SomeChange " + named + " from " + before + " to " + after;
			}

		}


		public Primitive deepCopy(Primitive objectUpdated)
		{
			OpenMetaverse.StructuredData.OSD osd = objectUpdated.GetOSD();
			if (objectUpdated is Avatar) {
				return Avatar.FromOSD(osd);
			}
			return Primitive.FromOSD(osd);
		}

		public void someThingElseNeedsUpdate(Primitive objectUpdated)
		{
			//throw new Exception("The method or operation is not implemented.");
		}

		public ObjectUpdate updatFromPrim(Primitive fromPrim)
		{
			ObjectUpdate update = new ObjectUpdate();
			update.Acceleration = fromPrim.Acceleration;
			update.AngularVelocity = fromPrim.AngularVelocity;
			update.CollisionPlane = fromPrim.CollisionPlane;
			update.Position = fromPrim.Position;
			update.Rotation = fromPrim.Rotation;
			update.State = fromPrim.PrimData.State;
			update.Textures = fromPrim.Textures;
			update.Velocity = fromPrim.Velocity;
			update.LocalID = fromPrim.LocalID;
			update.Avatar = (fromPrim is Avatar);
			return update;
		}



		public ObjectUpdate updateDiff(ObjectUpdate fromPrim, ObjectUpdate diff)
		{
			ObjectUpdate update = new ObjectUpdate();
			update.LocalID = fromPrim.LocalID;
			update.Avatar = fromPrim.Avatar;
			update.Acceleration = fromPrim.Acceleration - diff.Acceleration;
			update.AngularVelocity = fromPrim.AngularVelocity - diff.AngularVelocity;
			update.CollisionPlane = fromPrim.CollisionPlane - diff.CollisionPlane;
			update.Position = fromPrim.Position - diff.Position;
			update.Rotation = fromPrim.Rotation - diff.Rotation;
			update.State = (byte)(fromPrim.State - diff.State);
			update.Textures = fromPrim.Textures != diff.Textures ? diff.Textures : null;
			update.Velocity = fromPrim.Velocity - diff.Velocity;
			return update;
		}


		public void updateToPrim(Primitive prim, ObjectUpdate update)
		{
			prim.Acceleration = update.Acceleration;
			prim.AngularVelocity = update.AngularVelocity;
			prim.CollisionPlane = update.CollisionPlane;
			prim.Position = update.Position;
			prim.Rotation = update.Rotation;
			prim.PrimData.State = update.State;
			prim.Textures = update.Textures;
			prim.Velocity = update.Velocity;
		}
		delegate object DoWhat(Primitive objectUpdated, string p, Object vector3, Object vector3_4, Object diff);
        public object InformUpdate(Primitive objectUpdated, string p, Object before, Object after, Object diff)
        {
            Console.WriteLine("{0} {1} DIFF {2} BEFORE {3} AFTER {4}", p, objectUpdated, diff, before, after);
            if (diff is Vector3)
            {
                // if the change is too small skip the event
                if ((1 > ((Vector3)diff).Length()))
                {
                  //  return after;
                }
            }
            //  String lispName = "on-" + ((objectUpdated is Avatar) ? "avatar-" : "prim-") + p.ToLower() + "-updated";
            String lispName = "on-object-" + p.ToLower();
            SendEvent(lispName, objectUpdated, after);
            return after;
        }

		Object notifyUpdate(Primitive objectUpdated, ObjectUpdate before, ObjectUpdate after, DoWhat didUpdate)
		{
			ObjectUpdate diff = updateDiff(before, after);
			bool wasChanged = false;
			if (before.Acceleration != after.Acceleration) {
                after.Acceleration = (Vector3)didUpdate(objectUpdated, "Acceleration", before.Acceleration, after.Acceleration, diff.Acceleration);
				wasChanged = true;
			}
			if (before.AngularVelocity != after.AngularVelocity) {
                after.AngularVelocity = (Vector3)didUpdate(objectUpdated, "AngularVelocity", before.AngularVelocity, after.AngularVelocity, diff.AngularVelocity);
				wasChanged = true;
			}
			if (before.CollisionPlane != after.CollisionPlane) {
                after.CollisionPlane = (Vector4)didUpdate(objectUpdated, "CollisionPlane", before.CollisionPlane, after.CollisionPlane, diff.CollisionPlane);
				wasChanged = true;
			}
			if (before.Position != after.Position) {
                after.Position = (Vector3)didUpdate(objectUpdated, "Position", before.Position, after.Position, diff.Position);
				wasChanged = true;
			}
			if (before.Rotation != after.Rotation) {
                after.Rotation =(Quaternion) didUpdate(objectUpdated, "Rotation", before.Rotation, after.Rotation, diff.Rotation);
				wasChanged = true;
			}
			if (before.State != after.State) {
				didUpdate(objectUpdated, "State", before.State, after.State, diff.State);
				wasChanged = true;
			}
			if (before.Textures != after.Textures) {
				didUpdate(objectUpdated, "Textures", before.Textures, after.Textures, diff.Textures);
				wasChanged = true;
			}
			if (before.Velocity != after.Velocity) {
               // didUpdate(objectUpdated, "Velocity", before.Velocity, after.Velocity, diff.Velocity);
				if (before.Velocity == Vector3.Zero) {
					SendEvent("on-object-start-velosity", objectUpdated, after.Velocity);
                    SendEvent("on-object-position", objectUpdated, after.Position);
				} else
					if (after.Velocity == Vector3.Zero) {
                    SendEvent("on-object-position", objectUpdated, after.Position);
					SendEvent("on-object-stop-velosity", objectUpdated, -before.Velocity);
				} else {
					//SendEvent("on-object-change-velosity", objectUpdated, after.Velocity);
                    SendEvent("on-object-position", objectUpdated, after.Position);
                }            
				wasChanged = true;
			}
			if (!wasChanged) return null;
			return diff;
		}

        public override void Terrain_OnLandPatch(Simulator simulator, int x, int y, int width, float[] data)
        {
            //throw new NotImplementedException();
         //   SendEvent("On-Land-Patch", x, y, width, data);
            //            output("TextForm Terrain_OnLandPatch: "+simulator.ToString()+"/"+x.ToString()+"/"+y.ToString()+" w="+width.ToString());
        }

        //void Self_OnAnimationsChanged(InternalDictionary<UUID, int> agentAnimations)
        //{
        //    //throw new NotImplementedException();
        //    SendEvent("On-Animations-Changed", agentAnimations);
        //    //           output("TextForm Self_OnAnimationsChanged: ");
        //}

        //void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties properties)
        //{
        //    SendEvent("On-Object-Properties", simulator, properties);
        //    // Handled by Object Listener
        //    //throw new NotImplementedException();
        //    //           output("TextForm Objects_OnObjectProperties: ");
        //}

        //void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        //{
        //    SendEvent("On-Object-Updated", simulator, update, regionHandle, timeDilation);
        //    //throw new NotImplementedException();
        //    //            output("TextForm Objects_OnObjectUpdated: ");
        //}

        //void Objects_OnObjectKilled(Simulator simulator, uint objectID)
        //{
        //    //throw new NotImplementedException();
        //    SendEvent("On-Object-Killed", simulator, objectID);
        //}

        //void Objects_OnNewPrim(Simulator simulator, Primitive prim, ulong regionHandle, ushort timeDilation)
        //{
        //    // Handled by Object Listener
        //    //throw new NotImplementedException();
        //    //            output("TextForm Objects_OnNewPrim: "+simulator.ToString()+" "+prim.ToString());
        //    //Listeners.Objects objects = (Listeners.Objects)ObjectSystem;
        //    //objects.SetCurrentSimulator(simulator);
        //    //objects.BlockUntilProperties(prim);
        //    //if (prim.Properties.Name != null)
        //    //    SendEvent("on-new-prim",prim.Properties.Name,prim.Properties.ObjectID,prim.Properties.Description);
        //    //else SendEvent("On-New-Prim", simulator, prim, regionHandle, timeDilation);

        //}

        //void Objects_OnNewFoliage(Simulator simulator, Primitive foliage, ulong regionHandle, ushort timeDilation)
        //{
        //    //throw new NotImplementedException();
        //    //            output("TextForm Objects_OnNewFoliage: ");

        //    Listeners.Objects objects = (Listeners.Objects)ObjectSystem;
        //    objects.SetCurrentSimulator(simulator);
        //    objects.BlockUntilProperties(foliage);

        //    if (foliage.Properties.Name != null)
        //        SendEvent("on-new-foliage",foliage.Properties.Name,foliage.Properties.ObjectID,foliage.Properties.Description);
        //    else SendEvent("On-New-Foliage", simulator, foliage, regionHandle, timeDilation);
        //}

        //void Objects_OnNewAvatar(Simulator simulator, Avatar avatar, ulong regionHandle, ushort timeDilation)
        //{
        //    //throw new NotImplementedException();
        //    SendEvent("On-New-Avatar-Hook", simulator, avatar, regionHandle, timeDilation);
        //    SendEvent("on-new-avatar",avatar.Name,avatar.ID);
        //}

        //void Avatars_OnAvatarProperties(UUID avatarID, Avatar.AvatarProperties properties)
        //{
        //    //throw new NotImplementedException();
        //    output("TextForm Avatars_OnAvatarProperties: ");
        //}

        public override void Avatars_OnPointAt(UUID sourceID, UUID targetID, Vector3d targetPos, PointAtType lookType, float duration, UUID id)
        {
            // output("TextForm Avatars_OnLookAt: " + sourceID.ToString() + " to " + targetID.ToString() + " at " + targetID.ToString() + " with type " + lookType.ToString() + " duration " + duration.ToString());
            if (targetID == client.Self.AgentID)
            {
                output("  (TARGET IS SELF)");
                SendEvent("on-self-point-target", GetObject(sourceID), lookType);
            }
            SendEvent("on-avatar-point", GetObject(sourceID), GetObject(targetID), targetPos, lookType.ToString(), duration, GetObject(id));
        }


        public override void Avatars_OnLookAt(UUID sourceID, UUID targetID, Vector3d targetPos, LookAtType lookType, float duration, UUID id)
        {
            if (lookType == LookAtType.Idle) return;
           // output("TextForm Avatars_OnLookAt: " + sourceID.ToString() + " to " + targetID.ToString() + " at " + targetID.ToString() + " with type " + lookType.ToString() + " duration " + duration.ToString());
            if (targetID == client.Self.AgentID)
            {
                output("  (TARGET IS SELF)");
                SendEvent("on-self-look-target", GetObject(sourceID), lookType);
            }
            SendEvent("on-avatar-look", GetObject(sourceID), GetObject(targetID), targetPos, lookType.ToString(), duration, GetObject(id));
        }

        public object GetObject(UUID id)
        {
            object ret = GetPrimitive(id);
            if (ret != null) return ret;
            ret = GetAvatar(id);
            if (ret != null) return ret;
            ret = GetAnimationName(id);
            if (ret != null) return ret;
            return id;
        }

        public object GetPrimitive(UUID id)
        {
            Primitive prim;
            if (prims.TryGetValue(id, out prim))
            {
                return prim;
            }
            return null;
        }
		public void SendEvent(string eventName, params object[] args)
		
        {
            if (eventName.Contains("on-avatar-look")) return;
			Console.WriteLine(eventName + " " + client.argsListString(args));
			client.SendEvent(eventName, args);
		}


        public override void Avatars_OnAvatarProperties(UUID avatarID, Avatar.AvatarProperties properties)
		{
            SendEvent("On-Avatar-Properties", GetAvatar(avatarID), properties);
		}

        public override void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties props)
		{
			Primitive prim;

			if (prims.TryGetValue(props.ObjectID, out prim)) {
				if (primsAwaitingSelect.Contains(prim)) {
					primsAwaitingSelect.Remove(prim);
				}
				//if (Program.Verbosity > 2)
				///botoutput("Received properties for " + props.ObjectID.ToString());               
				//lock (prim)  prim.Properties = props;
				lock(primsKnown)
				if (!primsKnown.Contains(prim)) {
					primsKnown.Add(prim);
					SendEvent("on-new-prim", props.Name, props.ObjectID.ToString(), props.Description);
				}
				CalcStats(prim);
				describePrimToAI(prim);

			} else {
				output("Received object properties for untracked object " + props.ObjectID.ToString());
			}

		}

		void Avatars_OnAnimationsChanged(InternalDictionary<UUID, int> agentAnimations)
		{



		}

		static Dictionary<string, string> animationName = new Dictionary<string, string>();
		static String GetAnimationName(UUID uuid)
		{
			String uuidname = uuid.ToString();
			if (animationName.ContainsKey(uuidname)) {
				return animationName[uuidname];
			}
			foreach (FieldInfo fi in typeof(Animations).GetFields())
			{
				if (fi.GetValue(null).Equals(uuid)) {
					animationName[uuidname] = fi.Name;
					return fi.Name;
				}
			}
			return uuidname;
		}



		//void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties properties)
		//{
		//    sim = simulator;
		//    lock (pendingPrims)
		//    {
		//        if (pendingPrims.ContainsKey(properties.ObjectID))
		//        {
		//            lock (prims)
		//            {
		//                Primitive p = pendingPrims[properties.ObjectID];
		//                prims[properties.Name] = p;
		//                primsByLocalID[p.LocalID] = p;
		//                p.Properties = properties;
		//                describePrimToAI(p);
		//                pendingPrims.Remove(properties.ObjectID);

		//                UUID groupId = properties.GroupID;
		//                if (groupId != UUID.Zero)
		//                {
		//                    lock (primGroups)
		//                    {
		//                        if (!primGroups.ContainsKey(groupId))
		//                            primGroups[groupId] = new List<Primitive>();
		//                        primGroups[groupId].Add(prims[properties.Name]);
		//                        //botoutput("group count " + groupId + " " + primGroups[groupId].Count);
		//                    }
		//                }
		//            }
		//            if (maxNameLength == -1 || properties.Name.Length > maxNameLength)
		//                maxNameLength = properties.Name.Length;
		//        }
		//    }
		//}

		//void Objects_OnNewPrim(Simulator simulator, Primitive prim, ulong regionHandle, ushort timeDilation)
		//{
		//    sim = simulator;
		//    try
		//    {
		//        lock (newLock)
		//        {
		//            primsAwaitingSelect.Add(prim);
		//            CalcStats(prim);

		//            TimeSpan dtime = DateTime.Now - burstStartTime;
		//            if (primsAwaitingSelect.Count >= burstSize && dtime > burstInterval)
		//            {
		//                burstStartTime = DateTime.Now;

		//                uint[] ids = new uint[burstSize];
		//                for (int i = 0; i < burstSize; ++i)
		//                {
		//                    ids[i] = primsAwaitingSelect[i].LocalID;
		//                    pendingPrims[primsAwaitingSelect[i].ID] = primsAwaitingSelect[i];
		//                }

		//                botclient.Objects.SelectObjects(simulator, ids);
		//                primsAwaitingSelect.RemoveRange(0, burstSize);
		//                //primsAwaitingSelect.Clear();
		//            }
		//        }
		//CalcStats(prim);
		//        describePrimToAI(prim);
		//    }
		//    catch (Exception e)
		//    {
		//        botoutput("ERR:" + e.StackTrace);
		//    }
		//}
		public void CalcStats(Primitive prim)
		{
			if (prim is Avatar)	return;
			BlockUntilProperties(prim);
			if (boringNamesHeuristic(prim) == 0)
				client.BoringNamesCount++;
			else
				client.GoodNamesCount++;
		}

		public bool tryGetPrim(string name, out Primitive prim)
		{
			prim = null;

			//string[] toks = name.Split(null);
			//if (toks.Length == 2 && toks[0] == "object")
			//{
			//    int i = Convert.ToInt32(toks[1]);
			//    if (i > 0 && i <= numberedObjects.Count)
			//    {
			//        prim = prims[numberedObjects[i - 1]];
			//        BlockUntilProperties(prim);
			//        return true;
			//    }
			//}

			//if (shortNames.ContainsKey(name))
			//{
			//    prim = prims[shortNames[name]];
			//    BlockUntilProperties(prim);
			//    return true;
			//}

			//foreach (string primName in prims.Keys)
			//{
			//    if (primName.Length >= name.Length && primName.Substring(0, name.Length) == name)
			//    {
			//        prim = prims[primName];
			//        BlockUntilProperties(prim);
			//        return true;
			//    }
			//}
			return false;
		}

		public void describePrim(Primitive prim)
		{
			if (prim is Avatar)	return;
			BlockUntilProperties(prim);
			output(prim.Properties.Name + ": " + prim.Properties.Description);
			if (prim.Sound != UUID.Zero)
				output("This object makes sound.");
			if (prim.Properties.SalePrice != 0)
				output("This object is for sale for L" + prim.Properties.SalePrice);
		}

		public void describePrimToAI(Primitive prim)
		{
			if (prim is Avatar) {
				Avatar avatar = (Avatar)prim;
				describeAvatarToAI(avatar);
				return;
			}
			if (!primsKnown.Contains(prim))	return;
			BlockUntilProperties(prim);
			if (prim.Properties.Name != null) {
				//botenqueueLispTask("(on-prim-description '(" + prim.Properties.Name + ") '" + prim.Properties.Description + "' )");
				SendEvent("on-prim-dist", prim, Vector3.Distance(client.Self.SimPosition, prim.Position));
				SendEvent("on-prim-pos", prim, prim.Position);
				SendEvent("on-prim-description", prim, "" + prim.Properties.Description);
				//botoutput(prim.Properties.Name + ": " + prim.Properties.Description);
				//if (prim.Sound != UUID.Zero)
				//    botoutput("This object makes sound.");
				//if (prim.Properties.SalePrice != 0)
				//    botoutput("This object is for sale for L" + prim.Properties.SalePrice);
			}
		}

		public int comp(Primitive p1, Primitive p2)
		{
			return(int)(getFitness(p1) - getFitness(p2));
		}

		public List<Primitive> getPrimitives(int num)
		{

			List<Primitive> ret = new List<Primitive>();

			prims.ForEach(delegate(Primitive prim){
						  ret.Add(prim);




						 });

			//foreach (Primitive prim in prims.ForEach.Values)
			//{
			//    ret.Add(prim);
			//}


			if (ret.Count <= num) {
				updateNumberedObjects(ret, num);
				return ret;
			} else {
				ret.Sort(new Comparison<Primitive>(comp));
				updateNumberedObjects(ret, num);
				return ret.GetRange(0, num);
			}
		}

		void updateNumberedObjects(List<Primitive> ret, int num)
		{
			numberedObjects.Clear();
			for (int i = 0; i < num && i < ret.Count; ++i) {
				if (ret[i].Properties != null) numberedObjects.Add(ret[i].Properties.Name);
			}
		}

		float getFitness(Primitive prim)
		{
			float fitness = 1;
			foreach (ObjectHeuristic heuristic in objectHeuristics)
			{
				fitness *= heuristic(prim);
			}
			return fitness;
		}

		float distanceHeuristic(Primitive prim)
		{
			if (prim != null)
				return(float)(1.0 / Math.Exp((double)Vector3.Distance(client.Self.RelativePosition, prim.Position)));
			else
				return(float)0.01;
		}

		float nameLengthHeuristic(Primitive prim)
		{
			if ((prim != null) && (prim.Properties.Name != null)) {
				return(float)prim.Properties.Name.Length / (float)maxNameLength;
			} else
				return(float)0.1;
		}

		float boringNamesHeuristic(Primitive prim)
		{
			string name = prim.Properties.Name;
			if (name == "Object" || name == "Component" || name == null)
				return(float)0.1;
			else
				return 1;
		}

		bool tryGetBuildingPos(List<Primitive> group, out Vector3 centroid)
		{
			centroid = new Vector3();
			if (group.Count < 4)
				return false;
			else {
				bool first = true;
				Vector3 min = new Vector3(), max = new Vector3(), pos;
				foreach (Primitive prim in group)
				{
					if (prim != null && prim.Position != null) {
						pos = prim.Position;

						if (first) {
							min = pos;
							max = pos;
							first = false;
						} else {
							if (pos.X < min.X)
								min.X = pos.X;
							if (pos.Y < min.Y)
								min.Y = pos.Y;
							if (pos.Z < min.Z)
								min.Z = pos.Z;

							if (pos.X > max.X)
								max.X = pos.X;
							if (pos.Y > max.Y)
								max.Y = pos.Y;
							if (pos.Z > max.Z)
								max.Z = pos.Z;
						}
					}
				}

				Vector3 size = max - min;
				if (size.X > buildingSize && size.Y > buildingSize && size.Z > buildingSize) {
					centroid = min + (size * (float)0.5);
					return true;
				} else
					return false;
			}
		}

		public int posComp(Vector3 v1, Vector3 v2)
		{
			return(int)(Vector3.Mag(client.Self.RelativePosition - v1) -
						Vector3.Mag(client.Self.RelativePosition - v2));
		}

		public List<Vector3> getBuildings(int num)
		{
			List<Vector3> ret = new List<Vector3>();
			foreach (List<Primitive> group in primGroups.Values)
			{
				Vector3 pos = new Vector3();
				if (tryGetBuildingPos(group, out pos))
					ret.Add(pos);
			}

			if (ret.Count <= num)
				return ret;
			else {
				ret.Sort(new Comparison<Vector3>(posComp));
				return ret.GetRange(0, num);
			}
		}

		public string getObjectName(Primitive prim)
		{
			BlockUntilProperties(prim);
			string name = getObjectShortName(prim);
			for (int i = 0; i < numberedObjects.Count; ++i)
				if (numberedObjects[i] == prim.Properties.Name)
					name = (i + 1) + ": " + name;
			return name;
		}

		public string getObjectShortName(Primitive prim)
		{
			if (prim.Properties.Name.Length < maxShortNameLength) {
				return prim.Properties.Name;
			} else {
				if (reverseShortNames.ContainsKey(prim.Properties.Name))
					return reverseShortNames[prim.Properties.Name];
				else {
					string shortName = "";
					int i = 0;
					foreach (string token in prim.Properties.Name.Split(null))
					{
						if (i != 0)	shortName += " ";
						i++;
						shortName += token;
						if (!shortNames.ContainsKey(shortName)) {
							shortNames[shortName] = prim.Properties.Name;
							reverseShortNames[prim.Properties.Name] = shortName;
							return shortName;
						}
					}
					return prim.Properties.Name;
				}
			}
		}

		public string primType(Primitive target)
		{
			if (target.PrimData.PCode == PCode.Prim)
				return target.PrimData.Type.ToString();
			return target.PrimData.PCode.ToString();

		}

        public override void Objects_OnNewAvatar(Simulator simulator, Avatar avatar, ulong regionHandle, ushort timeDilation)
		{
            lock (avatars)
                avatars.Add(avatar.LocalID, avatar.ID, avatar);
			try {
				lock (avatarCache)
				{
					if (avatar != null) {
						if (!avatarCache.ContainsKey(avatar.Name)) {
							avatarCache[avatar.Name] = avatar;
						}
					}
				}
				describeAvatarToAI(avatar);
			} catch (Exception e) {
				output("err :" + e.StackTrace);
			}
		}

		public int numAvatars()
		{
			return avatarCache.Count;
		}

		public int comp(Avatar a1, Avatar a2)
		{
			return(int)(Vector3.Distance(a1.Position, compPos) - Vector3.Distance(a2.Position, compPos));
		}

		public List<Avatar> getAvatarsNear(Vector3 pos, int num)
		{
			compPos = pos;
			List<Avatar> avatarList = new List<Avatar>();
			foreach (Avatar avatar in avatarCache.Values)
			if (avatar.Name != client.Self.Name)
				avatarList.Add(avatar);

			if (avatarList.Count > num) {
				avatarList.Sort(new Comparison<Avatar>(comp));

				for (; searchStep * num > avatarList.Count; --searchStep) ;

				List<Avatar> ret = new List<Avatar>();
				for (int i = 0; i < num && i < avatarList.Count; i += searchStep)
					ret.Add(avatarList[i]);
				searchStep = (searchStep + 1) % 4 + 1;
				updateNumberedAvatars(ret);
				return ret;
			} else {
				updateNumberedAvatars(avatarList);
				return avatarList;
			}
		}

		public void updateNumberedAvatars(List<Avatar> avatars)
		{
			numberedAvatars.Clear();
			for (int i = 0; i < avatars.Count; ++i)
				numberedAvatars.Add(avatars[i].Name);
		}

		public bool tryGetAvatarById(UUID id, out Avatar avatar)
		{
			avatar = null;
			foreach (Avatar av in avatarCache.Values)
			{
				if (av.ID == id) {
					avatar = av;
					return true;
				}
			}
			return false;
		}

		public bool tryGetAvatar(string name, out Avatar avatar)
		{
			avatar = null;

			string[] toks = name.Split(null);
			if (toks.Length == 2 && toks[0] == "person") {
				try {
					int i = Convert.ToInt32(toks[1]);
					if (i > 0 && i <= numberedAvatars.Count) {
						avatar = avatarCache[numberedAvatars[i - 1]];
						return true;
					}
				} catch (FormatException) {
				}
			}

			if (avatarCache.ContainsKey(name)) {
				avatar = avatarCache[name];
				return true;
			}

			foreach (string avatarName in avatarCache.Keys)
			{
				if (avatarName.Length >= name.Length && avatarName.Substring(0, name.Length) == name) {
					avatar = avatarCache[avatarName];
					return true;
				}
			}

			return false;
		}

		public string getAvatarName(Avatar avatar)
		{
			string name = avatar.Name;
			for (int i = 0; i < numberedAvatars.Count; ++i)
				if (numberedAvatars[i] == name)
					name = (i + 1) + ": " + name;
			return name;
		}

		public void describeAvatar(Avatar avatar)
		{
		//	string verb;
			//if (avatar.SittingOn == 0)
			//    verb = "standing";
			//else
			//    verb = "sitting";
			//botoutput(avatar.Name + " is " + verb + " in " + avatar.CurrentSim.Name + ".");
			output(avatar.Name + " is " + Vector3.Distance(client.Self.SimPosition, avatar.Position).ToString() + " distant.");
			if (avatar.ProfileProperties.BornOn != null)
				output("Born on: " + avatar.ProfileProperties.BornOn);
			if (avatar.ProfileProperties.AboutText != null)
				output("About their second life: " + avatar.ProfileProperties.AboutText);
			if (avatar.ProfileProperties.FirstLifeText != null)
				output("About their first life: " + avatar.ProfileProperties.FirstLifeText);
			if (avatar.ProfileInterests.LanguagesText != null)
				output("Languages spoken: " + avatar.ProfileInterests.LanguagesText);
			if (avatar.ProfileInterests.SkillsText != null)
				output("Skills: " + avatar.ProfileInterests.SkillsText);
			if (avatar.ProfileInterests.WantToText != null)
				output("Wants to: " + avatar.ProfileInterests.WantToText);
		}

		public void describeAvatarToAI(Avatar avatar)
		{
			// string verb;
			// if (avatar.SittingOn == 0)
			//     verb = "standing";
			// else
			//     verb = "sitting";
			//botoutput(avatar.Name + " is " + verb + " in " + avatar.CurrentSim.Name + ".");
			//botoutput(avatar.Name + " is " + Vector3.Distance(Client.Self.SimPosition, avatar.Position).ToString() + " distant.");
			SendEvent("on-avatar-dist", avatar, Vector3.Distance(client.Self.SimPosition, avatar.Position));
			SendEvent("on-avatar-pos", avatar, avatar.Position);
			SendEvent("on-avatar-description", avatar, avatar.GroupName);
			//  botenqueueLispTask("(on-avatar-posture (@\"" + avatar.Name + "\") (@\"" + verb + "\") )");

			/*
			if (avatar.ProfileProperties.BornOn != null)
				botoutput("Born on: " + avatar.ProfileProperties.BornOn);
			if (avatar.ProfileProperties.AboutText != null)
				botoutput("About their second life: " + avatar.ProfileProperties.AboutText);
			if (avatar.ProfileProperties.FirstLifeText != null)
				botoutput("About their first life: " + avatar.ProfileProperties.FirstLifeText);
			if (avatar.ProfileInterests.LanguagesText != null)
				botoutput("Languages spoken: " + avatar.ProfileInterests.LanguagesText);
			if (avatar.ProfileInterests.SkillsText != null)
				botoutput("Skills: " + avatar.ProfileInterests.SkillsText);
			if (avatar.ProfileInterests.WantToText != null)
				botoutput("Wants to: " + avatar.ProfileInterests.WantToText);
			*/

		}

	}
}
