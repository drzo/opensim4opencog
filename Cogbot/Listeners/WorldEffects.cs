using System;
using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Packets;
using PathSystem3D.Navigation;
using System.Reflection;

namespace cogbot.Listeners
{
    public partial class WorldObjects
    {

        //private static List<UUID> EffectsSent = new List<UUID>();
        public static readonly List<String> SkippedEffects = new List<string>();
        private readonly SimAssetStore _simAssetSystem;
        public SimAssetStore SimAssetSystem
        {
            get { return _simAssetSystem; }
        }


        public override void Self_OnAnimationsChanged(InternalDictionary<UUID, int> agentAnimations)
        {
            if (!MaintainAnims) return;
            // Avatars_OnAvatarAnimation(client.Self.AgentID, agentAnimations);
        }

        //private object AsType(Object te, Type type, MemberInfo info)
        //{
        //    if (type.IsInstanceOfType(te)) return te;
        //    return new NullType(type);
        //}
        public override void Appearance_OnAppearanceUpdated(Primitive.TextureEntry te)
        {
            if (te!=null)
            {
                client.SendNetworkEvent("On-Appearance-Updated", te);                
            }
            //client.SendNetworkEvent("On-Appearance-Updated", AsType(te, typeof(Primitive.TextureEntry)));
        }

        /// <summary>
        /// Process incoming avatar animations
        /// </summary>
        /// <param name="packet"></param>
        /// <param name="sim"></param>
        private void AvatarAnimationHandler(Packet packet, Simulator sim)
        {

            if (!IsMaster(sim)) return;
            AvatarAnimationPacket anims = (AvatarAnimationPacket)packet;

            InternalDictionary<UUID, int> signaledAnims = new InternalDictionary<UUID, int>();

            for (int i = 0; i < anims.AnimationList.Length; i++)
                signaledAnims.Dictionary.Add(anims.AnimationList[i].AnimID, anims.AnimationList[i].AnimSequenceID);

            Avatars_OnAvatarAnimation(anims.Sender.ID, signaledAnims);

        }

        public override void Avatars_OnAvatarAnimation(UUID avatarID, InternalDictionary<UUID, int> anims)
        {
            if (!MaintainAnims) return;
                EventQueue.Enqueue(() =>
                {
                    SimAvatar avatar = (SimAvatar)GetSimObjectFromUUID(avatarID);
                    if (avatar == null)
                    {
                        return;
                    }
                    if (UseEventSource(avatar))
                    {
                        avatar.OnAvatarAnimations(anims);
                    }
                });
        }

        public SimObjectEvent SendPipelineEvent(SimObjectEvent param1)
        {
            client.SendPipelineEvent(param1);
            return param1;
        }
        /*
             * TODO
             On-Avatar-Sit-Changed
            simulator: "OpenSim Test"
            avatar: '(avatar "Nephrael Rae")
            sittingOn: 3135593693
            oldSeat: 0
             */
        public override void Objects_OnAvatarSitChanged(Simulator simulator, Avatar avatar, uint sittingOn, uint oldSeat)
        {
            if (!IsMaster(simulator))
            {
                return;
            }
            if (!MaintainActions) return;
                EventQueue.Enqueue(() =>
                {
                    SimObject user = GetSimObject(avatar, simulator);
                    SimObject newSit = GetSimObject(sittingOn, simulator);
                    SimObject oldSit = GetSimObject(oldSeat, simulator);
                    string newSitName = null;
                    string oldSitName = null;
                    if (newSit != null)
                    {
                        newSitName = newSit.SitName;
                    }
                    if (newSitName == null) newSitName = "SitOnObject";
                    if (oldSit != null)
                    {
                        oldSitName = oldSit.SitName;
                    }
                    if (oldSitName == null) oldSitName = "SitOnObject";

                    //if (SendAllEvents)
                    // SendNewEvent("On-Avatar-Sit-Changed", user, newSit, oldSit);
                    if (user != null)
                    {
                        if (oldSeat != 0)
                            LogSitEvent(user, SimEventStatus.Stop, oldSitName,
                                        ToParameter("doneBy", user),
                                        ToParameter("objectActedOn", oldSit));
                        if (sittingOn != 0)
                            LogSitEvent(user, SimEventStatus.Start, newSitName,
                                        ToParameter("doneBy", user),
                                        ToParameter("objectActedOn", newSit));
                        if (sittingOn + oldSeat == 0)
                            LogSitEvent(user, SimEventStatus.Once, "SitChangedUnknown", ToParameter("doneBy", user));
                    }
                    else
                    {
                        //object[] eventArgs = new object[] { user, newSit, oldSit };
                        if (oldSit != null)
                            oldSit.AddCanBeTargetOf(1, SendPipelineEvent(
                                                           new SimObjectEvent(SimEventStatus.Stop, newSitName,
                                                                              SimEventType.SIT, SimEventClass.REGIONAL,
                                                                              ToParameter("doneBy", avatar),
                                                                              ToParameter("objectActedOn", oldSit))));
                        if (newSit != null)
                            newSit.AddCanBeTargetOf(1, SendPipelineEvent(
                                                           new SimObjectEvent(SimEventStatus.Start, newSitName,
                                                                              SimEventType.SIT, SimEventClass.REGIONAL,
                                                                              ToParameter("doneBy", avatar),
                                                                              ToParameter("objectActedOn", newSit))));
                    }
                });
        }

        private void LogSitEvent(SimObject user, SimEventStatus updown, string p, params NamedParam[] args)
        {
            if (!MaintainActions) return;
            if (user !=null)
            {
                user.Parent = null;
                user.LogEvent(SendPipelineEvent(new SimObjectEvent(updown, p, SimEventType.SIT, SimEventClass.REGIONAL, args)));
            }
            //Console.WriteLine(user + " " + p + " " + ScriptEngines.ScriptEventListener.argsListString(args));
        }


        public void OnObjectSound(UUID objectID, UUID soundID, float gain)
        {
            if (!MaintainSounds) return;
            SimObject o = SimObjectFn(objectID);
            if (o == null) return;
            if (soundID == UUID.Zero)
            {
                o.OnSound(UUID.Zero, gain);
                SendNewRegionEvent(SimEventType.EFFECT, "On-Attach-Sound-Gain-Change", o, gain);
            }
            else
            {
                SimAsset sound = RequestAsset(soundID, AssetType.Sound, true);
                o.OnSound(soundID, gain);
                SendNewRegionEvent(SimEventType.EFFECT, "On-Single-Sound-Gain-Change", o, sound, gain);
            }
            // RegionMasterTexturePipeline.RequestAsset(soundID, AssetType.SoundWAV, true);
        }

        public override void Sound_OnSoundTrigger(UUID soundID, UUID ownerID, UUID objectID, UUID parentID, float gain,
                                                  ulong regionHandle, Vector3 position)
        {
            if (!MaintainSounds) return;

            RequestAsset(soundID, AssetType.Sound, true);

                EventQueue.Enqueue(() =>
                {
                    if (objectID != UUID.Zero) OnObjectSound(objectID, soundID, gain);
                    else
                    {
                        SendNewRegionEvent(SimEventType.EFFECT, "On-Sound-Position-Trigger", soundID, 
                            ownerID, parentID, gain, regionHandle, position);
                    }
                });
        }


        public override void Sound_OnPreloadSound(UUID soundID, UUID ownerID, UUID objectID)
        {
            if (!MaintainSounds) return;
            RequestAsset(soundID, AssetType.Sound, true);
            //base.Sound_OnPreloadSound(soundID, ownerID, objectID);
            //WriteLine("preload sound " + soundID);
        }


        public override void Sound_OnAttachSound(UUID soundID, UUID ownerID, UUID objectID, float gain, SoundFlags flags)
        {
            if (!MaintainSounds) return;
            RequestAsset(soundID, AssetType.Sound, true);                
            EventQueue.Enqueue(() => OnObjectSound(objectID, soundID, gain));
            //SendNewEvent("On-Attach-Sound", soundID, ownerID, objectID, gain, flags);
            //base.Sound_OnAttachSound(soundID, ownerID, objectID, gain, flags);
        }

        public override void Sound_OnAttachSoundGainChange(UUID objectID, float gain)
        {
            if (!MaintainSounds) return;
                EventQueue.Enqueue(() =>
                {
                    OnObjectSound(objectID, UUID.Zero, gain);
                });
            //base.Sound_OnAttachSoundGainChange(objectID, gain);
        }

        private SimObject SimObjectFn(UUID objectID)
        {
            SimObject O = GetSimObjectFromUUID(objectID);
            if (O==null)
            {
                //GetSource()
                return null;
                O = CreateSimObject(objectID,this, null);                
            }
            return O;
        }


        private void MeanCollisionAlertHandler(Packet packet, Simulator sim)
        {
            if (!IsMaster(sim)) return;
            {
                MeanCollisionAlertPacket collision = (MeanCollisionAlertPacket)packet;

                for (int i = 0; i < collision.MeanCollision.Length; i++)
                {
                    MeanCollisionAlertPacket.MeanCollisionBlock block = collision.MeanCollision[i];

                    DateTime time = Utils.UnixTimeToDateTime(block.Time);
                    MeanCollisionType type = (MeanCollisionType)block.Type;

                    Self_OnMeanCollision(type, block.Perp, block.Victim, block.Mag, time);
                }
            }
        }


        public override void Self_OnMeanCollision(MeanCollisionType type, UUID perp, UUID victim, float magnitude,
                                          DateTime time)
        {
            if (!MaintainEffects) return;
                EventQueue.Enqueue(() =>
                {
                    SimObject perpAv, victimAv;
                    if (TryGetSimObject(perp, out perpAv) &&
                        TryGetSimObject(victim, out victimAv))
                    {
                        // if (victimAv.Name == client.Self.Name)
                        //   WriteLine(perpAv.Name + " bumped into $bot like " + type);
                        // else if (perpAv.Name == client.Self.Name)
                        //   WriteLine("$bot bumped into " + victimAv.Name + " like " + type);   
                        SimObjectEvent newSimObjectEvent = new SimObjectEvent(SimEventStatus.Once,
                                                    "MeanCollisionType-" + type, SimEventType.SOCIAL, SimEventClass.REGIONAL,
                                                    ToParameter("primaryObjectMoving", perpAv),
                                                    ToParameter("objectActedOn", victimAv),
                                                    ToParameter("initialSpeedOfPrimaryObjectMoving", "MetersPerSecond", magnitude));
                        perpAv.LogEvent(newSimObjectEvent);
                    }
                });
        }

        static WorldObjects()
        {

            SkippedEffects.Add("LookAtType-Idle");
            SkippedEffects.Add("LookAtType-FreeLook");
            //SkippedEffects.Add("PointAtType.None");
        }

        public override void Avatars_OnPointAt(UUID sourceID, UUID targetID,
                                               Vector3d targetPos, PointAtType lookType, float duration, UUID id)
        {
            // we have our own packet handler
            client.Avatars.OnPointAt -= Avatars_OnPointAt;
            if (!MaintainEffects) return;
            SendEffect(client.Network.CurrentSim, sourceID, targetID, targetPos, "PointAtType-" + lookType.ToString(), duration, id);
        }

        public override void Avatars_OnLookAt(UUID sourceID, UUID targetID,
                                              Vector3d targetPos, LookAtType lookType, float duration, UUID id)
        {
            // we have our own packet handler
            client.Avatars.OnLookAt -= Avatars_OnLookAt;
            if (!MaintainEffects) return;
            SendEffect(client.Network.CurrentSim, sourceID, targetID, targetPos, "LookAtType-" + lookType.ToString(), duration, id);
        }

        public bool UseEventSource(Object so)
        {
            if (so is SimAvatar)
            {
                SimAvatar A = (SimAvatar)so;
                if (!A.IsRegionAttached) return false;
                if (A.Distance(TheSimAvatar) < 30) return true;
                if (A.IsRegionAttached && A.GetName().Contains("Rajesh"))
                    return true;
            }
            //return true;
            return false;
        }

        public void SendEffect(Simulator sim, UUID sourceID, UUID targetID, Vector3d targetPos, string effectType, float duration,
                               UUID id)
        {
            if (!MaintainEffects) return;
            if (sourceID == client.Self.AgentID) return; //not sending our own effects
            if (!IsMaster(sim)) return;
            if (client.MasterKey == UUID.Zero) return;
            if (!(client.MasterKey == targetID || sourceID == client.MasterKey)) return;
            if (id != UUID.Zero)
            {
                // if (EffectsSent.Contains(id)) return;
                // EffectsSent.Add(id);
            }
            object s = sourceID;
            object t = targetID;
            object p = targetPos;

            //if (SkippedEffects.Contains(effectType)) return;
            SimObject source = GetSimObjectFromUUID(sourceID);
            if (source == null)
            {
                if (sourceID != UUID.Zero)
                {
                    source = GetSource(sim, sourceID, source, ref s, PCode.Avatar);
                    if (source == null) return;
                }
                //  RequestAsset(sourceID, AssetType.Object, true);
            }
            else
            {
                s = source;
            }
            if (source is SimObjectImpl && !(source is SimAvatar))
            {
                Debug("Write source is Object " + source);
            }
            SimObject target = GetSimObjectFromUUID(targetID);
            if (target == null)
            {
                if (targetID != UUID.Zero)
                {
                    target = GetSource(sim, targetID, target, ref t, PCode.None);
                    if (target == null) return;
                }
                // RequestAsset(targetID, AssetType.Object, true);
            }
            else
            {
                t = target;
            }
            double dist;
            SimObject ST = target;
            if (ST == null) ST = source;
            if (targetPos.X < 256)
            {
                if (targetPos == Vector3d.Zero)
                {
                    p = SimHeading.UNKNOWN;
                }
                else
                {
                    if (ST != null)
                    {
                        if (ST.IsRegionAttached)
                            p = (ST.GlobalPosition + targetPos);
                        else
                        {
                            p = AsRLocation(sim, targetPos, ST);
                        }

                    }
                    else
                    {
                        p = new Vector3((float) targetPos.X, (float) targetPos.Y, (float) targetPos.Z);
                    }
                }
            }
            else
            {
                SimObject posTarget = GetSimObjectFromVector(targetPos, out dist);
                if (dist < 0.5)
                {
                    p = posTarget;
                    if (targetID == UUID.Zero)
                    {
                        // now we have a target
                        t = posTarget;

                        // todo should we revert back to position?
                        //p = targetPos;
                    }
                }
                else
                {
                    if (targetID == UUID.Zero)
                    {
                        // now we have a target
                        t = targetPos;

                        // todo should we revert back to position?
                        //p = targetPos;
                    }
                }
            }

                EventQueue.Enqueue(() =>
                                        {
                                            //if (source != null) source;
                                            // WriteLine("ClientManager Avatars_OnLookAt: " + sourceID.ToString() + " to " + targetID.ToString() + " at " + targetID.ToString() + " with type " + lookType.ToString() + " duration " + duration.ToString());
                                            if (targetID == client.Self.AgentID)
                                            {
                                                // if (lookType == LookAtType.Idle) return;
                                                //WriteLine("  (TARGET IS SELF)");
                                                client.SendPersonalEvent(SimEventType.EFFECT,
                                                                         "on-effect-targeted-self",
                                                                         ToParameter("doneBy", s),
                                                                         ToParameter("objectActedOn", TheSimAvatar),
                                                                         ToParameter("eventPartiallyOccursAt", p),
                                                                         ToParameter("simDuration", duration),
                                                                         ToParameter("effectType", effectType));
                                                // ()/*GetObject*/(sourceID), effectType);
                                            }
                                            if (s is UUID)
                                            {

                                            }


                                            if (source != null)
                                            {
                                                source.OnEffect(effectType, t, p, duration, id);
                                            }
                                            else
                                            {
                                                SimObjectEvent evt = new SimObjectEvent(SimEventStatus.Once, effectType,
                                                                                        SimEventType.EFFECT, SimEventClass.REGIONAL,
                                                                                        ToParameter("doneBy", s),
                                                                                        ToParameter("objectActedOn", t),
                                                                                        ToParameter(
                                                                                            "eventPartiallyOccursAt", p),
                                                                                        ToParameter("simDuration",
                                                                                                    duration),
                                                                                        AsEffectID(id));

                                                if (t is SimObject)
                                                {
                                                    ((SimObject) t).AddCanBeTargetOf(2, evt);
                                                }
                                                RegisterUUID(id, effectType);
                                                //TODO 
                                                if (UseEventSource(s))
                                                    SendPipelineEvent(evt);
                                                //SendNewEvent("on-effect", effectType, s, t, p, duration, AsEffectID(id));
                                            }
                                        });
        }

        /*
            On-Effect
            type: "Sphere"
            sourceID: "00000000-0000-0000-0000-000000000000"
            targetID: "00000000-0000-0000-0000-000000000000"
            targetPos: '(Vector3d 256126.750573638 256129.791631699 25.2663780227304)
            duration: 0.25
            id: "a31efee6-a426-65a6-5ef2-d1345be49233"

            On-Effect
            type: "Beam"
            sourceID: '(avatar "Kuffher Hauptmann")
            targetID: "00000000-0000-0000-0000-000000000000"
            targetPos: '(Vector3d 234680.541992188 277992.395370483 350.615783691406)
            duration: 0.7
            id: "6f777a51-56a2-4c2e-2266-3ea6b9bc4945"

            On-Effect
            type: "Beam"
            sourceID: "ec8777dc-62cd-46df-8d36-98373459c23a"
            targetID: "00000000-0000-0000-0000-000000000000"
            targetPos: '(Vector3d 234556.786750793 277914.178848267 1049.44689941406)
            duration: 0.7
            id: "0c743618-6c68-171e-d515-dbda81f58258"

            On-Effect
            type: "Beam"
            sourceID: '(avatar "Kuffher Hauptmann")
            targetID: "00000000-0000-0000-0000-000000000000"
            targetPos: '(Vector3d 234680.526870728 277992.386642456 351.192932128906)
            duration: 0.7
            id: "eb064996-7879-fbab-972c-acb272ea3a91"

            On-Effect
            type: "Beam"
            sourceID: "109bacd0-650d-46f4-84cf-0e8c154123c4"
            targetID: "00000000-0000-0000-0000-000000000000"
            targetPos: '(Vector3d 234559.817420959 277769.408673286 1002.50750732422)
            duration: 0.7
            id: "bd25925f-7688-5d81-8834-e7631942c5c5"

		         
         */

        public override void Avatars_OnEffect(EffectType type, UUID sourceID, UUID targetID, Vector3d targetPos,
                                              float duration, UUID id)
        {
            // we have our own packet handler
            client.Avatars.OnEffect -= Avatars_OnEffect;
            SendEffect(client.Network.CurrentSim, sourceID, targetID, targetPos, "EffectType-" + type.ToString(), duration, id);
            //SimRegion.TaintArea(targetPos);
        }


        /// <summary>
        /// Process an incoming effect
        /// </summary>
        private void ViewerEffectHandler(Packet packet, Simulator simulator)
        {
            if (!IsMaster(simulator)) return;
            if (simulator != client.Network.CurrentSim)
            {
                //Debug("ViewerEffectHandler: from a differnt sim than current " + simulator);
            }
            if (!MaintainEffects) return;
            ViewerEffectPacket effect = (ViewerEffectPacket)packet;
            GridClient Client = client;

            foreach (ViewerEffectPacket.EffectBlock block in effect.Effect)
            {
                EffectType type = (EffectType)block.Type;

                // most effect types have at least these properties
                UUID sourceAvatar = new UUID(block.TypeData, 0);
                UUID targetObject = new UUID(block.TypeData, 16);
                Vector3d targetPos = new Vector3d(block.TypeData, 32);
                //Color4 color;
                //if (block.Color.Length == 4)
                //{
                //    color = new Color4(block.Color, 0);
                //}
                //else
                //{
                //    Client.Log("Received a ViewerEffect.EffectBlock.Color array with " + block.Color.Length + 
                //        " bytes", Helpers.LogLevel.Warning);
                //    color = Color4.Black;
                //}

                // Each ViewerEffect type uses it's own custom binary format for additional BVHData. Fun eh?
                switch (type)
                {
                    case EffectType.Beam:
                    case EffectType.Point:
                    case EffectType.Trail:
                    case EffectType.Sphere:
                    case EffectType.Spiral:
                    case EffectType.Edit:
                        // if (Avatars_OnEffect != null)
                        {
                            if (block.TypeData.Length == 56)
                            {
                                try
                                {
                                    SendEffect(simulator, sourceAvatar, targetObject, targetPos, "EffectType-" + type.ToString(), block.Duration, block.ID);
                                }
                                catch (Exception e)
                                {
                                    Debug(e.Message, Helpers.LogLevel.Error, Client, e);
                                }
                            }
                            else
                            {
                                Debug("Received a " + type.ToString() +
                                      " ViewerEffect with an incorrect TypeData size of " +
                                      block.TypeData.Length + " bytes", Helpers.LogLevel.Warning, Client);
                            }
                            return;
                        }
                        break;
                    case EffectType.LookAt:
                        //if (OnLookAt != null)
                        {
                            if (block.TypeData.Length == 57)
                            {
                                LookAtType lookAt = (LookAtType)block.TypeData[56];

                                try
                                {
                                    SendEffect(simulator, sourceAvatar, targetObject, targetPos, "LookAtType-" + lookAt.ToString(), block.Duration, block.ID);
                                }
                                catch (Exception e)
                                {
                                    Debug(e.Message, Helpers.LogLevel.Error, Client, e);
                                }
                                return;
                            }
                            else
                            {
                                Debug("Received a LookAt ViewerEffect with an incorrect TypeData size of " +
                                      block.TypeData.Length + " bytes", Helpers.LogLevel.Warning, Client);
                            }
                        }
                        break;
                    case EffectType.PointAt:
                        // if (Avatars_OnPointAt != null)
                        {
                            if (block.TypeData.Length == 57)
                            {
                                PointAtType pointAt = (PointAtType)block.TypeData[56];

                                try
                                {
                                    SendEffect(client.Network.CurrentSim, sourceAvatar, targetObject, targetPos, "PointAtType-" + pointAt.ToString(), block.Duration, block.ID);
                                }
                                catch (Exception e)
                                {
                                    Debug(e.Message, Helpers.LogLevel.Error, Client, e);
                                }
                                return;
                            }
                            else
                            {
                                Debug("Received a PointAt ViewerEffect with an incorrect TypeData size of " +
                                      block.TypeData.Length + " bytes", Helpers.LogLevel.Warning, Client);
                            }
                        }
                        break;
                    case EffectType.Text:
                    case EffectType.Icon:
                    case EffectType.Connector:
                    case EffectType.FlexibleObject:
                    case EffectType.AnimalControls:
                    case EffectType.AnimationObject:
                    case EffectType.Cloth:
                    case EffectType.Glow:
                    default:
                        SendNewRegionEvent(SimEventType.EFFECT, "OnViewerEffect", type, sourceAvatar, targetObject, targetPos);
                        break;
                }
                SendEffect(simulator, sourceAvatar, targetObject, targetPos, "EffectType." + type, 0.1f, block.ID);
            }
        }


    }
}
