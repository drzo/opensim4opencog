using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.Listeners;

namespace cogbot.TheOpenSims
{
    public class ListAsSet<T> : List<T>
    {
        public bool AddTo(T item)
        {
            if (base.Contains(item)) return false;
            base.Add(item);
            return true;
        }
        public override string ToString()
        {
            String s = "[";
            foreach (T t in this)
            {
                s += "," + t;
            }
            return s + "]";
        }
    }

    public class BotRegionModel
    {
        static public ListAsSet<SimAvatar> avatars = new ListAsSet<SimAvatar>();
        static public SimMovementStore store = new SimMovementStore();



        public ListAsSet<SimObject> objects = new ListAsSet<SimObject>();
        public BotClient Client;
        public readonly WorldObjects WorldSystem;
        public static BotRegionModel BotWorld = null;
        //        TheBotsInspector inspector = new TheBotsInspector();
        public BotRegionModel(BotClient Client)
        {
            BotWorld = this;
            this.Client = Client;
            WorldSystem = Client.WorldSystem;
            SimObjectType.LoadDefaultTypes();
            CatchUp(Client.Network.CurrentSim);
            Client.Avatars.OnAvatarAnimation += Avatars_OnAvatarAnimation;
            Client.Objects.OnObjectProperties += Objects_OnObjectProperties;
            ///  inspector.Show();
        }

        private void CatchUp(Simulator simulator)
        {
            simulator.ObjectsAvatars.ForEach(delegate(Avatar item)
            {
                GetSimAvatar(item);
            });
            simulator.ObjectsPrimitives.ForEach(delegate(Primitive item)
            {
                GetSimObject(item);
            });
        }

        void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties props)
        {
            Primitive prim = WorldSystem.GetPrimitive(props.ObjectID);
            if (prim != null)
            {
                SimObject updateMe = GetSimObject(prim);
                updateMe.UpdateProperties(props);
            }
        }

        void Avatars_OnAvatarAnimation(UUID avatarID, InternalDictionary<UUID, int> anims)
        {
            Avatar avatar = WorldSystem.GetAvatar(avatarID);
            if (avatar != null) GetSimAvatar(avatar);
        }

        public SimObject GetSimObject(Primitive prim)
        {
            if (prim is Avatar)
            {
                return GetSimAvatar((Avatar)prim);
            }
            lock (objects) foreach (SimObject obj in objects)
                {
                    if (obj.thePrim == prim)
                        return obj;
                }
            // not found
            SimObject obj0 = new SimObject(prim.ToString(), prim, WorldSystem);
            lock (objects) objects.AddTo(obj0);
            return obj0;
        }

        public SimAvatar GetSimAvatar(Avatar prim)
        {
            lock (avatars) foreach (SimAvatar obj in avatars)
                {
                    if (obj.theAvatar.Name == prim.Name)
                        return obj;
                }
            SimAvatar obj0 = new SimAvatar(prim, WorldSystem);
            obj0.SetClient(Client);
            lock (avatars) avatars.AddTo(obj0);
            //lock (objects) objects.AddTo(obj0);
            return obj0;
        }

        internal static void AddTracking(SimAvatar simAvatar, BotClient Client)
        {
          //  Client.Objects.OnObjectUpdated += Objects_OnObjectUpdated;
            //throw new Exception("The method or operation is not implemented.");
        }
    }
    public class SimMovementStore
    {
        List<SimMovement> Movements = new List<SimMovement>();
        public SimMovementStore()
        {
        }


        internal void Add(SimMovement LastMovement, SimMovement thisMove)
        {
            Movements.Add(thisMove);
        }
    }

    public class TrackedPrim
    {
        SimAvatar Target;
        Quaternion RotationLast;
        Vector3 PostionLast;
        float moveDist = 4;
        Vector3 RotationPostionLast;
        uint LocalID = 0;
        //SimMovementStore MovementStore;
        SimMovement LastMovement;
        public TrackedPrim(SimAvatar tracked)
        {
            Target = tracked;
            RotationPostionLast = tracked.GetSimPosition();
            LocalID = tracked.thePrim.LocalID;
            PostionLast = tracked.GetSimPosition();
            RotationLast = tracked.GetSimRotation();
        }

        public virtual void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            if (update.LocalID == LocalID)
            {
                if (update.Position != PostionLast)
                {
                    NewPosition(update.Position, moveDist);
                }
                if (RotationLast != update.Rotation)
                {
                    NewPosition(update.Position, 0.5f);
                    RotationLast = update.Rotation;
                    RotationPostionLast = update.Position;
                }
            }

        }

        private void NewPosition(Vector3 vector3, float moveDist)
        {
            if (Vector3.Distance(vector3, PostionLast) > moveDist)
            {
                SimMovement thisMove = new SimMovement(PostionLast, vector3);
               // MovementStore.Add(LastMovement, thisMove);
                LastMovement = thisMove;
                PostionLast = vector3;
            }
        }
    }

}
