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
