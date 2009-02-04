using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.Listeners;
using System.Collections;

namespace cogbot.TheOpenSims
{
    public class ListAsSet<T> : List<T>
    {
        // synchronization
        public bool Remove(T item)
        {
            lock (this)
                return base.Remove(item);
        }

        // synchronization
        public void ForEach(Action<T> act)
        {
            foreach (T item in CopyOf())
            {
                act(item);
            }
        }

        // synchronization
        public T Find(Predicate<T> act)
        {
            foreach (T item in CopyOf())
            {
                if (act(item)) return item;
            }
            return default(T);
        }

        public bool AddTo(T item)
        {
            lock (this)
            {
                if (true)
                {
                    {
                        IEnumerator enumer = base.GetEnumerator();
                        while (enumer.MoveNext())
                        {
                            if (item.Equals( (T)enumer.Current)) return false;
                        }
                    }
                }
                else
                {
                    if (base.Contains(item)) return false;
                }
                base.Add(item);
                return true;
            }
        }

        // return a copy
        public Enumerator GetEnumerator()
        {
            return CopyOf().GetEnumerator();
        }

        public class BaseEnumerable : IEnumerable<T> 
        {
            readonly IEnumerator<T> be;
            public BaseEnumerable(IEnumerator<T> r)
            {
                be = r;
            }


            #region IEnumerable<T> Members

            IEnumerator<T> IEnumerable<T>.GetEnumerator()
            {
                return be;
            }

            #endregion

            #region IEnumerable Members

            IEnumerator IEnumerable.GetEnumerator()
            {
                return be;
            }

            #endregion
        }
        // return the fast underlying
        public IEnumerable<T> GetBaseEnumerable()
        {
            return new BaseEnumerable(base.GetEnumerator());
        }
        // synchronization
        public List<T> CopyOf()
        {
            List<T> list = new List<T>();
            lock (this)
            {
                IEnumerator enumer = base.GetEnumerator();
                while (enumer.MoveNext())
                {
                    list.Add((T)enumer.Current);
                }
            }
            return list;
        }

        public new void Add(T item)
        {
            AddTo(item);
        }
        public override string ToString()
        {
            List<T> copy = CopyOf();
            switch (copy.Count)
            {
                case 0: return "[]";
                case 1: return "[" + copy[0] + "]";
                default:
                    {
                        String s = "";
                        foreach (T t in copy)
                        {
                            s += "," + t;
                        }
                        return "["+s.Substring(1) + "]";
                    }
            }
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
