using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.Listeners;
using System.Collections;
using System.IO;

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
        public class SimPosition
        {
            public SimPosition(Vector3 firstP, Quaternion firtsR)
            {
                LastVector3 = RoundPoint(firstP);
                LastRotation = firtsR;
            }

            Vector3 LastVector3;
            Quaternion LastRotation;

            float MovedAllot = 4.0f;
            internal SimMovement Update(Vector3 point, Quaternion rotation)
            {
                point = RoundPoint(point);
                if (RotationDiffernt(rotation, LastRotation))
                {
                    return MakeMovement(point);
                }
                if (Vector3.Distance(LastVector3, point) > MovedAllot)
                {
                    return MakeMovement(point);
                }
                return null;
            }

            static Vector3 RoundPoint(Vector3 point)
            {
                Vector3 vect3 = new Vector3(point);
                vect3.X = (float)Math.Round(vect3.X, 1);
                vect3.Y = (float)Math.Round(vect3.Y, 1);
                vect3.Z = (float)Math.Round(vect3.Z, 0);
                return vect3;
            }

            private bool RotationDiffernt(Quaternion rotation, Quaternion LastRotation)
            {
                Quaternion diff = rotation - LastRotation;
                return (diff.Length() > 0.2);
            }

            private SimMovement MakeMovement(Vector3 point)
            {
                if (Vector3.Distance(LastVector3, point) > MovedAllot / 3)
                {
                    Console.WriteLine("WAYPOINT "+ LastVector3 +" -> " + point );
                    SimMovement move = new SimMovement(point, LastVector3);
                    LastVector3 = point;
                    return move;
                }
                return null;
            }

        }

        List<SimMovement> Movements = new List<SimMovement>();
        Dictionary<uint, SimPosition> LastPosition = new Dictionary<uint, SimPosition>();
        readonly string RegionFileName;



        public SimMovement GetBeginToEndMovement(Vector3 begin, Vector3 end, float maxDist)
        {
            SimMovement b = GetBeginMovement(begin, maxDist);
            SimMovement e = GetEndMovement(end, maxDist);
            if (b == null || e == null) return null;
            SimMovement m = new SimMovement(b.End, e.Begin);
            List<SimMovement> list = new List<SimMovement>();
            list.Add(b);
            list.Add(m);
            list.Add(e);
            return new SimMovementComplex(list);
        }

        public SimMovement GetBeginMovement(Vector3 vect, float maxDist)
        {
            SimMovement bestBegin = null;
            float distToBegin = maxDist;
            foreach (SimMovement move in Movements)
            {
                float close = Vector3.Distance(vect, move.Begin);
                if (close < distToBegin)
                {
                    bestBegin = move;
                    distToBegin = close;
                }
                else
                {
                    if (!move.IsOneDirrection)
                    {
                        close = Vector3.Distance(vect, move.End);
                        if (close < distToBegin)
                        {
                            bestBegin = move.Reverse();
                            distToBegin = close;
                        }
                    }
                }
            }
            return bestBegin;
        }

        public SimMovement GetEndMovement(Vector3 vect, float maxDist)
        {
            SimMovement bestEnd = null;
            float distToEnd = maxDist;
            foreach (SimMovement move in Movements)
            {
                float close = Vector3.Distance(vect, move.End);
                if (close < distToEnd)
                {
                    bestEnd = move;
                    distToEnd = close;
                }
                else
                {
                    if (!move.IsOneDirrection)
                    {
                        close = Vector3.Distance(vect, move.Begin);
                        if (close < distToEnd)
                        {
                            bestEnd = move.Reverse();
                            distToEnd = close;
                        }
                    }
                }
            }
            return bestEnd;
        }

        public SimMovementStore(string regionFileName)
        {
            RegionFileName = regionFileName;
            LoadFromFile();
        }

        void SaveToFile()
        {
            FileInfo save = new FileInfo(RegionFileName);
            if (save.Exists)
            {
                save.Delete();
            }
            FileStream stream = save.Open(FileMode.OpenOrCreate, FileAccess.Write);
            StreamWriter sw = new StreamWriter(stream);
            foreach(SimMovement sm in Movements) {
                sw.WriteLine(sm.ToFileString());
            }
            sw.Close();
        }

        void LoadFromFile()
        {
            FileInfo read = new FileInfo(RegionFileName);
            if (!read.Exists) return;
            FileStream stream = read.Open(FileMode.Open, FileAccess.Read);
            TextReader tr = new StreamReader(stream);
            while (tr.Peek() != -1)
            {
                string s = tr.ReadLine();
                LoadFromLine(s);
            }
            tr.Close();
        }

        private void LoadFromLine(string s)
        {
            SimMovement sm = new SimMovement(s);
            Movements.Add(sm);
        }

        public void Update(uint agentID, Vector3 point, Quaternion rotation)
        {
            return;
            if (!LastPosition.ContainsKey(agentID))
            {
                LastPosition[agentID] = new SimPosition(point, rotation);
            }
            else
            {
                SimMovement move = LastPosition[agentID].Update(point, rotation);
                if (move != null) Movements.Add(move);
            }
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
