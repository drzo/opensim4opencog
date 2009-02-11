using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.Listeners;
using System.Collections;
using System.IO;

namespace cogbot.TheOpenSims.Navigation
{
    public class SimPathStore
    {
        Dictionary<uint, PrimTracker> TrackedAgents = new Dictionary<uint, PrimTracker>();

        public void Update(uint agentID, Vector3 point, Quaternion rotation)
        {
            return;
            if (!TrackedAgents.ContainsKey(agentID))
            {
                TrackedAgents[agentID] = new PrimTracker(SimWaypoint.Create(point), rotation);
            }
            else
            {
                PrimTracker tracker = TrackedAgents[agentID];
                SimRoute move = tracker.Update(point, rotation);
                if (move != null) Movements.Add(move);
            }
        }



        List<SimRoute> Movements = new List<SimRoute>();
        readonly string RegionFileName;

        public SimRoute GetBeginToEndMovement(Vector3 begin, Vector3 end, float maxDist)
        {
            SimRoute b = GetBeginMovement(begin, maxDist);
            SimRoute e = GetEndMovement(end, maxDist);
            if (b == null || e == null) return null;
            SimRoute m = new SimRoute(b.End, e.Begin);
            List<SimRoute> list = new List<SimRoute>();
            list.Add(b);
            list.Add(m);
            list.Add(e);
            return new SimMovement(list);
        }

        public SimRoute GetBeginMovement(Vector3 vect, float maxDist)
        {
            SimRoute bestBegin = null;
            float distToBegin = maxDist;
            foreach (SimRoute move in Movements)
            {
                float close = Vector3.Distance(vect, move.Begin.GetSimPosition());
                if (close < distToBegin)
                {
                    bestBegin = move;
                    distToBegin = close;
                }
                else
                {
                    if (!move.IsOneDirrection)
                    {
                        close = Vector3.Distance(vect, move.End.GetSimPosition());
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

        public SimRoute GetEndMovement(Vector3 vect, float maxDist)
        {
            SimRoute bestEnd = null;
            float distToEnd = maxDist;
            foreach (SimRoute move in Movements)
            {
                float close = Vector3.Distance(vect, move.End.GetSimPosition());
                if (close < distToEnd)
                {
                    bestEnd = move;
                    distToEnd = close;
                }
                else
                {
                    if (!move.IsOneDirrection)
                    {
                        close = Vector3.Distance(vect, move.Begin.GetSimPosition());
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

        public SimPathStore(string regionFileName)
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
            foreach(SimRoute sm in Movements) {
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
            SimRoute sm = new SimRoute(s);
            Movements.Add(sm);
        }



    }

    public class PrimTracker
    {
        protected float MovedAllot = 2.0f;
        SimWaypoint WayPoint;
        Quaternion Orientation;
        public PrimTracker(SimPosition firstP, Quaternion firtsR)
        {
            WayPoint = SimWaypoint.Create(firstP.GetSimPosition());
            Orientation = firtsR;
        }

        public SimRoute Update(Vector3 point3, Quaternion rotation)
        {
            float dist = Vector3.Distance(WayPoint.GetSimPosition(), point3);
            if (dist > MovedAllot * 2)
            {
                WayPoint = SimWaypoint.Create(point3);
                return null;
            }
            if (dist > MovedAllot)
            {
                SimWaypoint point = SimWaypoint.Create(point3);
                return MakeMovement(point);
            }

            if (RotationDiffernt(rotation, Orientation))
            {
                SimWaypoint point = SimWaypoint.Create(point3);
                SimRoute move = MakeMovement(point);
                if (move != null)
                {
                    Orientation = rotation;
                }
                return move;
            }

            return null;
        }

        private SimRoute MakeMovement(SimWaypoint point)
        {
            if (Vector3.Distance(WayPoint.GetSimPosition(), point.GetSimPosition()) > MovedAllot / 3)
            {
                Console.WriteLine("WAYPOINT " + WayPoint + " -> " + point);
                SimRoute move = new SimRoute(point, WayPoint);
                WayPoint = point;
                return move;
            }
            return null;
        }

        static bool RotationDiffernt(Quaternion rotation, Quaternion Orientation)
        {
            Quaternion diff = rotation - Orientation;
            return (diff.Length() > 0.2);
        }
    }
}
