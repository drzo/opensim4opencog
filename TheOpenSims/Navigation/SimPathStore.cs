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
                SimMovement move = tracker.Update(point, rotation);
                if (move != null) Movements.Add(move);
            }
        }



        List<SimMovement> Movements = new List<SimMovement>();
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
            return new SimRoute(list);
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



    }

    public class PrimTracker
    {
        protected float MovedAllot = 2.0f;
        SimWaypoint WayPoint;
        Quaternion Orientation;
        public PrimTracker(SimWaypoint firstP, Quaternion firtsR)
        {
            WayPoint = firstP;
            Orientation = firtsR;
        }

        public SimMovement Update(Vector3 point3, Quaternion rotation)
        {
            float dist = Vector3.Distance(WayPoint, point3) ;
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
                SimMovement move = MakeMovement(point);
                if (move != null)
                {
                    Orientation = rotation;
                }
                return move;
            }

            return null;
        }

        private SimMovement MakeMovement(SimWaypoint point)
        {
            if (Vector3.Distance(WayPoint, point) > MovedAllot / 3)
            {
                Console.WriteLine("WAYPOINT " + WayPoint + " -> " + point);
                SimMovement move = new SimMovement(point, WayPoint);
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
