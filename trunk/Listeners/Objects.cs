using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Listeners
{
    public delegate float ObjectHeuristic(Primitive prim);

    class Objects : Listener
    {
        public int burstSize = 100;
        public float burstTime = 1;
        private DateTime burstStartTime;
        private TimeSpan burstInterval;
        public float buildingSize = 5;
        public List<Primitive> newPrims;
        public Dictionary<string,Primitive> prims;
        private Dictionary<UUID, Primitive> pendingPrims;
        public List<ObjectHeuristic> objectHeuristics;
        public Dictionary<UUID, List<Primitive>> primGroups;
        private Object newLock = new Object();
        private int maxShortNameLength = 0;
        private int maxNameLength;
        public Dictionary<string, string> shortNames;
        public Dictionary<string, string> reverseShortNames;
        public List<string> numberedObjects;

        public Objects(TextForm parent)
            : base(parent)
        {
            newPrims = new List<Primitive>();
            prims = new Dictionary<string, Primitive>();
            pendingPrims = new Dictionary<UUID, Primitive>();
            primGroups = new Dictionary<UUID, List<Primitive>>();
            shortNames = new Dictionary<string, string>();
            reverseShortNames = new Dictionary<string, string>();
            numberedObjects = new List<string>();

            objectHeuristics = new List<ObjectHeuristic>();
            objectHeuristics.Add(new ObjectHeuristic(distanceHeuristic));
            objectHeuristics.Add(new ObjectHeuristic(nameLengthHeuristic));
            objectHeuristics.Add(new ObjectHeuristic(boringNamesHeuristic));

            maxNameLength = -1;

            client.Objects.OnNewPrim += new ObjectManager.NewPrimCallback(Objects_OnNewPrim);
            client.Objects.OnObjectProperties += new ObjectManager.ObjectPropertiesCallback(Objects_OnObjectProperties);

            burstStartTime = DateTime.Now;
            burstInterval = new TimeSpan(0, 0, 0, 0, (int)(burstTime * 1000));
        }

        void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties properties)
        {
            lock (pendingPrims)
            {
                if (pendingPrims.ContainsKey(properties.ObjectID))
                {
                    lock (prims)
                    {
                        prims[properties.Name] = pendingPrims[properties.ObjectID];
                        describePrimToAI(pendingPrims[properties.ObjectID]);
                        pendingPrims.Remove(properties.ObjectID);

                        UUID groupId = properties.GroupID;
                        if (groupId != UUID.Zero)
                        {
                            lock (primGroups)
                            {
                                if (!primGroups.ContainsKey(groupId))
                                    primGroups[groupId] = new List<Primitive>();
                                primGroups[groupId].Add(prims[properties.Name]);
                                //parent.output("group count " + groupId + " " + primGroups[groupId].Count);
                            }
                        }
                    }
                    if (maxNameLength == -1 || properties.Name.Length > maxNameLength)
                        maxNameLength = properties.Name.Length;
                }
            }
        }

        void Objects_OnNewPrim(Simulator simulator, Primitive prim, ulong regionHandle, ushort timeDilation)
        {
            try
            {
                lock (newLock)
                {
                    newPrims.Add(prim);
                    CalcStats(prim);

                    TimeSpan dtime = DateTime.Now - burstStartTime;
                    if (newPrims.Count >= burstSize && dtime > burstInterval)
                    {
                        burstStartTime = DateTime.Now;

                        uint[] ids = new uint[burstSize];
                        for (int i = 0; i < burstSize; ++i)
                        {
                            ids[i] = newPrims[i].LocalID;
                            pendingPrims[newPrims[i].ID] = newPrims[i];
                        }

                        client.Objects.SelectObjects(simulator, ids);
                        newPrims.RemoveRange(0, burstSize);
                        //newPrims.Clear();
                    }
                }
                describePrimToAI(prim);
            }
            catch (Exception e)
            {
                parent.output("ERR:" + e.StackTrace);
            }
        }
        public void CalcStats(Primitive prim)
        {
            if (boringNamesHeuristic(prim) == 0)
                parent.BoringNamesCount++;                
            else
                parent.GoodNamesCount++;
        }

        public bool tryGetPrim(string name, out Primitive prim)
        {
            prim = null;

            string[] toks = name.Split(null);
            if (toks.Length == 2 && toks[0] == "object")
            {
                int i = Convert.ToInt32(toks[1]);
                if (i > 0 && i <= numberedObjects.Count)
                {
                    prim = prims[numberedObjects[i - 1]];
                    return true;
                }
            }

            if (shortNames.ContainsKey(name))
            {
                prim = prims[shortNames[name]];
                return true;
            }

            foreach (string primName in prims.Keys)
            {
                if (primName.Length >= name.Length && primName.Substring(0, name.Length) == name)
                {
                    prim = prims[primName];
                    return true;
                }
            }
            return false;
        }

        public void describePrim(Primitive prim)
        {
            parent.output(prim.Properties.Name + ": " + prim.Properties.Description);
            if (prim.Sound != UUID.Zero)
                parent.output("This object makes sound.");
            if (prim.Properties.SalePrice != 0)
                parent.output("This object is for sale for L" + prim.Properties.SalePrice);
        }

        public void describePrimToAI(Primitive prim)
        {
            if (prim != null && prim.Properties != null)
            {
                if (prim.Properties.Name != null)
                {
                    //parent.enqueueLispTask("(on-prim-description '(" + prim.Properties.Name + ") '" + prim.Properties.Description + "' )");
                    parent.enqueueLispTask("(on-prim-dist (@\"" + prim.Properties.Name + "\") (@\"" + prim.Properties.ObjectID.ToString() + "\") " + Vector3.Distance(client.Self.SimPosition, prim.Position).ToString() + " )");
                    parent.enqueueLispTask("(on-prim-pos (@\"" + prim.Properties.Name + "\") (@\"" + prim.Properties.ObjectID.ToString() + "\") (@\"" + prim.Position.ToString() + "\") )");
                    parent.enqueueLispTask("(on-prim-description  (@\"" + prim.Properties.Name + "\") (@\"" + prim.Properties.ObjectID.ToString() + "\") (@\"" + prim.Properties.Description + "\") )");

                    //parent.output(prim.Properties.Name + ": " + prim.Properties.Description);
                    //if (prim.Sound != UUID.Zero)
                    //    parent.output("This object makes sound.");
                    //if (prim.Properties.SalePrice != 0)
                    //    parent.output("This object is for sale for L" + prim.Properties.SalePrice);
                }
            }
        }

        public int comp(Primitive p1, Primitive p2)
        {
            return (int)(getFitness(p1) - getFitness(p2));
        }

        public List<Primitive> getPrimitives(int num)
        {
            List<Primitive> ret = new List<Primitive>();
            foreach (Primitive prim in prims.Values)
            {
                ret.Add(prim);
            }

            if (ret.Count <= num)
            {
                updateNumberedObjects(ret,num);
                return ret;
            }
            else
            {
                ret.Sort(new Comparison<Primitive>(comp));
                updateNumberedObjects(ret,num);
                return ret.GetRange(0, num);
            }
        }

        void updateNumberedObjects(List<Primitive> ret, int num)
        {
            numberedObjects.Clear();
            for (int i = 0; i < num && i < ret.Count; ++i)
            {
                numberedObjects.Add(ret[i].Properties.Name);
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
                return (float)(1.0 / Math.Exp((double)Vector3.Distance(client.Self.RelativePosition, prim.Position)));
            else
                return (float)0.01;
        }

        float nameLengthHeuristic(Primitive prim)
        {
            if ((prim != null) && (prim.Properties.Name != null))
            {
                return (float)prim.Properties.Name.Length / (float)maxNameLength;
            }
            else
                return (float)0.1;
        }

        float boringNamesHeuristic(Primitive prim)
        {
            if (prim == null || prim.Properties == null) return (float)0.0;
            string name = prim.Properties.Name;
            if (name == "Object" || name == "Component" || name == null)
                return (float)0.1;
            else
                return 1;
        }

        bool tryGetBuildingPos(List<Primitive> group, out Vector3 centroid)
        {
            centroid = new Vector3();
            if (group.Count < 4)
                return false;
            else
            {
                bool first = true;
                Vector3 min = new Vector3(), max = new Vector3(), pos;
                foreach (Primitive prim in group)
                {
                    if (prim != null && prim.Position != null)
                    {
                        pos = prim.Position;

                        if (first)
                        {
                            min = pos;
                            max = pos;
                            first = false;
                        }
                        else
                        {
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
            return (int)(Vector3.Mag(client.Self.RelativePosition - v1) - 
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
            else
            {
                ret.Sort(new Comparison<Vector3>(posComp));
                return ret.GetRange(0, num);
            }
        }

        public string getObjectName(Primitive prim)
        {
            string name = getObjectShortName(prim);
            for (int i = 0; i < numberedObjects.Count; ++i)
                if (numberedObjects[i] == prim.Properties.Name)
                    name = (i + 1) + ": " + name;
            return name;
        }

        public string getObjectShortName(Primitive prim)
        {
            if (prim.Properties.Name.Length < maxShortNameLength)
            {
                return prim.Properties.Name;
            }
            else
            {
                if (reverseShortNames.ContainsKey(prim.Properties.Name))
                    return reverseShortNames[prim.Properties.Name];
                else
                {
                    string shortName = "";
                    int i = 0;
                    foreach (string token in prim.Properties.Name.Split(null))
                    {
                        if (i != 0) shortName += " ";
                        i++;
                        shortName += token;
                        if (!shortNames.ContainsKey(shortName))
                        {
                            shortNames[shortName] = prim.Properties.Name;
                            reverseShortNames[prim.Properties.Name] = shortName;
                            return shortName;
                        }
                    }
                    return prim.Properties.Name;
                }
            }
        }
    }
}
