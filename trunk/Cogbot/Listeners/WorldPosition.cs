using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.Listeners
{
    partial class WorldObjects
    {

        private static WorldPathSystem _SimPaths;
        public Vector3 compPos;


        public WorldPathSystem SimPaths
        {
            get { return _SimPaths; }
        }

        internal SimObject GetSimObjectFromVector(Vector3d here, out double dist)
        {
            SimObject retObj = null;
            dist = double.MaxValue;
            if (here == Vector3d.Zero) return retObj;
            foreach (SimObject obj in GetAllSimObjects())
            {
                if (obj.IsRegionAttached())
                {
                    Vector3d at = obj.GetWorldPosition();
                    if (at == here)
                    {
                        dist = 0;
                        return obj;
                    }
                    double ld = Vector3d.Distance(at, here);
                    if (ld < dist)
                    {
                        retObj = obj;
                        dist = ld;
                    }
                }
            }
            return retObj;
        }

        private object AsLocation(Simulator simulator, Vector3 position, SimPosition pos)
        {
            SimRegion r = SimRegion.GetRegion(simulator);
            if (position == Vector3.Zero)
            {
                if (r == null)
                {
                    if (pos != null && pos.IsRegionAttached()) return new SimHeading(pos); 
                    return SimHeading.UNKNOWN;
                }
                return r;
            }
            Quaternion rot = Quaternion.Identity;
            if (pos!=null && pos.IsRegionAttached())
            {
                rot = pos.GetSimRotation();
            }
            return new SimHeading(r.GetPathStore(position), position, rot);
        }

        private object AsRLocation(Simulator simulator, Vector3d targetPos, SimPosition pos)
        {
            Vector3 lpos = new Vector3((float)targetPos.X, (float)targetPos.Y, (float)targetPos.Z);
            SimRegion r = SimRegion.GetRegion(simulator);
            if (targetPos == Vector3d.Zero)
            {
                if (r == null)
                {
                    if (pos != null && pos.IsRegionAttached()) return new SimHeading(pos);
                    return SimHeading.UNKNOWN;
                }
                return r;
            }
            return new SimHeading(pos, lpos);
        }

        private object AsLocation(UUID reg, Vector3 position)
        {
            SimRegion r = GetRegion(reg);
            if (position == Vector3.Zero)
            {
                if (r == null)
                {
                    if (reg == UUID.Zero)
                    {
                        return SimHeading.UNKNOWN;
                    }
                    return SimHeading.UNKNOWN;
                    // return r;
                }
                return r;
            }
            return new SimHeading(r.GetPathStore(position), position, Quaternion.Identity);
        }

        internal SimRegion GetRegion(ulong RegionHandle)
        {
            if (RegionHandle == 0) return null;
            return SimRegion.GetRegion(RegionHandle,client);
        }

        public SimRegion GetRegion(UUID uuid)
        {
            return SimRegion.GetRegion(uuid, client);
        }

        private bool tryGetBuildingPos(List<Primitive> group, out Vector3 centroid)
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
                if (size.X > buildingSize && size.Y > buildingSize && size.Z > buildingSize)
                {
                    centroid = min + (size * (float)0.5);
                    return true;
                }
                else
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
        public int comp(Avatar a1, Avatar a2)
        {
            return (int)(Vector3.Distance(a1.Position, compPos) - Vector3.Distance(a2.Position, compPos));
        }

        public List<Avatar> getAvatarsNear(Vector3 pos, int num)
        {
            compPos = pos;
            List<Avatar> avatarList = new List<Avatar>();
            foreach (SimAvatar simavatar in SimAvatars)
            {
                Avatar avatar = simavatar.theAvatar;
                if (avatar.Name != client.Self.Name)
                    avatarList.Add(avatar);
            }

            if (avatarList.Count > num)
            {
                avatarList.Sort(new Comparison<Avatar>(comp));

                for (; searchStep * num > avatarList.Count; --searchStep) ;

                List<Avatar> ret = new List<Avatar>();
                for (int i = 0; i < num && i < avatarList.Count; i += searchStep)
                    ret.Add(avatarList[i]);
                searchStep = (searchStep + 1) % 4 + 1;
                updateNumberedAvatars(ret);
                return ret;
            }
            else
            {
                updateNumberedAvatars(avatarList);
                return avatarList;
            }
        }
        public SimPosition GetVector(string[] args, out int argsUsed)
        {
            argsUsed = 0;
            if (args.Length == 0) return TheSimAvatar;
            if (args.Length >= 2)
            {
                Vector3 target;
                if (float.TryParse(args[0], out target.X) &&
                    float.TryParse(args[1], out target.Y))
                {
                    argsUsed = 2;
                    target.Z = TheSimAvatar.GetSimPosition().Z;
                    if (args.Length == 3)
                    {
                        Single.TryParse(args[2], out target.Z);
                        argsUsed = 3;
                    }
                    return SimWaypointImpl.CreateLocal(target, TheSimAvatar.GetPathStore());
                }
            }
            SimObject O = null;
            UUID uuid;
            if (UUID.TryParse(args[0],out uuid))
            {
                O = GetSimObjectFromUUID(uuid);
                if (O != null)
                    return O;
            }
            Primitive prim = GetPrimitive(args, out argsUsed);
            if (prim != null) return GetSimObject(prim);


            argsUsed = 0;
            string destination = String.Empty;

            // Handle multi-word sim names by combining the arguments
            foreach (string arg in args)
            {
                destination += arg + " ";
            }
            int consume = args.Length;
            destination = destination.Trim();

            string[] tokens = destination.Split(new char[] { '/' });
            float x, y = 128;
            if (!float.TryParse(tokens[0], out x))
            {
                x = 128;
            } else
            {
                argsUsed++;
            }
            string sim = tokens[argsUsed];
            SimRegion region = SimRegion.GetRegion(sim);
            if (region==null) return null;
            argsUsed += 1;
            float z = region.AverageHieght;
            if (tokens.Length > argsUsed+1)
            {
                if (!float.TryParse(tokens[argsUsed], out x) ||
                    !float.TryParse(tokens[argsUsed+1], out y))
                {
                    return null;
                }
                argsUsed += 2;
                if (tokens.Length > argsUsed)
                {
                    if (float.TryParse(tokens[argsUsed], out z))
                    {
                        argsUsed += 1;
                    }
                }
            }
            Vector3 v3 = new Vector3(x, y, z);
            return SimWaypointImpl.CreateLocal(v3, region.GetPathStore(v3));
        }

        public List<SimObject> GetNearByObjects(Vector3d here, object except, float maxDistance, bool rootOnly)
        {
            if (here.X < 256f)
            {
                throw new ArgumentException("GetNearByObjects is not using GetWorldPostion?");
            }
            List<SimObject> nearby = new List<SimObject>();
            foreach (SimObject obj in GetAllSimObjects())
            {
                if (obj != except)
                {
                    if (rootOnly && !obj.IsRoot) continue;
                    if (obj.IsRegionAttached() && Vector3d.Distance(obj.GetWorldPosition(), here) <= maxDistance)
                        nearby.Add(obj);
                }
            }
            ;
            return nearby;
        }

        private float distanceHeuristic(SimObject prim)
        {
            if (prim != null)
                return (float)(1.0 / Math.Exp((double)TheSimAvatar.Distance(prim))
                               );
            else
                return (float)0.01;
        }

    }
}
