using System;
using System.Collections;
using System.Collections.Generic;
using cogbot.ScriptEngines;
using cogbot.TheOpenSims;
using MushDLR223.ScriptEngines;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.Listeners
{
    public class DefaultWorldGroupProvider : ICollectionProvider
    {
        readonly WorldObjects world;
        public DefaultWorldGroupProvider(WorldObjects objects)
        {
            world = objects;
            AddObjectGroup("selected", () =>
            {
                SimActor avatar = this.avatar;
                if (avatar == null) return null; 
                return avatar.GetSelectedObjects();
            });
            AddObjectGroup("avatars", () => WorldObjects.SimAvatars.CopyOf());
            AddObjectGroup("master", () =>
                                         {
                                             var v = new List<object>();
                                             if (objects.client.MasterKey != UUID.Zero)
                                             {
                                                 v.Add(objects.CreateSimAvatar(objects.client.MasterKey, objects, null));

                                             }
                                             else
                                             {
                                                 v.Add(objects.client.MasterName);
                                             }
                                             return v;
                                         });
            AddObjectGroup("self", () =>
                                       {
                                           SimActor avatar = this.avatar;
                                           if (avatar == null) return null; 
                                           var v = new List<SimObject> { avatar }; return v;
                                       });
            AddObjectGroup("region", () =>
                                         {
                                             List<SimObject> here = new List<SimObject>();
                                             SimActor avatar = this.avatar;
                                             if (avatar == null) return null;
                                             ulong areg = avatar.RegionHandle;
                                             foreach (SimObject o in WorldObjects.SimObjects.CopyOf())
                                             {
                                                 if (o.RegionHandle == areg) here.Add(o);
                                             }
                                             return here;
                                         });
            AddObjectGroup("all", () => WorldObjects.SimObjects.CopyOf());
            AddObjectGroup("known", () =>
                                        {
                                            SimActor avatar = this.avatar;
                                            if (avatar == null) return null; 
                                            return avatar.GetKnownObjects();
                                        });
            AddObjectGroup("target", () =>
                                         {
                                             SimActor avatar = this.avatar;
                                             if (avatar == null) return null;
                                             var v = new List<SimPosition>();
                                             var a = avatar.CurrentAction;
                                             if (a != null && a.Target != null)
                                             {
                                                 v.Add(a.Target);
                                                 return v;
                                             }
                                             SimPosition p = avatar.ApproachPosition;
                                             if (p != null)
                                             {
                                                 v.Add(p);
                                                 return v;
                                             }
                                             var r = GetTargetInEvent(v, avatar);
                                             if (r != null) return r;
                                             if (objects.client.MasterKey != UUID.Zero)
                                             {
                                                 var master = WorldObjects.GetSimObjectFromUUID(objects.client.MasterKey);
                                                 if (master != null)
                                                 {
                                                     return GetTargetInEvent(v, master);
                                                 }
                                             }
                                             return null;
                                         });
        }

        private static IList GetTargetInEvent(List<SimPosition> v, SimObject master)
        {
            var ae = ((SimObject)master).ActionEventQueue;
            if (ae != null) foreach (SimObjectEvent e in ae)
            {
                var t = e["target"] as SimPosition;
                if (t != null)
                {
                    v.Add(t);
                    return v;
                }
            }
            return null;
        }

        protected SimActor avatar
        {
            get { return world.TheSimAvatar; }
        }

        public ICollection GetGroup(string arg0Lower)
        {
            Func<IList> func;
            if (ObjectGroups.TryGetValue(arg0Lower, out func))
            {
                if (func == null) return null;
                return func();
            }
            return null;
        }

        readonly Dictionary<string, Func<IList>> ObjectGroups = new Dictionary<string, Func<IList>>();
        public IEnumerable<string> GroupNames
        {
            get { lock (ObjectGroups) return new List<string>(ObjectGroups.Keys); }
        }

        public void AddObjectGroup(string selecteditems, Func<IList> func)
        {
            lock (ObjectGroups)
            {
                ObjectGroups[selecteditems.ToLower()] = func;
            }
        }
    }
}