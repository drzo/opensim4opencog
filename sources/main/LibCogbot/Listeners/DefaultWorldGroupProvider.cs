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
            // set of currently selected objects
            AddObjectGroup("selected", () =>
            {
                SimActor avatar = this.avatar;
                if (avatar == null) return null; 
                return avatar.GetSelectedObjects();
            });
            // all known accounts
            AddObjectGroup("accounts", () => WorldObjects.SimAvatars.CopyOf());
            // this bot's master(s)
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
            // the current bot
            AddObjectGroup("self", () =>
                                       {
                                           SimActor avatar = this.avatar;
                                           if (avatar == null) return null; 
                                           var v = new List<SimObject> { avatar }; return v;
                                       });
            // list of all av's, attachments, and objects in this region
            // used in "moveprim $regionprims <0,0,-1>
            AddObjectGroup("regionprims", () =>
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
            // list of all av's, attachments, and objects known to system
            AddObjectGroup("allprims", () => WorldObjects.SimObjects.CopyOf());
            // list of all objects that have an affordance or makes sense for the bot to know about
            AddObjectGroup("selfknownprims", () =>
                                        {
                                            SimActor avatar = this.avatar;
                                            if (avatar == null) return null; 
                                            return avatar.GetKnownObjects();
                                        });
            // the 'current object' - the current object is determined in a complex way
            // but is generally the last object acted upon by the av. it's only allowed to be changed every 30 sec
            // and might be overwritten by aiml. 
            // the intent is to support the notion of a pronoun 'it'
            // if the bot's head is free (not being animated) it will look at target
            AddObjectGroup("lasteventprim", () =>
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
                                             SimPosition p = null;// avatar.ApproachPosition;
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