using System;
using System.Collections;
using System.Collections.Generic;
using Cogbot.ScriptEngines;
using Cogbot.World;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace Cogbot
{
    public class DefaultWorldGroupProvider : ICollectionProvider
    {
        readonly WorldObjects world;
        public DefaultWorldGroupProvider(WorldObjects objects)
        {
            world = objects;
            AddObjectGroup("selected", "set of currently selected objects", () =>
                                                                                {
                                                                                    SimActor avatar = this.avatar;
                                                                                    if (avatar == null) return null;
                                                                                    return avatar.GetSelectedObjects();
                                                                                });
            AddObjectGroup("none", "empty list", () => new List<SimObject>());
            AddObjectGroup("assets", "known assets", () => WorldObjects.SimRootObjects.CopyOf());
            AddObjectGroup("objects", "known sim root objects", () => WorldObjects.SimRootObjects.CopyOf());
            AddObjectGroup("caller", "current command caller", () =>
                                                                   {
                                                                       UUID callerID = BotClient.SessionToCallerId(
                                                                           objects.client.CurrentCommand.
                                                                               CurrentRequest.CallerAgent);
                                                                       if (callerID == UUID.Zero) return null;
                                                                       var o =
                                                                           WorldObjects.GetSimObjectFromUUID(callerID);
                                                                       if (o == null) return null;
                                                                       return SingleNameValue.AsCollection(o);
                                                                   });
            AddObjectGroup("prims", "all prims (attached and otherwise and avatars etc)",
                           () => WorldObjects.SimObjects.CopyOf());
            AddObjectGroup("childprims", "known childs rez in world not attachments",
                           () => WorldObjects.SimChildObjects.CopyOf());
            AddObjectGroup("attachments", "known attacments (everyones)",
                           () => WorldObjects.SimAttachmentObjects.CopyOf());
            // all known accounts
            AddObjectGroup("accounts", "known accounts", () => WorldObjects.SimAccounts.CopyOf());
            // all known avatars
            AddObjectGroup("avatars", "known avatars", () => WorldObjects.SimAvatars.CopyOf());
            // this bot's master(s)
            AddObjectGroup("master", "known masters", () =>
                                                          {
                                                              var v = new List<object>();
                                                              if (objects.client.MasterKey != UUID.Zero)
                                                              {
                                                                  v.Add(objects.CreateSimAvatar(
                                                                            objects.client.MasterKey, objects, null));

                                                              }
                                                              else
                                                              {
                                                                  v.Add(objects.client.MasterName);
                                                              }
                                                              return v;
                                                          });
            // the current bot
            AddObjectGroup("self", "the robot's avatar", () =>
                                                             {
                                                                 SimActor avatar = this.avatar;
                                                                 if (avatar == null) return null;
                                                                 var v = new List<SimObject> {avatar};
                                                                 return v;
                                                             });
            AddObjectGroup("nearest", "nearest root prim", () =>
                                                               {
                                                                   var sortme = WorldObjects.SimRootObjects.CopyOf();
                                                                   if (sortme.Count == 0) return null;
                                                                   sortme.Sort(this.avatar.CompareDistance);
                                                                   var v = new List<SimObject> {sortme[0]};
                                                                   return v;
                                                               });
            AddObjectGroup("regionprims",
                           "list of all av's, attachments, and objects in this region used in 'moveprim $regionprims <0,0,-1>'",
                           () =>
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
            AddObjectGroup("allprims", "list of all av's, attachments, and objects known to system",
                           () => WorldObjects.SimObjects.CopyOf());

            AddObjectGroup("selfknownprims",
                           "list of all objects that have an affordance or makes sense for the bot to know about",
                           () =>
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
            AddObjectGroup("lasteventprim", "the 'current object' - the current object is determined in a complex way",
                           () =>
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
                                   SimPosition p = null; // avatar.ApproachPosition;
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
            if (ae != null) foreach (CogbotEvent e in ae)
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

        public string NameSpace
        {
            get { return world.client.GetName(); }
        }

        public ICollection GetGroup(ICollectionRequester requester, string name)
        {
            return world.ResolveForExternal(name);
        }

        public IEnumerable<string> SettingNames(ICollectionRequester requester, int depth)
        {
            return GroupNames;
        }

        public void SetValue(ICollectionRequester requester, string name, object value)
        {
           // if (!ScriptManager.HasSetting(this, name)) return;
            //@TODO dmiles
        }

        public bool AcceptsNewKeys
        {
            get
            {
                return false;
            }
        }


        public IEnumerable<string> GroupNames
        {
            get { lock (ObjectGroups) return new List<string>(ObjectGroups.Keys); }
        }

        public void AddObjectGroup(string selecteditems, string desc, Func<IList> func)
        {
            lock (ObjectGroups)
            {
                ObjectGroups[selecteditems.TrimStart(' ', '$').ToLower()] = new SingleNameValue(selecteditems, func);
            }
        }

        protected Dictionary<string, IKeyValuePair<string, object>> ObjectGroups
        {
            get { return world.ObjectGroups; }
        }
    }
}