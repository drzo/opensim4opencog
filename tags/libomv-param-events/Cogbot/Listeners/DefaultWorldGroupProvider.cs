using System;
using System.Collections;
using System.Collections.Generic;
using cogbot.ScriptEngines;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Listeners
{
    public class DefaultWorldGroupProvider : ICollectionProvider
    {
        public DefaultWorldGroupProvider(WorldObjects objects, SimActor avatar)
        {
            AddObjectGroup("selected", avatar.GetSelectedObjects);
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
            AddObjectGroup("self", () => { var v = new List<SimObject> { avatar }; return v; });
            AddObjectGroup("all", () => WorldObjects.SimObjects.CopyOf());
            AddObjectGroup("known", avatar.GetKnownObjects);
        }

        public ICollection GetGroup(string arg0Lower)
        {
            Func<IList> func;
            if (ObjectGroups.TryGetValue(arg0Lower, out func))
            {

                return func();
            }
            return null;
        }

        readonly Dictionary<string, Func<IList>> ObjectGroups = new Dictionary<string, Func<IList>>();
        public void AddObjectGroup(string selecteditems, Func<IList> func)
        {
            lock (ObjectGroups)
            {
                ObjectGroups[selecteditems.ToLower()] = func;
            }
        }
    }
}