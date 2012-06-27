using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Threading;
using MushDLR223.Utilities;
using Exception=System.Exception;
using Thread = System.Threading.Thread;
using DotLisp;
namespace MushDLR223.ScriptEngines
{
    public partial class ScriptManager
    {
        public static readonly Dictionary<object, List<ICollectionProvider>> _CollectionProviders = new Dictionary<object, List<ICollectionProvider>>();


        public static void AddGroupProvider(ICollectionRequester requester, ICollectionProvider provider)
        {
            var CollectionProviders = FOCCollectionProviders(requester);
            lock (CollectionProviders)
            {
                if (!CollectionProviders.Contains(provider)) CollectionProviders.Add(provider);
            }
        }
        private static List<ICollectionProvider> FOCCollectionProviders(ICollectionRequester requester)
        {
            List<ICollectionProvider> providers;
            lock (_CollectionProviders)
            {
                if (!_CollectionProviders.TryGetValue(requester.RequesterID, out providers))
                {
                    return _CollectionProviders[requester.RequesterID] = new List<ICollectionProvider>();
                }
                return providers;
            }
        }
        private static IEnumerable<ICollectionProvider> GetCollectionProviders(ICollectionRequester requester)
        {
            requester.SessionMananger = new RequesterSession(requester);
            return LockInfo.CopyOf(FOCCollectionProviders(requester));
        }

        static public ICollection GetGroup(ICollectionRequester requester, string namespaec, string varname)
        {
            List<object> rv = new List<object>();
            ICollection c = null;
            int fc = 0;
            var CollectionProviders = GetCollectionProviders(requester);
            lock (CollectionProviders)
            {
                foreach (var nv in GetProviders(requester, namespaec))
                {
                    ICollection v = nv.GetGroup(requester, varname);
                    if (v == null) continue;
                    fc++;
                    if (fc == 2)
                    {
                        foreach (var e in c)
                        {
                            if (!rv.Contains(e)) rv.Add(e);
                        }
                        foreach (var e in v)
                        {
                            if (!rv.Contains(e)) rv.Add(e);
                        }
                    }
                    else if (fc > 2)
                    {
                        foreach (var e in v)
                        {
                            if (!rv.Contains(e)) rv.Add(e);
                        }
                    }
                    c = v;
                }
            }
            if (fc == 0) return null;
            if (fc == 1) return c;
            return rv;
        }

        static public IEnumerable<ICollectionProvider> GetProviders(ICollectionRequester requester, string namespaec)
        {
            var namespaec0 = ToKey(namespaec);
            var CollectionProviders = GetCollectionProviders(requester);

            lock (CollectionProviders)
            {
                var sp = requester.SessionMananger.SkippedProviders;
                var all = new List<ICollectionProvider>();
                foreach (var nv in CollectionProviders)
                {
                    var nsp = nv.NameSpace;
                    if (!string.IsNullOrEmpty(nsp) && ToKey(nv.NameSpace) != namespaec0) continue;
                    if (!sp.Contains(nv))
                    {
                        all.Add(nv);
                        //   sp.Add(nv);
                    }
                }
                return all;
            }
        }

        public static string ToKey(string namespaec)
        {
            return Parser.ToKey(namespaec);
        }

        static public IEnumerable<string> GetNameSpaces(ICollectionRequester requester)
        {
            var CollectionProviders = GetCollectionProviders(requester);
            lock (CollectionProviders)
            {
                var all = new HashSet<string>();
                foreach (var nv in CollectionProviders)
                {
                    all.Add(ToKey(nv.NameSpace));
                }
                return all;
            }
        }
        static public IEnumerable<string> SettingNames(ICollectionRequester requester, int depth)
        {
            var CollectionProviders = GetCollectionProviders(requester);
            lock (CollectionProviders)
            {
                var all = new HashSet<string>();
                foreach (var nv in CollectionProviders)
                {
                    string ns = ToKey(nv.NameSpace);
                    IEnumerable<string> cvol = nv.SettingNames(requester, depth);
                    if (cvol != null)
                    {
                        foreach (var c in cvol)
                        {
                            all.Add(ns + "." + ToKey(c));
                        }
                    }
                }
                return all;
            }
        }

        public static bool HasSetting(ICollectionRequester requester, ICollectionProvider provider, string name)
        {
            name = ToKey(name);
            foreach (var settingName in provider.SettingNames(requester, 1))
            {
                if (ToKey(settingName) == name) return true;
            }
            return false;
        }
        static public void AquireSettingsLock()
        {
            Monitor.Enter(_CollectionProviders);
        }
        static public void ReleaseSettingsLock()
        {
            Monitor.Exit(_CollectionProviders);
        }
        public static bool SendSettingsChange(ICollectionRequester requester, string namespac, string name, object valeu)
        {
            foreach (ICollectionProvider provider in GetProviders(requester, namespac))
            {
                if (ReferenceEquals(provider, requester)) continue;
                provider.SetValue(requester, name, valeu);
                lock (VarListeners)
                {
                    foreach (var action in VarListeners)
                    {
                        action(requester, namespac, name, valeu);
                    }
                }
            }
            return true;
        }

        public static bool AddSetting(ICollectionRequester requester, string namespac, string name, object valeu)
        {
            bool somethngTookIt = false;
            foreach (ICollectionProvider provider in GetProviders(requester, namespac))
            {
                if (!HasSetting(requester, provider, name))
                {
                    if (!provider.AcceptsNewKeys) continue;
                }
                try
                {
                    provider.SetValue(requester, name, valeu);
                    somethngTookIt = true;

                }
                catch (Exception e)
                {
                    DLRConsole.DebugWriteLine("AddSetting " + e);
                }
            }
            return somethngTookIt;
        }

        public static HashSet<Action<object, string, string, object>> VarListeners =
            new HashSet<Action<object, string, string, object>>();

        public static int Generation
        {
            get
            {
                return DateTime.Now.Millisecond/100;
            }
        }

        public static void RegisterVarListener(Action<object, string, string, object> action)
        {
            lock (VarListeners)
            {
                VarListeners.Add(action);
            }
        }
        public static void UnregisterVarListener(Action<object, string, string, object> action)
        {
            lock (VarListeners)
            {
                VarListeners.Remove(action);
            }
        }
    }
}
