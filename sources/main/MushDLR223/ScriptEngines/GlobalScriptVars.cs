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
        public static readonly Dictionary<object, Dictionary<string, ICollectionProvider>> _CodeRegistrars = new Dictionary<object, Dictionary<string, ICollectionProvider>>();
        public static readonly Dictionary<object, List<ICollectionProvider>> _CollectionProviders = new Dictionary<object, List<ICollectionProvider>>();

        /// <summary>
        /// Returns an enumeration of Settings that Others have Overridden
        /// </summary>
        /// <param name="requester"></param>
        /// <param name="provider"></param>
        /// <returns></returns>
        public static ICollection<string> AddGroupProvider(ICollectionRequester requester, ICollectionProvider provider)
        {
            var CodeRegistrars = FOCCodeRegistrars(requester);
            var CollectionProviders = FOCCollectionProviders(requester);
            lock (CollectionProviders)
            {
                if (!CollectionProviders.Contains(provider))
                {
                    CollectionProviders.Add(provider);
                    foreach(string sn in provider.SettingNames(requester, 1))
                    {
                        CodeRegistrars[ToKey(sn)] = provider; 
                    }
                }
            }
            return new List<string>();
        }

        private static Dictionary<string, ICollectionProvider> FOCCodeRegistrars(ICollectionRequester requester)
        {
            Dictionary<string, ICollectionProvider> providers;
            lock (_CodeRegistrars)
            {
                if (!_CodeRegistrars.TryGetValue(requester.RequesterID, out providers))
                {
                    return _CodeRegistrars[requester.RequesterID] = new Dictionary<string, ICollectionProvider>();
                }
                return providers;
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

        [ThreadStatic]
        static private Dictionary<string, Dictionary<string, int>> checkingFallbacksOfN = null;
        static private Dictionary<string, Dictionary<string, int>> frameDepthFallbacksOfN = null;
        static public bool LoopingOn(string name, string type)
        {
            int fc = (new System.Diagnostics.StackTrace(true)).FrameCount;
            if (GettingDeeper(name, type, fc))
            {
                if (LoopingOn0(name, type))
                {
                    return LoopingOn0(name, type + "1");
                }
            }
            return false;
        }
        static public bool GettingDeeper(string name, string type, int fc)
        {
            Dictionary<string, int> framebacksOf;

            if (frameDepthFallbacksOfN == null)
            {
                frameDepthFallbacksOfN = new Dictionary<string, Dictionary<string, int>>();
            }
            lock (frameDepthFallbacksOfN)
            {
                if (!frameDepthFallbacksOfN.TryGetValue(type, out framebacksOf))
                {
                    framebacksOf = frameDepthFallbacksOfN[type] = new Dictionary<string, int>();
                }
            }
            int fgen, fggen = fc;
            lock (framebacksOf)
            {
                if (!framebacksOf.TryGetValue(name, out fgen))
                {
                    framebacksOf[name] = fggen;
                }
                else if (fgen < fggen)
                {
                    framebacksOf.Remove(name);
                    return true;
                }
                else
                {
                    framebacksOf[name] = fggen;
                }
            }
            return false;
        }
        static public bool LoopingOn0(string name, string type)
        {
            Dictionary<string, int> fallbacksOf;
            if (checkingFallbacksOfN == null)
            {
                checkingFallbacksOfN = new Dictionary<string, Dictionary<string, int>>();
            }
            lock (checkingFallbacksOfN)
            {
                if (!checkingFallbacksOfN.TryGetValue(type, out fallbacksOf))
                {
                    fallbacksOf = checkingFallbacksOfN[type] = new Dictionary<string, int>();
                }
            }
            int gen, ggen = Generation;
            lock (fallbacksOf)
            {
                if (!fallbacksOf.TryGetValue(name, out gen))
                {
                    fallbacksOf[name] = ggen;
                }
                else if (gen == ggen)
                {
                    return true;
                }
                else
                {
                    fallbacksOf[name] = ggen;
                }
            }
            return false;
        }
        static public ICollection GetGroup(ICollectionRequester requester, string namespaec, string varname)
        {
            if (LoopingOn(namespaec + "." + varname, "GetGroup"))
            {
                return null;
            }
            var CodeRegistrars = FOCCodeRegistrars(requester);
            
            ICollectionProvider defaultProvider;
            if (varname.ToLower() == "currentaction")
            {

            }
            if (CodeRegistrars.TryGetValue(varname, out defaultProvider) && defaultProvider != null)
            {
                ICollection v = defaultProvider.GetGroup(requester, varname);
                return v;
            }

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
                HashSet<object> sp = null;
                if (requester.SessionMananger != null) sp = requester.SessionMananger.SkippedProviders;
                var all = new List<ICollectionProvider>();
                foreach (var nv in CollectionProviders)
                {
                    var nsp = nv.NameSpace;
                    if (!string.IsNullOrEmpty(nsp) && ToKey(nv.NameSpace) != namespaec0) continue;
                    if (sp == null || !sp.Contains(nv))
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
            if (LoopingOn(namespac + "." + name, "update"))
            {
                return false;
            }
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
                DateTime now = DateTime.Now;
                return now.Millisecond/100 + now.Second*100;
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
