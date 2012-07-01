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

        [ThreadStatic]
        static private Dictionary<string, Dictionary<string, int>> checkingFallbacksOfN = null;
        static public bool LoopingOn(string name, string type)
        {
            if (LoopingOn0(name, type))
            {
                return LoopingOn0(name, type + "1");
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
                else if (gen == Generation)
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

        public static SysVarsDict SysVarsAsDict()
        {
            return new SysVarsDict(SysVars, BackingDict);
        }
        public readonly static List<IKeyValuePair<string,object>> SysVars = new List<IKeyValuePair<string, object>>();
        private static IDictionary<string, object> BackingDict = new Dictionary<string, object>();

        public static void LoadSysVars(Type t)
        {
            lock (SysVars)
            {
                foreach (
                    var s in
                        t.GetMembers(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance |
                                     BindingFlags.Static))
                {
                    ConfigSettingAttribute cs0;
                    if (!s.IsDefined(typeof(ConfigSettingAttribute), false))
                    {
                        if (!ConfigSettingAttribute.IsGoodForConfig(s)) continue;
                        cs0 = ConfigSettingAttribute.CreateSetting(s);
                    }
                    else
                    {
                        var cs = s.GetCustomAttributes(typeof(ConfigSettingAttribute), false);
                        if (cs == null) continue;
                        if (cs.Length == 0) continue;
                        cs0 = (ConfigSettingAttribute)cs[0];
                        cs0.SetMember(s);
                    }
                    if (!SysVars.Contains(cs0)) SysVars.Add(cs0);
                    //  WriteLine("Setting: " + cs0.Description);
                }
                var st = t.BaseType;
                if (st != null && st != typeof(object))
                {
                    LoadSysVars(st);
                }
            }
        }
    }

    public class SysVarsDict : IDictionary<string, object>
    {
        private List<IKeyValuePair<string, object>> Impl;
        private IDictionary<string, object> Backup;

        public SysVarsDict(List<IKeyValuePair<string, object>> pairs, IDictionary<string,object> backup)
        {
            Impl = pairs;
            this.Backup = backup;
        }

        /// <summary>
        /// Adds an item to the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <param name="item">The object to add to the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        ///                 </exception>
        public void Add(KeyValuePair<string, string> item)
        {
            string key = item.Key;
            var kv = findKVP(key);
            if (kv == null) kv = makeKV(key);
            kv.Value = item.Value;
        }

        private IKeyValuePair<string, object> makeKV(string key)
        {
            return new KVBacked(key, Backup);
        }

        private IKeyValuePair<string, object> findKVP(string key)
        {
            key = Parser.ToKey(key);
            lock (Impl)
            {
                foreach (IKeyValuePair<string, object> list in Impl)
                {
                    if (Parser.ToKey(list.Key) == key) return list;
                }
            }
            return null;
        }

        /// <summary>
        /// Adds an item to the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <param name="item">The object to add to the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        ///                 </exception>
        public void Add(KeyValuePair<string, object> item)
        {
            string key = item.Key;
            var kv = findKVP(key);
            kv.Value = item.Value;
        }

        /// <summary>
        /// Removes all items from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only. 
        ///                 </exception>
        public void Clear()
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Determines whether the <see cref="T:System.Collections.Generic.ICollection`1"/> contains a specific value.
        /// </summary>
        /// <returns>
        /// true if <paramref name="item"/> is found in the <see cref="T:System.Collections.Generic.ICollection`1"/>; otherwise, false.
        /// </returns>
        /// <param name="item">The object to locate in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param>
        public bool Contains(KeyValuePair<string, object> item)
        {
            string key = item.Key;
            var kv = findKVP(key);
            return kv != null && kv.Value == item.Value;
        }

        /// <summary>
        /// Copies the elements of the <see cref="T:System.Collections.Generic.ICollection`1"/> to an <see cref="T:System.Array"/>, starting at a particular <see cref="T:System.Array"/> index.
        /// </summary>
        /// <param name="array">The one-dimensional <see cref="T:System.Array"/> that is the destination of the elements copied from <see cref="T:System.Collections.Generic.ICollection`1"/>. The <see cref="T:System.Array"/> must have zero-based indexing.
        ///                 </param><param name="arrayIndex">The zero-based index in <paramref name="array"/> at which copying begins.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="array"/> is null.
        ///                 </exception><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="arrayIndex"/> is less than 0.
        ///                 </exception><exception cref="T:System.ArgumentException"><paramref name="array"/> is multidimensional.
        ///                     -or-
        ///                 <paramref name="arrayIndex"/> is equal to or greater than the length of <paramref name="array"/>.
        ///                     -or-
        ///                     The number of elements in the source <see cref="T:System.Collections.Generic.ICollection`1"/> is greater than the available space from <paramref name="arrayIndex"/> to the end of the destination <paramref name="array"/>.
        ///                     -or-
        ///                     Type <paramref name="T"/> cannot be cast automatically to the type of the destination <paramref name="array"/>.
        ///                 </exception>
        public void CopyTo(KeyValuePair<string, object>[] array, int arrayIndex)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Removes the first occurrence of a specific object from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <returns>
        /// true if <paramref name="item"/> was successfully removed from the <see cref="T:System.Collections.Generic.ICollection`1"/>; otherwise, false. This method also returns false if <paramref name="item"/> is not found in the original <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </returns>
        /// <param name="item">The object to remove from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        ///                 </exception>
        public bool Remove(KeyValuePair<string, object> item)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Determines whether the <see cref="T:System.Collections.Generic.ICollection`1"/> contains a specific value.
        /// </summary>
        /// <returns>
        /// true if <paramref name="item"/> is found in the <see cref="T:System.Collections.Generic.ICollection`1"/>; otherwise, false.
        /// </returns>
        /// <param name="item">The object to locate in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param>
        public bool Contains(KeyValuePair<string, string> item)
        {
            string key = item.Key;
            var kv = findKVP(key);
            return kv != null && kv.Value == item.Value;
        }


        /// <summary>
        /// Gets the number of elements contained in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <returns>
        /// The number of elements contained in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </returns>
        public int Count
        {
            get { return Impl.Count; }
        }

        /// <summary>
        /// Gets a value indicating whether the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        /// </summary>
        /// <returns>
        /// true if the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only; otherwise, false.
        /// </returns>
        public bool IsReadOnly
        {
            get { return false; }
        }


        #region Implementation of IDictionary<string,string>

        /// <summary>
        /// Determines whether the <see cref="T:System.Collections.Generic.IDictionary`2"/> contains an element with the specified key.
        /// </summary>
        /// <returns>
        /// true if the <see cref="T:System.Collections.Generic.IDictionary`2"/> contains an element with the key; otherwise, false.
        /// </returns>
        /// <param name="key">The key to locate in the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception>
        public bool ContainsKey(string key)
        {
            var kv = findKVP(key);
            return kv != null;
        }

        /// <summary>
        /// Adds an element with the provided key and value to the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </summary>
        /// <param name="key">The object to use as the key of the element to add.
        ///                 </param><param name="value">The object to use as the value of the element to add.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception><exception cref="T:System.ArgumentException">An element with the same key already exists in the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        ///                 </exception><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.IDictionary`2"/> is read-only.
        ///                 </exception>
        public void Add(string key, object value)
        {
            this[key] = value;
        }

        /// <summary>
        /// Removes the element with the specified key from the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </summary>
        /// <returns>
        /// true if the element is successfully removed; otherwise, false.  This method also returns false if <paramref name="key"/> was not found in the original <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </returns>
        /// <param name="key">The key of the element to remove.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.IDictionary`2"/> is read-only.
        ///                 </exception>
        public bool Remove(string key)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Adds an element with the provided key and value to the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </summary>
        /// <param name="key">The object to use as the key of the element to add.
        ///                 </param><param name="value">The object to use as the value of the element to add.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception><exception cref="T:System.ArgumentException">An element with the same key already exists in the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        ///                 </exception><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.IDictionary`2"/> is read-only.
        ///                 </exception>
        public void Add(string key, string value)
        {
            this[key] = value;
        }


        /// <summary>
        /// Gets the value associated with the specified key.
        /// </summary>
        /// <returns>
        /// true if the object that implements <see cref="T:System.Collections.Generic.IDictionary`2"/> contains an element with the specified key; otherwise, false.
        /// </returns>
        /// <param name="key">The key whose value to get.
        ///                 </param><param name="value">When this method returns, the value associated with the specified key, if the key is found; otherwise, the default value for the type of the <paramref name="value"/> parameter. This parameter is passed uninitialized.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception>
        public bool TryGetValue(string key, out object value)
        {
            value = null;
            var kv = findKVP(key);
            if (kv == null) return false;
            value = kv.Value;
            return true;
        }

        /// <summary>
        /// Gets or sets the element with the specified key.
        /// </summary>
        /// <returns>
        /// The element with the specified key.
        /// </returns>
        /// <param name="key">The key of the element to get or set.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception><exception cref="T:System.Collections.Generic.KeyNotFoundException">The property is retrieved and <paramref name="key"/> is not found.
        ///                 </exception><exception cref="T:System.NotSupportedException">The property is set and the <see cref="T:System.Collections.Generic.IDictionary`2"/> is read-only.
        ///                 </exception>
        object IDictionary<string, object>.this[string key]
        {
            get { return findKVP(key).Value; }
            set { findKVP(key).Value = value; }
        }

        /// <summary>
        /// Gets the value associated with the specified key.
        /// </summary>
        /// <returns>
        /// true if the object that implements <see cref="T:System.Collections.Generic.IDictionary`2"/> contains an element with the specified key; otherwise, false.
        /// </returns>
        /// <param name="key">The key whose value to get.
        ///                 </param><param name="value">When this method returns, the value associated with the specified key, if the key is found; otherwise, the default value for the type of the <paramref name="value"/> parameter. This parameter is passed uninitialized.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception>
        public bool TryGetValue(string key, out string value)
        {
            value = null;
            var kv = findKVP(key);
            if (kv == null) return false;
            value = "" + kv.Value;
            return true;
        }

        /// <summary>
        /// Gets or sets the element with the specified key.
        /// </summary>
        /// <returns>
        /// The element with the specified key.
        /// </returns>
        /// <param name="key">The key of the element to get or set.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception><exception cref="T:System.Collections.Generic.KeyNotFoundException">The property is retrieved and <paramref name="key"/> is not found.
        ///                 </exception><exception cref="T:System.NotSupportedException">The property is set and the <see cref="T:System.Collections.Generic.IDictionary`2"/> is read-only.
        ///                 </exception>
        public object this[string key]
        {
            get { return AsString(findKVP(key).Value); }
            set
            {
                IKeyValuePair<string, object> exists = findKVP(key);
                if (exists != null)
                {
                    exists.Value = value;
                    return;
                }
                exists = this.makeKV(key);
                Impl.Add(exists);
                exists.Value = value;
            }
        }

        private string AsString(object value)
        {
            return "" + value;
        }
        private object AsObject(string value)
        {
            return value;
        }

        /// <summary>
        /// Gets an <see cref="T:System.Collections.Generic.ICollection`1"/> containing the keys of the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.Generic.ICollection`1"/> containing the keys of the object that implements <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </returns>
        public ICollection<string> Keys
        {
            get
            {
                List<string> keys = new List<string>();
                lock(Impl)
                {
                    foreach (var keyValuePair in Impl)
                    {
                        keys.Add(keyValuePair.Key);
                    }
                }
                return keys;
            }
        }

        /// <summary>
        /// Gets an <see cref="T:System.Collections.Generic.ICollection`1"/> containing the values in the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.Generic.ICollection`1"/> containing the values in the object that implements <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </returns>
        public ICollection<object> Values
        {
            get { throw new NotImplementedException(); }
        }

        #endregion

        public T GetValue<T>(string key, T i)
        {
            object value;
            if (TryGetValue(key, out value))
            {
                return (T) Convert.ChangeType(value, typeof (T));
            }
            return i;
        }

        #region Implementation of IEnumerable

        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <returns>
        /// A <see cref="T:System.Collections.Generic.IEnumerator`1"/> that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>1</filterpriority>
        public IEnumerator<KeyValuePair<string, object>> GetEnumerator()
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Returns an enumerator that iterates through a collection.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.IEnumerator"/> object that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>2</filterpriority>
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        #endregion
    }

    internal class KVBacked : IKeyValuePair<string, object>
    {
        private IDictionary<string, object> Backup;
        public object Value
        {
            get { return Backup[Key]; }
            set { Backup[Key] = value; }
        }

        public string Key { get; private set; }
        public string Comments
        {
            get { return ""; }
        }

        public string DebugInfo
        {
            get { return Key + " = " + Value; }
        }

        public KVBacked(string key, IDictionary<string, object> backup)
        {
            Key = key;
            this.Backup = backup;
            if (!backup.ContainsKey(key)) backup[key] = null;
        }

        #region Implementation of IDisposable

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {
           
        }

        #endregion
    }

    public class SysVarsDictEnumr : IEnumerator<KeyValuePair<string, string>>
    {
        private List<IKeyValuePair<string, object>> Impl;
        public SysVarsDictEnumr(List<IKeyValuePair<string, object>> pairs)
        {
            Impl = pairs;
        }

        #region Implementation of IDisposable

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {
            throw new NotImplementedException();
        }

        #endregion

        #region Implementation of IEnumerator

        /// <summary>
        /// Advances the enumerator to the next element of the collection.
        /// </summary>
        /// <returns>
        /// true if the enumerator was successfully advanced to the next element; false if the enumerator has passed the end of the collection.
        /// </returns>
        /// <exception cref="T:System.InvalidOperationException">The collection was modified after the enumerator was created. 
        ///                 </exception><filterpriority>2</filterpriority>
        public bool MoveNext()
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Sets the enumerator to its initial position, which is before the first element in the collection.
        /// </summary>
        /// <exception cref="T:System.InvalidOperationException">The collection was modified after the enumerator was created. 
        ///                 </exception><filterpriority>2</filterpriority>
        public void Reset()
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Gets the element in the collection at the current position of the enumerator.
        /// </summary>
        /// <returns>
        /// The element in the collection at the current position of the enumerator.
        /// </returns>
        public KeyValuePair<string, string> Current
        {
            get { throw new NotImplementedException(); }
        }

        /// <summary>
        /// Gets the current element in the collection.
        /// </summary>
        /// <returns>
        /// The current element in the collection.
        /// </returns>
        /// <exception cref="T:System.InvalidOperationException">The enumerator is positioned before the first element of the collection or after the last element.
        ///                 </exception><filterpriority>2</filterpriority>
        object IEnumerator.Current
        {
            get { return Current; }
        }

        #endregion
    }
}
