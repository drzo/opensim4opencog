using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Threading;
using System.Xml.Serialization;
using MushDLR223.Utilities;
using Exception=System.Exception;
using Thread = System.Threading.Thread;
using DotLisp;
namespace MushDLR223.ScriptEngines
{
    public partial class ScriptManager
    {

        public static SysVarsDict SysVarsAsDict()
        {
            return new SysVarsDict(SysVars0, BackingDict);
        }

        public static List<IKeyValuePair<string, object>> GetSysVars(object ctx)
        {
            ConfigSettingAttribute.sysvarCtx = ctx;
            return SysVars0;
        }
        public readonly static List<IKeyValuePair<string, object>> SysVars0 = new List<IKeyValuePair<string, object>>();
        private static IDictionary<string, object> BackingDict = new Dictionary<string, object>();

        public static void LoadSysVars(Type t)
        {
            lock (SysVars0)
            {
                ConfigSettingAttribute pca = ConfigSettingAttribute.FindConfigSetting(t, false);
                bool allMembers = pca != null;
                foreach (
                    var s in
                        t.GetMembers(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance |
                                     BindingFlags.Static))
                {
                    ConfigSettingAttribute cs0;
                    if (!s.IsDefined(typeof(ConfigSettingAttribute), true))
                    {
                        if (!ConfigSettingAttribute.IsGoodForConfig(s, true, allMembers, true, allMembers))
                            continue;
                        cs0 = ConfigSettingAttribute.FindConfigSetting(s, true);
                    }
                    else
                    {
                        cs0 = ConfigSettingAttribute.FindConfigSetting(s, true);
                    }
                    if (!cs0.IsNonValue && !SysVars0.Contains(cs0))
                    {
                        SysVars0.Add(cs0);
                    }
                    //  WriteLine("Setting: " + cs0.Description);
                }
                var st = t.BaseType;
                if (st != null && st != typeof(object))
                {
                    LoadSysVars(st);
                }
            }
        }
        public static ICollection<IKeyValuePair<string, object>> FindMatchingSysvars(IEnumerable<IKeyValuePair<string, object>> sysvars, string find, bool exactMatch, bool caseSensitive)
        {
            List<IKeyValuePair<string, object>> match = new List<IKeyValuePair<string, object>>();
            foreach (var sv in sysvars)
            {
                string svn = sv.Key;
                if (!caseSensitive) svn = svn.ToLower();
                if (exactMatch)
                {
                    if (svn == find) match.Add(sv);
                }
                else
                {
                    if (svn.Contains(find)) match.Add(sv);
                }
            }
            return match;
        }

        public static object ChangeType(object value, Type type)
        {
            if (type.IsInstanceOfType(value)) return value;
            if (type.IsEnum)
            {
                if (value is String)
                {
                    string vs = (String) value;
                    try
                    {
                        var e = Enum.Parse(type, vs, false);
                        if (e != null) return e;
                        e = Enum.Parse(type, vs, true);
                        if (e != null) return e;
                    }
                    catch (ArgumentException)
                    {

                    }
                }
            }
            foreach (TypeChanger tc in LockInfo.CopyOf(TypeChangers))
            {
                bool didit;
                object o = tc(value, type, out didit);
                if (didit) return o;
            }
            if (value is IConvertible)
            {
                if (typeof(IConvertible).IsAssignableFrom(type))
                {
                    try
                    {
                        return Convert.ChangeType(value, type);
                    }
                    catch
                    {
                    }
                }
            }
            if (value == null) return null;
            if (type == typeof(string))
            {
                if (value is string[]) return Parser.Rejoin((string[])value, 0);
            }
            return null;// Convert.ChangeType(value, type);
        }

        public static void AddTypeChanger(TypeChanger tc)
        {
            lock (TypeChangers) TypeChangers.Add(tc);
        }

        private static readonly List<TypeChanger> TypeChangers = new List<TypeChanger>();
        public delegate object TypeChanger(object value, Type to, out bool converted);

        static readonly Dictionary<Type, KeyValuePair<List<PropertyInfo>, List<FieldInfo>>> PropForTypes = new Dictionary<Type, KeyValuePair<List<PropertyInfo>, List<FieldInfo>>>();

        private static KeyValuePair<List<PropertyInfo>, List<FieldInfo>> GetPropsForTypes(Type t)
        {
            KeyValuePair<List<PropertyInfo>, List<FieldInfo>> kv;

            if (PropForTypes.TryGetValue(t, out kv)) return kv;

            lock (PropForTypes)
            {
                if (!PropForTypes.TryGetValue(t, out kv))
                {
                    kv = new KeyValuePair<List<PropertyInfo>, List<FieldInfo>>(new List<PropertyInfo>(),
                                                                               new List<FieldInfo>());
                    var ta = t.GetCustomAttributes(typeof(XmlTypeAttribute), false);
                    bool specialXMLType = false;
                    if (ta != null && ta.Length > 0)
                    {
                        XmlTypeAttribute xta = (XmlTypeAttribute)ta[0];
                        specialXMLType = true;
                    }
                    HashSet<string> lowerProps = new HashSet<string>();
                    BindingFlags flags = BindingFlags.Instance | BindingFlags.Public; //BindingFlags.NonPublic
                    foreach (
                        PropertyInfo o in t.GetProperties(flags))
                    {
                        if (o.CanRead)
                        {

                            if (o.Name.StartsWith("_")) continue;
                            if (o.DeclaringType == typeof(Object)) continue;
                            if (!lowerProps.Add(o.Name.ToLower())) continue;
                            if (o.GetIndexParameters().Length > 0)
                            {
                                continue;
                            }
                            if (specialXMLType)
                            {
                                var use = o.GetCustomAttributes(typeof(XmlArrayItemAttribute), false);
                                if (use == null || use.Length < 1) continue;
                            }
                            kv.Key.Add(o);

                        }
                    }
                    foreach (FieldInfo o in t.GetFields(flags))
                    {
                        if (o.Name.StartsWith("_")) continue;
                        if (o.DeclaringType == typeof(Object)) continue;
                        if (!lowerProps.Add(o.Name.ToLower())) continue;
                        if (specialXMLType)
                        {
                            var use = o.GetCustomAttributes(typeof(XmlArrayItemAttribute), false);
                            if (use == null || use.Length < 1) continue;
                        }
                        kv.Value.Add(o);
                    }
                }
                return kv;
            }
        }
        public static List<NamedParam> GetMemberValues(string prefix, Object properties)
        {
            return GetMemberValues(prefix, properties, null);
        }
        public static List<NamedParam> GetMemberValues(string prefix, Object properties, Action<MemberInfo, object> GetUUIDType)
        {
            List<NamedParam> dict = new List<NamedParam>();
            if (properties == null)
            {
                return dict;
            }
            Type t = properties.GetType();
            KeyValuePair<List<PropertyInfo>, List<FieldInfo>> vvv = GetPropsForTypes(t);
            HashSet<string> lowerProps = new HashSet<string>();
            BindingFlags flags = BindingFlags.Instance | BindingFlags.Public; //BindingFlags.NonPublic
            foreach (
                PropertyInfo o in vvv.Key)
            {
                {
                    try
                    {
                        var v = o.GetValue(properties, null);
                        if (v == null)
                        {
                            v = new NullType(properties, o);
                        }
                        if (GetUUIDType != null) GetUUIDType(o, v);
                        dict.Add(new NamedParam(properties, o, prefix + o.Name, o.PropertyType, v));
                    }
                    catch (Exception e)
                    {
                        DLRConsole.DebugWriteLine("" + e);
                    }
                }
            }
            foreach (FieldInfo o in vvv.Value)
            {
                try
                {
                    var v = o.GetValue(properties);
                    if (v == null)
                    {
                        v = new NullType(properties, o);
                    }
                    if (GetUUIDType != null) GetUUIDType(o, v);
                    dict.Add(new NamedParam(properties, o, prefix + o.Name, o.FieldType, v));
                }
                catch (Exception e)
                {
                    DLRConsole.DebugWriteLine("" + e);
                }
            }
            return dict;
        }
    }
    public class SysVarsDict : BackDict
    {
        public SysVarsDict(List<IKeyValuePair<string, object>> pairs, IDictionary<string, object> backIfNotSysvar)
            : base(pairs, backIfNotSysvar)
        {

        }
    }
    public class BackDict : IDictionary<string, object>
    {
        protected List<IKeyValuePair<string, object>> ListKeyValue;
        private IDictionary<string, object> Backup;
        public BackDict(List<IKeyValuePair<string, object>> pairs, IDictionary<string, object> backup)
        {
            ListKeyValue = pairs;
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
            return new KVBacked(key, Backup ?? this);
        }

        private IKeyValuePair<string, object> findKVP(string key)
        {
            key = Parser.ToKey(key);
            lock (ListKeyValue)
            {
                foreach (IKeyValuePair<string, object> list in ListKeyValue)
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
            get { return ListKeyValue.Count; }
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
                ListKeyValue.Add(exists);
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
                lock (ListKeyValue)
                {
                    foreach (var keyValuePair in ListKeyValue)
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
                return (T)ScriptManager.ChangeType(value, typeof(T));
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

        public bool IsReadOnly
        {
            get { return true || Backup == null; }
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
