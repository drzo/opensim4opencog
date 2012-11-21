using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.Threading;
using System.Xml;
using System.IO;
using System.Xml.Serialization;
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLParser;
using LogicalParticleFilter1;
using Lucene.Net.Store;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using RTParser.Database;
using RTParser.Normalize;
using RTParser.Utils;

using DataUnifiable = System.String;
using DataUnifiableYYY = RTParser.Unifiable;

namespace RTParser.Variables
{
    public delegate ISettingsDictionaryT<DataUnifiable> ParentProvider();
    public interface ISettingsDictionary : ISettingsDictionaryT<DataUnifiable>{}
    public interface ISettingsDictionaryT<T> : ITraceable, ITreeable
    {
        /// <summary>
        /// Adds a bespoke setting to the Settings class (accessed via the grabSettings(string name)
        /// method.
        /// </summary>
        /// <param name="name">The name of the new setting</param>
        /// <param name="value">The value associated with this setting</param>
        bool addSetting(string name, object value);
        /// <summary>
        /// Removes the named setting from this class
        /// </summary>
        /// <param name="name">The name of the setting to remove</param>
        bool removeSetting(string name);
        /// <summary>
        /// Updates the named setting with a new value whilst retaining the position in the
        /// dictionary
        /// </summary>
        /// <param name="name">the name of the setting</param>
        /// <param name="value">the new value</param>
        bool updateSetting(string name, object value);
        /// <summary>
        /// Returns the value of a setting given the name of the setting
        /// </summary>
        /// <param name="name">the name of the setting whose value we're interested in</param>
        /// <returns>the value of the setting</returns>
        T grabSetting(string name);
        /// <summary>
        /// Checks to see if a setting of a particular name exists
        /// </summary>
        /// <param name="name">The setting name to check</param>
        /// <returns>Existential truth value</returns>
        bool containsLocalCalled(string name);
        bool containsSettingCalled(string name);

    }

    public interface SettingsDictionary : ISettingsDictionary, IDictionary<string, DataUnifiable>, ICollectionProvider
    {
        void loadSettings(string uri);
        void loadSettings(string uri, Request req);
        void InsertFallback(ParentProvider func);
        void clearHierarchy();
        void clearSettings();
        void InsertMetaProvider(ParentProvider func);
        void InsertProvider(ParentProvider provider);
        string ToDebugString();
        string GetMeta(string name, string prop);
        string bbPrefix { get; set; }
        void Clone(ISettingsDictionary settingsDictionary);
        string grabSetting(string name, bool searchUpMt);
    }

    public interface KeyValueList
    {
        //ICollection<string> Keys { get; }
        bool ContainsKey(string name);
        ICollection<string> Values { get; }
        int Count { get; }
        ICollection<string> Keys { get; }
        bool IsOrdered { get; set; }
        void Add(string name, string value);
        //string this[string name] { get; }
        void Clear();
        void Remove(string name);

        string GetValue(string normalizedName);
        void AddKey(string name);

        void AddFallback(string p);
    }

    public class KeyValueListSIProlog : KeyValueList
    {
        public string dictMt;
        public string predicateName;
        public string arg1Name;
        private LogicalParticleFilter1.SIProlog prologEngine;

        public KeyValueListSIProlog(SIProlog pl, string mtName, string predicate)
        {
            prologEngine = pl;
            dictMt = mtName;
            predicateName = predicate;
        }

        public bool ContainsKey(string name)
        {
            return GetArgVal(name, false) != null;
        }

        public ICollection<string> Values
        {
            get
            {
                List<string> values = new List<string>();
                foreach (var key in Keys)
                {
                    values.Add(GetValue(key));
                }
                return values;
            }
        }

        public int Count
        {
            get {
                List<Dictionary<string, string>> bingingsList;
                this.prologEngine.askQuery(QueryForNameValue("KEY", "VALUE"), dictMt, out bingingsList);
                return bingingsList.Count;
            }
        }

        public ICollection<string> Keys
        {
            get
            {
                List<Dictionary<string, string>> bingingsList;
                this.prologEngine.askQuery(QueryForNameValue("KEY", "VALUE"), dictMt, out bingingsList);
                if (bingingsList.Count == 0) return new string[0];
                List<string> keys = new List<string>();
                foreach (var list in bingingsList)
                {
                    keys.Add(ArgToValue(list["KEY"]));
                }
                return keys;
            }

        }

        public bool IsOrdered
        {
            get { return true;  }
            set { throw new NotImplementedException(); }
        }

        public void Add(string name, string value)
        {
            name = KeyCase.NormalizeKey(name);
            string valArg = MakeArg(value);
            //Remove(name);
            string before = GetArgVal(name, false);
            if (valArg == before) return;
            if (before != null)
            {
                prologEngine.replaceInKB(QueryForNameValue(MakeArg(name), before) + ".",
                                         QueryForNameValue(MakeArg(name), valArg) + ".", dictMt);
            }
            else
            {
                prologEngine.appendKB(QueryForNameValue(MakeArg(name), valArg) + ".\n", dictMt);
            }
            string now = GetArgVal(name, false);
            if (now != valArg)
            {
                throw new NotImplementedException("asserting " + valArg);
            }
        }

        private string MakeArg(string value)
        {
            var value2 = value.Replace("\\", "\\\\").Replace("\"", "\\\"");
            if (value2 == value)
            {
                if (value.ToLower() == value)
                {
                   // return value;
                }
            }
            return "\"" + value2 + "\"";
        }

        public void Clear()
        {
            prologEngine.clearKB(dictMt);
        }

        public void Remove(string name)
        {
            name = KeyCase.NormalizeKey(name);
            string valArg = GetArgVal(name, false);
            if (valArg == null) return;
            string remove = QueryForNameValue(MakeArg(name), valArg);
            var didit = prologEngine.retractKB(remove, dictMt);
            var valarg2 = GetArgVal(name, false);
            if (null != valarg2)
            {
                throw new NotImplementedException("retracting " + valArg);
            }

        }

        public string GetArgVal(string normalizedName, bool followGenlMt)
        {
            normalizedName = KeyCase.NormalizeKey(normalizedName);
            List<Dictionary<string, string>> bingingsList;
            this.prologEngine.askQuery(QueryForNameValue(MakeArg(normalizedName), "VALUE"), dictMt, followGenlMt, out bingingsList);
            int cnt = bingingsList.Count;
            if (cnt == 0) return null;
            string res;
            if (cnt == 1 || true)
            {
                res = bingingsList[0]["VALUE"];
            }
            else
            {
                res = bingingsList[cnt - 1]["VALUE"];
            }
            int len = res.Length;
            return res;
        }

        public string GetValue(string normalizedName)
        {
            var res = GetArgVal(normalizedName, true);
            return ArgToValue(res);
        }

        private string ArgToValue(string res)
        {
            if (res == null) return null;
            int len = res.Length;
            if (res.StartsWith("\"") && res.EndsWith("\""))
            {
                res = res.Substring(1, len - 2);
            }
            return res;
        }

        private string QueryForNameValue(string name, string value)
        {
            return predicateName + "(" + name + "," + value + ")";
        }

        public void AddKey(string name)
        {
            string before = GetValue(name);
            if (before == null)
            {
                Add(name, "");
            }
        }

        public void AddFallback(string p)
        {
            if (Equals(p,dictMt))
            {
                return;
            }
            prologEngine.connectMT(dictMt, p);
        }
    }
    public class KeyValueListCSharp : KeyValueList
    {
        public KeyValueList settingsHash0;// = new Dictionary<string, DataUnifiable>();
        public Dictionary<string, DataUnifiable> settingsHash;// = new Dictionary<string, DataUnifiable>();
        public IList<string> orderedKeys = new List<string>();

        public string GetValue(string key)
        {
            if (settingsHash0 != null) return settingsHash0.GetValue(key);
            return settingsHash[key];
        }

        public ICollection<string> Values
        {
            get
            {
                if (settingsHash0 != null) return settingsHash0.Values;
                return settingsHash.Values;
            }
        }

        public ICollection<string> Keys
        {
            get
            {
                if (orderedKeys == null)
                {
                    if (settingsHash0 != null) return settingsHash0.Keys;
                    return settingsHash.Keys;
                }
                return orderedKeys;
            }
        }

        public bool IsOrdered
        {
            get { return orderedKeys != null || (settingsHash0 != null && settingsHash0.IsOrdered); }
            set { throw new NotImplementedException(); }
        }

        public int Count
        {
            get
            {
                if (orderedKeys == null)
                {
                    if (settingsHash0 != null) return settingsHash0.Count;
                    return settingsHash.Count;
                }
                return orderedKeys.Count;
            }
        }

        public bool ContainsKey(string name)
        {
            if (orderedKeys != null && orderedKeys.Contains(name)) return true;
            if (settingsHash0 != null && settingsHash0.ContainsKey(name)) return true;
            if (settingsHash != null && settingsHash.ContainsKey(name)) return true;
            return false;
        }

        public void Add(string name, string value)
        {
            if (orderedKeys != null)
            {
                if (!orderedKeys.Contains(name)) orderedKeys.Add(name);
            } 
            if (settingsHash0 != null) settingsHash0.Add(name, value);
            if (settingsHash != null) settingsHash[name] = value;
        }

        public KeyValueListCSharp(IList<string> list, Dictionary<System.String, System.String> dictionary)
        {
            orderedKeys = list;
            settingsHash = dictionary;
        }
        public void Remove(string name)
        {
            if (orderedKeys != null) orderedKeys.Remove(name);
            if (settingsHash != null) settingsHash.Remove(name);
            if (settingsHash0 != null) settingsHash0.Remove(name);
        }

        public void Clear()
        {
            if (orderedKeys != null) orderedKeys.Clear();
            if (settingsHash != null) settingsHash.Clear();
            if (settingsHash0 != null) settingsHash0.Clear();
        }

        public void AddKey(string name)
        {
            if (orderedKeys != null)
            {
                orderedKeys.Add(name);
            }
            else
            {
                if (settingsHash != null)
                {
                    if (!settingsHash.ContainsKey(name))
                    {
                        settingsHash.Add(name, null);
                    }
                }
                if (settingsHash0 != null)
                {
                    if (!settingsHash0.ContainsKey(name))
                    {
                        settingsHash0.Add(name, null);
                    }
                }
            }
        }

        public void AddFallback(string p)
        {
            //throw new NotImplementedException();
        }
    }
    /// <summary>
    /// A bespoke Dictionary<,> for loading, adding, checking, removing and extracting
    /// settings.
    /// </summary>
    public class SettingsDictionaryReal : SettingsDictionary, ISettingsDictionary, IDictionary<string, DataUnifiable>, ICollectionProvider
    {
        #region Attributes

        /// <summary>
        /// Holds a dictionary of settings
        /// </summary>
        public readonly KeyValueList settingsHash;

        /// <summary>
        /// Contains an ordered collection of all the keys (unfortunately Dictionary<,>s are
        /// not ordered)
        /// </summary>
        KeyValueList KeyZ
        {
            get { return settingsHash; }
        }

        // prechecks and uses if settings exist
        private List<ParentProvider> _overides = new List<ParentProvider>();
        // fallbacks (therefore inherits)
        private List<ParentProvider> _fallbacks = new List<ParentProvider>();
        // fallbacks (therefore inherits)
        private List<ParentProvider> _listeners = new List<ParentProvider>();
        // fallbacks (therefore inherits)
        //private readonly PrefixProvider prefixProvider;
        internal bool IsIdentityReadOnly = false;
        internal bool SuspendUpdates = false;

        /// <summary>
        /// The bot this dictionary is associated with (only for writting log)
        /// </summary>
        protected AltBot bot;
        protected AltBot rbot
        {
            get { return bot; }
        }

        private string theNameSpace;
        public bool TrimKeys = true;
        private string fromFile;

        /// <summary>
        /// Adds an item to the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <param name="item">The object to add to the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        ///                 </exception>
        public void Add(KeyValuePair<string, DataUnifiable> item)
        {
            Add(item.Key, item.Value);
        }

        /// <summary>
        /// Removes all items from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only. 
        ///                 </exception>
        public void Clear()
        {
            clearSettings();
        }

        /// <summary>
        /// Determines whether the <see cref="T:System.Collections.Generic.ICollection`1"/> contains a specific value.
        /// </summary>
        /// <returns>
        /// true if <paramref name="item"/> is found in the <see cref="T:System.Collections.Generic.ICollection`1"/>; otherwise, false.
        /// </returns>
        /// <param name="item">The object to locate in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param>
        public bool Contains(KeyValuePair<string, DataUnifiable> item)
        {
            return Unifiable.IsStringMatch(grabSetting(item.Key), item.Value);
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
        public void CopyTo(KeyValuePair<string, DataUnifiable>[] array, int arrayIndex)
        {
            foreach (string key in Keys)
            {
                array[arrayIndex++] = new KeyValuePair<string, DataUnifiable>(key, grabSetting(key));
            }
        }

        public void CopyFrom(SettingsDictionary from, bool localOnly, bool existingOnly)
        {
            foreach(string key in from.Keys)
            {
                if (existingOnly && !containsSettingCalled(key))
                {
                    continue;
                }
                if (localOnly && !containsLocalCalled(key))
                {
                    continue;
                }
                addSetting(key, from.grabSetting(key));
            }
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
        public bool Remove(KeyValuePair<string, DataUnifiable> item)
        {
            foreach (var hash in SettingNames(rbot.ObjectRequester, 1))
            {
                if (IsKeyMatch(item.Key, hash))
                {
                    var v = grabSetting(hash);
                    if (Unifiable.IsStringMatch(v, item.Value))
                    {
                        return removeSetting(hash);
                    }
                }
            }
            return false;
        }

        private static bool IsKeyMatch(string key, string hash)
        {
            if (key == null) return true;
            return Unifiable.IsStringMatch(key, hash);
        }

        /// <summary>
        /// The number of items in the dictionary
        /// </summary>
        public int Count
        {
            get
            {
                return this.orderedKeysCount;
            }
        }

        /// <summary>
        /// Gets a value indicating whether the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        /// </summary>
        /// <returns>
        /// true if the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only; otherwise, false.
        /// </returns>
        public bool IsReadOnly
        {
            get { return SuspendUpdates || TextPatternUtils.IsTrue(grabSetting("isReadOnly")); }
        }

        public string NameSpace
        {
            get { return theNameSpace; }
            set
            {
                AddName(value);
                theNameSpace = value;
            }
        }

        public bool IsTraced { get; set; }

        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <returns>
        /// A <see cref="T:System.Collections.Generic.IEnumerator`1"/> that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>1</filterpriority>
        public IEnumerator<KeyValuePair<string, DataUnifiable>> GetEnumerator()
        {
            return new SettingsDictionaryEnumerator(Keys, this);
        }

        public override string ToString()
        {
            return theNameSpace + "(" + Count + ") ";
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

        public string ToDebugString()
        {
            return theNameSpace + "(" + Count + ") " + DictionaryAsXML.DocumentElement.InnerXml.Replace("<item name=", "\n<item name =");
        }

        /// <summary>
        /// An XML representation of the contents of this dictionary
        /// </summary>
        public XmlDocument DictionaryAsXML
        {
            get
            {
                XmlDocument result = new XmlDocument();
                XmlDeclaration dec = result.CreateXmlDeclaration("1.0", "UTF-8", "");
                result.AppendChild(dec);
                XmlNode root = result.CreateNode(XmlNodeType.Element, "root", "");
                XmlAttribute newAttr = result.CreateAttribute("name");
                lock (orderedKeyLock)
                {
                    string dupeCheck = "";
                    newAttr.Value = NameSpace;
                    if (fromFile != null)
                    {
                        newAttr = result.CreateAttribute("fromfile");
                        newAttr.Value = fromFile;
                    }
                    result.AppendChild(root);
                    dupeCheck = "";
                    foreach (var normalizedName in Overides)
                    {
                        string nameValue = normalizedName.NameSpace;
                        if (dupeCheck == nameValue)
                        {
                            WriteErrorLine("Overides DUPES " + dupeCheck);
                            break;
                        }
                        dupeCheck = nameValue;
                        XmlNode item = result.CreateNode(XmlNodeType.Element, "override", "");
                        XmlAttribute name = result.CreateAttribute("name");
                        name.Value = nameValue;
                        item.Attributes.Append(name);
                        root.AppendChild(item);
                    }
                    dupeCheck = "";
                    foreach (var normalizedName in Fallbacks)
                    {
                        string nameValue = normalizedName.NameSpace;
                        if (dupeCheck == nameValue)
                        {
                            WriteErrorLine("Fallbacks DUPES " + dupeCheck);
                            break;
                        }
                        dupeCheck = nameValue;

                        XmlNode item = result.CreateNode(XmlNodeType.Element, "fallback", "");
                        XmlAttribute name = result.CreateAttribute("name");
                        name.Value = nameValue;
                        item.Attributes.Append(name);
                        root.AppendChild(item);
                    }
                    dupeCheck = "";
                    foreach (var normalizedName in Listeners)
                    {
                        string nameValue = normalizedName.NameSpace;
                        if (dupeCheck == nameValue)
                        {
                            WriteErrorLine("Listeners DUPES " + dupeCheck); 
                            break;
                        }
                        dupeCheck = nameValue;

                        XmlNode item = result.CreateNode(XmlNodeType.Element, "synchon", "");
                        XmlAttribute name = result.CreateAttribute("name");
                        name.Value = nameValue;
                        item.Attributes.Append(name);
                        root.AppendChild(item);
                    }
                    /*foreach (var normalizedName in this.prefixProvider._prefixes)
                    {
                        XmlNode item = result.CreateNode(XmlNodeType.Element, "prefixes", "");
                        XmlAttribute name = result.CreateAttribute("name");
                        name.Value = normalizedName.Key;
                        XmlAttribute value = result.CreateAttribute("value");
                        value.Value = normalizedName.Value().NameSpace;
                        item.Attributes.Append(name);
                        item.Attributes.Append(value);
                        root.AppendChild(item);
                    }
                     */
                    foreach (var normalizedName in ProvidersFrom(this.MetaProviders))
                    {
                        string nameValue = normalizedName.NameSpace;
                        if (dupeCheck == nameValue)
                        {
                            WriteErrorLine("MetaProviders DUPES " + dupeCheck);
                            break;
                        }
                        dupeCheck = nameValue;

                        XmlNode item = result.CreateNode(XmlNodeType.Element, "metaproviders", "");
                        XmlAttribute name = result.CreateAttribute("name");
                        name.Value = nameValue;
                        item.Attributes.Append(name);
                        root.AppendChild(item);
                    }
                    foreach (var normalizedName in LockInfo.CopyOf(this.maskedVars))
                    {
                        XmlNode item = result.CreateNode(XmlNodeType.Element, "maskedvar", "");
                        XmlAttribute name = result.CreateAttribute("name");
                        name.Value = normalizedName;
                        item.Attributes.Append(name);
                        root.AppendChild(item);
                    }
                    foreach (string n in this.LocalKeys)
                    {
                        XmlNode item = result.CreateNode(XmlNodeType.Element, "item", "");
                        XmlAttribute name = result.CreateAttribute("name");
                        name.Value = n; ;
                        XmlAttribute value = result.CreateAttribute("value");
                        value.Value = this.settingsHash.GetValue(TransformKey(n));
                        item.Attributes.Append(name);
                        item.Attributes.Append(value);
                        root.AppendChild(item);
                    }

                }
                return result;
            }
        }

        private void WriteErrorLine(string p0)
        {
            DLRConsole.DebugWriteLine("ERROR: " + p0);
        }

        public List<ISettingsDictionary> Listeners
        {
            get { return ProvidersFrom(this._listeners); }
        }

        public List<ISettingsDictionary> Overides
        {
            get { return ProvidersFrom(this._overides); }
        }

        #endregion


        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot for whom this is a settings dictionary</param>
        public SettingsDictionaryReal(String name, AltBot bot, KeyValueList list)
        {
            settingsHash = list;
            IsTraced = true;
            theNameSpace = name;// ScriptManager.ToKey(name);//.ToLower();
            this.bot = bot;
            IsSubsts = name.Contains("subst");
            TrimKeys = !name.Contains("subst");
            bot.RegisterDictionary(name, this);
            if (!IsSubsts) this.InsertMetaProvider(bot.GetRelationMetaProps);
            IsTraced = false;
        }

        public bool IsOrdered
        {
            get { return settingsHash.IsOrdered;}  set { settingsHash.IsOrdered = value; }
        }

        #region Methods
        public void loadSettings(string pathToSettings)
        {
            loadSettings(pathToSettings, SettingsPolicy.Default, null);
        }
        /// <summary>
        /// Loads bespoke settings into the class from the file referenced in pathToSettings.
        /// 
        /// The XML should have an XML declaration like this:
        /// 
        /// <?xml version="1.0" encoding="utf-8" ?> 
        /// 
        /// followed by a <root> tag with child nodes of the form:
        /// 
        /// <item name="name" value="value"/>
        /// </summary>
        /// <param name="pathToSettings">The file containing the settings</param>
        public void loadSettings(string pathToSettings, Request request)
        {
            loadSettings(pathToSettings, SettingsPolicy.Default, request);   
        }
        public void loadSettings(string pathToSettings, SettingsPolicy settingsPolicy, Request request)
        {
            if (pathToSettings == null) return;
            AddMonitorPath(pathToSettings, settingsPolicy);
            loadSettingsNow(pathToSettings, settingsPolicy, request);
        }
        public void loadSettingsFromPrefix(string prefixToSettings, SettingsPolicy settingsPolicy, Request request)
        {
            lock (MonitorPaths)
            {
                foreach (var path in MonitorPaths)
                {
                    string pathToSettings = HostSystem.Combine(prefixToSettings, path.Key);
                    try
                    {
                        loadSettingsNow(pathToSettings, path.Value, request);
                    }
                    catch (FileNotFoundException fnf)
                    {

                    }
                }
            }
        }

         public void loadSettingsNow(string pathToSettings,  SettingsPolicy settingsPolicy, Request request)
        {
            if (request == null)
                request = bot.GetBotRequest("<!-- Loads Config to " + this + " from: '" + pathToSettings + "' -->"); 
             pathToSettings = HostSystem.ResolveToExistingPath(pathToSettings);
             pathToSettings = HostSystem.FileSystemPath(pathToSettings);
             OutputDelegate writeToLog = this.writeToLog;
             if (request != null) writeToLog = request.writeToLog;
            if (pathToSettings == null) return;
            lock (orderedKeyLock)
            {
                if (pathToSettings.Length > 0)
                {
                    if (HostSystem.FileExists(pathToSettings))
                    {
                        XmlDocumentLineInfo xmlDoc = new XmlDocumentLineInfo(pathToSettings, true);
                        bool prev = IsIdentityReadOnly;
                        try
                        {
                            var stream = HostSystem.GetStream(pathToSettings);
                            xmlDoc.Load(stream);
                            HostSystem.Close(stream);
                            IsIdentityReadOnly = false;
                            loadSettingNode(this, xmlDoc, settingsPolicy,  request);
                        }
                        catch (ChatSignal e)
                        {
                            throw;
                        }
                        catch (Exception e)
                        {
                            writeToLog("ERROR loadSettings '{1}'\n {0} ", e, pathToSettings);
                        }
                        finally
                        {
                            IsIdentityReadOnly = prev;
                        }
                    }
                    else
                    {
                        writeToLog("No settings found in: " + pathToSettings);
                        return;
                    }
                    writeToLog("Loaded Settings found in: " + pathToSettings);
                    if (fromFile == null) fromFile = pathToSettings;
                }
                else
                {
                    throw new FileNotFoundException(pathToSettings);
                }
            }
        }

        public static void loadSettingsNow(ISettingsDictionary dict0, string prefix, string pathToSettings0,
            SettingsPolicy settingsPolicy, Request request)
        {
            if (pathToSettings0 == null) return;
            string pathToSettings = HostSystem.Combine(prefix, pathToSettings0);
            pathToSettings = HostSystem.FileSystemPath(pathToSettings);
            SettingsDictionaryReal dict = ToSettingsDictionary(dict0);
            OutputDelegate writeToLog = dict.writeToLog;
            // or else
            // ReSharper disable ConstantNullColescingCondition
            writeToLog = writeToLog ?? request.writeToLog;
            // ReSharper restore ConstantNullColescingCondition
            lock (LockInfo.Watch(dict.orderedKeyLock))
            {
                if (pathToSettings.Length > 0)
                {
                    if (HostSystem.DirExists(pathToSettings))
                    {
                        foreach (string s in HostSystem.GetFiles(pathToSettings, "*.xml"))
                        {
                            loadSettingsNow(dict, null, s, settingsPolicy, request);
                        }
                        return;
                    }
                    if (!HostSystem.FileExists(pathToSettings))
                    {
                        writeToLog("WARNING no settings file: " + pathToSettings);
                        //throw new FileNotFoundException(pathToSettings);
                        return;
                    }

                    try
                    {
                        XmlDocumentLineInfo xmlDoc = new XmlDocumentLineInfo(pathToSettings, true);
                        var stream = HostSystem.GetStream(pathToSettings);
                        xmlDoc.Load(stream);
                        HostSystem.Close(stream);
                        loadSettingNode(dict, xmlDoc, settingsPolicy,  request);
                        writeToLog("Loaded Settings found in: " + pathToSettings);
                        if (dict.fromFile == null) dict.fromFile = pathToSettings;
                    }
                    catch (Exception e)
                    {
                        writeToLog("ERROR loadSettings: " + pathToSettings + "\n" + e);
                    }
                }
                else
                {
                    throw new FileNotFoundException("settings for " + dict);
                }
            }
        }

        private void writeToLog(string message, params object[] args)
        {
            message = TextPatternUtils.SafeFormat(message, args);
            string tol = message.Trim().ToLower();
            if (tol.StartsWith("error") || tol.StartsWith("warn")) message = "-DICTLOG: " + message;
            var nameSpace = this.NameSpace;
            if (!message.Contains(nameSpace)) message += " in " + nameSpace;
            if (!tol.Contains("dictlog")) message = "DICTLOG: " + message;
            if (bot != null) bot.writeToLog(message);
            else AltBot.writeDebugLine(message);
        }

        /// <summary>
        /// Loads bespoke settings to the class from the XML supplied in the args.
        /// 
        /// The XML should have an XML declaration like this:
        /// 
        /// <?xml version="1.0" encoding="utf-8" ?> 
        /// 
        /// followed by a <root> tag with child nodes of the form:
        /// 
        /// <item name="name" value="value"/>
        /// </summary>
        /// <param name="settingsAsXML">The settings as an XML document</param>
        static public void loadSettingNode(ISettingsDictionary dict, IEnumerable Attributes, SettingsPolicy settingsPolicy, Request request)
        {
            if (Attributes == null) return;
            foreach (object o in Attributes)
            {
                if (o is XmlNode)
                {
                    XmlNode n = (XmlNode)o;
                    loadSettingNode(dict, n, settingsPolicy,  request);
                }
            }
        }

        private static void loadNameValueSetting(ISettingsDictionary dict, string name, string value, string updateOrAddOrDefualt, XmlNode myNode, SettingsPolicy settingsPolicy, Request request)
        {
            updateOrAddOrDefualt = updateOrAddOrDefualt.ToLower().Trim();
            bool overwriteExisting = settingsPolicy.overwriteExisting;
            bool onlyIfUnknown = settingsPolicy.onlyIfUnknown;

            overwriteExisting =
                Boolean.Parse(StaticXMLUtils.GetAttribValue(myNode, "overwriteExisting", "" + overwriteExisting));

            onlyIfUnknown =
                Boolean.Parse(StaticXMLUtils.GetAttribValue(myNode, "onlyIfKnown", "" + onlyIfUnknown));

            string returnNameWhenSet =
                StaticXMLUtils.GetAttribValue(myNode, "return-name-when-set", null);
            if (returnNameWhenSet != null)
            {
                var returnNameWhenSetUPPER = StaticXMLUtils.Trim(StaticXMLUtils.ToUpper(returnNameWhenSet));
                if (returnNameWhenSetUPPER.Length == 0) { returnNameWhenSet = "false"; }
                else if (IsMissing(returnNameWhenSet)) returnNameWhenSet = "value";
                else if (StaticXMLUtils.IsFalseOrNo(returnNameWhenSetUPPER)) returnNameWhenSet = "value";
                else if (StaticXMLUtils.IsTrueOrYes(returnNameWhenSetUPPER)) returnNameWhenSet = "name";
            }
            returnNameWhenSet =
                StaticXMLUtils.GetAttribValue(myNode, "set-return", returnNameWhenSet);
            if (returnNameWhenSet != null)
            {
                ToSettingsDictionary(dict).addMetaValue(name, "set-return", returnNameWhenSet);
            }

            SettingsDictionaryReal dictionary = ToSettingsDictionary(dict);
            string englishFormatter =
                StaticXMLUtils.GetAttribValue(myNode, "formatter,pred-format,genformat,format,printf,lucene,english", null);
            if (englishFormatter != null)
            {
                string formatter = englishFormatter;
                formatter = " " + formatter + " ";
                int formatterQA = formatter.IndexOf(" | ");
                if (formatterQA != -1)
                {
                    // query mode
                    var formatterQ = formatter.Substring(formatterQA + 2).Trim();
                    dictionary.addMetaValue(name, "format-assert", formatter);
                    // assert mode
                    var formatterA = formatter.Substring(0, formatterQA).Trim();
                    dictionary.addMetaValue(name, "query-assert", formatter);
                }
                else
                {
                    // both query/assert
                    dictionary.addMetaValue(name, "format-assert", englishFormatter);
                    dictionary.addMetaValue(name, "format-query", englishFormatter);
                }
            }
            englishFormatter = StaticXMLUtils.GetAttribValue(myNode, "assert,format-assert", null);
            if (englishFormatter != null)
            {
                dictionary.addMetaValue(name, "format-assert", englishFormatter);
            }
            englishFormatter = StaticXMLUtils.GetAttribValue(myNode, "query,format-query", null);
            if (englishFormatter != null)
            {
                dictionary.addMetaValue(name, "format-query", englishFormatter);
            }
            englishFormatter = StaticXMLUtils.GetAttribValue(myNode, "whword", null);
            if (englishFormatter != null)
            {
                dictionary.addMetaValue(name, "format-whword", englishFormatter);
            }

            bool dictcontainsLocalCalled = dict.containsLocalCalled(name);

            if (updateOrAddOrDefualt == "add")
            {
                if (!overwriteExisting)
                {
                    if (dictcontainsLocalCalled)
                    {
                        return;
                    }
                }
                if (onlyIfUnknown && dictcontainsLocalCalled)
                {
                    var old = dict.grabSetting(name);
                    if (!TextPatternUtils.IsUnknown(old))
                    {
                        return;
                    }
                }
                WithoutTrace(dict, () => dict.addSetting(name, Unifiable.MakeUnifiableFromString(value, false)));
            }
            else
            {
                bool inherited = !dictcontainsLocalCalled && dict.containsSettingCalled(name);
                // update only
                var old = dict.grabSetting(name);
                if (inherited && onlyIfUnknown)
                {
                    if (!TextPatternUtils.IsUnknown(old))
                    {
                        return;
                    }
                }
                if (onlyIfUnknown && dictcontainsLocalCalled)
                {
                    if (!TextPatternUtils.IsUnknown(old))
                    {
                        return;
                    }
                }
                WithoutTrace(dict, () => dict.updateSetting(name, Unifiable.MakeUnifiableFromString(value, false)));
            }
        }

        static R WithoutTrace<R>(ISettingsDictionary dict, Func<R> func)
        {
            return StaticXMLUtils.WithoutTrace(dict, func);
        }

        static public void loadSettingNode(ISettingsDictionary dict, XmlNode myNode, SettingsPolicy settingsPolicy, Request request)
        {
            lock (LockInfo.Watch(dict))
            {

                SettingsDictionary settingsDict = ToSettingsDictionary(dict);
                WithoutTrace(dict, () =>
                {
                    loadSettingNode0(settingsDict, myNode, settingsPolicy, request);
                    return true;
                });
            }
        }

        static public void loadSettingNode0(ISettingsDictionary dict, XmlNode myNode, SettingsPolicy settingsPolicy, Request request)
        {
            bool onlyIfUnknown = settingsPolicy.onlyIfUnknown;

            if (myNode == null) return;
            if (myNode.NodeType == XmlNodeType.Comment) return;
            if (myNode.NodeType == XmlNodeType.Attribute)
            {
                // attribues should not overwrite existing? 
                loadNameValueSetting(dict, myNode.Name, myNode.Value, "add", myNode, settingsPolicy, request);
                return;
            }
            int atcount = 0;
            if (myNode.Attributes != null)
            {
                atcount = myNode.Attributes.Count;
                if (myNode.Attributes["xmlns"] != null) atcount = atcount - 1;
            }
            if (myNode.NodeType == XmlNodeType.XmlDeclaration)
            {
                loadSettingNode(dict, myNode.Attributes, new SettingsPolicy(false, onlyIfUnknown), request);
                loadSettingNode(dict, myNode.ChildNodes, settingsPolicy, request);
                return;
            }
            string lower = myNode.Name.ToLower();

            if (myNode.NodeType == XmlNodeType.Document || lower == "#document")
            {
                loadSettingNode(dict, myNode.Attributes, new SettingsPolicy(false, onlyIfUnknown), request);
                loadSettingNode(dict, myNode.ChildNodes, settingsPolicy, request);
                return;
            }
            if (lower == "substitutions")
            {
                //loadSettingNode(dict, myNode.Attributes, false, true, request);
                SettingsDictionaryReal substDict = ToSettingsDictionary(dict);
                string substName = StaticXMLUtils.GetAttribValue(myNode, "name,dict,value", "input");
                SettingsDictionary chdict = request.GetSubstitutions(substName, true);
                foreach (XmlNode n in myNode.ChildNodes)
                {
                    substName = n.Name.ToLower();
                    /// ProgramQ            ProgramD
                    if (substName != "substitution" && substName != "substitute" && !IsNameValueTag(substName))
                    {
                        chdict = request.GetSubstitutions(substName, false);
                        try
                        {
                            if (chdict == null)
                            {
                                chdict = request.GetSubstitutions(substName, true);
                                substDict.writeToLog("Creating substitutions: " + chdict);
                            }
                            loadSettingNode(chdict, n.ChildNodes, settingsPolicy, request);
                            continue;
                        }
                        catch (Exception e)
                        {
                            substDict.writeToLog("ERROR {0}", e);
                            //continue;
                            throw;
                        }
                    }
                    else
                    {
                        /// ProgramD shoukd nbot actually be here
                        loadSettingNode(chdict, n, settingsPolicy, request);
                    }
                }
                return;
            }
            if (myNode.NodeType == XmlNodeType.Element)
            {
                string href = StaticXMLUtils.GetAttribValue(myNode, "href", null);
                if (href != null && href.Length > 0)
                {
                    string name = StaticXMLUtils.GetAttribValue(myNode, "id", myNode.Name);
                    loadNameValueSetting(dict, name, href, "add", myNode, new SettingsPolicy(false, true), request);
                    return;
                }
            }

            if (lower == "bot")
            {
                var p = myNode.ParentNode;
                if (p != null && p.Name.ToLower() == "bots")
                {
                    return; //we are not program D definining bots TODO maybe we will be
                    loadSettingNode(dict, myNode.ChildNodes, settingsPolicy, request);
                    loadSettingNode(dict, myNode.Attributes, new SettingsPolicy(false, false), request);
                    return;
                }
            }

            if (lower == "root" || lower == "vars" || lower == "items" || lower == "properties"
                || lower == "bots" || lower == "testing" || lower == "predicates")
            {
                loadSettingNode(dict, myNode.ChildNodes, settingsPolicy, request);
                loadSettingNode(dict, myNode.Attributes, new SettingsPolicy(false, false), request);
                return;
            }

            if ((lower == "include"))
            {
                bool overwriteExisting = settingsPolicy.overwriteExisting;
                string path = StaticXMLUtils.GetAttribValue(myNode, "path", myNode.InnerText);

                overwriteExisting =
                    Boolean.Parse(StaticXMLUtils.GetAttribValue(myNode, "overwriteExisting", "" + overwriteExisting));

                onlyIfUnknown =
                    Boolean.Parse(StaticXMLUtils.GetAttribValue(myNode, "onlyIfKnown", "" + onlyIfUnknown));

                loadSettingsNow(ToSettingsDictionary(dict),null, path, new SettingsPolicy(overwriteExisting, onlyIfUnknown),  request);
                return;
            }
            SettingsDictionaryReal settingsDict = ToSettingsDictionary(dict);
            if ((lower == "parent" || lower == "override" || lower == "fallback" || lower == "listener"
                || lower == "provider" || lower == "synchon" || lower == "prefixes"
                || lower == "metaproviders" || lower == "formatter"))
            {
                string name = StaticXMLUtils.GetAttribValue(myNode, "value,dict,name", null);
                if (!String.IsNullOrEmpty(name))
                {
                    ParentProvider pp = settingsDict.FindDictionary(name, null);
                    if (pp == null /* || pp() == null*/)
                    {
                        settingsDict.writeToLog("DEBUG9 Cannot ResolveToObject settings line {0} in {1}", name, settingsDict);
                        return;
                    }
                    var pdict = pp();
                    switch (lower)
                    {
                        case "provider":
                            settingsDict.InsertProvider(pp);
                            return;
                        case "parent":
                            settingsDict.InsertFallback(pp);
                            return;
                        case "synchon":
                        case "listener":
                            settingsDict.InsertListener(pp);
                            return;
                        case "fallback":
                            settingsDict.InsertFallback(pp);
                            return;
                        case "override":
                            settingsDict.InsertOverrides(pp);
                            return;
                        case "metaproviders":
                            settingsDict.InsertMetaProvider(pp);
                            return;
                            /*
                        case "prefixes":
                            settingsDict.AddChild(StaticXMLUtils.GetAttribValue(myNode, "prefix,name,dict,value", name), pp);
                            return;*/
                        default:
                            settingsDict.writeToLog("ERROR cannot make a name/v from " + StaticXMLUtils.TextAndSourceInfo(myNode));
                            return;
                    }
                    return;
                }
            }
            if ((lower == "maskedvar"))
            {
                string name = StaticXMLUtils.GetAttribValue(myNode, "name", "");
                if (name == "")
                {
                }
                if (settingsDict == null)
                {
                    ///warned already
                    return;
                }
                settingsDict.maskSetting(name);
                return;

            }
            //<predicates><predicate name="failed" default="" set-return="value"/>
            //<properties><property name="name" value="YourBot"/>
            //<vars><set name="accountability" >gnucash</set>
            //<properties><entry key="programd.aiml-schema.namespace-uri">http://alicebot.org/2001/AIML-1.0.1</entry>
            //<param name="PrintStackTraces" value="false"/>
            //<parameter name="channel" value="#some-channel"/>
            //<input><substitute find="=reply" replace=""/>
            //<substitution><old>:\)</old><new> smile </new></substitution>
            if (IsNameValueTag(lower))
            {

                string name = StaticXMLUtils.GetAttribValue(myNode, "name,var,old,key,find,param", null);
                if (name == null)
                {
                    XmlNode holder = StaticXMLUtils.FindNode("name,var,old,key,find", myNode, null);
                    if (holder == null)
                    {
                        settingsDict.SettingsLog("ERROR cannot make a name/v from " + StaticXMLUtils.TextAndSourceInfo(myNode));
                        return;
                    }
                    name = holder.InnerText;
                }
                string value = StaticXMLUtils.GetAttribValue(myNode, "value,href,default,replace,new,enabled", null);
                if (value == null)
                {
                    XmlNode holder = StaticXMLUtils.FindNode("value,default,replace,new", myNode, null);
                    if (holder != null)
                    {
                        value = holder.InnerXml;
                    }
                }
                if (value == null)
                {
                    string maybe = myNode.InnerXml.Trim();
                    if (maybe != null) value = maybe;
                }
                if (value == null)
                    settingsDict.writeToLog("ERROR cannot make a n/value from " + StaticXMLUtils.TextAndSourceInfo(myNode));

                loadNameValueSetting(dict, name, value, StaticXMLUtils.GetAttribValue(myNode, "type", "add"), myNode,
                            settingsPolicy, request);
                return;
            }
            if (lower == "learn" || lower == "srai" || lower == "aiml" || lower == "that" || lower == "category" || lower == "topic")
            {
                request.Loader.loadAIMLNode(myNode, request.LoadOptions, request);
                return;
            }
            if (myNode.NodeType == XmlNodeType.Element && atcount == 0)
            {
                int cs = myNode.ChildNodes.Count;
                string value = myNode.InnerXml.Trim();
                string itext = myNode.InnerText.Trim();
                if (itext == value)
                {
                    string name = myNode.Name;
                    loadNameValueSetting(dict, name, value, "add", myNode, new SettingsPolicy(false, true), request);
                    return;
                }

            }
            {
                settingsDict.writeToLog("-DICTRACE: ERROR unknow settings line {0} in {1}", StaticXMLUtils.TextAndSourceInfo(myNode), dict);
            }
        }

        private static bool IsNameValueTag(string lower)
        {
            return lower == "item" || lower == "set" || lower == "entry" || lower == "predicate" || lower == "property" ||
            lower == "substitution" || lower == "param" || lower == "parameter" || lower == "substitute";
        }


        public ParentProvider FindDictionary(string name, ParentProvider fallback)
        {
            if (name == null)
            {
                return fallback;
            }
            /*if (name.EndsWith(".prefixProvider"))
            {
                int keylen = name.Length - ".prefixProvider".Length;

                var dict = FindDictionary(name.Substring(0, keylen), fallback);
                if (dict != null)
                {
                    var sd = ToSettingsDictionary(dict());
                    if (sd != null)
                        return ToParentProvider(sd.prefixProvider);
                }
            }*/
            var pp = FindDictionary0(name, fallback);
            if (pp != null) return pp.Invoke;
            Func<ParentProvider> provider0 = () => FindDictionary0(name, fallback);
            return () =>
                       {
                           //new ProvidedSettingsDictionary(name, provider0)
                           throw new NotImplementedException();
                           return null;
                       };
        }

        public ParentProvider FindDictionary0(string name, ParentProvider fallback)
        {
            var rtpbotobjCol = ScriptManager.ResolveToObject(this, name);
            if (rtpbotobjCol == null || rtpbotobjCol.Count == 0)
            {
                string clipIt;
                /*var prep = prefixProvider.GetChildPrefixed(name, out clipIt);
                if (prep != null)
                {
                    if (clipIt == ".") return prep;
                    var pp0 = prep();
                    return prep;
                }*/
                var botGetDictionary = bot.GetDictionary(name);
                if (botGetDictionary != null) return ToParentProvider(botGetDictionary);
                //writeToLog("DEBUG9 Cannot ResolveToObject0 settings line {0} in {1}", name, this);
                //return () => new ProvidedSettingsDictionary(NameSpace + "." + name, () => FindDictionary(name, null).Invoke());
                return fallback;
            }
            //if (tr)
            ParentProvider pp = ToParentProvider(rtpbotobjCol);
            if (pp == null)
            {
                ///warned already
                return fallback;
            }
            return pp;
        }

        public static SettingsDictionaryReal ToSettingsDictionary(object dictionary)
        {
            if (dictionary == null)
            {
                AltBot.writeDebugLine("-DICTRACE: Warning ToSettingsDictionary got NULL");
                return null;
            }
            if (dictionary is SubQuery) dictionary = ((SubQuery)dictionary).TargetSettings;
            if (dictionary is User) dictionary = ((User)dictionary).Predicates;
            SettingsDictionaryReal sd = dictionary as SettingsDictionaryReal;
            if (sd != null) return sd;
            AltBot.writeDebugLine("-DICTRACE: Warning ToSettingsDictionary got type={0} '{1}'",
                                  dictionary.GetType(),
                                  dictionary);
            return null;
        }

        public static ParentProvider ToParentProvider(object dictionary)
        {
            if (dictionary == null)
            {
                AltBot.writeDebugLine("-DICTRACE: Warning ToParentProvider got NULL");
                return null;
            }
            ParentProvider sd = dictionary as ParentProvider;
            if (sd != null) return sd;
            if (dictionary is ISettingsDictionary)
            {
                return (() => (ISettingsDictionary)dictionary);
            }
            if (dictionary is IEnumerable)
            {
                foreach (var VARIABLE in dictionary as IEnumerable)
                {
                    ParentProvider e = ToParentProvider(VARIABLE);
                    if (e != null) return e;
                }
            }
            AltBot.writeDebugLine("-DICTRACE: Warning ToParentProvider got type={0} '{1}'",
                                  dictionary.GetType(),
                                  dictionary);
            return null;
        }

        public void SaveTo(string dir, string rootname, string filename)
        {
            HostSystem.CreateDirectory(dir);
            string tofile = HostSystem.Combine(dir, filename);
            if (fromFile == null) fromFile = tofile;
            HostSystem.BackupFile(tofile);
            XmlDocument xmldoc;
            lock (orderedKeyLock)
            {
                var restore = NameSpace;
                try
                {
                    NameSpace = rootname;
                    xmldoc = DictionaryAsXML;
                }
                finally
                {
                    NameSpace = restore;
                }
            }
            xmldoc.Save(tofile);
        }

        /// <summary>
        /// Adds a bespoke setting to the Settings class (accessed via the grabSettings(string name)
        /// method.
        /// </summary>
        /// <param name="name">The name of the new setting</param>
        /// <param name="value">The value associated with this setting</param>
        public bool addSetting(string name, object value0)
        {
            var value = TransformValueIn(value0);
            foreach (var setname in GetSettingsAliases(name))
            {
                addSetting0(setname, value);
            }
            return addSetting0(name, value);
        }

        private IEnumerable<string> GetSettingsAliases(string name)
        {
            if (NoSettingsAliaes) return NO_SETTINGS;
            string key = TransformKey(TransformName(name));
            string[] aliases;
            if (!AltBot.SettingsAliases.TryGetValue(key, out aliases))
            {
                return NO_SETTINGS;
            }
            return aliases;
        }

        public bool addSetting0(string name, DataUnifiable value)
        {
            bool found = true;
            lock (orderedKeyLock)
            {
                name = TransformName(name);
                string normalizedName = TransformKey(name);
                if (normalizedName == "issubsts")
                {
                    IsSubsts = TextPatternUtils.IsTrue(value);
                }
                if (IsMaskedVar(normalizedName))
                {
                    updateListeners(name, value, true, !found);
                    SettingsLog("ERROR MASKED ADD SETTING '" + name + "'=" + str(value) + " ");
                    return false;
                }
                if (!AllowedNameValue(name, value))
                {
                    SettingsLog("!NameValueCheck ADD Setting Local '" + name + "'=" + str(value) + " ");
                    return true;
                }
                // check blackboard
                if ((isBBPrefixCorrect()))
                {
                    string bbKey = bbPrefix + MakeCaseInsensitive.TransformInput(name).ToLower(); ;
                    this.bot.setBBHash0(bbKey, value);
                }
                SettingsLog("ADD LOCAL '" + name + "'=" + str(value) + " ");
                value = MakeLocalValue(name, value);
                //found = this.removeSettingReal(name);
                if (value != null)
                {
                    this.settingsHash.Add(normalizedName, value);
                    //this.settingsHash.AddKey(name);
                }
                updateListeners(name, value, true, !found);
            }
            return !found;
        }

        private bool isBBPrefixCorrect()
        {
            if (bbPrefix == null) return false;
            if (bbPrefix == "user")
            {
                return rbot.LastUser == null || rbot.LastUser.Predicates == this;
            }
            return true;
        }

        private bool IsMaskedVar(string name)
        {
            lock (maskedVars) if (maskedVars.Contains(name)) return true;
            foreach (ParentProvider list in _overides)
            {
                var p = list();
                if (p != null)
                {
                    if (p.containsLocalCalled(name))
                    {
                        return true;
                    }
                }
            }
            return false;
        }

        private DataUnifiable MakeLocalValue(string name, DataUnifiable value)
        {
            DataUnifiable oldSetting = grabSetting(name);
            bool isCollection = IsCollection(name);            
            if (isCollection)
            {
                string commaVersion = "<li>" + value.ToString() + "</li>";
                string svalue = "";
                if (IsMissing(oldSetting))
                {
                    svalue = "<xor>" + commaVersion + "</xor>";
                }
                else
                {
                    BestUnifiable bunif = (((object)oldSetting) as BestUnifiable);
                    if (bunif != null)
                    {
                        oldSetting = bunif.AddItem(value);
                    }
                    else
                    {
                        var nl = new List<DataUnifiable>();
                        nl.Add(oldSetting);
                        nl.Add(value);
                        oldSetting = new BestUnifiable(nl);
                    }
                    string soldSetting =  rbot.ToValueString(oldSetting);
                    svalue = soldSetting;// "<or>" + commaVersion + soldSetting.Substring(4);
                }
                value = svalue;
                return value;
            }
            return value;
        }

        private bool IsCollection(string name)
        {
            return false;// name == "topic";
        }

        public static string[] unsettableTs =
            {
                "adverbs",
                "atomic",
                "atomic0",
                "atomic1",
                "atomic2",
                "atomic3",
                "biography",
                "biography1",
                "biography2",
                "biography3",
                "bot",
                "bot1",
                "bot2",
                "bot3",
                "botmaster",
                "client",
                "client1",
                "continuations",
                "default",
                "default",
                "default1",
                "default2",
                "default3",
                "default4",
                "default5",
                "default6",
                "general",
                "general1",
                "general2",
                "general3",
                "general4",
                "general5",
                "general6",
                "general7",
                "general8",
                "general9",
                "general10",
                "general11",
                "general12",
                "general13",
                "inquiry",
                "interjection",
                "iu",
                "knowledge",
                "knowledge1",
                "knowledge2",
                "knowledge3",
                "knowledge4",
                "knowledge5",
                "knowledge6",
                "knowledge7",
                "knowledge8",
                "knowledge9",
                "knowledge10",
                "knowledge11",
                "knowledge12",
                "maths",
                "parts",
                "pickup",
                "pickup1",
                "pickup2",
                "predicates",
                "questions",
                "quotes",
                "quotes1",
                "reduce",
                "reducer",
                "reducer1",
                "reductions",
                "salutations",
                "stack",
                "stories",
                "that",
                "that1",
                "that2",
                "that3",
                "that4",
                "that5",
                "that6",
                "topics",
                "words",
                "xfind",
                "ok",
                "that",
                "nothing"
            };

        public static HashSet<string> UnsettableTopic = new HashSet<string>(unsettableTs);
        
        protected bool AllowedNameValue(string name, DataUnifiable value)
        {
            if (name.Length < 1)
            {
                writeToLog(String.Format("! NameValueCheck '{0}' = '{1}'", name, value));
                return false;
            }
            if (name == "topic")
            {
                if (UnsettableTopic.Contains(value.ToLower())) return false;
            }
            string s = (string)value;

            if ((s == null) || s.Contains(">") || s.Contains("_to_") || name == "startgraph")
            {
                writeToLog(String.Format("! NameValueCheck {0} = {1}", name, rbot.ToValueString(value)));
                return !SuspendUpdates;
            }
            if (IsIdentityReadOnly && (name.ToLower() == "name" || name.ToLower() == "id"))
            {
                if (TextPatternUtils.IsUnknown(value) || s.Length < 3 || s.ToLower() == "friend"
                     ||  s.ToLower() == "that really")
                {
                    writeToLog(String.Format("! NameValueCheck {0} = {1}", name, value));
                    return false;
                }
                return !SuspendUpdates;
            }
            return !SuspendUpdates;
        }

        private void updateListeners(string name, DataUnifiable value, bool locally, bool addedNew)
        {
            if (SuspendUpdates) return;
            if (LoopingOn(name, "update"))
            {
                return;
            }
            foreach (var l in Listeners)
            {
                if (addedNew) l.addSetting(name, value);
                else
                    l.updateSetting(name, value);
            }
        }

        public static int generation
        {
            get
            {
                return MushDLR223.ScriptEngines.ScriptManager.Generation;
            }
        }

        [ThreadStatic]
        private Dictionary<string, Dictionary<string, int>> checkingFallbacksOfN = null;
        public bool LoopingOn(string name, string type)
        {
            if (LoopingOn0(name, type))
            {
                if (LoopingOn0(name, type + "1"))
                {
                    if (ScriptManager.GettingDeeper(NameSpace + "." + name, type + "-aiml",
                                                   (new System.Diagnostics.StackTrace(true)).FrameCount))
                    {
                        return true;
                    }
                    return true;
                }
                      
            }
            return false;
        }
        public bool LoopingOn0(string name, string type)
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
            int gen, ggen = generation;
            lock (fallbacksOf)
            {
                if (!fallbacksOf.TryGetValue(name, out gen))
                {
                    fallbacksOf[name] = ggen;
                }
                else if (gen == generation)
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

        public DataUnifiable TransformValueIn(object value)
        {
            var s = TransformValue0(value);
            if (s == null) return Unifiable.NULL;
            if (s == "") return Unifiable.Empty;
            if (Unifiable.IsMissing(value)) 
            {
                if (NoSettingsAliaes) return null;
                return Unifiable.MISSING;
            }
            return s;
        }
        public DataUnifiable TransformValueOut(DataUnifiable value)
        {
            DataUnifiable s = TransformValue0(value);
            if (s == null) return Unifiable.NULL;
            if (s == "OM")
            {
                if (NoSettingsAliaes) return null;
                return Unifiable.MISSING;
            }
            if (s == "")
            {
                return "";// Unifiable.Empty;
            }
            if (s == Unifiable.Empty)
            {
                return "";// 
            }

            return s;
        }

        public DataUnifiable TransformValue0(object value)
        {
            if (Unifiable.IsNull(value))
            {
                // writeToLog("ERROR " + value + " NULL");
                return null;
                //return Unifiable.NULL;
            }

            if (Unifiable.IsEMPTY(value))
            {
                // writeToLog("ERROR " + value + " NULL");
                return "";
                //return Unifiable.NULL;
            }
            if (Unifiable.IsMissing(value))
            {
                //   writeToLog("ERROR " + value + " NULL");
                if (NoSettingsAliaes) return null;
                return "OM";
            }
            string s0 = value.ToString();
            if (s0.Contains("<xor"))
            {
                Console.WriteLine("I am doing someting crazy " + s0);
            }
            string ss = s0.ToUpper();
            if (ss.Contains("TAG-"))
            {
                return Unifiable.EnglishNothing;
            }
            if (Unifiable.IsIncomplete(value))
            {
                //   writeToLog("ERROR " + value + " NULL");
                if (NoSettingsAliaes) return null;
                return Unifiable.INCOMPLETE.AsString();
            }
            if (Unifiable.IsMulti(value))
            {
                return (string) value;
            }
            if (value is Unifiable) return (Unifiable)value;
            if (value is string) return (string)value;
            Type valueTyp = value.GetType();
            if (valueTyp.IsPrimitive) return s0;
            var v = StaticXMLUtils.ValueText((string)value);
            if (false)if (v.Contains("<") || v.Contains("&"))
            {
                writeToLog("!@ERROR BAD INPUT? " + value);
            }
            if (false) if (v.Contains("???"))
            {
                writeToLog("!?????@ERROR BAD INPUT? " + value);
            }
            return v;
        }

        public bool addListSetting(string name, DataUnifiable value)
        {
            lock (orderedKeyLock)
            {
                name = TransformName(name);
                value = TransformValueIn(value);
                string normalizedName = TransformKey(name);
                if (normalizedName.Length > 0)
                {
                    this.removeSetting(name);
                    this.settingsHash.Add(normalizedName, value);
                }
            }
            return true;
        }

        ICollection<string> LocalKeys 
        {
            get { return KeyZ.Keys; }
        }

        protected int orderedKeysCount
        {
            get { lock (KeyZ) return KeyZ.Count; }
        }

        private bool orderedKeysContains(string name)
        {
            lock (KeyZ) return KeyZ.ContainsKey(name);
        }

        private void orderedKeysRemove(string name)
        {
            lock (KeyZ) KeyZ.Remove(name);

        }

        private void orderedKeysClear()
        {
            lock (KeyZ) KeyZ.Clear();
        }


        internal string TransformName(string name)
        {
            return name;
        }

        /// <summary>
        /// Removes the named setting from this class
        /// </summary>
        /// <param name="name">The name of the setting to remove</param>
        public bool removeSetting(string name)
        {
			return removeSettingReal(name);
            return addSetting(name, Unifiable.MISSING);
        }

        public bool removeSettingReal(string name)
        {
            lock (orderedKeyLock)
            {
                if (SuspendUpdates) return true;
                name = TransformName(name);
                string normalizedName = TransformKey(name);
                bool ret = orderedKeysContains(name);
                this.orderedKeysRemove(name);
                // shouldnt need this next one (but just in case)
                this.orderedKeysRemove(normalizedName);
                this.removeFromHash(name);
                if (ret)
                {
                    //maskedettings.Remove(normalizedName);
                }
                return ret;
            }
        }


        public string TransformKey(string name)
        {
            if (TrimKeys) name = name.Trim();
            if (name == "inloop")
            {
            }
            name = name.ToLower();
            name = name.Replace("favorite", "fav");
            name = name.Replace("fav_", "fav");
            name = name.Replace("fav.", "fav");

            if (false) foreach (var k in new[] { "favorite", "fav" })
                {
                    if (name.StartsWith(k))
                    {
                        if (name.Length > k.Length) name = name.Substring(k.Length);
                    }
                }
            if (name == "uname")
            {

            }
            return name;
            //return MakeCaseInsensitive.TransformInput(name);
        }

        /// <summary>
        /// Removes a named setting from the Dictionary<,>
        /// </summary>
        /// <param name="name">the key for the Dictionary<,></param>
        private void removeFromHash(string name)
        {
            lock (orderedKeyLock)
            {
                name = TransformName(name);
                string normalizedName = TransformKey(name);
                this.settingsHash.Remove(normalizedName);
            }
        }

        /// <summary>
        /// Updates the named setting with a new value whilst retaining the position in the
        /// dictionary
        /// </summary>
        /// <param name="name">the name of the setting</param>
        /// <param name="value">the new value</param>
        public bool updateSetting(string name, object value0)
        {
            if (SuspendUpdates) return true;
            var value = TransformValueIn(value0);
            foreach (var setname in GetSettingsAliases(name))
            {
                updateSetting0(setname, value);
            }
            return updateSetting0(name, value);
        }
        public bool updateSetting0(string name, DataUnifiable value)
        {
            bool overriden = false;
            foreach (var p in Overides)
            {
                if (p.updateSetting(name, value))
                {
                    SettingsLog("OVERRIDDEN UPDATE " + p + " '" + name + "'=" + str(value));
                    overriden = true;
                }
            }
            if (overriden)
            {
                return true;
            }
            lock (orderedKeyLock)
            {
                name = TransformName(name);
                string normalizedName = TransformKey(name);
                if (this.settingsHash.ContainsKey(normalizedName))
                {
                    var old = this.settingsHash.GetValue(normalizedName);

                    updateListeners(name, value, true, false);
                    if (IsMaskedVar(normalizedName))
                    {
                        SettingsLog("MASKED Not Update Local '" + name + "'=" + str(value) + " keeped " + str(old));

                    }
                    else
                    {
                        if (AllowedNameValue(name, value))
                        {
                            value = MakeLocalValue(name, value);
                            if (value == old) return true;
                            this.removeFromHash(name);
                            SettingsLog("UPDATE Setting Local '" + name + "'=" + str(value));
                            this.settingsHash.Add(normalizedName, value);
                            // check blackboard
                            if ((isBBPrefixCorrect()) && (this.bot.myChemistry != null))
                            {
                                string bbKey = bbPrefix + MakeCaseInsensitive.TransformInput(name).ToLower(); ;
                                this.bot.setBBHash0(bbKey, value);
                            }
                            SettingsLog("ADD LOCAL '" + name + "'=" + str(value) + " ");
                            return true;
                        }
                        else
                        {
                            SettingsLog("NOT_UPDATE Setting Local '" + name + "'=" + str(value));
                        }
                    }
                }
                // before fallbacks
                if (IsMaskedVar(normalizedName))
                {
                    SettingsLog("MASKED NOT UPDATE FALLBACKS '" + name + "'=" + str(value));
                    return false;
                }
            }
            foreach (var parent in Fallbacks)
            {
                if (parent.updateSetting(name, value))
                {
                    SettingsLog("PARENT UPDATE " + parent + " '" + name + "'=" + str(value));
                    return true;
                }
            }
            return false;
        }

        public string str(DataUnifiable value)
        {
            value = TransformValueOut(value);
            return "'" + Unifiable.DescribeUnifiable(value) + "'";
        }

        /// <summary>
        /// Clears the dictionary to an empty state
        /// </summary>
        public void clearSettings()
        {
            lock (orderedKeyLock)
            {
                this.orderedKeysClear();
                this.settingsHash.Clear();
            }
        }
        public void clearHierarchy()
        {
            lock (orderedKeyLock)
            {
                _overides.Clear();
                _fallbacks.Clear();
              //  _fallbacks.Add(() => prefixProvider);
                lock (maskedVars) maskedVars.Clear();
            }
        }

        public void clearSyncs()
        {
            _listeners.Clear();
            //_listeners.Add(() => prefixProvider);
        }

        private HashSet<string> maskedVars = new HashSet<string>();
        public void maskSetting(string name)
        {
            name = TransformName(name);
            name = TransformKey(name);
            writeToLog("MASKING: " + name);
            lock (maskedVars) maskedVars.Add(name);
        }
        /// <summary>
        /// Returns the value of a setting given the name of the setting
        /// </summary>
        /// <param name="name">the name of the setting whose value we're interested in</param>
        /// <returns>the value of the setting</returns>
        public string grabSetting(string name, bool useBlackboad)
        {
            return grabSetting0(name, useBlackboad);
        }
        /// <summary>
        /// Returns the value of a setting given the name of the setting
        /// </summary>
        /// <param name="name">the name of the setting whose value we're interested in</param>
        /// <returns>the value of the setting</returns>
        public DataUnifiable grabSetting(string name)
        {
            try
            {
                name = TransformName(name);
                var setting = grabSetting0(name, true);
                var setting0 = TransformValueOut(setting);
                return setting0;
            }
            catch (Exception e)
            {
                writeToLog("ERROR {0}", e);

                return default(DataUnifiable);
            }
        }

        public DataUnifiable grabSettingOrDefault(string name, string fallback)
        {
            //HashSet<ISettingsDictionary> noGo = new HashSet<ISettingsDictionary>() { this };
            foreach (ParentProvider overide in GraphMaster.CopyOf(_overides))
            {
                var dict = overide();
              //  if (!noGo.Add(dict)) continue;
                if (dict.containsLocalCalled(name))
                {
                    DataUnifiable v = grabSettingOrDefault(dict, name, fallback);
                    SettingsLog("OVERRIDE '{0}'='{1}'", name, str(v));
                    return v;
                }
            }
            lock (orderedKeyLock)
            {
                string normalizedName = TransformKey(name);

                if (this.settingsHash.ContainsKey(normalizedName))
                {
                    string v = this.settingsHash.GetValue(normalizedName);
                    if (IsMaskedVar(normalizedName))
                    {
                        SettingsLog("MASKED RETURNLOCAL '" + name + "=NULL instead of" + str(v));
                        return null;
                    }
                    SettingsLog("LOCALRETURN '" + name + "'=" + str(v));
                    return v;
                }
                else if (Fallbacks.Count > 0)
                {
                    ISettingsDictionary firstFallBack = null;
                    bool returnIt;
                    DataUnifiable u = CheckFallbacks(Fallbacks, name, normalizedName, ref firstFallBack, out returnIt);
                    if (returnIt) return u;
                    if (firstFallBack != null)
                    {
                        string v0 = firstFallBack.grabSetting(name);
                        if (!IsMissing(v0))
                        {
                            SettingsLog("RETURN FALLBACK0 '" + name + "'=" + str(v0));
                            return v0;
                        }
                    }
                }
                SettingsLog("MISSING '" + name + "'");
                return fallback;

            }
        }

        private DataUnifiable CheckFallbacks(IEnumerable<ISettingsDictionary> fallbacks, string name, string normalizedName, ref ISettingsDictionary firstFallBack, out bool returnIt)
        {
            if (LoopingOn(name, "fallback"))
            {
                returnIt = false;
                return null;
            }
            foreach (ISettingsDictionary list in fallbacks)
            {
               // if (!noGo.Add(list)) continue;
                firstFallBack = firstFallBack ?? list;
                object watch = LockInfo.Watch(list);
                lock (watch)
                {
                    bool prev = list.IsTraced;
                    list.IsTraced = false;
                    DataUnifiable v = grabSettingOrDefault(list, name, null);
                    ;
                    if (v == null) continue;

                    if (IsMaskedVar(normalizedName))
                    {
                        SettingsLog("MASKED PARENT '" + name + "=NULL instead of" + str(v));
                        list.IsTraced = prev;
                        returnIt = true;
                        return null;
                    }
                    SettingsLog("RETURN FALLBACK '" + name + "'=" + str(v));
                    list.IsTraced = prev;
                    if (!TextPatternUtils.IsFalse(v))
                    {
                        returnIt = true;
                        return v;
                    }
                }
            }
            returnIt = false;
            return null;
        }

        private DataUnifiable grabSettingOrDefault(ISettingsDictionaryT<DataUnifiable> dictionary, string name, DataUnifiable o)
        {
            if (dictionary.containsSettingCalled(name)) { return dictionary.grabSetting(name);}
            return o;
        }

        public DataUnifiable grabSetting0(string name, bool useBlackboard)
        {
            //string normalizedName = MakeCaseInsensitive.TransformInput(name);
            string normalizedName = TransformKey(name);
            // check blackboard
            if (useBlackboard)
            {
                if ((isBBPrefixCorrect()) && (this.bot.myChemistry != null))
                {
                    string bbKey = bbPrefix + normalizedName.ToLower();
                    if (!bbKey.Contains(" "))
                    {
                        string bbValue = this.bot.getBBHash0(bbKey);
                        //Console.WriteLine("*** grabSetting from BB : {0} ={1}", bbKey, bbValue);
                        if (bbValue.Length > 0)
                        {
                            // update local value
                            if (this.orderedKeysContains(normalizedName))
                            {
                                this.removeFromHash(normalizedName);
                                this.settingsHash.Add(normalizedName, bbValue);
                            }
                            return bbValue;
                        }
                    //Console.WriteLine("*** grabSetting use internal: {0}",name);
                    }
                }
            }

            this.mayUseOverides = true;
            if (LoopingOn(name, "override"))
            {
                mayUseOverides = false;
            }
            //HashSet<ISettingsDictionary> noGo = new HashSet<ISettingsDictionary>() {this};
            if (this.mayUseOverides)
            {
                foreach (ParentProvider overide in GraphMaster.CopyOf(_overides))
                {
                    var dict = overide();
                    //if (!noGo.Add(dict)) continue;
                    if (dict.containsSettingCalled(name))
                    {
                        DataUnifiable v = dict.grabSetting(name);
                        SettingsLog("OVERRIDE '" + name + "'=" + str(v));
                        return v;
                    }
                }
            }
            bool isMaskedVar = IsMaskedVar(name);
            bool needsUnlock = System.Threading.Monitor.TryEnter(orderedKeyLock, TimeSpan.FromSeconds(2));
            //string normalizedName = TransformKey(name);
            // check blackboard
            if ((isBBPrefixCorrect()) && (this.bot.myChemistry != null))
            {
                string bbKey = bbPrefix + normalizedName.ToLower();
                if (!bbKey.Contains(" "))
                {
                    string bbValue = this.bot.getBBHash0(bbKey);
                    //Console.WriteLine("*** grabSetting from BB : {0} ={1}", bbKey, bbValue);
                    if (bbValue.Length > 0 && false)
                    {
                        // update local value
                        if (this.orderedKeysContains(normalizedName))
                        {
                            this.removeFromHash(normalizedName);
                            this.settingsHash.Add(normalizedName, bbValue);
                        }
                        return bbValue;
                    }

                    //Console.WriteLine("*** grabSetting use internal: {0}",name);
                }
            } 
            try
            {
                if (containsLocalCalled0(name))
                {
                    if (isMaskedVar) throw new InvalidOperationException("conatins a masked var!");
                    DataUnifiable v = localValue(name, normalizedName);
                    if (IsMissing(v))
                    {
                        foreach (var setname in GetSettingsAliases(name))
                        {
                            if (containsLocalCalled0(name))
                            {
                                var tsetting = localValue(setname, setname);
                                if (!IsMissing(tsetting))
                                    return tsetting;
                            }
                        }
                        return null;
                    }
                    return v;
                }
                if (Fallbacks.Count > 0)
                {
                    ISettingsDictionary firstFallBack = null;

                    bool returnIt;
                    DataUnifiable u = CheckFallbacks(Fallbacks, name, normalizedName, ref firstFallBack, out returnIt);
                    if (returnIt)
                    {
                        if (isMaskedVar && !Unifiable.IsNull(u))
                        {
                            throw new InvalidOperationException("conatins a masked var too!");
                        } 
                        return u;
                    }
                    if (firstFallBack != null)
                    {
                        var v0 = firstFallBack.grabSetting(name);
                        if (!IsMissing(v0))
                        {
                            if (isMaskedVar) throw new InvalidOperationException("conatins a masked var three!"); 
                            SettingsLog("RETURN FALLBACK0 '" + name + "'=" + str(v0));
                            return v0;
                        }
                    }
                }
                if (IsSubsts) return Unifiable.MISSING;
                if (name == "maxlogbuffersize")
                {
                }
                SettingsLog("MISSING '" + name + "'");
                return Unifiable.MISSING;
            }
            finally
            {
                if (needsUnlock) System.Threading.Monitor.Exit(orderedKeyLock);
            }
        }

        public static bool IsMissing(object tsetting)
        {
            return TextPatternUtils.IsIncomplete(tsetting);
        }

        private DataUnifiable localValue(string name, string normalizedName)
        {
            if (this.settingsHash.ContainsKey(normalizedName))
            {
                DataUnifiable v = this.settingsHash.GetValue(normalizedName);
                v = TransformValueOut(v);
                if (IsMaskedVar(normalizedName))
                {
                    SettingsLog("MASKED RETURNLOCAL '" + name + "=NULL instead of" + str(v));
                    return null;
                }
                this.IsTraced = false;
                SettingsLog("LOCALRETURN '" + name + "'=" + str(v));
                return v;
            }
            return null;
        }

        public void SettingsLog(string message, params object[] args)
        {
            if (message.Contains("ERROR") && !message.Contains("ERROR: The requ"))
            {
                //IsTraced = true;
                writeToLog("DICTLOG: " + NameSpace + "  " + message, args);
                return;
            }
            string fmt = TextPatternUtils.SafeFormat(message, args);
            if (false && fmt.Contains("???") /*|| fmt.Contains(" 'name'='")*/)
            {
                writeToLog("ERROR DICTLOG ???????: " + NameSpace + " (" + fmt + ")   " + message, args);
            }
            if (!IsTraced) return;
            IsTraced = false;
            var fc = new StackTrace().FrameCount;
            writeToLog("DICTLOG: " + NameSpace + " (" + fc + ")   " + message, args);
            if (fc > 200)
            {
                //throw new 
                writeToLog("ERROR DICTLOG OVERFLOWING: " + NameSpace + " (" + fc + ")   " + message, args);
                //Console.ReadLine();
            }
        }

        /// <summary>
        /// Checks to see if a setting of a particular name exists
        /// </summary>
        /// <param name="name">The setting name to check</param>
        /// <returns>Existential truth value</returns>
        public bool containsLocalCalled(string name)
        {
            if (containsLocalCalled0(name)) return true;
            foreach (var setname in GetSettingsAliases(name))
            {
                if (containsLocalCalled0(setname)) return true;
            }
            return false;
        }
        public bool containsLocalCalled0(string name)
        {
            name = TransformName(name);
            string normalizedName = TransformKey(name);

            if (IsMaskedVar(normalizedName)) return false;

            if (normalizedName.Length > 0)
            {
                bool needsUnlock = System.Threading.Monitor.TryEnter(orderedKeyLock, TimeSpan.FromSeconds(2));
                try
                {
                    return settingsHash.ContainsKey(normalizedName);
                }
                finally
                {
                    if (needsUnlock) System.Threading.Monitor.Exit(orderedKeyLock);
                }
            }
            return false;
        }

        public bool containsSettingCalled(string name)
        {
            if (containsLocalCalled(name)) return true;
            foreach (ISettingsDictionary dictionary in Overides)
            {
                if (dictionary.containsLocalCalled(name)) return true;
            }
            if (LoopingOn(name, "contains"))
            {
                return false;
            }
            foreach (ISettingsDictionary dictionary in Fallbacks)
            {
                if (dictionary.containsSettingCalled(name)) return true;
            }
            return false;
        }

        /// <summary>
        /// Returns a collection of the names of all the settings defined in the dictionary
        /// </summary>
        /// <returns>A collection of the names of all the settings defined in the dictionary</returns>
        public IEnumerable<string> SettingNames(ICollectionRequester requester, int depth)
        {
            //       get
            {
                lock (orderedKeyLock)
                {
                    /*IEnumerable<string> prefixProviderSettingNames = prefixProvider.SettingNames(requester, depth);
                    var list = prefixProviderSettingNames as List<string>;
                    if (list == null)
                    {
                        list = new List<string>();
                        list.AddRange(prefixProviderSettingNames);
                    }
                    if (list.Count > 0)
                    {
                        list.AddRange(orderedKeys);
                        return list.ToArray();
                    }*/
                    List<string> list = new List<string>();
                    int i = 0;
                    foreach (var s in LocalKeys)
                    {
                        list.Add(s);
                    }
                    return list;
                }
            }
        }

        protected object orderedKeyLock
        {
            get
            {
               // return new object();
                return LockInfo.Watch(KeyZ);
            }
        }

        public List<ISettingsDictionary> Fallbacks
        {
            get
            {
                return ProvidersFrom(_fallbacks);
            }
        }


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
            return containsSettingCalled(key);
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
        public void Add(string key, DataUnifiable value)
        {
            addSetting(key, value);
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
            return removeSetting(key);
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
        public bool TryGetValue(string key, out DataUnifiable value)
        {
            value = grabSetting(key);
            return !IsMissing(value);
        }

        public DataUnifiable this[string name]
        {
            get { return IndexGet(this, name); }
            set { IndexSet(this, name, value); }
        }

        /// <summary>
        /// Gets an <see cref="T:System.Collections.Generic.ICollection`1"/> containing the keys of the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.Generic.ICollection`1"/> containing the keys of the object that implements <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </returns>
        public ICollection<string> Keys
        {
            get { lock (orderedKeyLock) return LocalKeys; }
        }

        /// <summary>
        /// Gets an <see cref="T:System.Collections.Generic.ICollection`1"/> containing the values in the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.Generic.ICollection`1"/> containing the values in the object that implements <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </returns>
        public ICollection<DataUnifiable> Values
        {
            get { return settingsHash.Values; }
        }

        public static void IndexSet(ISettingsDictionary dictionary, string name, DataUnifiable value)
        {
            if (Unifiable.IsIncomplete(value))
            {
                AltBot.writeDebugLine("IndexSet IsIncomplete " + name);
                //return;
            }
            dictionary.addSetting(name, value);
        }

        public static DataUnifiable IndexGet(ISettingsDictionary dictionary, string name)
        {
            return dictionary.grabSetting(name);
        }

        public static IEnumerable<string> NO_SETTINGS = new string[0];
        public static IEnumerable<string> TOO_DEEP = new string[0];
        private bool _isSubsts;
        public bool IsSubsts
        {
            get { return _isSubsts; }
            set
            {
                _isSubsts = value;
                if (value) TrimKeys = false;
            }
        }


        public void InsertMetaProvider(ParentProvider pp)
        {
            AddSettingToCollection(pp, MetaProviders);
        }

        public void AddSettingToCollection(ParentProvider pp, List<ParentProvider> cols)
        {
            AddSettingToCollection(null, pp, cols);
        }
        public void AddSettingToCollection(ISettingsDictionary dictionary, List<ParentProvider> cols)
        {
            AddSettingToCollection(dictionary, null, cols);
        }

        public void AddSettingToCollection(ISettingsDictionary dictionary, ParentProvider pp, List<ParentProvider> cols)
        {
            dictionary = GetDictionary(dictionary, pp);
            if (CheckAddToCollection(dictionary, pp, cols))
            {
                if (pp == null) pp = () => dictionary;
                if (cols.Count > 1000)
                {
                    return;
                }
                cols.Insert(0, pp);
            }
        }

        private ISettingsDictionary GetDictionary(ISettingsDictionary dictionary, ParentProvider pp)
        {
            if (dictionary == null)
            {
                if (pp == null)
                {
                    writeToLog("ERROR: should not place NULL inside self");
                    return null;
                }
                dictionary = (ISettingsDictionary)pp();
                if (dictionary == null && WarnOnNull)
                {
                    writeToLog("WARN: NULL inside pp");
                    return null;
                }
            }
            return dictionary;
        }

        public bool CheckAddToCollection(ISettingsDictionary dictionary, ParentProvider pp, List<ParentProvider> cols)
        {
            dictionary = GetDictionary(dictionary, pp);
            if (dictionary == null)
            {
                if (pp == null)
                {
                    writeToLog("ERROR: NULL Provider and IDictionary");
                    return false;
                }
                if (WarnOnNull)
                {
                    writeToLog("WARNING: NULL Dictionary");
                }
            }
            if (dictionary == this)
            {
                writeToLog("ERROR: should not place inside self");
                return false;
            }
            object watch = LockInfo.Watch(cols);
            lock (watch)
            {
                foreach (var deep in cols)
                {
                    if (deep == pp)
                    {
                        return false;
                    }
                    var inner = deep();
                    if (inner == null)
                    {
                        writeToLog("WARN: NULL Parent IDictionary " + dictionary);
                        continue;
                    }
                    if (inner == dictionary)
                    {
                       // writeToLog("WARN: alread contains inner " + inner);
                        //cols.Remove(deep);
                        return false;
                    }
                    if (inner == this)
                    {
                        writeToLog("WARN: this was found in inner " + inner);
                        return false;
                    }
                    if (inner is SettingsDictionaryReal)
                    {
                        var sd = (SettingsDictionaryReal)inner;
                        if (!sd.CheckAddToCollection(dictionary, pp, sd._listeners))
                        {
                            writeToLog("WARN: this was found in inner fallbacks " + inner);
                            return false;
                        }
                    }
                }
                return true;
            }
        }

        /// <summary>
        /// //"$bot feels $value emotion towards $user";
        /// </summary>
        public String DefaultFormatter = "$subject $relation is $value";  //default
        /// <summary>
        /// $user $relation $value   $robot  $dict
        ///   1      2        3       4       5 
        /// </summary>
        /// <param name="relation"></param>
        /// <returns></returns>
        public string GetMeta(string relation, string meta)
        {
            string realName;
            return WithProviders(this, relation, meta, out realName,
                                 MetaProviders,
                                 (realName0) => DefaultFormatter.Replace("$relation", realName0));
        }

        public static DataUnifiable WithProviders(ISettingsDictionary dictionary, string name, string props, out string realName,
            IEnumerable<ParentProvider> providers, Func<string, DataUnifiable> Else)
        {
            //SettingsDictionary dictionary = ToSettingsDictionary(dictionary0);
            realName = name;
            foreach (var provider in ProvidersFrom(providers, dictionary))
            {
                var v = provider.grabSetting(name);
                if (!IsMissing(v)) return v;
            }
            if (name.Contains(","))
            {
                foreach (string name0 in StaticXMLUtils.NamesStrings(name))
                {
                    var un = WithProviders(dictionary, name0, props, out realName, providers, Else);
                    if (!IsMissing(un))
                    {
                        return un;
                    }
                    if (dictionary.containsLocalCalled(name))
                    {
                        realName = name;
                    }
                }
                return Else(realName);
            }
            return null;
        }

        public string Preposition = "";
        public List<ParentProvider> MetaProviders = new List<ParentProvider>();
        public static bool NoSettingsAliaes = true;
        public static bool UseUndoPush = false;
        private bool mayUseOverides;
        public string bbPrefix { get; set; }

        public DataUnifiable GetSetReturn(string name, out string realName)
        {
            const string prop = "set-return";
            return WithProviders(this, name, prop, out realName, MetaProviders,
                                 realname =>
                                 {
                                     string prep = Preposition;
                                     return (String.IsNullOrEmpty(prep) ? "" : prep + " ")
                                            + realname;
                                 });
        }

        private List<ISettingsDictionary> ProvidersFrom(IEnumerable<ParentProvider> providers)
        {
            return ProvidersFrom(providers, this);
        }

        static List<ISettingsDictionary> ProvidersFrom(IEnumerable<ParentProvider> providers, object exceptFor)
        {
            var icol = (ICollection<ParentProvider>)providers;
            if (icol.Count > 1000)
            {

            }
            var found = new List<ISettingsDictionary>();
            object watch = LockInfo.Watch(providers);
            lock (watch)
            {
                foreach (var list in providers)
                {
                    ISettingsDictionary res = (ISettingsDictionary) list();
                    if (res == null)
                    {
                        //writeToLog("NULL provider " + list);
                        continue;
                    }
                    if (res == exceptFor)
                    {
                        //writeToLog("ERROR: Circular provider " + list);
                        continue;
                    }
                    found.Add(res);
                }
            }
            return found;
        }

        private void addMetaValue(string name, string subprops, string value)
        {
            AddMetaProviders(name, subprops, value, MetaProviders);
        }

        private void AddMetaProviders(string name, string subprops, string value, IEnumerable<ParentProvider> providers)
        {
            foreach (ISettingsDictionary s in ProvidersFrom(providers))
            {
                string np = name + "." + subprops;
                var v = s.grabSetting(np);
                s.addSetting(np, value);
            }
        }

        /// <summary>
        /// Copies the values in the current object into the SettingsDictionary passed as the target
        /// </summary>
        /// <param name="target">The target to recieve the values from this SettingsDictionary</param>
        public void Clone(ISettingsDictionary target)
        {
            var dt = ToSettingsDictionary(target);
            if (dt == this)
            {
                return;
            }
            lock (LockInfo.Watch(MetaProviders)) foreach (var pp in MetaProviders)
                {
                    dt.InsertMetaProvider(pp);
                }
            lock (orderedKeyLock)
            {
                foreach (string name in LocalKeys)
                {
                    target.addSetting(name, grabSetting(name));
                }
            }
        }
        /// <summary>
        /// Copies the values in the current object into the SettingsDictionary passed as the target
        /// If the keys are missing
        /// </summary>
        /// <param name="target">The target to recieve the values from this SettingsDictionary</param>
        static public void AddMissingKeys(ISettingsDictionary source, ISettingsDictionary target, ICollectionRequester requester)
        {
            // lock (orderedKeyLock)
            {
                foreach (string name in source.SettingNames(requester, 0))
                {
                    if (target.containsLocalCalled(name)) continue;
                    target.addSetting(name, source.grabSetting(name));
                }
            }
        }

        #endregion

        public void addObjectFields(Object obj)
        {
            foreach (var hash in obj.GetType().GetProperties())
            {
                addObjectProperty(obj, hash);
            }
        }

        public void AddObjectProperty(object o, String name)
        {
            addGetSet(new ObjectPropertyDictionary<DataUnifiable>(name, name, o));
        }


        private void addObjectProperty(object o, PropertyInfo info)
        {
            string name = info.Name;
            addGetSet(new ObjectPropertyDictionary<DataUnifiable>(name, name, o));
        }

        private void addGetSet(ObjectPropertyDictionary<DataUnifiable> o)
        {
            InsertProvider(() => { return o; });
        }

        public void InsertFallback(ParentProvider provider)
        {
            var providerD = provider();
            string pn = providerD.NameSpace;
            settingsHash.AddFallback(pn);
            AddSettingToCollection(provider, _fallbacks);
        }

        public void InsertListener(ParentProvider provider)
        {
            AddSettingToCollection(provider, _listeners);
        }

        public void InsertOverrides(ParentProvider provider)
        {
            AddSettingToCollection(provider, _overides);
        }

        public void InsertProvider(ParentProvider provider)
        {
            AddSettingToCollection(provider, _listeners);
            AddSettingToCollection(provider, _fallbacks);
        }
        

        /*
         * public void AddChild(string prefix, ParentProvider dict)
        {
            //ISettingsDictionary sdict = (ISettingsDictionary)dict();
            throw new NotImplementedException();
            //prefixProvider.AddChild(prefix, dict);
        }*/

        /*public void AddGetSetProperty(string topic, GetUnifiable<DataUnifiable> getter, Action<DataUnifiable> setter)
        {
            GetSetDictionary<DataUnifiable> prov = new GetSetDictionary<DataUnifiable>(topic, new GetSetProperty<DataUnifiable>(getter, setter));
            InsertProvider(() => prov);
        }

        internal void AddGetSetProperty(string p, CollectionProperty<DataUnifiable> v)
        {
            GetSetDictionary<DataUnifiable> prov = new GetSetDictionary<DataUnifiable>(p, v.GetProvider());
            InsertProvider(() => prov);
        }
        
        public DataUnifiable grabSetting(string name)
        {
            return WithoutTrace(this, () => grabSetting(name));
        }
        */
        public static DataUnifiable grabSettingDefault(ISettingsDictionary dictionary, string name, out string realName, SubQuery query)
        {
            bool succeed;
            return NamedValuesFromSettings.GetSettingForType(dictionary.NameSpace, query, dictionary, name,
                out realName, name, null, out succeed, null);
        }
        public static DataUnifiable grabSettingDefaultDict(ISettingsDictionary dictionary, string name, out string realName)
        {
            realName = name;
            var un = dictionary.grabSetting(name);
            if (IsMissing(un))
            {
                if (name.Contains(","))
                {
                    foreach (string name0 in StaticXMLUtils.NamesStrings(name))
                    {
                        un = grabSettingDefaultDict(dictionary, name0, out realName);
                        if (!IsMissing(un))
                        {
                            return un;
                        }
                    }
                    return un;
                }
                int intLen = name.Length;
                string[] chops = new string[] { "favorite.", "favorite", "fav" };
                foreach (var chop in chops)
                {
                    int chopLength = chop.Length;
                    if (chopLength >= intLen) continue;
                    if (name.StartsWith(chop))
                    {
                        string newName = name.Substring(chopLength);
                        string realName0;
                        DataUnifiable withChop = grabSettingDefaultDict(dictionary, newName, out realName0);
                        if (withChop != null)
                        {
                            realName = realName0;
                            return withChop;
                        }
                    }
                }
                foreach (var chop in chops)
                {

                    string realName0 = chop + name;
                    if (dictionary is SettingsDictionary)
                    {
                        SettingsDictionary sd = (SettingsDictionary)dictionary;
                        un = sd.grabSetting(realName0);
                    }
                    else
                    {
                        un = dictionary.grabSetting(realName0);
                    }
                    if (!IsMissing(un))
                    {
                        realName = realName0;
                        return un;
                    }
                }
            }
            return un;
        }
        /*
        public void AddPrefix(string prefix, ParentProvider dict)
        {
            prefixProvider.AddChild(prefix, dict);
        }
        */
        public static bool TryGetValue<T>(IDictionary<string, T> dictionary, string search, out T value)
        {
            object watch = LockInfo.Watch(dictionary);
            lock (watch)
            {
                if (dictionary.TryGetValue(search, out value))
                {
                    return true;
                }
                value = default(T);
                foreach (var kv in dictionary)
                {
                    if (StaticXMLUtils.SearchStringMatches(search, kv.Key))
                    {
                        value = kv.Value;
                        return true;
                    }
                }
            }
            return false;
        }

        public static bool removeSettingWithUndoCommit(SubQuery query, ISettingsDictionary dict, string name)
        {

            bool locally = dict.containsLocalCalled(name);
            string debugStr = String.Format("REMOVE {0} {1}", dict, name);
            if (UseUndoPush)
            {
                UndoStack.GetStackFor(query).pushValues(dict, name, null);
            }
            else
            {
                DataUnifiable prevSetting = dict.grabSetting(name);
                if (dict.removeSetting(name))
                {
                    if (locally) query.AddUndo("undoing " + debugStr, () => dict.addSetting(name, prevSetting));
                    else query.AddUndo("undoing " + debugStr, () => dict.updateSetting(name, prevSetting));
                }
            }
            query.AddSideEffect(debugStr, () => dict.removeSetting(name));
            return locally;
        }

        public static bool addSettingWithUndoCommit(SubQuery query, ISettingsDictionary dict, Func<string, object, bool> SideEffect, string name, object newValue)
        {
            bool locally = dict.containsLocalCalled(name);
            string debugStr = String.Format("ADD SETTING {0} {1} {2}", dict, name, newValue);
            if (UseUndoPush)
            {
                UndoStack.GetStackFor(query).pushValues(dict, name, null);
            }
            else
            {
                DataUnifiable prevSetting = dict.grabSetting(name);
                bool res = SideEffect(name, newValue);
                query.AddUndo("undo " + debugStr, () =>
                                  {
                                      var now = dict.grabSetting(name);
                                      if (now == newValue && now != prevSetting)
                                      {
                                          if (!locally)
                                          {
                                              dict.removeSetting(name);
                                          }
                                          else
                                          {
                                              if (locally) dict.addSetting(name, prevSetting);
                                          }
                                          if (!IsMissing(prevSetting))
                                          {
                                              dict.updateSetting(name, prevSetting);
                                          }
                                      }
                                  });
            }
            if (newValue.ToString().Contains(">"))
            {
                
            }
            query.AddSideEffect("SIDE EFFECT " + debugStr, () => SideEffect(name, newValue));
            return !locally;
        }

        public static T WithUndoCommit<T>(SubQuery query, ISettingsDictionary dict, Func<T> SideEffect, string name, DataUnifiable newValue)
        {
            bool locally = dict.containsLocalCalled(name);
            DataUnifiable prevSetting = dict.grabSetting(name);
            string debugStr = String.Format("ADD SETTING {0} {1} {2}", dict, name, newValue);
            T res = SideEffect();
            query.AddUndo("undo " + debugStr,() =>
                              {
                                  var now = dict.grabSetting(name);
                                  if (now == newValue && now != prevSetting)
                                  {
                                      if (!locally)
                                      {
                                          dict.removeSetting(name);
                                      }
                                      else
                                      {
                                          if (locally) dict.addSetting(name, prevSetting);
                                      }
                                      if (!IsMissing(prevSetting))
                                      {
                                          dict.updateSetting(name, prevSetting);
                                      }
                                  }
                              });
            query.AddSideEffect(debugStr, () => SideEffect());
            return res;
        }

        public bool DoSettingsCommand(string input, OutputDelegate console)
        {
            if(input=="")
            {
                console(ToDebugString());
                return true;
            }
            input = input + " ";
            int firstWhite = input.IndexOf(' ');
            string var = input.Substring(0, firstWhite).Trim();
            string value = input.Substring(firstWhite + 1).Trim();
            if (value == "")
            {
                console(var + " = " + (grabSetting(var) ?? " <NULL> "));
                return true;
            }
            console("addSetting: " + addSetting(var, value));
            return true;
        }

        #region ICollectionProvider Members

        public ICollection GetGroup(ICollectionRequester requester, string name)
        {
            return SingleNameValue.AsCollection(grabSetting(name));
        }

        #endregion

        #region Implementation of ICollectionProviderSettable

        public void SetValue(ICollectionRequester requester, string name, object value)
        {
            updateSetting(name, Unifiable.Create(value));
        }

        public bool AcceptsNewKeys
        {
            get { return true; }
        }

        public IEnumerable<string> SettingNames0
        {
            get { return Keys; }
        }

        #endregion

        public static void AddPseudonym(ISettingsDictionary dictionary, string key)
        {
            var dict = ToSettingsDictionary(dictionary);
            if (dict == null)
            {
                return;
            }
            if (dict.AddName(key))
            {
                dict.AddMonitorPath(key + ".xml", SettingsPolicy.Default);
            }
        }

        public List<string> Names = new List<string>();

        private bool AddName(string key)
        {
            key = TransformKey(key);
            lock (Names)
            {
                if (Names.Contains(key)) return false;
                Names.Add(key);
            }
            return true;
        }

        public Dictionary<string, SettingsPolicy> MonitorPaths = new Dictionary<string, SettingsPolicy>();
        public static bool WarnOnNull;

        private bool AddMonitorPath(string key, SettingsPolicy policy)
        {
            lock (MonitorPaths)
            {
                if (MonitorPaths.ContainsKey(key)) return false;
                MonitorPaths.Add(key, policy);
            }
            return true;
        }
    }

    public class SettingsPolicy
    {
        public SettingsPolicy(bool oE, bool oU)
        {
            overwriteExisting = oE;
            onlyIfUnknown = oU;
        }

        public static SettingsPolicy Default = new SettingsPolicy(true, false);
        public bool overwriteExisting = true;
        public bool onlyIfUnknown = false;
    }

    public class SettingsDictionaryEnumerator : IEnumerator<KeyValuePair<string, DataUnifiable>>
    {
        private readonly ISettingsDictionaryT<DataUnifiable> Root;
        private readonly IEnumerator<string> keysE;
        public SettingsDictionaryEnumerator(IEnumerable<string> keys, ISettingsDictionaryT<DataUnifiable> dict)
        {
            Root = dict;
            keysE = keys.GetEnumerator();
        }

        #region Implementation of IDisposable

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {
            keysE.Dispose();
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
            return keysE.MoveNext();
        }

        /// <summary>
        /// Sets the enumerator to its initial position, which is before the first element in the collection.
        /// </summary>
        /// <exception cref="T:System.InvalidOperationException">The collection was modified after the enumerator was created. 
        ///                 </exception><filterpriority>2</filterpriority>
        public void Reset()
        {
            keysE.Reset();
        }

        /// <summary>
        /// Gets the element in the collection at the current position of the enumerator.
        /// </summary>
        /// <returns>
        /// The element in the collection at the current position of the enumerator.
        /// </returns>
        public KeyValuePair<string, DataUnifiable> Current
        {
            get
            {
                string key = keysE.Current;
                return new KeyValuePair<string, DataUnifiable>(key, Root.grabSetting(key));
            }
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


