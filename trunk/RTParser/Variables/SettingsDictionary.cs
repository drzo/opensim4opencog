using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.Threading;
using System.Xml;
using System.IO;
using System.Xml.Serialization;
using RTParser;
using RTParser.Normalize;
using RTParser.Utils;

namespace RTParser.Utils
{
    public delegate ISettingsDictionary ParentProvider();
    public interface ISettingsDictionary
    {
        /// <summary>
        /// Adds a bespoke setting to the Settings class (accessed via the grabSettings(string name)
        /// method.
        /// </summary>
        /// <param name="name">The name of the new setting</param>
        /// <param name="value">The value associated with this setting</param>
        bool addSetting(string name, Unifiable value);
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
        bool updateSetting(string name, Unifiable value);
        /// <summary>
        /// Returns the value of a setting given the name of the setting
        /// </summary>
        /// <param name="name">the name of the setting whose value we're interested in</param>
        /// <returns>the value of the setting</returns>
        Unifiable grabSetting(string name);
        /// <summary>
        /// Checks to see if a setting of a particular name exists
        /// </summary>
        /// <param name="name">The setting name to check</param>
        /// <returns>Existential truth value</returns>
        bool containsLocalCalled(string name);
        bool containsSettingCalled(string name);

        string NameSpace { get; }
    }

    /// <summary>
    /// A bespoke Dictionary<,> for loading, adding, checking, removing and extracting
    /// settings.
    /// </summary>
    public class SettingsDictionary : ISettingsDictionary
    {
        #region Attributes

        public bool NoDebug = false;

        /// <summary>
        /// Holds a dictionary of settings
        /// </summary>
        readonly public Dictionary<string, Unifiable> settingsHash = new Dictionary<string, Unifiable>();

        /// <summary>
        /// Contains an ordered collection of all the keys (unfortunately Dictionary<,>s are
        /// not ordered)
        /// </summary>
        readonly private List<string> orderedKeys = new List<string>();

        // prechecks and uses if settings exist
        private List<ParentProvider> _overides = new List<ParentProvider>();
        // fallbacks (therefore inherits)
        private List<ParentProvider> _parent = new List<ParentProvider>();
        /// <summary>
        /// The bot this dictionary is associated with (only for writting log)
        /// </summary>
        protected RTParser.RTPBot bot;

        private string theNameSpace;

        /// <summary>
        /// The number of items in the dictionary
        /// </summary>
        public int Count
        {
            get
            {
                return this.orderedKeys.Count;
            }
        }

       public string NameSpace
       {
           get { return theNameSpace; }
           set { theNameSpace = value; }
       }

       public override string ToString()
       {
           return theNameSpace + "(" + Count + ") " ;
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
                newAttr.Value = NameSpace;
                result.AppendChild(root);
                foreach (string key in this.orderedKeys)
                {
                    XmlNode item = result.CreateNode(XmlNodeType.Element, "item", "");
                    XmlAttribute name = result.CreateAttribute("name");
                    name.Value = key;
                    XmlAttribute value = result.CreateAttribute("value");
                    value.Value = (Unifiable)this.settingsHash[key];
                    item.Attributes.Append(name);
                    item.Attributes.Append(value);
                    root.AppendChild(item);
                }
                return result;
            }
        }

        #endregion

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot for whom this is a settings dictionary</param>
        public SettingsDictionary(String name, RTParser.RTPBot bot, ParentProvider parent)
        {
            theNameSpace = name;
            this.bot = bot;
            if (parent != null) _parent.Add(parent);
        }

        #region Methods
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
        public void loadSettings(string pathToSettings)
        {
            lock (orderedKeys)
            {
                if (pathToSettings.Length > 0)
                {
                    FileInfo fi = new FileInfo(pathToSettings);
                    if (fi.Exists)
                    {
                        XmlDocument xmlDoc = new XmlDocument();
                        try
                        {
                            xmlDoc.Load(pathToSettings);
                            this.loadSettings(xmlDoc);
                        }
                        catch (Exception e)
                        {
                            writeToLog("loadSettings: " + pathToSettings + "\n" + e);
                        }
                    }
                    else
                    {
                        writeToLog("No settings found in: " + pathToSettings);
                    }
                    writeToLog("Loaded Settings found in: " + pathToSettings);
                }
                else
                {
                    throw new FileNotFoundException();
                }
            }
        }

        private void writeToLog(string s)
        {
            if (bot != null) bot.writeToLog(s); else RTPBot.writeDebugLine(s);
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
        public void loadSettings(XmlDocument settingsAsXML)
        {
            lock (orderedKeys)
            {
                // empty the hash
                this.clearSettings();

                XmlNodeList rootChildren = settingsAsXML.DocumentElement.ChildNodes;

                foreach (XmlNode myNode in rootChildren)
                {
                    loadSettingNode(myNode);
                }
            }
        }

        public void loadSettingNode(XmlNode myNode)
        {
            if (myNode.NodeType == XmlNodeType.Comment) return;
            if ((myNode.Name == "item"))
            {
                string name = RTPBot.GetAttribValue(myNode, "name", "").Trim();
                if (name=="")
                {
                    return;
                }
                string value = RTPBot.GetAttribValue(myNode, "value", null);
                if (value==null)
                {
                    value = myNode.InnerXml.Trim();
                }
                this.addSetting(name, value);
            }
        }

        /// <summary>
        /// Adds a bespoke setting to the Settings class (accessed via the grabSettings(string name)
        /// method.
        /// </summary>
        /// <param name="name">The name of the new setting</param>
        /// <param name="value">The value associated with this setting</param>
        public bool addSetting(string name, Unifiable value)
        {
            name = name.Trim().ToLower();
            lock (orderedKeys)
            {
                if (value.AsString().Contains("<"))
                {
                    RTPBot.writeDebugLine("BAD INPUT? " + value);
                }
                string key = MakeCaseInsensitive.TransformInput(Unifiable.Create(name));
                SettingsLog("ADD Setting Local " + name + "=" + value);
                if (key.Length > 0)
                {
                    this.removeSetting(key);
                    this.orderedKeys.Add(key);
                    this.settingsHash.Add(MakeCaseInsensitive.TransformInput(key), value);
                }
            }
            return true;
        }

        public bool addListSetting(string name, Unifiable value)
        {
            name = name.Trim().ToLower();
            lock (orderedKeys)
            {
                string key = MakeCaseInsensitive.TransformInput(Unifiable.Create(name));
                if (key.Length > 0)
                {
                    this.removeSetting(key);
                    this.orderedKeys.Add(key);
                    this.settingsHash.Add(MakeCaseInsensitive.TransformInput(key), value);
                }
            }
            return true;
        }

        /// <summary>
        /// Removes the named setting from this class
        /// </summary>
        /// <param name="name">The name of the setting to remove</param>
        public bool removeSetting(string name)
        {
            name = name.Trim().ToLower();
            lock (orderedKeys)
            {
                string normalizedName = MakeCaseInsensitive.TransformInput(name);
                bool ret = orderedKeys.Contains(normalizedName);
                this.orderedKeys.Remove(normalizedName);
                this.removeFromHash(normalizedName);
                return ret;
            }
        }

        /// <summary>
        /// Removes a named setting from the Dictionary<,>
        /// </summary>
        /// <param name="name">the key for the Dictionary<,></param>
        private void removeFromHash(string name)
        {
            name = name.Trim().ToLower();
            lock (orderedKeys)
            {
                string normalizedName = MakeCaseInsensitive.TransformInput(name);
                this.settingsHash.Remove(normalizedName);
            }
        }

        /// <summary>
        /// Updates the named setting with a new value whilst retaining the position in the
        /// dictionary
        /// </summary>
        /// <param name="name">the name of the setting</param>
        /// <param name="value">the new value</param>
        public bool updateSetting(string name, Unifiable value)
        {
            name = name.Trim().ToLower();
            bool overriden = false;
            foreach (var parent in _overides)
            {
                var p = parent();
                if (p.updateSetting(name, value))
                {
                    SettingsLog("OVERRIDDEN UPDATE " + p + " " + name + "=" + value);
                    overriden = true;
                }
            }
            if (overriden)
            {
                return true;
            }
            lock (orderedKeys)
            {
                string key = MakeCaseInsensitive.TransformInput(name);
                if (this.orderedKeys.Contains(key))
                {
                    var old = this.settingsHash[key];
                    this.removeFromHash(key);
                    SettingsLog("UPDATE Setting Local " + name + "=" + value);
                    this.settingsHash.Add(MakeCaseInsensitive.TransformInput(key), value);
                    return true;
                }
            }
            foreach (var parent in Parents)
            {
                if (parent.updateSetting(name, value))
                {
                    SettingsLog("PARENT UPDATE " + parent + " " + name + "=" + value);
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// Clears the dictionary to an empty state
        /// </summary>
        public void clearSettings()
        {
            lock (orderedKeys)
            {
                this.orderedKeys.Clear();
                this.settingsHash.Clear();
            }
        }

        /// <summary>
        /// Returns the value of a setting given the name of the setting
        /// </summary>
        /// <param name="name">the name of the setting whose value we're interested in</param>
        /// <returns>the value of the setting</returns>
        public Unifiable grabSetting(string name)
        {
            name = name.Trim().ToLower();
            foreach (ParentProvider overide in _overides)
            {
                ISettingsDictionary dict = overide();
                if (dict.containsSettingCalled(name))
                {
                    Unifiable v = dict.grabSetting(name);
                    SettingsLog("Returning Override " + name + "=" + v);
                    return v;
                }
            }
            lock (orderedKeys)
            {
                string normalizedName = MakeCaseInsensitive.TransformInput(name);
                if (this.containsLocalCalled(normalizedName))
                {
                    Unifiable v = this.settingsHash[normalizedName];
                    SettingsLog("Returning LocalSetting " + name + "=" + v);
                    return v;
                }
                else if (Parents.Count > 0)
                {
                    foreach (var list in Parents)
                    {
                        if (list.containsSettingCalled(name))
                        {
                            Unifiable v = list.grabSetting(name);
                            SettingsLog("Returning Parent " + name + "=" + v);
                            if (v != null && !Unifiable.IsFalse(v)) return v;
                        }
                    }
                    var v0 = Parents[0].grabSetting(name);
                    SettingsLog("Returning False " + name + "=" + v0);
                    return v0;
                }
                SettingsLog("Returning Empty " + name);
                return Unifiable.Empty;

            }
        }

        private void SettingsLog(string unifiable)
        {
            if (NoDebug) return;
            var fc = new StackTrace().FrameCount;
            RTPBot.writeDebugLine("DICTLOG: " + NameSpace + " (" + fc + ")   " + unifiable);
            if (fc > 200)
            {
                //throw new 
                RTPBot.writeDebugLine("DICTLOG OVERFLOWING: " + NameSpace + " (" + fc + ")   " + unifiable);
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
            name = name.Trim();
            lock (orderedKeys)
            {
                string normalizedName = MakeCaseInsensitive.TransformInput(name);
                if (normalizedName.Length > 0)
                {
                    return this.orderedKeys.Contains(normalizedName);
                }
                else
                {
                    return false;
                }
            }
        }

        public bool containsSettingCalled(string name)
        {
            var value = grabSettingNoDebug(name);
            return !Unifiable.IsNullOrEmpty(value);
        }

        /// <summary>
        /// Returns a collection of the names of all the settings defined in the dictionary
        /// </summary>
        /// <returns>A collection of the names of all the settings defined in the dictionary</returns>
        public string[] SettingNames
        {
            get
            {
                lock (orderedKeys)
                {
                    string[] result = new string[this.orderedKeys.Count];
                    this.orderedKeys.CopyTo(result, 0);
                    return result;
                }
            }
        }

        public List<ISettingsDictionary> Parents
        {
            get
            {
                var ps = new List<ISettingsDictionary>();
                lock (_parent) foreach (var hash in _parent)
                {
                    ps.Add(hash());

                }
                return ps;
            }
        }

        /// <summary>
        /// Copies the values in the current object into the SettingsDictionary passed as the target
        /// </summary>
        /// <param name="target">The target to recieve the values from this SettingsDictionary</param>
        public void Clone(SettingsDictionary target)
        {
            lock (orderedKeys)
            {
                foreach (string key in this.orderedKeys)
                {
                    target.addSetting(key, this.grabSetting(key));
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
            addGetSet(new ObjectPropertyDictionary(name, name, o));
        }


        private void addObjectProperty(object o, PropertyInfo info)
        {
            string name = info.Name;
            addGetSet(new ObjectPropertyDictionary(name, name, o));
        }

        private void addGetSet(ObjectPropertyDictionary o)
        {
            InsertProvider(() => { return o; });
        }

        public void InsertFallback(ParentProvider provider)
        {
            _parent.Insert(0, provider);
        }

        public void InsertOverrides(ParentProvider provider)
        {
            _overides.Insert(0, provider);
        }

        public void InsertProvider(ParentProvider provider)
        {
            _parent.Insert(0, provider);
        }

        public void AddGetSetProperty(string topic, GetUnifiable getter, Action<Unifiable> setter)
        {
            var prov = new GetSetDictionary(topic, new GetSetProperty(getter, setter));
            InsertProvider(() => prov);
        }

        internal void AddGetSetProperty(string p, CollectionProperty v)
        {
            GetSetDictionary prov = new GetSetDictionary(p, v.GetProvider());
            InsertProvider(() => prov);
        }

        public Unifiable grabSettingNoDebug(string name)
        {
            if (NoDebug) return grabSetting(name);
            NoDebug = true;
            try
            {
                var v = grabSetting(name);
                return v;
            }
            finally
            {
                NoDebug = false;                
            }
        }
    }
}
