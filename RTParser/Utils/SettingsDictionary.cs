using System;
using System.Collections.Generic;
using System.Reflection;
using System.Xml;
using System.IO;
using RTParser.Normalize;

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
        bool containsSettingCalled(string name);
    }

    /// <summary>
    /// A bespoke Dictionary<,> for loading, adding, checking, removing and extracting
    /// settings.
    /// </summary>
    public class SettingsDictionary : ISettingsDictionary
    {
        #region Attributes

        /// <summary>
        /// Holds a dictionary of settings
        /// </summary>
        readonly private Dictionary<string, Unifiable> settingsHash = new Dictionary<string, Unifiable>();

        /// <summary>
        /// Contains an ordered collection of all the keys (unfortunately Dictionary<,>s are
        /// not ordered)
        /// </summary>
        readonly private List<string> orderedKeys = new List<string>();

        private List<ParentProvider> _parent = new List<ParentProvider>();
        /// <summary>
        /// The bot this dictionary is associated with
        /// </summary>
        protected RTParser.RTPBot bot;

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
                result.AppendChild(root);
                foreach (string key in this.orderedKeys)
                {
                    XmlNode item = result.CreateNode(XmlNodeType.Element, "item", "");
                    XmlAttribute name = result.CreateAttribute("name");
                    name.Value = key;
                    XmlAttribute value = result.CreateAttribute( "value");
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
        public SettingsDictionary(RTParser.RTPBot bot, ParentProvider parent)
        {
            this.bot = bot;
            if (parent!=null) _parent.Add(parent);
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
            if (bot != null) bot.writeToLog(s); else Console.WriteLine(s);
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
            if ((myNode.Name == "item") & (myNode.Attributes.Count == 2))
            {
                if ((myNode.Attributes[0].Name == "name") & (myNode.Attributes[1].Name == "value"))
                {
                    string name = myNode.Attributes["name"].Value;
                    Unifiable value = Unifiable.Create(myNode.Attributes["value"].Value);
                    if (name.Length > 0)
                    {
                        this.addSetting(name, value);
                    }
                }
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
            lock (orderedKeys)
            {
                string key = MakeCaseInsensitive.TransformInput(name);
                if (this.orderedKeys.Contains(key))
                {
                    this.removeFromHash(key);
                    this.settingsHash.Add(MakeCaseInsensitive.TransformInput(key), value);
                    return true;
                }
            }
            foreach (var parent in Parents)
            {
                parent.updateSetting(name, value);                
            }
            return true;
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
            lock (orderedKeys)
            {
                string normalizedName = MakeCaseInsensitive.TransformInput(name);
                if (this.containsSettingCalled(normalizedName))
                {
                    return (Unifiable) this.settingsHash[normalizedName];
                }
                else
                {
                    foreach (var list in Parents)
                    {
                        if (list.containsSettingCalled(name))
                        {
                            Unifiable v = list.grabSetting(name);
                            if (v != null && !Unifiable.IsFalse(v)) return v;
                        }
                    }
                    if (Parents.Count>0)
                    {
                        return Parents[0].grabSetting(name);
                    }
                    return Unifiable.Empty;
                }
            }
        }

        /// <summary>
        /// Checks to see if a setting of a particular name exists
        /// </summary>
        /// <param name="name">The setting name to check</param>
        /// <returns>Existential truth value</returns>
        public bool containsSettingCalled(string name)
        {
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
                foreach (var hash in _parent)
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
                addObjectProperty(obj,hash);
            }
        }

        public void AddObjectProperty(object o, String name)
        {
            addGetSet(new PropertySetter(name, name, o));
        }


        private void addObjectProperty(object o, PropertyInfo info)
        {
            string name = info.Name;
            addGetSet(new PropertySetter(name,name,o));
        }

        private void addGetSet(PropertySetter o)
        {
            InsertProvider(() => { return o; });
        }

        public void InsertProvider(ParentProvider provider)
        {
            _parent.Insert(0, provider);
        }
    }

    internal class PropertySetter : ISettingsDictionary
    {
        readonly private object obj;
        readonly private string named;
        private object oldValue = null;
        public PropertySetter(string name, string pname, object o)
        {
            obj = o;
            named = name;
        }
        private void propSet(object p)
        {
            PropertyInfo info = obj.GetType().GetProperty(named);
            info.SetValue(obj, p, null);
        }
        private object propGet()
        {
            PropertyInfo info = obj.GetType().GetProperty(named);
            return info.GetValue(obj, null);
        }


        #region ISettingsDictionary Members

        public bool addSetting(string name, Unifiable value)
        {
            if (!containsSettingCalled(name)) return false;
            oldValue = propGet();
            propSet(value);
            return true;
        }

        public bool removeSetting(string name)
        {
            if (!containsSettingCalled(name)) return false;
            propSet(oldValue);
            return true;
        }

        public bool updateSetting(string name, Unifiable value)
        {
            if (containsSettingCalled(name))
            {
                oldValue = propGet();
                propSet(value);
                return true;
            }
            return false;
        }

        public Unifiable grabSetting(string name)
        {
            if (containsSettingCalled(name))
            {
                return Unifiable.Create(propGet());
            }
            return Unifiable.Empty;
        }

        public bool containsSettingCalled(string name)
        {
            return named.ToLower() == name.ToLower();
        }

        #endregion
    }
}
