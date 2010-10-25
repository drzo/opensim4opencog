using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.Threading;
using System.Xml;
using System.IO;
using System.Xml.Serialization;
using Lucene.Net.Store;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using RTParser;
using RTParser.Database;
using RTParser.Normalize;
using RTParser.Utils;

namespace RTParser.Variables
{
    public delegate ISettingsDictionary ParentProvider();
    public interface ISettingsDictionary : ITraceable, ITreeable
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

    }

    /// <summary>
    /// A bespoke Dictionary<,> for loading, adding, checking, removing and extracting
    /// settings.
    /// </summary>
    public class SettingsDictionary : ISettingsDictionary, IDictionary<string, Unifiable>
    {
        #region Attributes

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
        private List<ParentProvider> _fallbacks = new List<ParentProvider>();
        // fallbacks (therefore inherits)
        private List<ParentProvider> _listeners = new List<ParentProvider>();
        // fallbacks (therefore inherits)
        private readonly PrefixProvider prefixProvider;
        internal bool IsIdentityReadOnly = true;
        internal bool SuspendUpdates = false;

        /// <summary>
        /// The bot this dictionary is associated with (only for writting log)
        /// </summary>
        protected RTPBot bot;

        private string theNameSpace;
        public bool TrimKeys = true;
        private string fromFile;

        /// <summary>
        /// Adds an item to the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <param name="item">The object to add to the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        ///                 </exception>
        public void Add(KeyValuePair<string, Unifiable> item)
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
        public bool Contains(KeyValuePair<string, Unifiable> item)
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
        public void CopyTo(KeyValuePair<string, Unifiable>[] array, int arrayIndex)
        {
            foreach (string key in Keys)
            {
                array[arrayIndex++] = new KeyValuePair<string, Unifiable>(key, grabSetting(key));
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
        public bool Remove(KeyValuePair<string, Unifiable> item)
        {
            foreach (var hash in SettingNames(1))
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
                return this.orderedKeys.Count;
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
            get { return SuspendUpdates || TextPatternUtils.IsTrue(grabSettingNoDebug("isReadOnly")); }
        }

        public string NameSpace
        {
            get { return theNameSpace; }
            set { theNameSpace = value; }
        }

        public bool IsTraced { get; set; }

        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <returns>
        /// A <see cref="T:System.Collections.Generic.IEnumerator`1"/> that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>1</filterpriority>
        public IEnumerator<KeyValuePair<string, Unifiable>> GetEnumerator()
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
                lock (orderedKeys)
                {

                    newAttr.Value = NameSpace;
                    if (fromFile != null)
                    {
                        newAttr = result.CreateAttribute("fromfile");
                        newAttr.Value = fromFile;
                    }
                    result.AppendChild(root);

                    foreach (var normalizedName in Overides)
                    {
                        XmlNode item = result.CreateNode(XmlNodeType.Element, "override", "");
                        XmlAttribute name = result.CreateAttribute("name");
                        name.Value = normalizedName.NameSpace;
                        item.Attributes.Append(name);
                        root.AppendChild(item);
                    }
                    foreach (var normalizedName in Fallbacks)
                    {
                        XmlNode item = result.CreateNode(XmlNodeType.Element, "fallback", "");
                        XmlAttribute name = result.CreateAttribute("name");
                        name.Value = normalizedName.NameSpace;
                        item.Attributes.Append(name);
                        root.AppendChild(item);
                    }
                    foreach (var normalizedName in Listeners)
                    {
                        XmlNode item = result.CreateNode(XmlNodeType.Element, "synchon", "");
                        XmlAttribute name = result.CreateAttribute("name");
                        name.Value = normalizedName.NameSpace;
                        item.Attributes.Append(name);
                        root.AppendChild(item);
                    }
                    foreach (var normalizedName in this.prefixProvider._prefixes)
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
                    foreach (var normalizedName in ProvidersFrom(this.MetaProviders))
                    {
                        XmlNode item = result.CreateNode(XmlNodeType.Element, "metaproviders", "");
                        XmlAttribute name = result.CreateAttribute("name");
                        name.Value = normalizedName.NameSpace;
                        item.Attributes.Append(name);
                        root.AppendChild(item);
                    }
                    foreach (var normalizedName in this.makedvars)
                    {
                        XmlNode item = result.CreateNode(XmlNodeType.Element, "maskedvar", "");
                        XmlAttribute name = result.CreateAttribute("name");
                        name.Value = normalizedName;
                        item.Attributes.Append(name);
                        root.AppendChild(item);
                    }
                    foreach (string n in this.orderedKeys)
                    {
                        XmlNode item = result.CreateNode(XmlNodeType.Element, "item", "");
                        XmlAttribute name = result.CreateAttribute("name");
                        name.Value = n; ;
                        XmlAttribute value = result.CreateAttribute("value");
                        value.Value = this.settingsHash[TransformKey(n)];
                        item.Attributes.Append(name);
                        item.Attributes.Append(value);
                        root.AppendChild(item);
                    }

                }
                return result;
            }
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
        public SettingsDictionary(String name, RTPBot bot, ParentProvider parent)
        {
            theNameSpace = name;
            IsSubsts = name.Contains("subst");
            TrimKeys = !name.Contains("subst");
            this.bot = bot;
            if (!IsSubsts)
            {
                if (bot.RelationMetaProps != null) this.InsertMetaProvider(bot.GetRelationMetaProps);
            }
            IsTraced = true;
            bot.RegisterDictionary(name, this);
            if (parent != null) _fallbacks.Add(parent);
            prefixProvider = new PrefixProvider();
            string prefixName = name + ".prefixProvider";
            prefixProvider.NameSpace = prefixName;
            ParentProvider pp = () => prefixProvider;
            bot.RegisterDictionary(prefixName, prefixProvider);
            var dict = FindDictionary(prefixName, () => this);
            IsTraced = false;
            AddSettingToCollection(null, pp, _fallbacks);
            AddSettingToCollection(null, pp, _listeners);
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
        public void loadSettings(string pathToSettings, Request request)
        {
            OutputDelegate writeToLog = request.writeToLog;
            if (pathToSettings == null) return;
            lock (orderedKeys)
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
                            this.loadSettings(xmlDoc, request);
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

        static public void loadSettings(ISettingsDictionary dict0, string pathToSettings,
            bool overwriteExisting, bool onlyIfUnknown, Request request)
        {
            if (pathToSettings == null) return;
            SettingsDictionary dict = ToSettingsDictionary(dict0);
            OutputDelegate writeToLog = dict.writeToLog;
            // or else
            // ReSharper disable ConstantNullColescingCondition
            writeToLog = writeToLog ?? request.writeToLog;
            // ReSharper restore ConstantNullColescingCondition
            lock (dict.orderedKeys)
            {
                if (pathToSettings.Length > 0)
                {
                    if (HostSystem.DirExists(pathToSettings))
                    {
                        foreach (string s in HostSystem.GetFiles(pathToSettings, "*.xml"))
                        {
                            loadSettings(dict, s, overwriteExisting, onlyIfUnknown, request);
                        }
                        return;
                    }
                    if (!HostSystem.FileExists(pathToSettings))
                    {
                        writeToLog("ERROR No settings found in: " + pathToSettings);
                        //throw new FileNotFoundException(pathToSettings);
                        return;
                    }

                    try
                    {
                        XmlDocumentLineInfo xmlDoc = new XmlDocumentLineInfo(pathToSettings, true);
                        var stream = HostSystem.GetStream(pathToSettings);
                        xmlDoc.Load(stream);
                        HostSystem.Close(stream);
                        loadSettingNode(dict, xmlDoc, overwriteExisting, onlyIfUnknown, request);
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
            else RTPBot.writeDebugLine(message);
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
        public void loadSettings(XmlDocument settingsAsXML, Request request)
        {
            lock (orderedKeys)
            {
                if (settingsAsXML.DocumentElement == null)
                {
                    writeToLog("ERROR no doc element in " + settingsAsXML);
                }
                loadSettingNode(this, settingsAsXML.Attributes, true, false, request);
                loadSettingNode(this, settingsAsXML.DocumentElement, true, false, request);
            }
        }

        static public void loadSettingNode(ISettingsDictionary dict, IEnumerable Attributes, bool overwriteExisting, bool onlyIfUnknown, Request request)
        {
            if (Attributes == null) return;
            foreach (object o in Attributes)
            {
                if (o is XmlNode)
                {
                    XmlNode n = (XmlNode)o;
                    loadSettingNode(dict, n, overwriteExisting, onlyIfUnknown, request);
                }
            }
        }

        private static void loadNameValueSetting(ISettingsDictionary dict, string name, string value, string updateOrAddOrDefualt, XmlNode myNode, bool overwriteExisting, bool onlyIfUnknown, Request request)
        {
            updateOrAddOrDefualt = updateOrAddOrDefualt.ToLower().Trim();

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

            SettingsDictionary dictionary = ToSettingsDictionary(dict);
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
                WithoutTrace(dict, () => dict.addSetting(name, Unifiable.MakeStringUnifiable(value, false)));
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
                WithoutTrace(dict, () => dict.updateSetting(name, Unifiable.MakeStringUnifiable(value, false)));
            }
        }

        static R WithoutTrace<R>(ISettingsDictionary dict, Func<R> func)
        {
            return StaticXMLUtils.WithoutTrace(dict, func);
        }

        static public void loadSettingNode(ISettingsDictionary dict, XmlNode myNode, bool overwriteExisting, bool onlyIfUnknown, Request request)
        {
            lock (dict)
            {

                SettingsDictionary settingsDict = ToSettingsDictionary(dict);
                WithoutTrace(dict, () =>
                {
                    loadSettingNode0(settingsDict, myNode, overwriteExisting,
                                     onlyIfUnknown, request);
                    return true;
                });
            }
        }

        static public void loadSettingNode0(ISettingsDictionary dict, XmlNode myNode, bool overwriteExisting, bool onlyIfUnknown, Request request)
        {

            if (myNode == null) return;
            if (myNode.NodeType == XmlNodeType.Comment) return;
            if (myNode.NodeType == XmlNodeType.Attribute)
            {
                // attribues should not overwrite existing? 
                loadNameValueSetting(dict, myNode.Name, myNode.Value, "add", myNode, overwriteExisting, onlyIfUnknown, request);
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
                loadSettingNode(dict, myNode.Attributes, false, onlyIfUnknown, request);
                loadSettingNode(dict, myNode.ChildNodes, overwriteExisting, onlyIfUnknown, request);
                return;
            }
            string lower = myNode.Name.ToLower();

            if (myNode.NodeType == XmlNodeType.Document || lower == "#document")
            {
                loadSettingNode(dict, myNode.Attributes, false, onlyIfUnknown, request);
                loadSettingNode(dict, myNode.ChildNodes, overwriteExisting, onlyIfUnknown, request);
                return;
            }
            if (lower == "substitutions")
            {
                //loadSettingNode(dict, myNode.Attributes, false, true, request);
                SettingsDictionary substDict = ToSettingsDictionary(dict);
                string substName = StaticXMLUtils.GetAttribValue(myNode, "name,dict,value", "input");
                var chdict = request.GetSubstitutions(substName, true);
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
                            loadSettingNode(chdict, n.ChildNodes, overwriteExisting, onlyIfUnknown, request);
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
                        loadSettingNode(chdict, n, overwriteExisting, onlyIfUnknown, request);
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
                    loadNameValueSetting(dict, name, href, "add", myNode, false, true, request);
                    return;
                }
            }

            if (lower == "bot")
            {
                var p = myNode.ParentNode;
                if (p != null && p.Name.ToLower() == "bots")
                {
                    loadSettingNode(dict, myNode.ChildNodes, overwriteExisting, onlyIfUnknown, request);
                    loadSettingNode(dict, myNode.Attributes, false, false, request);
                    return;
                }
            }

            if (lower == "root" || lower == "vars" || lower == "items" || lower == "properties"
                || lower == "bots" || lower == "testing" || lower == "predicates")
            {
                loadSettingNode(dict, myNode.ChildNodes, overwriteExisting, onlyIfUnknown, request);
                loadSettingNode(dict, myNode.Attributes, false, false, request);
                return;
            }
            if ((lower == "include"))
            {
                string path = StaticXMLUtils.GetAttribValue(myNode, "path", myNode.InnerText);

                overwriteExisting =
                    Boolean.Parse(StaticXMLUtils.GetAttribValue(myNode, "overwriteExisting", "" + overwriteExisting));

                onlyIfUnknown =
                    Boolean.Parse(StaticXMLUtils.GetAttribValue(myNode, "onlyIfKnown", "" + onlyIfUnknown));

                loadSettings(ToSettingsDictionary(dict), path, overwriteExisting, onlyIfUnknown, request);
                return;
            }
            SettingsDictionary settingsDict = ToSettingsDictionary(dict);
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
                        case "prefixes":
                            settingsDict.AddChild(StaticXMLUtils.GetAttribValue(myNode, "prefix,name,dict,value", name), pp);
                            return;
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
                            overwriteExisting, onlyIfUnknown, request);
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
                    loadNameValueSetting(dict, name, value, "add", myNode, false, true, request);
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
            if (name.EndsWith(".prefixProvider"))
            {
                int keylen = name.Length - ".prefixProvider".Length;

                ParentProvider dict = FindDictionary(name.Substring(0, keylen), fallback);
                if (dict != null)
                {
                    var sd = ToSettingsDictionary(dict());
                    if (sd != null)
                        return ToParentProvider(sd.prefixProvider);
                }
            }
            ParentProvider pp = FindDictionary0(name, fallback);
            if (pp != null) return pp.Invoke;
            Func<ParentProvider> provider0 = () => FindDictionary0(name, fallback);
            return () => new ProvidedSettingsDictionary(name, provider0);
        }

        public ParentProvider FindDictionary0(string name, ParentProvider fallback)
        {
            var rtpbotobjCol = ScriptManager.ResolveToObject(this, name);
            if (rtpbotobjCol == null || rtpbotobjCol.Count == 0)
            {
                string clipIt;
                var prep = prefixProvider.GetChildPrefixed(name, out clipIt);
                if (prep != null)
                {
                    if (clipIt == ".") return prep;
                    var pp0 = prep();
                    return prep;
                }
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

        public static SettingsDictionary ToSettingsDictionary(object dictionary)
        {
            if (dictionary == null)
            {
                RTPBot.writeDebugLine("-DICTRACE: Warning ToSettingsDictionary got NULL");
                return null;
            }
            if (dictionary is SubQuery) dictionary = ((SubQuery)dictionary).TargetSettings;
            if (dictionary is User) dictionary = ((User)dictionary).Predicates;
            SettingsDictionary sd = dictionary as SettingsDictionary;
            if (sd != null) return sd;
            RTPBot.writeDebugLine("-DICTRACE: Warning ToSettingsDictionary got type={0} '{1}'",
                                  dictionary.GetType(),
                                  dictionary);
            return null;
        }

        public static ParentProvider ToParentProvider(object dictionary)
        {
            if (dictionary == null)
            {
                RTPBot.writeDebugLine("-DICTRACE: Warning ToParentProvider got NULL");
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
            RTPBot.writeDebugLine("-DICTRACE: Warning ToParentProvider got type={0} '{1}'",
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
            lock (orderedKeys)
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
        public bool addSetting(string name, Unifiable value)
        {
            value = TransformValueIn(value);
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
            if (!RTPBot.SettingsAliases.TryGetValue(key, out aliases))
            {
                return NO_SETTINGS;
            }
            return aliases;
        }

        public bool addSetting0(string name, Unifiable value)
        {
            bool found = true;
            lock (orderedKeys)
            {
                name = TransformName(name);
                string normalizedName = TransformKey(name);
                if (normalizedName == "issubsts")
                {
                    IsSubsts = TextPatternUtils.IsTrue(value);
                }
                if (makedvars.Contains(normalizedName))
                {
                    SettingsLog("ERROR MASKED ADD SETTING '" + name + "'=" + str(value) + " ");
                    return false;
                }
                if (normalizedName.Length > 0)
                {
                    if (!AllowedNameValue(name, value))
                    {
                        SettingsLog("!NameValueCheck ADD Setting Local '" + name + "'=" + str(value) + " ");
                        return true;
                    }
                    SettingsLog("ADD LOCAL '" + name + "'=" + str(value) + " ");
                    found = this.removeSettingReal(name);
                    if (value != null)
                    {
                        this.orderedKeys.Add(name);
                        this.settingsHash.Add(normalizedName, value);
                    }
                    updateListeners(name, value, true, !found);
                }
                else
                {
                    SettingsLog("ERROR ADD Setting Local '" + name + "'=" + str(value) + " ");
                }
            }
            return !found;
        }

        protected bool AllowedNameValue(string name, Unifiable value)
        {
            if (IsIdentityReadOnly && (name.ToLower() == "name" || name.ToLower() == "id"))
            {
                string s = (string)value;
                if (s == null || TextPatternUtils.IsUnknown(value) || s.Length < 3 || s.ToLower() == "friend"
                     || s.Contains(">") || s.ToLower() == "that really")
                {
                    writeToLog("! NameValueCheck " + name + " = " + value);
                    return false;
                }
                return true;
            }
            return !SuspendUpdates;
        }

        private void updateListeners(string name, Unifiable value, bool locally, bool addedNew)
        {
            if (SuspendUpdates) return;
            foreach (var list in _listeners)
            {
                var l = list();
                if (addedNew) l.addSetting(name, value);
                else
                    l.updateSetting(name, value);
            }
        }
        public Unifiable TransformValueIn(Unifiable value)
        {
            string s = TransformValue0(value);
            if (s == null) return Unifiable.NULL;
            if (s == "") return Unifiable.Empty;
            if (Unifiable.IsMissing(value)) 
            {
                if (NoSettingsAliaes) return null;
                return Unifiable.MISSING;
            }
            return s;
        }
        public Unifiable TransformValueOut(Unifiable value)
        {
            string s = TransformValue0(value);
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

        public string TransformValue0(Unifiable value)
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
            if (Unifiable.IsIncomplete(value))
            {
                //   writeToLog("ERROR " + value + " NULL");
                if (NoSettingsAliaes) return null;
                return Unifiable.INCOMPLETE.AsString();
            }
            var v = StaticXMLUtils.ValueText(value);
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

        public bool addListSetting(string name, Unifiable value)
        {
            lock (orderedKeys)
            {
                name = TransformName(name);
                value = TransformValueIn(value);
                string normalizedName = TransformKey(name);
                if (normalizedName.Length > 0)
                {
                    this.removeSetting(name);
                    this.orderedKeys.Add(name);
                    this.settingsHash.Add(normalizedName, value);
                }
            }
            return true;
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
            lock (orderedKeys)
            {
                if (SuspendUpdates) return true;
                name = TransformName(name);
                string normalizedName = TransformKey(name);
                bool ret = orderedKeys.Contains(name);
                this.orderedKeys.Remove(name);
                // shouldnt need this next one (but just in case)
                this.orderedKeys.Remove(normalizedName);
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
            return name;
            //return MakeCaseInsensitive.TransformInput(name);
        }

        /// <summary>
        /// Removes a named setting from the Dictionary<,>
        /// </summary>
        /// <param name="name">the key for the Dictionary<,></param>
        private void removeFromHash(string name)
        {
            lock (orderedKeys)
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
        public bool updateSetting(string name, Unifiable value)
        {
            if (SuspendUpdates) return true;
            value = TransformValueIn(value);
            foreach (var setname in GetSettingsAliases(name))
            {
                updateSetting0(setname, value);
            }
            return updateSetting0(name, value);
        }
        public bool updateSetting0(string name, Unifiable value)
        {
            bool overriden = false;
            foreach (var parent in _overides)
            {
                var p = parent();
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
            lock (orderedKeys)
            {
                name = TransformName(name);
                string normalizedName = TransformKey(name);
                if (this.settingsHash.ContainsKey(normalizedName))
                {
                    var old = this.settingsHash[normalizedName];

                    if (makedvars.Contains(normalizedName))
                    {
                        SettingsLog("MASKED Not Update Local '" + name + "'=" + str(value) + " keeped " + str(old));
                    }
                    else
                    {
                        updateListeners(name, value, true, false);
                        if (AllowedNameValue(name, value))
                        {
                            this.removeFromHash(name);
                            SettingsLog("UPDATE Setting Local '" + name + "'=" + str(value));
                            this.settingsHash.Add(normalizedName, value);
                            return true;
                        }
                        else
                        {
                            SettingsLog("NOT_UPDATE Setting Local '" + name + "'=" + str(value));
                        }
                    }
                }
                // before fallbacks
                if (makedvars.Contains(normalizedName))
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

        public string str(Unifiable value)
        {
            value = TransformValueOut(value);
            return "'" + Unifiable.DescribeUnifiable(value) + "'";
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
        public void clearHierarchy()
        {
            lock (orderedKeys)
            {
                _overides.Clear();
                _fallbacks.Clear();
                _fallbacks.Add(() => prefixProvider);
                makedvars.Clear();
            }
        }

        public void clearSyncs()
        {
            _listeners.Clear();
            _listeners.Add(() => prefixProvider);
        }

        private HashSet<string> makedvars = new HashSet<string>();
        public void maskSetting(string name)
        {
            name = TransformName(name);
            name = TransformKey(name);
            writeToLog("MASKING: " + name);
            lock (orderedKeys) makedvars.Add(name);
        }

        /// <summary>
        /// Returns the value of a setting given the name of the setting
        /// </summary>
        /// <param name="name">the name of the setting whose value we're interested in</param>
        /// <returns>the value of the setting</returns>
        public Unifiable grabSetting(string name)
        {
#if debug
            var v = grabSetting0(name);
            if (Unifiable.IsNullOrEmpty(v))
            {
                writeToLog("DICT '{0}'=null", null);
            }
            return v;
#else
            try
            {
                name = TransformName(name);
                var setting = grabSetting0(name);
                setting = TransformValueOut(setting);
                return setting;
            }
            catch (Exception e)
            {
                writeToLog("ERROR {0}", e);

                return null;
            }
#endif
        }

        public string grabSettingOrDefault(string name, string fallback)
        {
            HashSet<ISettingsDictionary> noGo = new HashSet<ISettingsDictionary>() { this };
            foreach (ParentProvider overide in _overides)
            {
                ISettingsDictionary dict = overide();
                if (!noGo.Add(dict)) continue;
                if (dict.containsLocalCalled(name))
                {
                    string v = grabSettingOrDefault(dict, name, fallback);
                    SettingsLog("OVERRIDE '{0}'='{1}'", name, str(v));
                    return v;
                }
            }
            lock (orderedKeys)
            {
                string normalizedName = TransformKey(name);

                if (this.settingsHash.ContainsKey(normalizedName))
                {
                    string v = this.settingsHash[normalizedName];
                    if (makedvars.Contains(normalizedName))
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
                    Unifiable u = CheckFallbacks(noGo, Fallbacks, name, normalizedName, ref firstFallBack, out returnIt);
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

        private Unifiable CheckFallbacks(HashSet<ISettingsDictionary> noGo, IEnumerable<ISettingsDictionary> fallbacks, string name, string normalizedName, ref ISettingsDictionary firstFallBack, out bool returnIt)
        {
            foreach (ISettingsDictionary list in fallbacks)
            {
                if (!noGo.Add(list)) continue;
                firstFallBack = firstFallBack ?? list;
                lock (list)
                {
                    bool prev = list.IsTraced;
                    list.IsTraced = false;
                    Unifiable v = grabSettingOrDefault(list, name, null);
                    ;
                    if (v == null) continue;

                    if (makedvars.Contains(normalizedName))
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

        private string grabSettingOrDefault(ISettingsDictionary dictionary, string name, string o)
        {
            if (dictionary.containsLocalCalled(name)) return dictionary.grabSetting(name);
            if (dictionary.containsSettingCalled(name)) return dictionary.grabSetting(name);
            return o;

        }

        public Unifiable grabSetting0(string name)
        {
            HashSet<ISettingsDictionary> noGo = new HashSet<ISettingsDictionary>() {this};
            foreach (ParentProvider overide in _overides)
            {
                ISettingsDictionary dict = overide();
                if (!noGo.Add(dict)) continue;
                if (dict.containsSettingCalled(name))
                {
                    Unifiable v = dict.grabSetting(name);
                    SettingsLog("OVERRIDE '" + name + "'=" + str(v));
                    return v;
                }
            }
            lock (orderedKeys)
            {
                string normalizedName = TransformKey(name);
                if (containsLocalCalled0(name))
                {
                    Unifiable v = localValue(name, normalizedName);
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
                    Unifiable u = CheckFallbacks(noGo, Fallbacks, name, normalizedName, ref firstFallBack, out returnIt);
                    if (returnIt) return u;
                    if (firstFallBack != null)
                    {
                        var v0 = firstFallBack.grabSetting(name);
                        if (!IsMissing(v0))
                        {
                            SettingsLog("RETURN FALLBACK0 '" + name + "'=" + str(v0));
                            return v0;
                        }
                    }
                }
                SettingsLog("MISSING '" + name + "'");
                return Unifiable.MISSING;
            }
        }

        public static bool IsMissing(Unifiable tsetting)
        {
            return TextPatternUtils.IsIncomplete(tsetting);
        }

        private Unifiable localValue(string name, string normalizedName)
        {
            if (this.settingsHash.ContainsKey(normalizedName))
            {
                Unifiable v = this.settingsHash[normalizedName];
                v = TransformValueOut(v);
                if (makedvars.Contains(normalizedName))
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
            lock (orderedKeys)
            {
                name = TransformName(name);
                string normalizedName = TransformKey(name);

                if (makedvars.Contains(normalizedName)) return true;

                if (normalizedName.Length > 0)
                {
                    return settingsHash.ContainsKey(normalizedName);

                    if (!this.settingsHash.ContainsKey(normalizedName))
                    {
                        if (!this.orderedKeys.Contains(name))
                        {
                            if (!this.orderedKeys.Contains(name.ToUpper()))
                            {
                                writeToLog("Missing odered key " + name);
                            }
                        }
                        return true;
                    }
                    return false;
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
            return !IsMissing(value);
        }

        /// <summary>
        /// Returns a collection of the names of all the settings defined in the dictionary
        /// </summary>
        /// <returns>A collection of the names of all the settings defined in the dictionary</returns>
        public IEnumerable<string> SettingNames(int depth)
        {
            //       get
            {
                lock (orderedKeys)
                {
                    IEnumerable<string> prefixProviderSettingNames = prefixProvider.SettingNames(depth);
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
                    }
                    string[] result = new string[this.orderedKeys.Count];
                    this.orderedKeys.CopyTo(result, 0);
                    return result;
                }
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
        public void Add(string key, Unifiable value)
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
        public bool TryGetValue(string key, out Unifiable value)
        {
            value = grabSetting(key);
            return !IsMissing(value);
        }

        public Unifiable this[string name]
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
            get { return orderedKeys; }
        }

        /// <summary>
        /// Gets an <see cref="T:System.Collections.Generic.ICollection`1"/> containing the values in the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.Generic.ICollection`1"/> containing the values in the object that implements <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </returns>
        public ICollection<Unifiable> Values
        {
            get { return settingsHash.Values; }
        }

        public static void IndexSet(ISettingsDictionary dictionary, string name, Unifiable value)
        {
            dictionary.addSetting(name, value);
        }

        public static Unifiable IndexGet(ISettingsDictionary dictionary, string name)
        {
            return dictionary.grabSetting(name);
        }

        public static IEnumerable<string> NO_SETTINGS = new string[0];
        public static IEnumerable<string> TOO_DEEP = new string[0];
        public bool IsSubsts;


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
                dictionary = pp();
                if (dictionary == null)
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
                writeToLog("WARNING: NULL Dictionary");
            }
            if (dictionary == this)
            {
                writeToLog("ERROR: should not place inside self");
                return false;
            }
            lock (cols)
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
                        writeToLog("WARN: alread contains inner " + inner);
                        cols.Remove(deep);
                        return true;
                    }
                    if (inner == this)
                    {
                        writeToLog("WARN: this was found in inner " + inner);
                        return false;
                    }
                    if (inner is SettingsDictionary)
                    {
                        var sd = (SettingsDictionary)inner;
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

        public static Unifiable WithProviders(ISettingsDictionary dictionary, string name, string props, out string realName,
            IEnumerable<ParentProvider> providers, Func<string, Unifiable> Else)
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

        public Unifiable GetSetReturn(string name, out string realName)
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
            var found = new List<ISettingsDictionary>();
            lock (providers)
            {
                foreach (var list in providers)
                {
                    var res = list();
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
            lock (MetaProviders) foreach (var pp in MetaProviders)
                {
                    dt.InsertMetaProvider(pp);
                }
            lock (orderedKeys)
            {
                foreach (string name in this.orderedKeys)
                {
                    target.addSetting(name, this.grabSetting(name));
                }
            }
        }
        /// <summary>
        /// Copies the values in the current object into the SettingsDictionary passed as the target
        /// If the keys are missing
        /// </summary>
        /// <param name="target">The target to recieve the values from this SettingsDictionary</param>
        public void AddMissingKeys(ISettingsDictionary target)
        {
            lock (orderedKeys)
            {
                foreach (string name in this.orderedKeys)
                {
                    if (target.containsLocalCalled(name)) continue;
                    target.addSetting(name, this.grabSetting(name));
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

        public void AddChild(string prefix, ParentProvider dict)
        {
            ISettingsDictionary sdict = dict();
            prefixProvider.AddChild(prefix, dict);
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
            return WithoutTrace(this, () => grabSetting(name));
        }

        public static Unifiable grabSettingDefault(ISettingsDictionary dictionary, string name, out string realName, SubQuery query)
        {
            bool succeed;
            return NamedValuesFromSettings.GetSettingForType(dictionary.NameSpace, query, dictionary, name,
                out realName, name, null, out succeed, null);
        }
        public static Unifiable grabSettingDefaultDict(ISettingsDictionary dictionary, string name, out string realName)
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
                        Unifiable withChop = grabSettingDefaultDict(dictionary, newName, out realName0);
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
                        un = sd.grabSettingNoDebug(realName0);
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

        public void AddPrefix(string prefix, ParentProvider dict)
        {
            prefixProvider.AddChild(prefix, dict);
        }

        public static bool TryGetValue<T>(IDictionary<string, T> dictionary, string search, out T value)
        {
            lock (dictionary)
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
            if (UseUndoPush)
            {
                UndoStack.GetStackFor(query).pushValues(dict, name, null);
            }
            else
            {
                Unifiable prevSetting = dict.grabSetting(name);
                if (dict.removeSetting(name))
                {
                    if (locally) query.AddUndo(() => dict.addSetting(name, prevSetting));
                    else query.AddUndo(() => dict.updateSetting(name, prevSetting));
                }
            }
            query.AddSideEffect(String.Format("REMOVE {0} {1}", dict, name), () => dict.removeSetting(name));
            return locally;
        }

        public static bool addSettingWithUndoCommit(SubQuery query, ISettingsDictionary dict, Func<string, Unifiable, bool> SideEffect, string name, Unifiable newValue)
        {
            bool locally = dict.containsLocalCalled(name);
            if (UseUndoPush)
            {
                UndoStack.GetStackFor(query).pushValues(dict, name, null);
            }
            else
            {
                Unifiable prevSetting = dict.grabSetting(name);
                bool res = SideEffect(name, newValue);
                query.AddUndo(() =>
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
            if (newValue.AsString().Contains(">"))
            {
                
            }
            query.AddSideEffect(String.Format("ADD SETTING {0} {1} {2}", dict, name, newValue), () => SideEffect(name, newValue));
            return !locally;
        }

        public static T WithUndoCommit<T>(SubQuery query, ISettingsDictionary dict, Func<T> SideEffect, string name, Unifiable newValue)
        {
            bool locally = dict.containsLocalCalled(name);
            Unifiable prevSetting = dict.grabSetting(name);
            T res = SideEffect();
            query.AddUndo(() =>
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
            query.AddSideEffect(String.Format("ADD SETTING {0} {1} {2}", dict, name, newValue), () => SideEffect());
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
                console(var + " = " + grabSettingNoDebug(var));
                return true;
            }
            console("addSetting: " + addSetting(var, value));
            return true;
        }
    }
    public class SettingsDictionaryEnumerator : IEnumerator<KeyValuePair<string, Unifiable>>
    {
        private readonly ISettingsDictionary Root;
        private readonly IEnumerator<string> keysE;
        public SettingsDictionaryEnumerator(IEnumerable<string> keys, ISettingsDictionary dict)
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
        public KeyValuePair<string, Unifiable> Current
        {
            get
            {
                string key = keysE.Current;
                return new KeyValuePair<string, Unifiable>(key, Root.grabSetting(key));
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


