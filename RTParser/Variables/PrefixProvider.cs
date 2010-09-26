using System;
using System.Collections.Generic;

namespace RTParser.Variables
{
    internal class PrefixProvider : ISettingsDictionary
    {
        static readonly IEnumerable<ISettingsDictionary> NONE = new ISettingsDictionary[0];
        public Dictionary<string, ParentProvider> _prefixes = new Dictionary<string, ParentProvider>();
        private string theNameSpace;

        public override string ToString()
        {
            return "[PrefixProvider: " + NameSpace + "]";
        }
        public Unifiable this[string name]
        {
            get { return SettingsDictionary.IndexGet(this, name); }
            set { SettingsDictionary.IndexSet(this, name, value); }
        }

        public ISettingsDictionary GetChild(string fullname, out string childsSettingName)
        {
            foreach (var prefix in _prefixes)
            {
                if (fullname.StartsWith(prefix.Key))
                {
                    childsSettingName = fullname.Substring(prefix.Key.Length);
                    return prefix.Value();
                }
            }
            childsSettingName = fullname;
            return null;
        }

        public ParentProvider GetChildPrefixed(string fullname, out string childsSettingName)
        {
            foreach (var prefix in _prefixes)
            {
                if (fullname.StartsWith(prefix.Key))
                {
                    childsSettingName = fullname.Substring(prefix.Key.Length);
                    return prefix.Value;
                }
            }
            childsSettingName = fullname;
            return null;
        }

        public IEnumerable<ISettingsDictionary> GetChildren(string fullname)
        {
            List<ISettingsDictionary> dicts = null;
            foreach (var prefix in _prefixes)
            {
                if (fullname.StartsWith(prefix.Key))
                {
                    dicts = dicts ?? new List<ISettingsDictionary>();
                    dicts.Add(prefix.Value());
                }
            }
            return (IEnumerable<ISettingsDictionary>)(dicts ?? NONE);
        }


        public ParentProvider AddChild(string prefix, ISettingsDictionary dict)
        {
            ParentProvider pp = () => dict;
            _prefixes.Add(prefix, pp);
            return pp;
        }

        public void AddChild(string prefix, ParentProvider pp)
        {
            _prefixes[prefix] = pp;
        }

        #region Implementation of ISettingsDictionary

        /// <summary>
        /// Adds a bespoke setting to the Settings class (accessed via the grabSettings(string name)
        /// method.
        /// </summary>
        /// <param name="name">The name of the new setting</param>
        /// <param name="value">The value associated with this setting</param>
        public bool addSetting(string name, Unifiable value)
        {
            if (!HasChildren) return false;
            string nextName;
            ISettingsDictionary dict = GetChild(name, out nextName);
            if (dict == null) return false;
            return dict.addSetting(nextName, value);
        }

        /// <summary>
        /// Removes the named setting from this class
        /// </summary>
        /// <param name="name">The name of the setting to remove</param>
        public bool removeSetting(string name)
        {
            if (!HasChildren) return false;
            string nextName;
            ISettingsDictionary dict = GetChild(name, out nextName);
            if (dict == null) return false;
            return dict.removeSetting(nextName);
        }

        /// <summary>
        /// Updates the named setting with a new value whilst retaining the position in the
        /// dictionary
        /// </summary>
        /// <param name="name">the name of the setting</param>
        /// <param name="value">the new value</param>
        public bool updateSetting(string name, Unifiable value)
        {
            if (!HasChildren) return false;
            string nextName;
            ISettingsDictionary dict = GetChild(name, out nextName);
            if (dict == null) return false;
            return dict.updateSetting(nextName, value);
        }

        /// <summary>
        /// Returns the value of a setting given the name of the setting
        /// </summary>
        /// <param name="name">the name of the setting whose value we're interested in</param>
        /// <returns>the value of the setting</returns>
        public Unifiable grabSetting(string name)
        {
            string nextName;
            ISettingsDictionary dict = GetChild(name, out nextName);
            if (dict == null) return null;
            return dict.grabSetting(nextName);
        }

        /// <summary>
        /// Checks to see if a setting of a particular name exists
        /// </summary>
        /// <param name="name">The setting name to check</param>
        /// <returns>Existential truth value</returns>
        public bool containsLocalCalled(string name)
        {
            if (!HasChildren) return false;
            string nextName;
            ISettingsDictionary dict = GetChild(name, out nextName);
            if (dict == null) return false;
            return dict.containsLocalCalled(nextName);
        }

        public bool HasChildren
        {
            get
            {
                return _prefixes.Count > 0;
            }
        }

        public bool containsSettingCalled(string name)
        {

            if (!HasChildren) return false;
            string nextName;
            ISettingsDictionary dict = GetChild(name, out nextName);
            if (dict == null) return false;
            return dict.containsSettingCalled(nextName);
        }


        public string NameSpace
        {
            get { return theNameSpace; }
            set { theNameSpace = value; }
        }

        public bool IsTraced
        {
            get
            {
                foreach (var prefix in _prefixes.Values)
                {
                    if (prefix().IsTraced) return true;
                }
                return false;
            }
            set
            {
                foreach (var prefix in _prefixes.Values)
                {
                    prefix().IsTraced = value;
                }
            }
        }

        public ParentProvider GetProvider(ISettingsDictionary dict)
        {
            foreach (var prefix in _prefixes.Values)
            {
                if (prefix() == dict) return prefix;
            }
            return null;
        }

        public IEnumerable<string> SettingNames(int depth)
        {
            //get
            {
                if (depth < 1) return SettingsDictionary.TOO_DEEP;
                List<String> list = new List<string>();
                foreach (var prefix in _prefixes.Values)
                {
                    ISettingsDictionary child = prefix();
                    foreach (var cn in child.SettingNames(depth - 1))
                    {
                        list.Add(prefix + cn);
                    }
                }
                return list;
            }
        }

        #endregion
    }
}