using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using MushDLR223.ScriptEngines;
using RTParser.Variables;
using DataUnifiable = RTParser.Unifiable;


namespace RTParser.Variables
{
    public class ProvidedSettingsDictionary : ISettingsDictionaryT<DataUnifiable>, ISettingsDictionary
    {
        internal Func<ParentProvider> provider;

        public SettingsDictionary AsSettingDictionary() {
            return Inner as SettingsDictionary;
        }

        public ProvidedSettingsDictionary(Func<ParentProvider> parent)
        {
            provider = parent;
        }

        public ProvidedSettingsDictionary(string parent, Func<ParentProvider> func)
        {
            provider = func;
            NameSpace = parent;
        }

        private ISettingsDictionary cache;
        public bool IsAvailable
        {
            get { return Inner != null; }
        }
        public ISettingsDictionary Inner
        {
            get
            {
                if (cache == null)
                {
                    try
                    {

                        if (provider != null)
                        {
                            var pp = provider();
                            if (pp != null)
                            {
                                cache = pp();
                            }
                        }
                    }
                    catch
                    {
                    }
                }
                return cache;
            }
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
            if (!IsAvailable) return false;
            return Inner.addSetting(name, value);
        }

        /// <summary>
        /// Removes the named setting from this class
        /// </summary>
        /// <param name="name">The name of the setting to remove</param>
        public bool removeSetting(string name)
        {
            if (!IsAvailable) return false;
            return Inner.removeSetting(name);
        }

        /// <summary>
        /// Updates the named setting with a new value whilst retaining the position in the
        /// dictionary
        /// </summary>
        /// <param name="name">the name of the setting</param>
        /// <param name="value">the new value</param>
        public bool updateSetting(string name, Unifiable value)
        {
            if (!IsAvailable) return false;
            return Inner.addSetting(name, value);
        }

        /// <summary>
        /// Returns the value of a setting given the name of the setting
        /// </summary>
        /// <param name="name">the name of the setting whose value we're interested in</param>
        /// <returns>the value of the setting</returns>
        public Unifiable grabSetting(string name)
        {
            if (!IsAvailable) return null;
            return Inner.grabSetting(name);
        }

        /// <summary>
        /// Checks to see if a setting of a particular name exists
        /// </summary>
        /// <param name="name">The setting name to check</param>
        /// <returns>Existential truth value</returns>
        public bool containsLocalCalled(string name)
        {
            if (!IsAvailable) return false;
            return Inner.containsLocalCalled(name);
        }

        public bool containsSettingCalled(string name)
        {
            if (!IsAvailable) return false;
            return Inner.containsSettingCalled(name);
        }

        public string NameSpace
        {
            get;
            set;
        }

        public bool IsTraced
        {
            get;
            set;
        }

        public IEnumerable<string> SettingNames(ICollectionRequester requester, int depth)
        {
            if (!IsAvailable) return new string[0];
            return Inner.SettingNames(requester, depth);
        }

        #endregion
    }
}
