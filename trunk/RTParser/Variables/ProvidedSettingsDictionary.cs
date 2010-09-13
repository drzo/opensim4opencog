using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using RTParser.Variables;

namespace RTParser.Variables
{
    public class ProvidedSettingsDictionary : ISettingsDictionary
    {
        internal Func<ISettingsDictionary> provider;

        public SettingsDictionary AsSettingDictionary() {
            return Inner as SettingsDictionary;
        }

        public ProvidedSettingsDictionary(Func<ISettingsDictionary> parent)
        {
            provider = (Func<ISettingsDictionary>)parent;
        }

        public ProvidedSettingsDictionary(string parent, Func<ISettingsDictionary> func)
        {
            provider = func;

        }

        public ISettingsDictionary Inner
        {
            get { return provider(); }
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
            return Inner.addSetting(name, value);
        }

        /// <summary>
        /// Removes the named setting from this class
        /// </summary>
        /// <param name="name">The name of the setting to remove</param>
        public bool removeSetting(string name)
        {
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
            return Inner.addSetting(name, value);
        }

        /// <summary>
        /// Returns the value of a setting given the name of the setting
        /// </summary>
        /// <param name="name">the name of the setting whose value we're interested in</param>
        /// <returns>the value of the setting</returns>
        public Unifiable grabSetting(string name)
        {
            return Inner.grabSetting(name);
        }

        /// <summary>
        /// Checks to see if a setting of a particular name exists
        /// </summary>
        /// <param name="name">The setting name to check</param>
        /// <returns>Existential truth value</returns>
        public bool containsLocalCalled(string name)
        {
            return Inner.containsLocalCalled(name);
        }

        public bool containsSettingCalled(string name)
        {
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

        public IEnumerable<string> SettingNames(int depth)
        {
            return Inner.SettingNames(depth);
        }

        #endregion
    }
}
