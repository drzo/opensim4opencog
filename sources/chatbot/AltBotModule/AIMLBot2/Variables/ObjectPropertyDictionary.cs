using System;
using System.Collections.Generic;
using System.Reflection;
using MushDLR223.ScriptEngines;
using AltAIMLbot.Utils;

namespace AltAIMLbot.Variables
{
    internal class ObjectPropertyDictionary<T> : ISettingsDictionaryT<T>
    {
        public bool IsTraced { get; set; }
        public IEnumerable<string> SettingNames(ICollectionRequester requester, int depth)
        {
            //get
            { return new[] { named }; }
        }

        public string NameSpace
        {
            // TODO: need a prepend?
            get { return named; }
        }

        readonly private object obj;
        readonly private string named;
        private object oldValue = null;
        public ObjectPropertyDictionary(string name, string pname, object o)
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

        public bool addSetting(string name, object value)
        {
            if (!containsLocalCalled(name)) return false;
            oldValue = propGet();
            propSet(value);
            return true;
        }

        public bool removeSetting(string name)
        {
            if (!containsLocalCalled(name)) return false;
            propSet(oldValue);
            return true;
        }

        public bool updateSetting(string name, object value)
        {
            if (containsLocalCalled(name))
            {
                oldValue = propGet();
                propSet(value);
                return true;
            }
            return false;
        }

        public T grabSetting(string name)
        {
            if (containsLocalCalled(name))
            {
                return (T)MushDLR223.ScriptEngines.ScriptManager.ChangeType(propGet(), typeof(T));
            }
            return (T)MushDLR223.ScriptEngines.ScriptManager.ChangeType(Unifiable.Empty, typeof(T));
        }

        public bool containsLocalCalled(string name)
        {
            return named.ToLower() == name.ToLower();
        }
        public bool containsSettingCalled(string name)
        {
            return named.ToLower() == name.ToLower();
        }

        #endregion
    }
}