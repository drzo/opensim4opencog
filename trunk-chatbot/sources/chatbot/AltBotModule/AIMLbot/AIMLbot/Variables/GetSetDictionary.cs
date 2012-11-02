using System;
using System.Collections;
using System.Collections.Generic;
using MushDLR223.ScriptEngines;
using RTParser.Variables;

namespace RTParser.Variables
{
    internal class GetSetDictionary<T> : ISettingsDictionaryT<T>
    {
        public bool IsTraced { get; set; }
        public IEnumerable<string> SettingNames(ICollectionRequester requester, int depth)
        {
         //   get 
            { return new[] { named }; }
        }

        public string NameSpace
        {
            // TODO: need a prepend?
            get { return named; }
        }

        readonly private GetSetProperty<T> info;
        readonly private string named;
        private object oldValue = null;
        public GetSetDictionary(string name, GetSetProperty<T> gs)
        {    
            named = name;
            info = gs;
        }
        private void propSet(object p)
        {
            info.SetValue(oldValue, p, null);
        }
        private object propGet()
        {
            return info.GetValue(oldValue, null);
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
                return (T)(object)Unifiable.Create(propGet());
            }
            return (T)(object)Unifiable.Empty;
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