using System;
using System.Collections;
using System.Collections.Generic;
using MushDLR223.Utilities;

namespace MushDLR223.ScriptEngines
{
    public delegate ICollection GetGroupFunc(string name);
    public interface ICollectionProvider
    {
        ICollection GetGroup(string name);
        IEnumerable<string> SettingNames(int depth);
    }

    public class GetGroupFuncHolder : ICollectionProvider
    {
        private GetGroupFunc ggf;
        private string Name;

        public GetGroupFuncHolder(string name, GetGroupFunc func)
        {
            this.Name = name;
            ggf = func;
        }

        public ICollection GetGroup(string name)
        {
            return ggf(name);
        }

        public IEnumerable<string> SettingNames(int depth)
        {
            if (Name == null) return null;
            return new [] { Name };
        }
    }
    public class DictionaryWrapper : ICollectionProvider
    {
        static public DictionaryWrapper CreateDictionaryWrapper(IDictionary<string,object> dict)
        {
            return new DictionaryWrapper(dict);
        }

        #region Implementation of ICollectionProvider

        private readonly IDictionary<string, object> Dict;

        public DictionaryWrapper(IDictionary<string, object> dict)
        {
            Dict = dict;
        }

        public ICollection GetGroup(string name)
        {
            object val;
            lock (Dict)
            {
                if (Dict.TryGetValue(name, out val))
                {
                    return SingleNameValue.AsCollection(val);
                }
            }
            return null;
        }

        public IEnumerable<string> SettingNames(int depth)
        {
            lock (Dict) return Dict.Keys;
        }

        #endregion
    }
    public class SingleNameValue : IKeyValuePair<string, object>, IDisposable
    {
        public static IList<IKeyValuePair<string, object>> MakeKVPs<T>(IDictionary<string, T> dict)
        {
            var list = new List<IKeyValuePair<string, object>>();
            foreach (var p in dict.Keys)
            {
                string key = p;
                list.Add(new SingleNameValue(p, () => AsCollection(dict[key])));
            }
            return list;
        }
        public static IList<IKeyValuePair<string, object>> MakeKVP(ICollectionProvider dict, int depth)
        {
            var list = new List<IKeyValuePair<string, object>>();
            foreach (var p in dict.SettingNames(depth))
            {
                string key = p;
                list.Add(new SingleNameValue(p, () => AsCollection(dict.GetGroup(key))));
            }
            return list;
        }

        private Func<IList> Funct;
        private Action Disp;

        public SingleNameValue(string name, Func<IList> func)
        {
            Key = name;
            Funct = func;
        }

        #region Implementation of IKeyValuePair

        public object Value
        {
            get { return Funct(); }
            set { throw new NotImplementedException(); }
        }

        public string Key
        { get; private set; }
        #endregion

        #region Implementation of IDisposable

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {
            if (Disp != null) Disp();
            Funct = null;
        }

        #endregion

        public static IList AsCollection(object value)
        {
            if (value == null) return null;
            if (value is IConvertible) return new[] { value };
            if (value is IList) return (IList)value;
            if (value is ICollection)
            {
                List<object> objs = new List<object>();
                var col = (ICollection)value;
                foreach (var c in col)
                {
                    objs.Add(c);
                }
                return objs;
            }
            return new[] { value };
        }
    }
}