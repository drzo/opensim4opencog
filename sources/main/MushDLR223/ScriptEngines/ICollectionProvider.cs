using System;
using System.Collections;
using System.Collections.Generic;

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

}