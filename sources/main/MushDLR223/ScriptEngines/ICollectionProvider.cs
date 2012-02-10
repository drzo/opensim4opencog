using System;
using System.Collections;

namespace MushDLR223.ScriptEngines
{
    public delegate ICollection GetGroupFunc(string name);
    public interface ICollectionProvider
    {
        ICollection GetGroup(string name);
    }
    public class GetGroupFuncHolder : ICollectionProvider
    {
        private GetGroupFunc ggf; 
        public GetGroupFuncHolder(GetGroupFunc func)
        {
            ggf = func;
        }

        public ICollection GetGroup(string name)
        {
            return ggf(name);
        }
    }

}