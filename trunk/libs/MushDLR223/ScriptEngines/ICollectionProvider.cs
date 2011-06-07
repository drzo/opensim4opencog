using System.Collections;

namespace MushDLR223.ScriptEngines
{
    public interface ICollectionProvider
    {
        ICollection GetGroup(string name);
    }

}