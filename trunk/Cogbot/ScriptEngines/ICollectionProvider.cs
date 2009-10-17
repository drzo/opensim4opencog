using System.Collections;

namespace cogbot.ScriptEngines
{
    public interface ICollectionProvider
    {
        ICollection GetGroup(string name);
    }

}