using System;

namespace MushDLR223.ScriptEngines
{

    public class FilterSpecAttribute : Attribute
    {
        public bool LastArgIsSelf = false;
    }

    public interface DenotingAnotherType
    {
        Type ImplementationType { get; }
    }

    public class ConvertToAttribute : Attribute, DenotingAnotherType
    {
        #region Implementation of DenotingAnotherType
        public Type ImplementationType {get; set;}
        #endregion
    }

    public interface MixinSubObjects
    {
        Type[] GetMixedTypes();
        object GetInstance(Type subtype);
        T GetInstance<T>();
    }
}