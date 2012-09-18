using System;

namespace RTParser.Variables
{
    public delegate Unifiable GetUnifiable();
    //public delegate void SetUnifiable(Unifiable unifiable);

    public class GetSetProperty
    {
        readonly GetUnifiable getter;
        readonly Action<Unifiable> setter;

        public GetSetProperty(GetUnifiable g, Action<Unifiable> s)
        {
            getter = g;
            setter = s;
        }

        public void SetValue(object oldValue, object newValue, object unused)
        {
            setter.Invoke(Unifiable.Create(newValue));
        }

        public object GetValue(object oldValue, object unused)
        {
            return getter();
        }
    }
}