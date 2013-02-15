using System;

namespace AltAIMLbot.Variables
{
    //public delegate void SetUnifiable(Unifiable unifiable);

    public delegate TT GetUnifiable<TT>();
    public class GetSetProperty<T>
    {
        readonly GetUnifiable<T> getter;
        readonly Action<T> setter;

        public GetSetProperty(GetUnifiable<T> g, Action<T> s)
        {
            getter = g;
            setter = s;
        }

        public void SetValue(object oldValue, object newValue, object unused)
        {
            setter.Invoke((T)(object)Unifiable.Create(newValue));
        }

        public object GetValue(object oldValue, object unused)
        {
            return getter();
        }
    }
}