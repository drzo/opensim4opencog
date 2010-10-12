using System;

namespace RTParser
{
    internal class EmptyUnifiable : StringUnifiable
    {
        public EmptyUnifiable()
            : base("")
        {
            //str = "";
        }
        public override void Append(string p)
        {
            throw new Exception("this " + AsString() + " cannot be appended with " + p);
        }
        public override void Append(Unifiable p)
        {
            throw new InvalidCastException("Empty Unifiable");
        }
    }
}