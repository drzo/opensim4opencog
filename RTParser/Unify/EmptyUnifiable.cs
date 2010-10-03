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

        public override bool IsEmpty
        {
            get
            {
                return true;
            }
        }

        public override void Append(Unifiable p)
        {
            throw new InvalidCastException("Empty Unifiable");
        }
    }
}