

using System;
using RTParser.Utils;

namespace RTParser
{
    public class StringAppendableUnifiable : StringUnifiable
    {
        public override Unifiable Frozen(SubQuery subquery)
        {
            return "" + ToValue(subquery);
        }

        public StringAppendableUnifiable()
        {
            IsAppendable = true;
        }

        public override void Clear()
        {
            str = "";
            splitted = null;
            rest = null;
        }

        public override string AsString()
        {
            return str.Trim().Replace("  ", " ");
        }
        public override void Append(string p)
        {
            splitted = null;
            rest = null;
            if (!IsAppendable)
            {
                throw new Exception("this " + AsString() + " cannot be appended with " + p);
            }
            if (Unifiable.IsNullOrEmpty(p)) return;
            if (IsEmpty)
            {
                str = p;
                return;
            }
            else
            {
                p = p.Trim();
                if (!NoSpaceAfter(str) && !NoSpaceBefore(p))
                {
                    str += " ";
                }
                str += p;
                str = str.Replace("  ", " ");
            }
        }

        public override void Append(Unifiable p)
        {
            Append(p.AsString());
        }

        private bool NoSpaceAfter(string str)
        {
            if (str.EndsWith("\"")) return true;
            if (str.EndsWith("\'")) return true;
            return false;
        }
        private bool NoSpaceBefore(string str)
        {
            if (str.StartsWith("\"")) return true;
            if (str.StartsWith("\'")) return true;
            return false;
        }
    }
}