

using System;
using RTParser.Utils;

namespace RTParser
{
    public class StringAppendableUnifiable : StringUnifiable, UnifiableList
    {
        public override Unifiable Frozen(SubQuery subquery)
        {
            return "" + ToValue(subquery);
        }

        public StringAppendableUnifiable()
        {
            Flags |= UFlags.APPENDABLE;
        }

        public override void Clear()
        {
            _str = "";
            base.SpoilCache();
        }

        public override string AsString()
        {
            return str;
        }


        public override void Append(string p)
        {
            if (string.IsNullOrEmpty(p)) return;
            base.SpoilCache();
            if (IsEmpty)
            {
                str = p;
                return;
            }
            if (!NoSpaceAfter(str) && !NoSpaceBefore(p))
            {
                p = str + " " + p;
            }
            else
            {
                p = str + p;
            }
            p = p.Replace("  ", " ").Trim();
            _str = p;
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

        public int Length
        {
            get { return str.Length; }
        }
    }

    public interface UnifiableList
    {
    }
}