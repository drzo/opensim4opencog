using java.io;

namespace RTParser
{
    public class StringAppendableUnifiable : StringUnifiable
    {
        public override Unifiable Frozen()
        {
            return "" + ToValue();
        }

        public StringAppendableUnifiable()
        {
            IsAppendable = true;
        }

        public override void Clear()
        {
            str = "";
            splitted = null;
        }

        public override string AsString()
        {
            return str.Trim().Replace("  ", " ");
        }

        public override void Append(Unifiable p)
        {
            if (!IsAppendable)
            {
                throw new InvalidObjectException("this " + AsString() + " cannot be appended with " + p);
            }
            if (Unifiable.IsNullOrEmpty(p)) return;
            if (str == "")
                str = p.AsString().Trim();
            else
            {
                p = p.Trim();
                if (!NoSpaceAfter(str) && !NoSpaceBefore(p))
                    str += " ";
                else
                {
                    str = str;
                    str += " ";
                }
                splitted = null;
                str += p.AsString().Trim();
                str = str.Replace("  ", " ");
            }
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