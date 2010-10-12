

using System;
using RTParser.Utils;

namespace RTParser
{
    public class StringAppendableUnifiableImpl : StringUnifiable, UnifiableList
    {
        public override void AddCategory(CategoryInfo template)
        {
            throw new NotImplementedException();
        }

        public override void RemoveCategory(CategoryInfo template)
        {
            throw new NotImplementedException();
        }

        public override Unifiable FullPath
        {
            get
            {
                throw new NotImplementedException(); 
                return Create(str);
            }
        }

        public override Unifiable Frozen(SubQuery subquery)
        {
            string str0 = ToValue(subquery);
            return str0;
            /*
            var u = new StringAppendableUnifiableImpl();
            u.str = str0;           
            return u;
             */
        }

        public StringAppendableUnifiableImpl()
        {
            Flags |= UFlags.APPENDABLE;
        }

        public override void Clear()
        {
            str = "";
            SpoilCache();
        }

        public override string AsString()
        {
            return base.AsString();
        }

        public override string ToUpper()
        {
            return base.ToUpper();
        }

        public override int RunLowMemHooks()
        {
            return base.RunLowMemHooks();
        }
        public override void Append(string p)
        {
            if (string.IsNullOrEmpty(p)) return;
            base.SpoilCache();
            if (IsNullOrEmpty(this))
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
            p = p.Replace("  ", " ");
            p = Trim(p);
            str = p;
        }

        public override void Append(Unifiable p)
        {
            if (p != null) Append(p.AsString());
        }

        private bool NoSpaceAfter(string str)
        {
            if (str.EndsWith("\"")) return true;
            if (str.EndsWith(">")) return true;
            if (str.EndsWith("\'")) return true;
            return false;
        }
        private bool NoSpaceBefore(string str)
        {
            if (str.StartsWith("\"")) return true;
            if (str.StartsWith("<")) return true;
            if (str.StartsWith("\'")) return true;
            return false;
        }

        public int Length
        {
            get { return str.Length; }
            set
            {
                str = str.Substring(0, value);
                SpoilCache();
            }
        }
    }

    public interface UnifiableList
    {
    }
}