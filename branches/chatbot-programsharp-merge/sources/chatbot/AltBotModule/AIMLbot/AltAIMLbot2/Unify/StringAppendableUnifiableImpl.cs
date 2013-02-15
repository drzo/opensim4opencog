

using System;
using AltAIMLbot;
using AltAIMLbot.Utils;

namespace AltAIMLbot
{
    [Serializable]
    public class StringAppendableUnifiableImpl : StringUnifiable, UnifiableList
    {
        public override Unifiable[] Possibles
        {
            get
            {
                return base.Possibles;
            }
        }
        public override bool AddCategory(IndexTarget template)
        {
            if (NOCateIndex) return false;
            Unifiable unify = ToUpper();
            return unify.AddCategory(template);            
        }
        public override bool RemoveCategory(IndexTarget template)
        {
            if (NOCateIndex) return false;
            Unifiable unify = ToUpper();
            return unify.RemoveCategory(template);
        }

        public override bool SameMeaningCS(Unifiable s, bool caseSensitive)
        {
            if (s is BestUnifiable) return s.SameMeaningCS(this, caseSensitive);
            if (ReferenceEquals(this, s)) return true;
            bool null2 = ReferenceEquals(s, null);
            if (null2) return false;
            if (caseSensitive)
            {
                if (str == s.AsString())
                {
                    return true;
                }
                return false;
            }
            if (ToUpper(str) == s.ToUpper())
            {
                return true;
            }
            return false;
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
            return str;
        }

        public override string ToUpper()
        {
            if (Trim(str).Length == 0) return "";
            return base.ToUpper();
        }

        protected override string GenerateSpecialName
        {
            get { return ToUpper(str); }
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