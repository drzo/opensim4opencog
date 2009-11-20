using System;
using System.Collections.Generic;
using RTParser.Utils;

namespace RTParser
{
    public class ListUnifiable : Unifiable

    {
        private Unifiable best;
        public List<Unifiable> List = new List<Unifiable>();

        protected override object Raw
        {
            get { throw new NotImplementedException(); }
        }

        public override bool IsTag(string s)
        {
            foreach (var u in List)
            {
                if (u.IsTag(s))
                {
                    best = u;
                    return true;
                }
            }
            return false;
        }

        public override bool IsMatch(Unifiable unifiable)
        {
            foreach (var u in List)
            {
                if (u.IsMatch(unifiable))
                {
                    best = u;
                    return true;
                }
            }
            return false;
        }

        public override bool IsLazyStar()
        {
            foreach (var u in List)
            {
                if (u.IsLazyStar())
                {
                    best = u;
                    return true;
                }
            }
            return false;
        }

        public override bool IsLongWildCard()
        {
            foreach (var u in List)
            {
                if (u.IsLongWildCard())
                {
                    best = u;
                    return true;
                }
            }
            return false;
        }

        public override bool IsShortWildCard()
        {
            foreach (var u in List)
            {
                if (u.IsShortWildCard())
                {
                    best = u;
                    return true;
                }
            }
            return true;
        }

        public override float Unify(Unifiable unifiable, SubQuery query)
        {
            best = null;
            float bestf = 0;
            foreach (var u in List)
            {
                float b = u.Unify(unifiable, query);
                if (b > bestf) best = u;
            }
            return unifiable.Unify(best, query);
            //return bestf;
        }

        public override string ToValue()
        {
            if (best != null) return best.ToValue();
            foreach (var u in List)
            {
                return u.ToValue();
            }
            throw new NotImplementedException();
        }

        public override string AsString()
        {
            if (best != null) return best.AsString();
            foreach (var u in List)
            {
                return u.AsString();
            }
            throw new NotImplementedException();
        }

        public override void Append(Unifiable part)
        {
            foreach (var u in List)
            {
                u.Append(part);
            }
        }

        public override void Clear()
        {
            foreach (var u in List)
            {
                u.Clear();
            }
        }

        public override Unifiable First()
        {
            if (best != null) return best.First();
            foreach (var u in List)
            {
                return u.First();
            }
            return best;
        }

        public override Unifiable Rest()
        {
            if (best != null) return best.Rest();
            foreach (var u in List)
            {
                return u.Rest();
            }
            return Empty;
        }
    }
}