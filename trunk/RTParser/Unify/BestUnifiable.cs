using System;
using System.Collections.Generic;
using RTParser.Utils;

namespace RTParser
{
    public class BestUnifiable : Unifiable

    {
        private Unifiable best;
        public List<Unifiable> List = new List<Unifiable>();

        public override object Raw
        {
            get
            {
                if (best != null) return best.Raw;
                throw noBest();
            }
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

        public override bool IsLazy()
        {
            foreach (var list in List)
            {
                if (list.IsLazy())
                {
                    best = list;
                    return true;
                }
            }
            return false;
        }

        public override float UnifyLazy(Unifiable unifiable)
        {
            best = null;
            float bestf = 0;
            foreach (var u in List)
            {
                float b = u.UnifyLazy(unifiable);
                if (b > bestf) best = u;
            }
            return unifiable.UnifyLazy(best);
            //return bestf;
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
            throw noBest();
        }

        public override object AsNodeXML()
        {
            if (best != null) return best.AsNodeXML();
            foreach (var u in List)
            {
                return u.AsNodeXML();
            }
            throw noBest();
        }

        public override Unifiable[] ToArray()
        {
            if (best != null) return best.ToArray();
            foreach (var u in List)
            {
                return u.ToArray();
            }
            throw noBest();
        }

        private Exception noBest()
        {
            throw new NotImplementedException();
        }

        public override string AsString()
        {
            if (best != null) return best.AsString();
            foreach (var u in List)
            {
                return u.AsString();
            }
            throw noBest();
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

        public override bool IsShort()
        {
            foreach (var list in List)
            {
                if (list.IsShort())
                {
                    best = list;
                    return true;
                }
            }
            return false;
        }
    }
}