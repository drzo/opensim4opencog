using System;
using System.Collections.Generic;
using RTParser.Utils;

namespace RTParser
{
    public class BestUnifiable : Unifiable

    {
        public override bool IsLitteral()
        {
            foreach (var u in List)
            {
                if (!u.IsLitteral())
                {
                    return false;
                }
            }
            return true;   
        }

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
        /*
        public bool CanUnify(Unifiable unifiable, SubQuery subquery)
        {
            foreach (var u in List)
            {
                if (u.IsMatch(unifiable, subquery))
                {
                    best = u;
                    return true;
                }
            }
            return false;
        }
        */
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

        public override bool IsFiniteWildCard()
        {
            foreach (var u in List)
            {
                if (u.IsFiniteWildCard())
                {
                    best = u;
                    return true;
                }
            }
            return true;
        }

        public override MatchWidth Width
        {
            get
            {
                if (IsLongWildCard())
                {
                    return MatchWidth.MORE_THAN_ONE;
                }
                if (IsFiniteWildCard()) return MatchWidth.ONE_OR_TWO;
                return MatchWidth.ONLY_ONE;
            }
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
        /*

        public override float UnifyLazy(Unifiable unifiable, SubQuery query)
        {
            best = null;
            float bestf = 0;
            foreach (var u in List)
            {
                float b = u.UnifyLazy(unifiable ,query);
                if (b > bestf) best = u;
            }
            return unifiable.UnifyLazy(best, query);
            //return bestf;
        }
        */
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

        public override bool ConsumeFirst(Unifiable fullpath, out Unifiable left, out Unifiable right, SubQuery query)
        {

            left = null;
            right = null;
            if (best != null)
            {
                bool res = best.ConsumeFirst(fullpath, out left, out right, query);
                if (res) return true;
            }
            foreach (var u in List)
            {
                if (object.ReferenceEquals(best, u)) continue;
                bool res = u.ConsumeFirst(fullpath, out left, out right, query);
                if (res)
                {
                    best = u;
                    return true;
                }
            }
            return false;
        }


        public override string ToValue(SubQuery subquery)
        {
            if (best != null)
            {
                string res = best.ToValue(subquery);
                if (res != null && res.Trim().Length > 0) return res;
            }
            foreach (var u in List)
            {
                if (object.ReferenceEquals(best,u)) continue;
                string res = u.ToValue(subquery);
                if (res != null && res.Trim().Length > 0)
                {
                    best = u;
                    return res;
                }
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

        public override void Append(string part)
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