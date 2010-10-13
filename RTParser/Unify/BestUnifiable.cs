using System;
using System.Collections.Generic;
using RTParser.Utils;

namespace RTParser
{
    public class BestUnifiable : Unifiable

    {

        public override string SpecialName
        {
            get { return AsString(); }
        }

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

        public override bool IsLitteralText()
        {
            if (best==null) throw noBest();
            return best.IsLitteralText();
            
        }

        private Unifiable best;
        public List<Unifiable> List = new List<Unifiable>();
        private bool insideSearch = false;

        public override int RunLowMemHooks()
        {
            int total = 0;
            lock (this)
            {
                if (insideSearch) return 0;
                try
                {
                    insideSearch = true;
                    foreach (var u in List)
                    {
                        if (u != null && ReferenceEquals(u, this))
                        {
                            total += u.RunLowMemHooks();
                        }
                    }
                    return total;
                }
                finally
                {
                    insideSearch = false;
                }
            }
        }

        public override bool Equals(object obj)
        {
            foreach (Unifiable unif in List)
            {
                if (!unif.Equals(obj)) return false;
            }
            return true;
        }

        public override int GetHashCode()
        {
            return -1;
        }

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

        public override bool IsAnySingleUnit()
        {
            foreach (var u in List)
            {
                if (u.IsAnySingleUnit())
                {
                    best = u;
                    return true;
                }
            }
            return true;
        }

        public override string ToUpper()
        {
            throw new NotImplementedException();
        }

        public override double Strictness()
        {
            if (best != null) return best.Strictness();
            double leastStrict = 0;
            foreach (var list in List)
            {
                double cand = list.Strictness();
                if (cand < leastStrict)
                {
                    leastStrict = cand;
                }
            }
            return leastStrict;
        }

        public override int CompareTo(Unifiable other)
        {
            double strictness = this.Strictness();
            double otherStrictness = other.Strictness();
            if (strictness == otherStrictness)
            {
                return AsString().CompareTo(other.AsString());
            }
            return strictness.CompareTo(otherStrictness);
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

        public override bool ConsumePath(int at, string[] fullpath, out string left, out Unifiable after, out int newAt, SubQuery query)
        {

            if (best != null)
            {
                bool res = best.ConsumePath(at, fullpath, out left, out after, out newAt, query);
                if (res) return true;
            }
            foreach (var u in List)
            {
                if (object.ReferenceEquals(best, u)) continue;
                bool res = u.ConsumePath(at, fullpath, out left, out after, out newAt, query);
                if (res)
                {
                    best = u;
                    return true;
                }
            }

            left = null;
            after = null;
            newAt = at;
            return false;
        }


        public override string ToValue(SubQuery subquery)
        {
            if (best != null)
            {
                string res = best.ToValue(subquery);
                if (res != null && Trim(res).Length > 0) return res;
            }
            foreach (var u in List)
            {
                if (object.ReferenceEquals(best,u)) continue;
                string res = u.ToValue(subquery);
                if (res != null && Trim(res).Length > 0)
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