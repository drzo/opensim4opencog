using System;
using System.Collections;
using System.Collections.Generic;
using System.Xml;
using AltAIMLbot.Utils;
using MushDLR223.Utilities;

namespace RTParser
{
    [Serializable]
    public class BestUnifiable : Unifiable

    {
        private Unifiable best;
        private bool insideSearch;
        [NonSerialized]
        public string rawCache = null;
        public ListAsSet<Unifiable> List = new ListAsSet<Unifiable>();
        [NonSerialized]
        public XmlNode nodeOuter;

        public BestUnifiable(string inlist, bool splitOnSpaces)
        {
            rawCache = null;// inlist;
            bool orSyntax = IsORSyntax(inlist);
            if (!inlist.StartsWith("<xor>"))
            {
                if (!orSyntax) throw new NotImplementedException();
            }
// ReSharper disable DoNotCallOverridableMethodsInConstructor
            SetFromString(inlist);
// ReSharper restore DoNotCallOverridableMethodsInConstructor
        }

        public override void SetFromString(string inlist)
        {
            string[] strings = null;
            if (inlist == "<xor></xor>") return;
            if (IsORSyntax(inlist))
            {
                inlist = inlist.Substring(1, inlist.Length - 2);
                strings = inlist.Split(new[] {"|"}, StringSplitOptions.None);
            }
            else
            {
                inlist = inlist.Replace("<ligt", "<li>");
                strings = inlist.Split(new[] { "<xor><li>", "</li><li>", "</li></xor>" },
                                                StringSplitOptions.RemoveEmptyEntries);
            }
            if (strings.Length == 0)
            {
                return;
            }
            foreach (string s in strings)
            {
                var u = (Unifiable) s;
                List.Add(u);
                Flags = u.Flags;
            }
            //best = strings[0];
        }

        public BestUnifiable(IEnumerable unifiable)
        {
            foreach (var enumerable in unifiable)
            {
                Unifiable e = Create(enumerable);
                List.Add(e);
                Flags = e.Flags;
            }
        }

        public void AddBest(Unifiable value)
        {
            lock (List) List.Insert(0, value);
            rawCache = null;
            best = value;
        }
        public BestUnifiable AddItem(Unifiable value)
        {
            var bu = new BestUnifiable(List);
            bu.List.Insert(0, value);
            return bu;
        }

        protected override string GenerateSpecialName
        {
            get { return ToUpper(AsString()); }
        }

        /// <summary>
        /// All members are litteral
        /// </summary>
        public override bool IsLitteral
        {
            get
            {
                foreach (Unifiable u in List)
                {
                    if (!u.IsLitteral)
                    {
                        return false;
                    }
                }
                return true;
            }
        }

        public override bool IsLitteralText
        {
            get
            {
                if (best == null) throw noBest();
                return best.IsLitteralText;
            }
        }

        public override object Raw
        {
            get
            {              
                if (rawCache != null) return rawCache;
                return AsString();
                if (best != null) return best.Raw;
                throw noBest();
            }
        }

        public override bool IsAnyText
        {
            get
            {
                foreach (Unifiable u in List)
                {
                    if (u.IsAnyText)
                    {
                        return true;
                    }
                }
                return false;
            }
        }

        public override bool IsHighPriority
        {
            get
            {
                foreach (Unifiable u in List)
                {
                    if (u.IsHighPriority)
                    {
                        return true;
                    }
                }
                return false;
            }
        }

        public override bool IsLazy
        {
            get
            {
                return true;
                foreach (Unifiable list in List)
                {
                    if (list.IsLazy)
                    {
                        //best = list;
                        return true;
                    }
                }
                return false;
            }
        }

        public override Unifiable[] Possibles
        {
            get { return List.ToArray(); }
        }

        public override string ToDebugString()
        {
            return AsString();
        }

        public override int RunLowMemHooks()
        {
            lock (this)
            {
                int total = 0;
                if (insideSearch) return 0;
                try
                {
                    insideSearch = true;
                    if (rawCache != null)
                    {
                        rawCache = null;
                        total++;
                    }
                    if (nodeOuter != null)
                    {
                        nodeOuter = null;
                        total++;
                    }
                    foreach (Unifiable u in List)
                    {
                        if (u != null && ReferenceEquals(u, this))
                        {
                            total += u.RunLowMemHooks();
                        }
                    }
                }
                finally
                {
                    insideSearch = false;
                }
                return total;
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

        public override bool SameMeaningCS(Unifiable s, bool caseSensitive)
        {
            foreach (Unifiable unifiable in List)
            {
                if (unifiable.SameMeaningCS(s, caseSensitive)) return true;
            }
            return false;
        }

        public override bool SameMeaning(Unifiable s)
        {
            foreach (Unifiable unifiable in List)
            {
                if (unifiable.SameMeaning(s)) return true;
            }
            return false;
        }

        public override void OfferNode(XmlNode node, string inner)
        {
            nodeOuter = node;
        }

        public override bool IsTag(string s)
        {
            foreach (Unifiable u in List)
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

        public override string ToUpper()
        {
            return Raw.ToString().ToUpper();
        }

        public override double Strictness
        {
            get
            {
                if (best != null) return best.Strictness;
                double leastStrict = 0;
                foreach (Unifiable list in List)
                {
                    double cand = list.Strictness;
                    if (cand < leastStrict)
                    {
                        leastStrict = cand;
                    }
                }
                return leastStrict;
            }
        }

        public override string ToKey()
        {
           // return "*";
            return ToUpper();
        }
        public override string OverlyMatchableString
        {
            get
            {
                return "*";
            }
        }
        public override bool IsExactKey
        {
            get { return false; }
        }

        public override bool WillMatch(string word, SubQuery query)
        {
            return WillMatch0(word.ToUpper(), query);
        }

        public override bool WillMatch0(string word, SubQuery query)
        {
            foreach (Unifiable u in List)
            {
                if (u.WillMatch0(word, query))
                {
                    return true;
                }
            }
            return false;
        }

        public override int CompareTo(Unifiable other)
        {
            double strictness = Strictness;
            double otherStrictness = other.Strictness;
            if (strictness == otherStrictness)
            {
                return AsString().CompareTo(other.AsString());
            }
            return strictness.CompareTo(otherStrictness);
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
            foreach (Unifiable u in List)
            {
                float b = u.Unify(unifiable, query);
                if (b > bestf) best = u;
            }
            return unifiable.Unify(best, query);
            //return bestf;
        }

        public override bool ConsumePath(int at, string[] fullpath, out string left, out Unifiable after, out int newAt,
                                         SubQuery query)
        {
            if (best != null)
            {
                bool res = best.ConsumePath(at, fullpath, out left, out after, out newAt, query);
                if (res) return true;
            }
            foreach (Unifiable u in List)
            {
                if (ReferenceEquals(best, u)) continue;
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
            foreach (Unifiable u in List)
            {
                if (ReferenceEquals(best, u)) continue;
                string res = u.ToValue(subquery);
                if (res != null && Trim(res).Length > 0)
                {
                    best = u;
                    return res;
                }
            }
            throw noBest();
        }

        public override object AsNodeXML
        {
            get
            {
                if (best != null) return best.AsNodeXML;
                foreach (Unifiable u in List)
                {
                    return u.AsNodeXML;
                }
                throw noBest();
            }
        }

        public override Unifiable[] ToArray()
        {
            if (best != null) return best.ToArray();
            foreach (Unifiable u in List)
            {
                return u.ToArray();
            }
            throw noBest();
        }

        public override bool IsWildCard
        {
            get { return true; }
        }

        public override bool IsCatchAll
        {
            get
            {
                foreach (Unifiable u in List)
                {
                    if (u.IsCatchAll)
                    {
                        // best = u;
                       // return true;
                    }
                }
                return false; 
            }
        }

        private Exception noBest()
        {
            throw new NotImplementedException();
        }

        public override string AsString()
        {
            //if (List.Count == 1) return List[0].AsString();
            if (rawCache != null) return rawCache;
            //if (best != null) return best.AsString();
            string results = "<xor>";
            foreach (Unifiable u in List)
            {
                results += "<li>" + u.AsString() + "</li>";
            }
            rawCache = results + "</xor>";
            return rawCache;
            throw noBest();
        }

        public override void Append(Unifiable part)
        {
            foreach (Unifiable u in List)
            {
                u.Append(part);
            }
        }

        public override void Append(string part)
        {
            foreach (Unifiable u in List)
            {
                u.Append(part);
            }
        }

        public override void Clear()
        {
            foreach (Unifiable u in List)
            {
                u.Clear();
            }
        }

        public override Unifiable First
        {
            get
            {
                if (best != null) return best.First;
                foreach (Unifiable u in List)
                {
                    return u.First;
                }
                return best;
            }
        }

        public override Unifiable Rest
        {
            get
            {
                if (best != null) return best.Rest;
                foreach (Unifiable u in List)
                {
                    return u.Rest;
                }
                return Empty;
            }
        }
    }
}