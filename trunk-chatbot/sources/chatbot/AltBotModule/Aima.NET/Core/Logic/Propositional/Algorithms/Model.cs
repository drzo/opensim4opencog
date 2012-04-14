using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Algorithms
{
    using System.Diagnostics;

    using Aima.Core.Logic.Propositional.Parsing;
    using Aima.Core.Logic.Propositional.Parsing.AST;

    public class Model : IPLVisitor {

        Dictionary<string, bool?> h = new Dictionary<string, bool?>();

        public bool? GetStatus(Symbol symbol)
        {
            bool? status;
            h.TryGetValue(symbol.Value, out status);
            return status;
        }

        public bool IsTrue(Symbol symbol) {
            bool? status;
            h.TryGetValue(symbol.Value, out status);
            if (status != null) 
            {
                return ((bool) status);
            }
            return false;
        }

        public bool IsFalse(Symbol symbol) {
            bool? status;
            h.TryGetValue(symbol.Value, out status);
            if (status != null) {
                return !((bool) status);
            }
            return false;
        }

        private bool IsUnknown(Symbol s) {
            bool? status;
            h.TryGetValue(s.Value, out status);
            return (status == null);

        }

        public Model Extend(Symbol symbol, bool? b) {
           // var m = new Model();
            return this.Extend(symbol.Value, b);
        }

        public Model Extend(string s, bool? b) {
            var m = new Model();

            foreach(var k in this.h.Keys)
            {
                m.h[k] = h[k];
            }

            m.h[s] = b;
            return m;
        }

        public void Print() {
            foreach(var k in this.h.Keys)
            {
                Debug.Write(k + " = " + h[k] + " ");
            }
            Debug.WriteLine("");
        }

        public string strPositives()
        {
            string list = "";
            foreach (var k in this.h.Keys)
            {
                if (h[k] == true)
                {
                    list += " " + k;
                }
            }
            return list;

        }
        public Sentence AsSentence()
        {
            ISet<Symbol> Asyms = GetAssignedSymbols();
            KnowledgeBase tempKB = new KnowledgeBase();
            if (Asyms == null)
            {
                Console.WriteLine("ERR: AsSentence() Asyms is null!!!");
            }
            else
            {
                Console.WriteLine("DBG: Asyms.Count={0}", Asyms.Count);
            }
            foreach (Symbol k in Asyms)
            {
                try
                {
                    if (IsTrue(k))
                    {
                        tempKB.Tell("(" + k.Value + ")");
                    }
                    else
                    {
                        tempKB.Tell("(NOT " + k.Value + ")");
                    }
                }
                catch (Exception e)
                {
                    Console.WriteLine("ERR: AsSentence() ", e.Message, e.StackTrace);
                }
            }
            if (tempKB.Size() == 0) Console.WriteLine("ERR: AsSentence() tempKB.Size==0");
            return tempKB.AsSentence();
        }

        public string AsSentenceString()
        {
            try
            {
                Sentence tempSentence = AsSentence();
                if (tempSentence != null)
                {
                    return tempSentence.ToString();
                }
                else
                {
                    Console.WriteLine("ERR: AsSentenceString() AsSentence returned Null!!! ");
                    return "";
                }
                //return AsSentence().ToString();
            }
            catch (Exception e)
            {
                Console.WriteLine("ERR: AsSentenceString() {0}\n{1}\n", e.Message, e.StackTrace);
                return "";
        
            }
        }

        public bool IsTrue(Sentence clause) {
            object result = clause.Accept(this, null);
            return (result == null) ? false
                    : ((bool) result);
        }

        public bool IsFalse(Sentence clause) {
            object o = clause.Accept(this, null);
            return (o != null) ? ((bool) o) == false : false;
        }

        public bool IsUnknown(Sentence clause) { // TODO TEST WELL
            object o = clause.Accept(this, null);
            return (o == null);
        }

        public bool IsActiveImplication(Sentence clause)
        {
            // returns true when (A=>B), A=t, B=t
            // Or  (A=>B) translated into (-A OR B), A=f (since its been inverted),B=t ...

            object result = clause.Accept(this, null);
            if (result == null) return false;
            if (!(clause is BinarySentence ))  return false;
            BinarySentence bs = (BinarySentence) clause;
            string oper = bs.Operator;
            if (oper.Equals("=>") || oper.Equals("IMPLIES"))
            {
                object firstValue = bs.First.Accept(this, null);
                object secondValue = bs.Second.Accept(this, null);
                if ((firstValue.Equals(true)) && (secondValue.Equals(true))) return true;
            }
            if (oper.Equals("OR"))
            {
                object firstValue = bs.First.Accept(this, null);
                object secondValue = bs.Second.Accept(this, null);
                if ((firstValue.Equals(false)) && (secondValue.Equals(true))) return true;
            }
            return false;
        }

        public string decodeImplication(Sentence clause)
        {
            string decode = clause.ToString();
            if (!(clause is BinarySentence)) return decode;
            BinarySentence bs = (BinarySentence) clause;
            string oper = bs.Operator;
            if (oper.Equals("=>") || oper.Equals("IMPLIES"))
            {
                decode = bs.ToString();
            }
            if (oper.Equals("OR"))
            {
                object firstValue = bs.First.Accept(this, null);
                object secondValue = bs.Second.Accept(this, null);
                if ((firstValue.Equals(false)) && (secondValue.Equals(true)))
                {
                    decode = "( (NOT " + bs.First.ToString() + ") => " + bs.Second.ToString() + "  )";
                }
            }
            return decode;

        }
        public Model Flip(Symbol s) {
            if (this.IsTrue(s)) {
                return this.Extend(s, false);
            }
            if (this.IsFalse(s)) {
                return this.Extend(s, true);
            }
            return this;
        }

        public override string ToString() {
            return h.ToString();
        }

        public object VisitSymbol(Symbol s, ISet<Sentence> arg)
        {
            return this.GetStatus(s);
        }

        public object VisitTrueSentence(TrueSentence ts, ISet<Sentence> arg)
        {
            return true;
        }

        public object VisitFalseSentence(FalseSentence fs, object arg) {
            return false;
        }

        public object VisitNotSentence(UnarySentence fs, ISet<Sentence> arg)
        {
            object negatedValue = fs.Negated.Accept(this, null);
            if (negatedValue != null)
            {
                return (bool) (!((bool) negatedValue));
            }
            return null;
        }

        public object VisitBinarySentence(BinarySentence bs, ISet<Sentence> arg)
        {
            object firstValue = bs.First.Accept(this, null);
            object secondValue = bs.Second.Accept(this, null);
            if ((firstValue == null) || (secondValue == null)) { // strictly not
                // true for or/and
                // -FIX later
                return null;
            } else {
                string oper = bs.Operator;
                if (oper.Equals("AND")) {
                    return this.EvaluateAnd((bool) firstValue, (bool) secondValue);
                }
                if (oper.Equals("OR")) {
                    return this.EvaluateOr((bool) firstValue, (bool) secondValue);
                }
                if (oper.Equals("=>") || oper.Equals("IMPLIES"))
                {
                    return this.EvaluateImplied((bool) firstValue,
                            (bool) secondValue);
                }
                if (oper.Equals("<=>") || oper.Equals("EQUIV"))
                {
                    return this.EvaluateBiConditional((bool) firstValue,
                            (bool) secondValue);
                }
                return null;
            }
        }

        public object VisitMultiSentence(MultiSentence fs, ISet<Sentence> argd)
        {
            return null;
        }

        public ISet<Symbol> GetAssignedSymbols() {
            ISet<Symbol> set = new HashedSet<Symbol>();
            foreach(var pair in h) 
            {
                Symbol key = new Symbol(pair.Key);
                if (!(this.IsUnknown(key))) 
                {
                    set.Add(key);
                }
            }
            return set;
        }

        public bool Matches(string variable, bool? value) {
            if (value == true) {
                return this.IsTrue(new Symbol(variable));
            }
            if (value == false) {
                return this.IsFalse(new Symbol(variable));
            }
            return false;
        }

        private bool EvaluateAnd(bool firstValue, bool secondValue) {
            if ((firstValue.Equals(true))
                    && (secondValue.Equals(true))) {
                return true;
            } else {
                return false;
            }
        }

        private bool EvaluateOr(bool firstValue, bool secondValue) {
            if ((firstValue.Equals(true))
                    || (secondValue.Equals(true))) {
                return true;
            } else {
                return false;
            }
        }

        private bool EvaluateImplied(bool firstValue, bool secondValue) {
            if ((firstValue.Equals(true))
                    && (secondValue.Equals(false))) {
                return false;
            } else {
                return true;
            }
        }

        private bool EvaluateBiConditional(bool firstValue,
                bool secondValue) {
            if (firstValue.Equals(secondValue)) {
                return true;
            } else {
                return false;
            }
        }
    }
}
