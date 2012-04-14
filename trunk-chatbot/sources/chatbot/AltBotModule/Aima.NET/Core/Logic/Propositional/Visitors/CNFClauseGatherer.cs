using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Visitors
{
    using Aima.Core.Logic.Propositional.Parsing.AST;

    public class CNFClauseGatherer : BasicTraverser {
        AndDetector detector;

        public CNFClauseGatherer() {
            detector = new AndDetector();
        }

        public override object VisitBinarySentence(BinarySentence bs, ISet<Sentence> args) {

            ISet<Sentence> soFar = (ISet<Sentence>) args;

            Sentence first = bs.First;
            Sentence second = bs.Second;
            this.ProcessSubTerm(second, this.ProcessSubTerm(first, soFar));

            return soFar;

        }

        public ISet<Sentence> GetClausesFrom(Sentence sentence) {
            ISet<Sentence> set = new HashedSet<Sentence>();
            if (sentence is Symbol) {
                set.Add(sentence);
            } else if (sentence is UnarySentence) {
                set.Add(sentence);
            } else {
                set = (ISet<Sentence>) sentence.Accept(this, set);
            }
            return set;
        }

        private ISet<Sentence> ProcessSubTerm(Sentence s, ISet<Sentence> soFar) {
            if (detector.ContainsEmbeddedAnd(s)) {
                return (ISet<Sentence>) s.Accept(this, soFar);
            } else {
                soFar.Add(s);
                return soFar;
            }
        }
    }
}
