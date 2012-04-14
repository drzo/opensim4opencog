using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Visitors
{
    using Aima.Core.Logic.Propositional.Parsing;
    using Aima.Core.Logic.Propositional.Parsing.AST;

    public class CNFTransformer : AbstractPLVisitor {
        public override object VisitBinarySentence(BinarySentence bs, ISet<Sentence> arg)
        {
            if (bs.IsBiconditional()) {
                return this.TransformBiConditionalSentence(bs);
            }
            if (bs.IsImplication()) {
                return this.TransformImpliedSentence(bs);
            }
            if (bs.IsOrSentence()
                && (bs.FirstTermIsAndSentence() || bs.SecondTermIsAndSentence())) 
            {
                return this.DistributeOrOverAnd(bs);
            }
            return base.VisitBinarySentence(bs, arg);
        }

        public override object VisitNotSentence(UnarySentence us, ISet<Sentence> arg)
        {
            return this.TransformNotSentence(us);
        }

        public Sentence Transform(Sentence s) {
            Sentence toTransform = s;
            while (!(toTransform.Equals(this.Step(toTransform)))) {
                toTransform = this.Step(toTransform);
            }

            return toTransform;
        }

        private Sentence Step(Sentence s) {
            return (Sentence) s.Accept(this, null);
        }

        private Sentence TransformBiConditionalSentence(BinarySentence bs) {
            Sentence first = new BinarySentence("=>", (Sentence) bs.First
                    .Accept(this, null), (Sentence) bs.Second.Accept(this,
                    null));
            Sentence second = new BinarySentence("=>", (Sentence) bs.Second
                    .Accept(this, null), (Sentence) bs.First
                    .Accept(this, null));
            return new BinarySentence("AND", first, second);
        }

        private Sentence TransformImpliedSentence(BinarySentence bs) {
            Sentence first = new UnarySentence((Sentence) bs.First.Accept(
                    this, null));
            return new BinarySentence("OR", first, (Sentence) bs.Second
                    .Accept(this, null));
        }

        private Sentence TransformNotSentence(UnarySentence us) {
            if (us.Negated is UnarySentence) {
                return (Sentence) ((UnarySentence) us.Negated).Negated
                        .Accept(this, null);
            } else if (us.Negated is BinarySentence) {
                BinarySentence bs = (BinarySentence) us.Negated;
                if (bs.IsAndSentence()) {
                    Sentence first = new UnarySentence((Sentence) bs.First
                            .Accept(this, null));
                    Sentence second = new UnarySentence((Sentence) bs.Second
                            .Accept(this, null));
                    return new BinarySentence("OR", first, second);
                } else if (bs.IsOrSentence()) {
                    Sentence first = new UnarySentence((Sentence) bs.First
                            .Accept(this, null));
                    Sentence second = new UnarySentence((Sentence) bs.Second
                            .Accept(this, null));
                    return new BinarySentence("AND", first, second);
                } else {
                    return (Sentence) base.VisitNotSentence(us, null);
                }
            } else {
                return (Sentence) base.VisitNotSentence(us, null);
            }
        }

        private Sentence DistributeOrOverAnd(BinarySentence bs) {
            BinarySentence andTerm = bs.FirstTermIsAndSentence() ? (BinarySentence) bs
                    .First
                    : (BinarySentence) bs.Second;
            Sentence otherterm = bs.FirstTermIsAndSentence() ? bs.Second : bs
                    .First;
            // (alpha or (beta and gamma) = ((alpha or beta) and (alpha or gamma))
            Sentence alpha = (Sentence) otherterm.Accept(this, null);
            Sentence beta = (Sentence) andTerm.First.Accept(this, null);
            Sentence gamma = (Sentence) andTerm.Second.Accept(this, null);
            Sentence distributed = new BinarySentence("AND", new BinarySentence(
                    "OR", alpha, beta), new BinarySentence("OR", alpha, gamma));
            return distributed;
        }
    }
}
