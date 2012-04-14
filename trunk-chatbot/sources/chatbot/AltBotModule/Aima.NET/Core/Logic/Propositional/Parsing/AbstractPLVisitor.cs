using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Parsing
{
    using System.Collections;

    using Aima.Core.Logic.Propositional.Parsing.AST;

    public class AbstractPLVisitor : IPLVisitor {
        private PEParser parser = new PEParser();

        public object VisitSymbol(Symbol s, ISet<Sentence> arg)
        {
            return new Symbol(s.Value);
        }

        public object VisitTrueSentence(TrueSentence ts, ISet<Sentence> arg)
        {
            return new TrueSentence();
        }

        public object VisitFalseSentence(FalseSentence fs, object arg) {
            return new FalseSentence();
        }

        public virtual object VisitNotSentence(UnarySentence fs, ISet<Sentence> arg)
        {
            return new UnarySentence((Sentence) fs.Negated.Accept(this, arg));
        }

        public virtual object VisitBinarySentence(BinarySentence fs, ISet<Sentence> arg)
        {
            return new BinarySentence(fs.Operator, (Sentence) fs.First
                .Accept(this, arg), (Sentence) fs.Second.Accept(this, arg));
        }

        public object VisitMultiSentence(MultiSentence fs, ISet<Sentence> arg)
        {
            var newTerms = fs.Sentences.Select(s => (Sentence)s.Accept(this, arg)).ToList();
            return new MultiSentence(fs.Operator, newTerms);
        }

        protected Sentence Recreate(Sentence ast) {
            return (Sentence) parser.Parse(ast.ToString());
        }
    }
}
