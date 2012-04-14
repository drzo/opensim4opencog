using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.FOL.KB.Data
{
    using System.Collections.ObjectModel;

    using Aima.Core.Logic.FOL.Inference.Proof;

    /**
     * @see http://logic.stanford.edu/classes/cs157/2008/lectures/lecture13.pdf
     * 
     * A Chain is a sequence of literals (while a clause is a set) - order is important for a chain.
     */

    /**
     * @author Ciaran O'Reilly
     * 
     */
    public class Chain
    {
        private static List<Literal> _emptyLiteralsList = new ReadOnlyCollection<Literal>(new List<Literal>()).ToList();
 
        private IList<Literal> literals;
        private IProofStep proofStep;

        public Chain() 
        {
            this.literals = new List<Literal>();
            // i.e. the empty chain
        }

        public Chain(IList<Literal> literals)
        {
            this.literals = literals;
        }

        public Chain(ISet<Literal> literals) 
        {
            this.literals = literals.ToList();
        }

        public IProofStep ProofStep
        {
            get
            {
                if (null == this.proofStep) 
                {
                    // Assume was a premise
                    this.proofStep = new ProofStepPremise(this);
                }
                return this.proofStep;
            }
        }

        public void SetProofStep(IProofStep pStep) 
        {
            this.proofStep = pStep;
        }

        public bool IsEmpty() 
        {
            return literals.Count == 0;
        }

        public void AddLiteral(Literal literal) 
        {
            literals.Add(literal);
        }

        public Literal GetHead() 
        {
            if (this.literals.Count == 0) 
            {
                return null;
            }
            return literals[0];
        }

        public IList<Literal> GetTail() 
        {
            if (this.literals.Count == 0) 
            {
                return _emptyLiteralsList;
            }
            return new ReadOnlyCollection<Literal>(literals.Skip(1).ToList());
        }

        public int GetNumberLiterals()
        {
            return literals.Count;
        }

        public IList<Literal> GetLiterals() 
        {
            return new ReadOnlyCollection<Literal>(literals);
        }

        /// <summary>
        /// A contrapositive of a chain is a permutation in which a different literal
        /// is placed at the front. The contrapositives of a chain are logically
        /// equivalent to the original chain.
        /// </summary>
        /// <returns>a list of contrapositives for this chain.</returns>
        public IList<Chain> GetContrapositives() 
        {
            var contrapositives = new List<Chain>();
            var lits = new List<Literal>();

            for (int i = 1; i < literals.Count; i++) 
            {
                lits.Clear();
                lits.Add(literals[i]);
                lits.AddRange(literals.Take(i-1));
                lits.AddRange(literals.Skip(i));
                var cont = new Chain(lits);
                cont.SetProofStep(new ProofStepChainContrapositive(cont, this));
                contrapositives.Add(cont);
            }

            return contrapositives;
        }

        public override string ToString()
        {
            var sb = new StringBuilder();
            sb.Append("<");

            for (var i = 0; i < this.literals.Count; i++)
            {
                if (i > 0)
                {
                    sb.Append(",");
                }

                sb.Append(this.literals[i].ToString());
            }

            sb.Append(">");

            return sb.ToString();
        }
    }
}
