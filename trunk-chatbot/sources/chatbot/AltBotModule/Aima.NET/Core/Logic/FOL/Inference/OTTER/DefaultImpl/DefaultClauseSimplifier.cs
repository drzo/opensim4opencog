using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.OTTER.DefaultImpl
{
    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class DefaultClauseSimplifier : IClauseSimplifier 
    {

        private Demodulation demodulation = new Demodulation();
        private IList<TermEquality> rewrites;

        public DefaultClauseSimplifier() 
        {
            rewrites = new List<TermEquality>();
        }

        public DefaultClauseSimplifier(IList<TermEquality> rewrites) 
        {
            this.rewrites = rewrites;
        }

        //
        // START-ClauseSimplifier
        public Clause Simplify(Clause c) {
            Clause simplified = c;

            // Apply each of the rewrite rules to
            // the clause
            foreach (TermEquality te in rewrites) 
            {
                Clause dc = simplified;
                // Keep applying the rewrite as many times as it
                // can be applied before moving on to the next one.
                while ((dc = this.demodulation.Apply(te, dc)) != null) 
                {
                    simplified = dc;
                }
            }

            return simplified;
        }
    }
}
