using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL
{
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class StandardizeApartResult
    {
        public ISentence OriginalSentence { get; private set; }
        public ISentence Standardized { get; private set; }
        public IDictionary<Variable, ITerm> ForwardSubstitution { get; private set; }
        public IDictionary<Variable, ITerm> ReverseSubstitution { get; private set; }

        public StandardizeApartResult(ISentence originalSentence,
                ISentence standardized, IDictionary<Variable, ITerm> forwardSubstitution,
                IDictionary<Variable, ITerm> reverseSubstitution)
        {
            this.OriginalSentence = originalSentence;
            this.Standardized = standardized;
            this.ForwardSubstitution = forwardSubstitution;
            this.ReverseSubstitution = reverseSubstitution;
        }
    }
}
