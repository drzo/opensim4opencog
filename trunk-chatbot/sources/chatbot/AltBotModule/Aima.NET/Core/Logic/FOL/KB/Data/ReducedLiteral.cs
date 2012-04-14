using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.KB.Data
{
    using Aima.Core.Logic.FOL.Parsing.AST;

    /// <summary>
    /// <see cref="http://logic.stanford.edu/classes/cs157/2008/lectures/lecture13.pdf"/> 
    /// </summary>
    public class ReducedLiteral : Literal 
    {
        private String strRep;

        public ReducedLiteral(IAtomicSentence atom): base(atom) 
        {
        }

        public ReducedLiteral(IAtomicSentence atom, bool negated) : base(atom, negated)
        {
            
        }

        public override Literal NewInstance(IAtomicSentence atom) 
        {
            return new ReducedLiteral(atom, IsNegativeLiteral());
        }

        public override string ToString() {
            if (this.strRep == null) {
                StringBuilder sb = new StringBuilder();
                sb.Append("[");
                if (IsNegativeLiteral()) {
                    sb.Append("~");
                }
                sb.Append(AtomicSentence.ToString());
                sb.Append("]");
                strRep = sb.ToString();
            }

            return strRep;
        }
    }

}
