using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Knowledge
{
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class Hypothesis
    {
        private ISentence hypothesis = null;

        public Hypothesis(ISentence hypothesis)
        {
            this.hypothesis = hypothesis;
        }

        /// <summary>
        /// <code><![CDATA[
        /// FORALL v (Classification(v) <=> ((Description1(v) AND Description2(v, Constant1))
        ///                                 OR
        ///                                  (Description1(v) AND Description3(v))
        ///                                 )
        ///          )
        /// ]]></code>
        /// </summary>
        /// <returns></returns>
        public ISentence GetHypothesis()
        {
            return hypothesis;
        }
    }
}
