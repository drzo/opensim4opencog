using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Knowledge
{
    using Aima.Core.Logic.FOL.KB;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 19.2, page 771.
    /// <code>
    /// <![CDATA[
    /// function CURRENT-BEST-LEARNING(examples, h) returns a hypothesis or fail
    ///   if examples is empty then
    ///      return h
    ///   e <- FIRST(examples)
    ///   if e is consistent with h then
    ///      return CURRENT-BEST-LEARNING(REST(examples), h)
    ///   else if e is a false positive for h then
    ///     for each h' in specializations of h consistent with examples seen so far do
    ///       h'' <- CURRENT-BEST-LEARNING(REST(examples), h')
    ///       if h'' != fail then return h''
    ///   else if e is a false negative for h then
    ///     for each h' in generalization of h consistent with examples seen so far do
    ///       h'' <- CURRENT-BEST-LEARNING(REST(examples), h')
    ///       if h'' != fail then return h''
    ///   return fail
    /// ]]>
    /// </code>
    /// Figure 19.2 The current-best-hypothesis learning algorithm. It searches for a
    /// consistent hypothesis that fits all the examples and backtracks when no consistent
    /// specialization/generalization can be found. To start the algorithm, any hypothesis
    /// can be passed in; it will be specialized or generalized as needed.
    /// </summary>
    public class CurrentBestLearning
    {
        private FOLDataSetDomain folDSDomain = null;
        private FOLKnowledgeBase kbForLearning = null;

        public CurrentBestLearning(FOLDataSetDomain folDSDomain,
                FOLKnowledgeBase kbForLearning)
        {
            this.folDSDomain = folDSDomain;
            this.kbForLearning = kbForLearning;
        }

        // TODO - Implement!!!
        public Hypothesis GetCurrentBestLearningHypothesis(IList<FOLExample> examples)
        {

            // TODO-use the default from pg 769 for now.
            var c1 = "patrons(v,Some)";
            var c2 = "patrons(v,Full) AND (hungry(v) AND type(v,French))";
            var c3 = "patrons(v,Full) AND (hungry(v) AND (type(v,Thai) AND fri_sat(v)))";
            var c4 = "patrons(v,Full) AND (hungry(v) AND type(v,Burger))";
            var sh = "FORALL v (will_wait(v) <=> (" + c1 + " OR (" + c2
                    + " OR (" + c3 + " OR (" + c4 + ")))))";

            Hypothesis h = new Hypothesis(this.kbForLearning.tell(sh));

            return h;
        }
    }
}
