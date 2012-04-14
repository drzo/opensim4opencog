using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.CSP
{
    /// <summary>
/// Artificial Intelligence A Modern Approach (3rd Ed.): Figure 6.1, Page 204.
/// The principal states and territories of Australia. Coloring this map can be
/// viewed as a constraint satisfaction problem (CSP). The goal is to assign
/// colors to each region so that no neighboring regions have the same color.
    /// </summary>
    public class MapCSProblem : CSProblem 
    {
        public static readonly Variable NSW = new Variable("NSW");
        public static readonly Variable NT = new Variable("NT");
        public static readonly Variable Q = new Variable("Q");
        public static readonly Variable SA = new Variable("SA");
        public static readonly Variable T = new Variable("T");
        public static readonly Variable V = new Variable("V");
        public static readonly Variable WA = new Variable("WA");
        public static readonly string Red = "RED";
        public static readonly string Green = "GREEN";
        public static readonly string Blue = "BLUE";

        private static IList<Variable> CollectVariables() 
        {
            IList<Variable> variables = new List<Variable>();
            variables.Add(NSW);
            variables.Add(WA);
            variables.Add(NT);
            variables.Add(Q);
            variables.Add(SA);
            variables.Add(V);
            variables.Add(T);
            return variables;
        }

        public MapCSProblem():base(CollectVariables()) {

            Domain colors = new Domain(new object[] { Red, Green, Blue });

            foreach (Variable var in Variables)
                SetDomain(var, colors);

            AddConstraint(new NotEqualConstraint(WA, NT));
            AddConstraint(new NotEqualConstraint(WA, SA));
            AddConstraint(new NotEqualConstraint(NT, SA));
            AddConstraint(new NotEqualConstraint(NT, Q));
            AddConstraint(new NotEqualConstraint(SA, Q));
            AddConstraint(new NotEqualConstraint(SA, NSW));
            AddConstraint(new NotEqualConstraint(SA, V));
            AddConstraint(new NotEqualConstraint(Q, NSW));
            AddConstraint(new NotEqualConstraint(NSW, V));
        }
    }
}
