using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.CSP
{
    using Aima.Core.Logic.FOL.Parsing.AST;
    using Aima.Core.Util.Datastructure;

    /// <summary>
    /// 
    /// Artificial Intelligence A Modern Approach (3rd Ed.): Figure 6.3, Page 209.
    /// 
    /// <code>
    /// function AC-3(csp) returns false if an inconsistency is found and true otherwise
    ///    inputs: csp, a binary CSP with components (X, D, C)
    ///    local variables: queue, a queue of arcs, initially all the arcs in csp
    ///    while queue is not empty do
    ///       (Xi, Xj) = REMOVE-FIRST(queue)
    ///       if REVISE(csp, Xi, Xj) then
    ///          if size of Di = 0 then return false
    ///             for each Xk in Xi.NEIGHBORS - {Xj} do
    ///                add (Xk, Xi) to queue
    ///    return true
    /// 
    /// function REVISE(csp, Xi, Xj) returns true iff we revise the domain of Xi
    ///    revised = false
    ///    for each x in Di do
    ///       if no value y in Dj allows (x ,y) to satisfy the constraint between Xi and Xj then
    ///          delete x from Di
    ///          revised = true
    ///    return revised
    /// </code>
    /// 
    /// Figure 6.3 The arc-consistency algorithm AC-3. After applying AC-3, either
    /// every arc is arc-consistent, or some variable has an empty domain, indicating
    /// that the CSP cannot be solved. The name "AC-3" was used by the algorithm's
    /// inventor (Mackworth, 1977) because it's the third version developed in the
    /// paper.
    /// </summary>
    public class AC3Strategy
    {

        public DomainRestoreInfo ReduceDomains(Variable var, object value, CSProblem csp)
        {
            DomainRestoreInfo result = new DomainRestoreInfo();
            Domain domain = csp.GetDomain(var);
            if (domain.Contains(value))
            {
                if (domain.Count() > 1)
                {
                    FIFOQueue<Variable> queue = new FIFOQueue<Variable>();
                    queue.Push(var);
                    result.StoreDomainFor(var, domain);
                    csp.SetDomain(var, new Domain(new object[] { value }));
                    this.ReduceDomains(queue, csp, result);
                }
            }
            else
            {
                result.SetEmptyDomainFound(true);
            }
            return result.Compactify();
        }

        public DomainRestoreInfo ReduceDomains(CSProblem csp) 
        {
            var result = new DomainRestoreInfo();
            var queue = new FIFOQueue<Variable>();
            foreach (Variable var in csp.Variables)
                queue.Push(var);
            this.ReduceDomains(queue, csp, result);
            return result.Compactify();
        }

        protected void ReduceDomains(FIFOQueue<Variable> queue, CSProblem csp,
                DomainRestoreInfo info) 
        {

            while (!(queue.Count == 0)) 
            {
                Variable var = queue.Pop();
                foreach (IConstraint constraint in csp.GetConstraints(var)) {
                    if (constraint.GetScope().Count == 2) {
                        Variable neighbor = csp.GetNeighbor(var, constraint);
                        if (this.Revise(neighbor, var, constraint, csp, info)) {
                            if (csp.GetDomain(neighbor).IsEmpty()) {
                                info.SetEmptyDomainFound(true);
                                return;
                            }
                            queue.Push(neighbor);
                        }
                    }
                }
            }
        }

        private bool Revise(Variable xi, Variable xj, IConstraint constraint,
                CSProblem csp, DomainRestoreInfo info) 
        {
            bool revised = false;
            var assignment = new Assignment();
            foreach (object vValue in csp.GetDomain(xi))
            {
                assignment.SetAssignment(xi, vValue);
                bool vValueOk = false;
                foreach (var nValue in csp.GetDomain(xj))
                {
                    assignment.SetAssignment(xj, nValue);
                    if (constraint.IsSatisfiedWith(assignment))
                    {
                        vValueOk = true;
                        break;
                    }
                }
                if (!vValueOk)
                {
                    info.StoreDomainFor(xi, csp.GetDomain(xi));
                    csp.RemoveValueFromDomain(xi, vValue);
                    revised = true;
                }
            }
            return revised;
        }
    }

}
