namespace Aima.Core.Logic.FOL
{
    using Aima.Core.Logic.FOL.Parsing.AST;

    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using Iesi.Collections.Generic;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 9.1, page 328.
    /// 
    /// <code>
    /// function UNIFY(x, y, theta) returns a substitution to make x and y identical
    ///   inputs: x, a variable, constant, list, or compound
    ///           y, a variable, constant, list, or compound
    ///           theta, the substitution built up so far (optional, defaults to empty)
    ///           
    ///   if theta = failure then return failure
    ///   else if x = y the return theta
    ///   else if VARIABLE?(x) then return UNIVY-VAR(x, y, theta)
    ///   else if VARIABLE?(y) then return UNIFY-VAR(y, x, theta)
    ///   else if COMPOUND?(x) and COMPOUND?(y) then
    ///       return UNIFY(x.ARGS, y.ARGS, UNIFY(x.OP, y.OP, theta))
    ///   else if LIST?(x) and LIST?(y) then
    ///       return UNIFY(x.REST, y.REST, UNIFY(x.FIRST, y.FIRST, theta))
    ///   else return failure
    ///   
    /// ---------------------------------------------------------------------------------------------------
    /// 
    /// function UNIFY-VAR(var, x, theta) returns a substitution
    ///            
    ///   if {var/val} E theta then return UNIFY(val, x, theta)
    ///   else if {x/val} E theta then return UNIFY(var, val, theta)
    ///   else if OCCUR-CHECK?(var, x) then return failure
    ///   else return add {var/x} to theta
    /// </code>
    /// 
    /// Figure 9.1 The unification algorithm. The algorithm works by comparing the structures
    /// of the inputs, elements by element. The substitution theta that is the argument to UNIFY is built
    /// up along the way and is used to make sure that later comparisons are consistent with bindings
    /// that were established earlier. In a compound expression, such as F(A, B), the OP field picks
    /// out the function symbol F and the ARGS field picks out the argument list (A, B).
    /// </summary>
    public class Unifier 
    {
        private static SubstVisitor _substVisitor = new SubstVisitor();
        private static VariableCollector _variableCollector = new VariableCollector();

        public IDictionary<Variable, ITerm> Unify(IFOLNode x, IFOLNode y) 
        {
            return this.Unify(x, y, new Dictionary<Variable, ITerm>());
        }

        /// <summary>
        /// <code>
        /// function UNIFY(x, y, theta) returns a substitution to make x and y identical
        ///   inputs: x, a variable, constant, list, or compound
        ///           y, a variable, constant, list, or compound
        ///           theta, the substitution built up so far (optional, defaults to empty)
        /// </code>
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <param name="theta"></param>
        /// <returns><![CDATA[a IDictionary<Variable, ITerm> representing the substitution (i.e. a set
        ///         of variable/term pairs) or null which is used to indicate a
        ///         failure to unify.]]></returns>
        public IDictionary<Variable, ITerm> Unify(IFOLNode x, IFOLNode y, IDictionary<Variable, ITerm> theta) 
        {
            // if theta = failure then return failure
            if (theta == null) 
            {
                return null;
            } 
            else if (x.Equals(y)) 
            {
                // else if x = y then return theta
                return theta;
            } 
            else if (x is Variable) 
            {
                // else if VARIABLE?(x) then return UNIVY-VAR(x, y, theta)
                return this.UnifyVar((Variable) x, y, theta);
            } 
            else if (y is Variable) 
            {
                // else if VARIABLE?(y) then return UNIFY-VAR(y, x, theta)
                return this.UnifyVar((Variable) y, x, theta);
            } 
            else if (this.IsCompound(x) && this.IsCompound(y)) {
                // else if COMPOUND?(x) and COMPOUND?(y) then
                // return UNIFY(x.ARGS, y.ARGS, UNIFY(x.OP, y.OP, theta))
                return this.Unify((List<IFOLNode>)x.GetArgs(), (List<IFOLNode>)y.GetArgs(), this.UnifyOps(this.Op(x), this.Op(y), theta));
            } else {
                // else return failure
                return null;
            }
        }

        // else if LIST?(x) and LIST?(y) then
        // return UNIFY(x.REST, y.REST, UNIFY(x.FIRST, y.FIRST, theta))
        public IDictionary<Variable, ITerm> Unify(IList<IFOLNode> x, IList<IFOLNode> y, IDictionary<Variable, ITerm> theta) 
        {
            if (theta == null) 
            {
                return null;
            } 
            else if (x.Count != y.Count) 
            {
                return null;
            } 
            else if (x.Count == 0 && y.Count == 0) 
            {
                return theta;
            } 
            else if (x.Count == 1 && y.Count == 1) 
            {
                return this.Unify(x[0], y[0], theta);
            } 
            else 
            {
                return this.Unify(x.Skip(1).ToList(), y.Skip(1).ToList(), this.Unify(x[0], y[0], theta));
            }
        }


        // Note: You can subclass and override this method in order
        // to re-implement the OCCUR-CHECK?() to always
        // return false if you want that to be the default
        // behavior, as is the case with Prolog.
        protected bool OccurCheck(IDictionary<Variable, ITerm> theta, Variable var, IFOLNode x) 
        {
            if (x is Function) 
            {
                var varsToCheck = _variableCollector.CollectAllVariables((Function) x);
                if (varsToCheck.Contains(var)) 
                {
                    return true;
                }

                // Now need to check if cascading will cause occurs to happen
                // e.g.
                // Loves(SF1(v2),v2)
                // Loves(v3,SF0(v3))
                // or
                // P(v1,SF0(v1),SF0(v1))
                // P(v2,SF0(v2),v2 )
                // or
                // P(v1, F(v2),F(v2),F(v2),v1, F(F(v1)),F(F(F(v1))),v2)
                // P(F(v3),v4, v5, v6, F(F(v5)),v4, F(v3), F(F(v5)))
                return this.CascadeOccurCheck(theta, var, varsToCheck,
                        new HashedSet<Variable>(varsToCheck));
            }
            return false;
        }

        /// <summary>
        /// <code>
        /// function UNIFY-VAR(var, x, theta) returns a substitution
        ///   inputs: var, a variable
        ///       x, any expression
        ///       theta, the substitution built up so far
        /// </code>
        /// </summary>
        /// <param name="var"></param>
        /// <param name="x"></param>
        /// <param name="theta"></param>
        /// <returns></returns>        
        private IDictionary<Variable, ITerm> UnifyVar(Variable var, IFOLNode x,IDictionary<Variable, ITerm> theta)
        {
            if (!(x is ITerm))
            {
                return null;
            }
            
            if (theta.Keys.Contains(var))
            {
                // if {var/val} E theta then return UNIFY(val, x, theta)
                return this.Unify(theta[var], x, theta);
            }

            if (theta.Keys.Contains((Variable)x))
            {
                // else if {x/val} E theta then return UNIFY(var, val, theta)
                return this.Unify(var, theta[(Variable)x], theta);
            }

            if (this.OccurCheck(theta, var, x))
            {
                // else if OCCUR-CHECK?(var, x) then return failure
                return null;
            }
            
            // else return add {var/x} to theta
            this.CascadeSubstitution(theta, var, (ITerm)x);
            return theta;
        }

        private IDictionary<Variable, ITerm> UnifyOps(String x, String y, IDictionary<Variable, ITerm> theta)
        {
            if (theta == null) 
            {
                return null;
            }
            return x.Equals(y) ? theta : null;
        }

        private string Op(IFOLNode x) 
        {
            return x.GetSymbolicName();
        }

        private bool IsCompound(IFOLNode x) 
        {
            return x.IsCompound();
        }

        private bool CascadeOccurCheck(IDictionary<Variable, ITerm> theta, Variable var, 
            ISet<Variable> varsToCheck, ISet<Variable> varsCheckedAlready) 
        {
            // Want to check if any of the variable to check end up
            // looping back around on the new variable.
            ISet<Variable> nextLevelToCheck = new HashedSet<Variable>();
            foreach (Variable v in varsToCheck) 
            {
                if (!theta.ContainsKey(v))
                {
                    continue;
                }

                ITerm t = theta[v];
                if (t.Equals(var)) 
                {
                    // e.g.
                    // v1=v2
                    // v2=SFO(v1)
                    return true;
                }

                if (t is Function) 
                {
                    // Need to ensure the function this variable
                    // is to be replaced by does not contain var.
                    ISet<Variable> indirectvars = _variableCollector.CollectAllVariables(t);
                    if (indirectvars.Contains(var)) 
                    {
                        return true;
                    }
                    
                    // Determine the next cascade/level
                    // of variables to check for looping
                    foreach (var iv in indirectvars.Where(iv => !varsCheckedAlready.Contains(iv)))
                    {
                        nextLevelToCheck.Add(iv);
                    }
                }
            }
            if (nextLevelToCheck.Count > 0) 
            {
                varsCheckedAlready.UnionWith(nextLevelToCheck);
                return this.CascadeOccurCheck(theta, var, nextLevelToCheck, varsCheckedAlready);
            }
            return false;
        }

        // See:
        // http://logic.stanford.edu/classes/cs157/2008/miscellaneous/faq.html#jump165
        // for need for this.
        private void CascadeSubstitution(IDictionary<Variable, ITerm> theta, Variable var, ITerm x) 
        {
            theta[var] = x;

            var keys = theta.Keys.ToArray();
            foreach (Variable v in keys) 
            {
                ITerm t = theta[v];
                theta[v] = _substVisitor.Subst(theta, t);
            }
        }
    }
}
