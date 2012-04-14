using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.CSP
{
    /// <summary>
    /// Base class for CSP solver implementations. Solving a CSP means finding an
    /// assignment, which is consistent and complete with respect to a CSP. This
    /// abstract class provides the central interface method and additionally an
    /// implementation of an observer mechanism.
    /// </summary>
    public abstract class SolutionStrategy
    {
        //TODO: figure out if this can be replaced with normal .NET events
        IList<ICSPStateListener> listeners = new List<ICSPStateListener>();

        public void AddCSPStateListener(ICSPStateListener listener)
        {
            listeners.Add(listener);
        }

        public void RemoveCSPStateListener(ICSPStateListener listener)
        {
            listeners.Remove(listener);
        }

        protected void FireStateChanged(CSProblem csp) 
        {
            foreach (ICSPStateListener listener in listeners)
                listener.StateChanged(csp.CopyDomains());
        }

        protected void FireStateChanged(Assignment assignment, CSProblem csp) 
        {
            foreach (ICSPStateListener listener in listeners)
                listener.StateChanged(assignment.Copy(), csp.CopyDomains());
        }

        public abstract Assignment Solve(CSProblem csp);
    }

}
