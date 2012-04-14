using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.CSP
{
    /// <summary>
    /// Interface which allows interested clients to register at a solution strategy
    /// and follow their progress step by step.
    /// </summary>
    public interface ICSPStateListener
    {
        void StateChanged(CSProblem csp);

        void StateChanged(Assignment assignment, CSProblem csp);
    }

}
