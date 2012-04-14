using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): page 92. <para />
    /// The evaluation function is construed as a cost estimate, so the node with the lowest evaluation 
    /// is expanded first.    
    /// </summary>
    public interface IEvaluationFunction
    {
        double F(Node n);
    }
}
