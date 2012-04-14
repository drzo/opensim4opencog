using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Local
{
    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): page 127.
    /// Each state is rated by the objective function, or (in Genetic Algorithm terminology) the fitness function.
    /// A fitness function should return higher values for better states.
    /// </summary>
    public interface IFitnessFunction
    {
        double GetValue(String individual);
    }
}
