using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.FOL.Inference.OTTER
{
    using Aima.Core.Logic.FOL.KB.Data;
    
    /// <summary>
    /// Heuristic for selecting lightest clause from SOS.
    ///  To avoid recalculating the lightest clause
    ///  on every call, the interface supports defining
    ///  the initial sos and updates to that set so
    ///  that it can maintain its own internal data
    ///  structures to allow for incremental re-calculation
    ///  of the lightest clause.
    /// </summary>
    public interface ILightestClauseHeuristic
    {

        // Get the lightest clause from the SOS
        Clause GetLightestClause();

        // SOS life-cycle methods allowing implementations
        // of this interface to incrementally update
        // the calculation of the lightest clause as opposed
        // to having to recalculate each time.
        void InitialSOS(ISet<Clause> clauses);

        void AddedClauseToSOS(Clause clause);

        void RemovedClauseFromSOS(Clause clause);
    }
}
