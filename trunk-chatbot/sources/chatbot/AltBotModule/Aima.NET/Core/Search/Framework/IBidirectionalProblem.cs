using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    /// <summary>
    /// <![CDATA[ An interface describing a problem that can be tackled from both directions 
    /// at once (i.e InitialState<->Goal).]]>
    /// </summary>
    public interface IBidirectionalProblem
    {
        Problem GetOriginalProblem();

        Problem GetReverseProblem();
    }
}
