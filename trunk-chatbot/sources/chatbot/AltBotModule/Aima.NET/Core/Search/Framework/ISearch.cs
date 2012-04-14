using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    using Aima.Core.Agent;

    public interface ISearch 
    {
        IList<IAction> Search(Problem p);

        Metrics GetMetrics();
    }
}
