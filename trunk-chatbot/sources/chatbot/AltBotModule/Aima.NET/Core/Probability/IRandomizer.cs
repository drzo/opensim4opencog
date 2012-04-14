using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability
{
    public interface IRandomizer
    {
        double NextDouble();
    }
}
