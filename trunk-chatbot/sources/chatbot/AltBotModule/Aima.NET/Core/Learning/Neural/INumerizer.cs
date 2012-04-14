using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    using Aima.Core.Learning.Framework;
    using Aima.Core.Util.Datastructure;

    public interface INumerizer
    {
        // A Numerizer understands how to convert an example from a particular
        // dataset
        // into a Pair of lists of doubles .The first represents the input to the
        // neural network and the second represents the desired output
        // See IrisDataSetNumerizer for a concrete example
        Pair<IList<double>, IList<double>> Numerize(Example e);

        string Denumerize(IList<double> outputValue);
    }
}
