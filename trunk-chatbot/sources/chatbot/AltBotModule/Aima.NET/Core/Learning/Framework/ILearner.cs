using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Framework
{
    public interface ILearner
    {
        void Train(DataSet ds);

        String Predict(Example e);

        int[] Test(DataSet ds);
    }
}
