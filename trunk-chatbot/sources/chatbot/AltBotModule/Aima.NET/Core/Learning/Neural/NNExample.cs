using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    using Aima.Core.Util.Math;

    public class NNExample 
    {
        private readonly IList<double> normalizedInput, normalizedTarget;

        public NNExample(IList<double> normalizedInput, IList<double> normalizedTarget) 
        {
            this.normalizedInput = normalizedInput;
            this.normalizedTarget = normalizedTarget;
        }

        public NNExample CopyExample()
        {
            IList<double> newInput = this.normalizedInput.ToList();
            IList<double> newTarget = this.normalizedTarget.ToList();
            return new NNExample(newInput, newTarget);
        }

        public Vector GetInput() 
        {
            Vector v = new Vector(normalizedInput);
            return v;

        }

        public Vector GetTarget() {
            Vector v = new Vector(normalizedTarget);
            return v;

        }

        /// <summary>
        /// compares the index having greatest value in target to indec having
        /// greatest value in prediction. Ifidentical, correct
        /// </summary>
        /// <param name="prediction"></param>
        /// <returns></returns>
        public bool IsCorrect(Vector prediction) {
            return this.GetTarget().IndexHavingMaxValue() == prediction
                    .IndexHavingMaxValue();
        }
    }

}
