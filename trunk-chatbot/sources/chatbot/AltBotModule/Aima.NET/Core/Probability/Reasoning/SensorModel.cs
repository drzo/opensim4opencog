using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Reasoning
{
    using Aima.Core.Util.Datastructure;

    public class SensorModel
    {
        private Table<string, string, double> table;

        private IList<string> states;

        public SensorModel(IList<string> states, IList<string> perceptions)
        {
            this.states = states;
            table = new Table<string, string, double>(states, perceptions);
        }

        public void SetSensingProbability(string state, string perception,
                double probability)
        {
            table.Set(state, perception, probability);
        }

        public double Get(string state, string perception)
        {
            return table.Get(state, perception);
        }

        public Matrix AsMatrix(string perception)
        {
            IList<double> values = this.states.Select(state => this.Get(state, perception)).ToList();
            // for (string state : aBelief.states()) {
            Matrix oMatrix = Matrix.CreateDiagonalMatrix(values);
            return oMatrix;
        }
    }

}
