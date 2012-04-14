using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability
{
    using System.Collections;

    using Aima.Core.Logic.Propositional.Algorithms;

    public class ProbabilityDistribution 
    {
        private IList<Row> rows;

        string[] variableNames;

        public ProbabilityDistribution(string variableNameOne): this(new[] { variableNameOne }) 
        {
        }

        public ProbabilityDistribution(string variableNameOne,
                string variableNameTwo): this(new[] { variableNameOne, variableNameTwo }) 
        {
        }

        public ProbabilityDistribution(string variableNameOne,
                string variableNameTwo, string variableNameThree):
            this(new[] { variableNameOne, variableNameTwo, variableNameThree })
        {
        }

        public ProbabilityDistribution(string variableNameOne,
                string variableNameTwo, string variableNameThree,
                string variableNameFour):
            this(new[] { variableNameOne, variableNameTwo,
                    variableNameThree, variableNameFour })
        {
        }

        public ProbabilityDistribution(string[] variableNames) {
            this.variableNames = variableNames;
            rows = new List<Row>();
        }

        public void Set(bool[] values, double probability) {
            var m = new Model();
            for (int i = 0; i < variableNames.Length; i++) {
                m = m.Extend(variableNames[i], values[i]);
            }
            rows.Add(new Row(m, probability));
        }

        public void Set(bool value1, double probability) {
            this.Set(new[] { value1 }, probability);
        }

        public void Set(bool value1, bool value2, double probability) {
            this.Set(new[] { value1, value2 }, probability);
        }

        public void Set(bool value1, bool value2, bool value3,
                double probability) {
            this.Set(new[] { value1, value2, value3 }, probability);
        }

        public void Set(bool value1, bool value2, bool value3,
                bool value4, double probability) {
            this.Set(new[] { value1, value2, value3, value4 }, probability);
        }

        public override string ToString() {
            var buf = new StringBuilder();
            foreach (Row row in rows) {
                buf.Append(row + "\n");
            }
            return buf.ToString();

        }

        public double ProbabilityOf(Dictionary<string, bool?> conditions) 
        {
            double prob = 0.0;
            foreach (Row row in rows) 
            {
                bool rowMeetsAllConditions = true;
                foreach(var variable in conditions.Keys)
                {
                    bool? value = conditions[variable];
                    if (!(row.Model.Matches(variable, value))) {
                        rowMeetsAllConditions = false;
                        break;
                        // return false;
                    }
                }
                if (rowMeetsAllConditions) {
                    prob += row.Probability;
                }
            }

            return prob;
        }

        public double ProbabilityOf(string variableName, bool b) {
            Dictionary<string, bool?> h = new Dictionary<string, bool?>();
            h[variableName] = b;
            return this.ProbabilityOf(h);
        }

        class Row {
            public Model Model { get; private set; }

            public double Probability { get; private set; }

            public Row(Model m, double probability) {
                this.Model = m;
                this.Probability = probability;
            }

            public override string ToString() {
                return this.Model + " => " + this.Probability;
            }
        }
    }
}
