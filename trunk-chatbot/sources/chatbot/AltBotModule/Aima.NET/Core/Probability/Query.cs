using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability
{
    public class Query
    {

        public string QueryVariable { get; private set; }

        public Dictionary<string, bool?> EvidenceVariables { get; private set; }

        public Query(string queryVariable, string[] evidenceVariables,
                bool[] evidenceValues)
        {
            this.QueryVariable = queryVariable;
            this.EvidenceVariables = new Dictionary<string, bool?>();
            for (int i = 0; i < evidenceVariables.Length; i++)
            {
                this.EvidenceVariables[evidenceVariables[i]] = evidenceValues[i];
            }
        }
    }
}
