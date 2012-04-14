using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Knowledge
{
    using Aima.Core.Learning.Framework;
    using Aima.Core.Logic.FOL;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class FOLExample 
    {
        private FOLDataSetDomain folDSDomain;
        private Example example;
        private int egNo;
        //
        private Constant ithExampleConstant;
        private ISentence classification;
        private ISentence description;

        public FOLExample(FOLDataSetDomain folDSDomain, Example example, int egNo) 
        {
            this.folDSDomain = folDSDomain;
            this.example = example;
            this.egNo = egNo;
            this.ConstructFOLEg();
        }

        public int GetExampleNumber() 
        {
            return egNo;
        }

        public ISentence GetClassification() 
        {
            return classification;
        }

        public ISentence GetDescription() 
        {
            return description;
        }

        public override string ToString() 
        {
            return classification + " " + Connectors.And + " " + description;
        }

        private void ConstructFOLEg()
        {
            ithExampleConstant = new Constant(folDSDomain.GetExampleConstant(egNo));

            var terms = new List<ITerm>();
            terms.Add(ithExampleConstant);
            // Create the classification sentence
            classification = new Predicate(folDSDomain.GetGoalPredicateName(), terms);
            if (!example.GetAttributeValueAsString(folDSDomain.GetDataSetTargetName()).Equals(folDSDomain.TrueGoalValue))
            {
                // if not true then needs to be a Not sentence
                classification = new NotSentence(classification);
            }

            // Create the description sentence
            var descParts = new List<ISentence>();
            foreach (String dname in folDSDomain.DescriptionDataSetNames)
            {
                String foldDName = folDSDomain.GetFOLName(dname);
                terms = new List<ITerm>();
                terms.Add(ithExampleConstant);
                // If multivalued becomes a two place predicate
                // e.g: Patrons(X1, Some)
                // otherwise: Hungry(X1) or ~ Hungry(X1)
                // see pg 769 of AIMA
                ISentence part = null;
                if (this.folDSDomain.IsMultivalued(dname))
                {
                    terms.Add(new Constant(this.folDSDomain.GetFOLName(this.example.GetAttributeValueAsString(dname))));
                    part = new Predicate(foldDName, terms);
                }
                else
                {
                    part = new Predicate(foldDName, terms);

                    // Need to determine if false
                    if (!this.folDSDomain.TrueGoalValue.Equals(this.example.GetAttributeValueAsString(dname)))
                    {
                        part = new NotSentence(part);
                    }
                }
                descParts.Add(part);
            }
            if (descParts.Count == 1)
            {
                this.description = descParts[0];
            }
            else if (descParts.Count > 1)
            {
                this.description = new ConnectedSentence(Connectors.And, descParts[0], descParts[1]);
                for (int i = 2; i < descParts.Count; i++)
                {
                    description = new ConnectedSentence(Connectors.And, description, descParts[i]);
                }
            }
        }
    }

}
