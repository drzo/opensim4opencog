using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace Aima.Core.Learning.Knowledge
{
    using Aima.Core.Learning.Framework;
    using Aima.Core.Logic.FOL.Domain;

    public class FOLDataSetDomain : FOLDomain 
    {
        //
        private static Regex allowableCharactersRegEx = new Regex("[^a-zA-Z_$0-9]");
        //
        private DataSetSpecification dataSetSpecification;

        public string TrueGoalValue { get; private set; }

        public IList<string> DescriptionPredicateNames
        {
            get
            {
                return this.descriptionPredicateNames;
            }
        }

        public IList<string> DescriptionDataSetNames
        {
            get
            {
                return this.descriptionDataSetNames;
            }
        }

        // Default example prefix, see pg679 of AIMA
        private string examplePrefix = "X";
        
        private IList<string> descriptionPredicateNames = new List<string>();
        
        private IList<string> descriptionDataSetNames = new List<string>();
        
        private IDictionary<string, string> dsToFOLNameMap = new Dictionary<String, String>();

        public FOLDataSetDomain(DataSetSpecification dataSetSpecification, string trueGoalValue) 
        {
            this.dataSetSpecification = dataSetSpecification;
            this.TrueGoalValue = trueGoalValue;
            this.ConstructFOLDomain();
        }

        public string GetDataSetTargetName() 
        {
            return dataSetSpecification.TargetAttribute;
        }

        public string GetGoalPredicateName() 
        {
            return this.GetFOLName(dataSetSpecification.TargetAttribute);
        }

        public bool IsMultivalued(string descriptiveDataSetName) 
        {
            var possibleValues = dataSetSpecification.GetPossibleAttributeValues(descriptiveDataSetName);
            
            // If more than two possible values
            // then is multivalued
            if (possibleValues.Count > 2) 
            {
                return true;
            }
            // If one of the possible values for the attribute
            // matches the true goal value then consider
            // it not being multivalued.
            return possibleValues.All(pv => !this.TrueGoalValue.Equals(pv));
        }

        public string GetExampleConstant(int egNo) 
        {
            var egConstant = examplePrefix + egNo;
            AddConstant(egConstant);
            return egConstant;
        }

        public string GetFOLName(string dsName) 
        {
            var folName = dsToFOLNameMap[dsName];
            if (folName == null) 
            {
                folName = dsName;
                //TODO": verify that below commented out code is really unnecessary
                //if (!Character.isJavaIdentifierStart(dsName.charAt(0))) {
                //    folName = "_" + dsName;
                //}s
                folName = allowableCharactersRegEx.Replace(folName, "_");
                
                dsToFOLNameMap[dsName] = folName;
            }

            return folName;
        }

        private void ConstructFOLDomain() 
        {
            // Ensure the target predicate is included
            AddPredicate(this.GetFOLName(dataSetSpecification.TargetAttribute));
            
            // Create the descriptive predicates
            foreach (var saName in this.dataSetSpecification.GetNamesOfStringAttributes()) 
            {
                if (this.dataSetSpecification.TargetAttribute.Equals(saName)) 
                {
                    // Don't add the target to the descriptive predicates
                    continue;
                }
                var folSAName = this.GetFOLName(saName);

                // Add a predicate for the attribute
                AddPredicate(folSAName);

                this.DescriptionPredicateNames.Add(folSAName);
                this.DescriptionDataSetNames.Add(saName);

                var attributeValues = this.dataSetSpecification.GetPossibleAttributeValues(saName);

                // If a multivalued attribute need to setup
                // Constants for the different possible values
                if (this.IsMultivalued(saName)) 
                {
                    foreach (var av in attributeValues) 
                    {
                        AddConstant(this.GetFOLName(av));
                    }
                }
            }
        }
    }

}
