using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Framework
{
    public class StringAttributeSpecification : IAttributeSpecification 
    {
        string attributeName;

        IList<string> attributePossibleValues;

        public StringAttributeSpecification(string attributeName,
                IList<string> attributePossibleValues) 
        {
            this.attributeName = attributeName;
            this.attributePossibleValues = attributePossibleValues;
        }

        public StringAttributeSpecification(string attributeName,
                string[] attributePossibleValues)
            : this(attributeName, attributePossibleValues.ToList())
        {
        }

        public bool IsValid(string value) 
        {
            return (attributePossibleValues.Contains(value));
        }

        public string GetAttributeName() {
            return attributeName;
        }

        public IList<string> PossibleAttributeValues() 
        {
            return attributePossibleValues;
        }

        public IAttribute CreateAttribute(string rawValue) {
            return new StringAttribute(rawValue, this);
        }
    }

}
