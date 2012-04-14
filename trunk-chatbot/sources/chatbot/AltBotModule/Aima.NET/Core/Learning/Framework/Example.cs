using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Framework
{
    public class Example 
    {
        Dictionary<String, IAttribute> attributes;

        private IAttribute targetAttribute;

        public Example(Dictionary<string, IAttribute> attributes,
                IAttribute targetAttribute) {
            this.attributes = attributes;
            this.targetAttribute = targetAttribute;
        }

        public String GetAttributeValueAsString(string attributeName) {
            return attributes[attributeName].ValueAsString();
        }

        public double GetAttributeValueAsDouble(String attributeName) 
        {
            var attribute = attributes[attributeName];
            if (attribute == null || !(attribute is NumericAttribute)) {
                throw new ArgumentException(
                        "cannot return numerical value for non numeric attribute");
            }
            return ((NumericAttribute) attribute).ValueAsDouble();
        }

        public override string ToString() 
        {
            return attributes.ToString();
        }

        public string TargetValue() 
        {
            return this.GetAttributeValueAsString(targetAttribute.Name());
        }

        public override bool Equals(object o) 
        {
            if (ReferenceEquals(null, o) || !(o is Example))
            {
                return false;
            }
            return Equals((Example)o);
        }

        public Example Numerize(
                Dictionary<string, Dictionary<string, int>> attrValueToNumber) 
        {
            var numerizedExampleData = new Dictionary<string, IAttribute>();
            foreach (string key in attributes.Keys) 
            {
                IAttribute attribute = attributes[key];
                if (attribute is StringAttribute) 
                {
                    int correspondingNumber = attrValueToNumber[key][attribute.ValueAsString()];
                    var spec = new NumericAttributeSpecification(key);
                    numerizedExampleData[key] = new NumericAttribute(correspondingNumber, spec);
                } 
                else 
                {// Numeric Attribute
                    numerizedExampleData[key] = attribute;
                }
            }
            return new Example(numerizedExampleData, numerizedExampleData[targetAttribute.Name()]);
        }

        public bool Equals(Example other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.attributes, this.attributes) && Equals(other.targetAttribute, this.targetAttribute);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return ((this.attributes != null ? this.attributes.GetHashCode() : 0) * 397) ^ (this.targetAttribute != null ? this.targetAttribute.GetHashCode() : 0);
            }
        }
    }

}
