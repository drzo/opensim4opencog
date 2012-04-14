using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Framework
{
    public class NumericAttributeSpecification : IAttributeSpecification {

        // a simple attribute representing a number reprsented as a double .
        private string name;

        public NumericAttributeSpecification(string name) {
            this.name = name;
        }

        public bool IsValid(string str) 
        {
            try 
            {
                Double.Parse(str);
                return true;
            } 
            catch 
            {
                return false;
            }
        }

        public string GetAttributeName() {
            return name;
        }

        public IAttribute CreateAttribute(string rawValue) {
            return new NumericAttribute(Double.Parse(rawValue), this);
        }
    }
}
