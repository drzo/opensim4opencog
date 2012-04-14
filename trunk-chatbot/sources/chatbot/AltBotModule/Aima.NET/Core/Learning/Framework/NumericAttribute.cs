using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Framework
{
    public class NumericAttribute : IAttribute 
    {
        double value;

        private NumericAttributeSpecification spec;

        public NumericAttribute(double rawvalue, NumericAttributeSpecification spec) 
        {
            this.value = rawvalue;
            this.spec = spec;
        }

        public string ValueAsString() 
        {
            return value.ToString();
        }

        public string Name() 
        {
            return spec.GetAttributeName().Trim();
        }

        public double ValueAsDouble() 
        {
            return value;
        }
    }
}
