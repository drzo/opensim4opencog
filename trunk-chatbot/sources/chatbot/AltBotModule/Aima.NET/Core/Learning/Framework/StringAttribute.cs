using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Framework
{
    public class StringAttribute : IAttribute 
    {
        private StringAttributeSpecification spec;

        private String value;

        public StringAttribute(String value, StringAttributeSpecification spec) {
            this.spec = spec;
            this.value = value;
        }

        public string ValueAsString() {
            return value.Trim();
        }

        public string Name() {
            return spec.GetAttributeName().Trim();
        }
    }
}
