using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Framework
{
    public class DataSetSpecification
    {
        IList<IAttributeSpecification> attributeSpecifications;

        public string TargetAttribute { get; set; }

        public DataSetSpecification()
        {
            this.attributeSpecifications = new List<IAttributeSpecification>();
        }

        public bool IsValid(IList<string> uncheckedAttributes)
        {
            if (attributeSpecifications.Count != uncheckedAttributes.Count)
            {
                throw new ArgumentException("size mismatch specsize = "
                        + attributeSpecifications.Count + " attrbutes size = "
                        + uncheckedAttributes.Count);
            }

            return !uncheckedAttributes.Where((t, i) => !this.attributeSpecifications[i].IsValid(t)).Any();
        }

        public IList<string> GetPossibleAttributeValues(string attributeName)
        {
            foreach (IAttributeSpecification attSpec in
                this.attributeSpecifications.Where(attSpec => attSpec.GetAttributeName().Equals(attributeName)))
            {
                return ((StringAttributeSpecification) attSpec).PossibleAttributeValues();
            }
            throw new ApplicationException(String.Format("No such attribute{0}", attributeName));
        }

        public IList<string> GetAttributeNames() 
        {
            return this.attributeSpecifications.Select(attSpec => attSpec.GetAttributeName()).ToList();
        }

        public void DefineStringAttribute(string name, string[] attributeValues)
        {
            attributeSpecifications.Add(new StringAttributeSpecification(name, attributeValues));
            TargetAttribute = name;// target defaults to last column added
        }

        public IAttributeSpecification GetAttributeSpecFor(string name)
        {
            foreach (var spec in
                this.attributeSpecifications.Where(spec => spec.GetAttributeName().Equals(name)))
            {
                return spec;
            }
            throw new ApplicationException(String.Format("no attribute spec for  {0}", name));
        }

        public void DefineNumericAttribute(string name)
        {
            attributeSpecifications.Add(new NumericAttributeSpecification(name));
        }

        public IList<string> GetNamesOfStringAttributes() 
        {
            return this.attributeSpecifications.OfType<StringAttributeSpecification>().Select(spec => ((IAttributeSpecification)spec).GetAttributeName()).ToList();
        }
    }

}
