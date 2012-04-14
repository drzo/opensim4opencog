using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Framework
{
    using System.IO;
    using System.Reflection;

    using Aima.Core.Learning.Data;

    public class DataSetFactory 
    {

        public DataSet FromFile(string filename, DataSetSpecification spec, string separator) 
        {
            // assumed file in data directory and ends in .csv
            var ds = new DataSet(spec);
            var assembly = Assembly.GetExecutingAssembly();

            var stream = assembly.GetManifestResourceStream(filename + ".csv");
            
            if (stream == null) throw new ArgumentOutOfRangeException("filename","resource does not exist");

            TextReader inputStream = new StreamReader(stream); 

            String line;
            while ((line = inputStream.ReadLine()) != null) 
            {
                ds.Add(ExampleFromString(line, spec, separator));
            }

            return ds;

        }

        public static Example ExampleFromString(string data, DataSetSpecification dataSetSpec, string separator) 
        {
            var attributes = new Dictionary<string, IAttribute>();
            var attributeValues = data.Split(separator.ToCharArray()).ToList();
            if (dataSetSpec.IsValid(attributeValues)) 
            {
                var names = dataSetSpec.GetAttributeNames();

                for (var i = 0; i < names.Count && i < attributeValues.Count; i++)
                {
                    var name = names[i];
                    var attributeSpec = dataSetSpec.GetAttributeSpecFor(name);
                    var attribute = attributeSpec.CreateAttribute(attributeValues[i]);
                    attributes[name] = attribute;
                }

                var targetAttributeName = dataSetSpec.TargetAttribute;
                return new Example(attributes, attributes[targetAttributeName]);
            } 
            else 
            {
                throw new ApplicationException(String.Format("Unable to construct Example from {0}", data));
            }
        }

        public static DataSet GetRestaurantDataSet()
        {
            var spec = CreateRestaurantDataSetSpec();
            return new DataSetFactory().FromFile("restaurant", spec, "\\s+");
        }

        public static DataSetSpecification CreateRestaurantDataSetSpec() {
            var dss = new DataSetSpecification();
            dss.DefineStringAttribute("alternate", Util.Util.YesNo());
            dss.DefineStringAttribute("bar", Util.Util.YesNo());
            dss.DefineStringAttribute("fri/sat", Util.Util.YesNo());
            dss.DefineStringAttribute("hungry", Util.Util.YesNo());
            dss.DefineStringAttribute("patrons", new[] { "None", "Some",
                    "Full" });
            dss.DefineStringAttribute("price", new[] { "$", "$$", "$$$" });
            dss.DefineStringAttribute("raining", Util.Util.YesNo());
            dss.DefineStringAttribute("reservation", Util.Util.YesNo());
            dss.DefineStringAttribute("type", new[] { "French", "Italian",
                    "Thai", "Burger" });
            dss.DefineStringAttribute("wait_estimate", new[] { "0-10",
                    "10-30", "30-60", ">60" });
            dss.DefineStringAttribute("will_wait", Util.Util.YesNo());
            // last attribute is the target attribute unless the target is
            // explicitly reset with dss.setTarget(name)

            return dss;
        }

        public static DataSet GetIrisDataSet()
        {
            var spec = CreateIrisDataSetSpec();
            return new DataSetFactory().FromFile("iris", spec, ",");
        }

        public static DataSetSpecification CreateIrisDataSetSpec() 
        {
            DataSetSpecification dss = new DataSetSpecification();
            dss.DefineNumericAttribute("sepal_length");
            dss.DefineNumericAttribute("sepal_width");
            dss.DefineNumericAttribute("petal_length");
            dss.DefineNumericAttribute("petal_width");
            dss.DefineStringAttribute("plant_category", new String[] { "setosa",
                    "versicolor", "virginica" });
            return dss;
        }
    }

}
