using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    using Aima.Core.Learning.Framework;
    using Aima.Core.Util.Datastructure;

    public class IrisDataSetNumerizer : INumerizer 
    {
        public Pair<IList<double>, IList<double>> Numerize(Example e) {
            IList<double> input = new List<double>();
            IList<double> desiredOutput = new List<double>();

            double sepalLength = e.GetAttributeValueAsDouble("sepal_length");
            double sepalWidth = e.GetAttributeValueAsDouble("sepal_width");
            double petalLength = e.GetAttributeValueAsDouble("petal_length");
            double petalWidth = e.GetAttributeValueAsDouble("petal_width");

            input.Add(sepalLength);
            input.Add(sepalWidth);
            input.Add(petalLength);
            input.Add(petalWidth);

            string plantCategoryString = e.GetAttributeValueAsString("plant_category");

            desiredOutput = this.ConvertCategoryToListOfDoubles(plantCategoryString);

            var io = new Pair<IList<double>, IList<double>>(input, desiredOutput);

            return io;
        }

        public string Denumerize(IList<double> outputValue) {
            IList<double> rounded = new List<double>();
            foreach (double d in outputValue) {
                rounded.Add(round(d));
            }
            if (rounded.Equals(new[]{0.0,0.0,1.0}.ToList()))
            {
                return "setosa";
            }
            if (rounded.Equals(new[]{0.0,1.0,0.0}.ToList())) {
                return "versicolor";
            }
            if (rounded.Equals(new[]{1.0,0.0,0.0}.ToList())) 
            {
                return "virginica";
            }
            return "unknown";
        }

        private double round(double d) {
            if (d < 0) {
                return 0.0;
            }
            if (d > 1) {
                return 1.0;
            }
            return Math.Round(d);
        }

        private IList<double> ConvertCategoryToListOfDoubles(
                string plantCategoryString)
        {
            if (plantCategoryString.Equals("setosa")) {
                return new[] { 0.0, 0.0, 1.0 }.ToList();
            }
            if (plantCategoryString.Equals("versicolor")) {
                return new[] { 0.0, 1.0, 0.0 }.ToList();
            }
            if (plantCategoryString.Equals("virginica")) {
                return new[] { 1.0, 0.0, 0.0 }.ToList();
            }
            throw new ApplicationException("invalid plant category");
        }
    }

}
