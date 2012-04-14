using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    using System.IO;
    using System.Reflection;

    using Aima.Core.Learning.Framework;
    using Aima.Core.Util.Datastructure;

    /// <summary>
    /// This class represents a source of examples to the rest of the nn
    /// framework. Assumes only one function approximator works on an instance at
    /// a given point in time
    /// </summary>
    public abstract class NNDataSet 
    {
        /// <summary>
        /// the parsed and preprocessed form of the dataset.
        /// </summary>
        private IList<NNExample> dataset;

        /// <summary>
        /// a copy from which examples are drawn.
        /// </summary>
        private IList<NNExample> presentlyProcessed = new List<NNExample>();

        /// <summary>
        /// list of mean Values for all components of raw data set
        /// </summary>
        private IList<double> means;

        /// <summary>
        /// list of stdev Values for all components of raw data set
        /// </summary>
        private IList<double> stdevs;

        /// <summary>
        /// the normalized data set
        /// </summary>
        protected IList<IList<double>> Nds;

        /// <summary>
        /// the column numbers of the "target"
        /// </summary>
        protected IList<int> TargetColumnNumbers;

        /// <summary>
        /// population delegated to subclass because only subclass knows which
        /// column(s) is target
        /// </summary>
        public abstract void SetTargetColumns();

        /// <summary>
        /// create a normalized data "table" from the data in the file. At this
        /// stage, the data isnot split into input pattern and tragets
        /// </summary>
        /// <param name="filename"></param>
        public void CreateNormalizedDataFromFile(string filename) 
        {
            IList<IList<double>> rds = new List<IList<double>>();

            var assembly = Assembly.GetExecutingAssembly();

            var stream = assembly.GetManifestResourceStream("../data/" + filename + ".csv");
            
            if (stream == null) throw new ArgumentOutOfRangeException("filename","resource does not exist");

            TextReader inputStream = new StreamReader(stream); 

            string line;
            while ((line = inputStream.ReadLine()) != null) 
            {
                rds.Add(ExampleFromString(line, ","));
            }

            // normalize raw dataset
            Nds = this.Normalize(rds);
        }

        /// <summary>
        /// create a normalized data "table" from the DataSet using numerizer. At
        /// this stage, the data isnot split into input pattern and targets TODO
        /// remove redundancy of recreating the target columns. the numerizer has
        /// already isolated the targets
        /// </summary>
        /// <param name="ds"></param>
        /// <param name="numerizer"></param>
        public void CreateNormalizedDataFromDataSet(DataSet ds, INumerizer numerizer)
        {
            IList<IList<double>> rds = this.RawExamplesFromDataSet(ds, numerizer);
            // normalize raw dataset
            Nds = this.Normalize(rds);
        }

        /// <summary>
        /// Gets (and removes) a random example from the 'presentlyProcessed'
        /// </summary>
        /// <returns></returns>
        public NNExample GetExampleAtRandom() {

            int i = Util.Util.RandomNumberBetween(0, (presentlyProcessed.Count - 1));
            var pp = presentlyProcessed[i];
            presentlyProcessed.RemoveAt(i);
            return pp;
        }

        /// <summary>
        /// Gets (and removes) a random example from the 'presentlyProcessed'
        /// </summary>
        /// <param name="index"></param>
        /// <returns></returns>
        public NNExample GetExample(int index)
        {
            var pp = presentlyProcessed[index];
            presentlyProcessed.RemoveAt(index);
            return pp;
        }

        /// <summary>
        /// check if any more examples remain to be processed
        /// </summary>
        /// <returns></returns>
        public bool HasMoreExamples() 
        {
            return this.presentlyProcessed.Count > 0;
        }

        /// <summary>
        /// check how many examples remain to be processed
        /// </summary>
        /// <returns></returns>
        public int HowManyExamplesLeft() 
        {
            return presentlyProcessed.Count;
        }

        /// <summary>
        /// refreshes the presentlyProcessed dataset so it can be used for a new
        /// epoch of training.
        /// </summary>
        public void RefreshDataset() 
        {
            presentlyProcessed = new List<NNExample>();
            foreach (NNExample e in dataset) {
                presentlyProcessed.Add(e.CopyExample());
            }
        }

        /// <summary>
        /// method called by clients to set up data set and make it ready for
        /// processing
        /// </summary>
        /// <param name="filename"></param>
        public void CreateExamplesFromFile(string filename)
        {
            CreateNormalizedDataFromFile(filename);
            SetTargetColumns();
            this.CreateExamples();

        }

        /// <summary>
        /// method called by clients to set up data set and make it ready for
        /// processing
        /// </summary>
        /// <param name="ds"></param>
        /// <param name="numerizer"></param>
        public void CreateExamplesFromDataSet(DataSet ds, INumerizer numerizer)
        {
            CreateNormalizedDataFromDataSet(ds, numerizer);
            SetTargetColumns();
            this.CreateExamples();

        }

        public IList<IList<double>> GetNormalizedData() {
            return Nds;
        }

        public IList<double> GetMeans() {
            return means;
        }

        public IList<double> GetStdevs() {
            return stdevs;
        }

        /// <summary>
        /// create Example instances from a normalized data "table".
        /// </summary>
        private void CreateExamples() 
        {
            dataset = new List<NNExample>();
            foreach (IList<double> dataLine in Nds) 
            {
                IList<double> input = new List<double>();
                IList<double> target = new List<double>();
                for (int i = 0; i < dataLine.Count; i++) {
                    if (TargetColumnNumbers.Contains(i)) {
                        target.Add(dataLine[i]);
                    } else {
                        input.Add(dataLine[i]);
                    }
                }
                dataset.Add(new NNExample(input, target));
            }
            this.RefreshDataset();// to populate the preentlyProcessed dataset
        }

        private IList<IList<double>> Normalize(IList<IList<double>> rds) 
        {
            int rawDataLength = rds[0].Count;
            IList<IList<double>> nds = new List<IList<double>>();

            means = new List<double>();
            stdevs = new List<double>();

            IList<IList<double>> normalizedColumns = new List<IList<double>>();
            // clculate means for each coponent of example data
            for (int i = 0; i < rawDataLength; i++) 
            {
                IList<double> columnValues = new List<double>();
                foreach (IList<double> rawDatum in rds) {
                    columnValues.Add(rawDatum[i]);
                }
                double mean = Util.Util.CalculateMean(columnValues);
                means.Add(mean);

                double stdev = Util.Util.CalculateStDev(columnValues, mean);
                stdevs.Add(stdev);

                normalizedColumns.Add(Util.Util.NormalizeFromMeanAndStdev(columnValues, mean, stdev));

            }
            // re arrange data from columns
            // TODO Assert normalized columns have same size etc

            int columnLength = normalizedColumns[0].Count;
            int numberOfColumns = normalizedColumns.Count;
            for (int i = 0; i < columnLength; i++) {
                IList<double> lst = new List<double>();
                for (int j = 0; j < numberOfColumns; j++) {
                    lst.Add(normalizedColumns[j][i]);
                }
                nds.Add(lst);
            }
            return nds;
        }

        private IList<double> ExampleFromString(string line, string separator) 
        {
            // assumes all values for inout and target are doubles
            IList<string> attributeValues = line.Split(separator.ToCharArray()).ToList();
            return attributeValues.Select(valString => double.Parse(valString)).ToList();
        }

        private IList<IList<double>> RawExamplesFromDataSet(DataSet ds,INumerizer numerizer) 
        {
            // assumes all values for inout and target are doubles
            IList<IList<double>> rds = new List<IList<double>>();
            for (int i = 0; i < ds.Count; i++) 
            {
                IList<double> rexample = new List<double>();
                Example e = ds.GetExample(i);
                Pair<IList<double>, IList<double>> p = numerizer.Numerize(e);
                IList<double> attributes = p.GetFirst();
                foreach (double d in attributes) 
                {
                    rexample.Add(d);
                }
                IList<double> targets = p.GetSecond();
                foreach (double d in targets) 
                {
                    rexample.Add(d);
                }
                rds.Add(rexample);
            }
            return rds;
        }
    }
}
