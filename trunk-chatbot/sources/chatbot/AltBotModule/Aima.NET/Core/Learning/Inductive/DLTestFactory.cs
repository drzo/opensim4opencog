using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Inductive
{
    using Aima.Core.Learning.Framework;

    public class DLTestFactory
    {

        public IList<DLTest> CreateDLTestsWithAttributeCount(DataSet ds, int i) 
        {
            if (i != 1) 
            {
                throw new ApplicationException("For now DLTests with only 1 attribute can be craeted , not"+ i);
            }
            var nonTargetAttributes = ds.GetNonTargetAttributes();
            var tests = new List<DLTest>();
            foreach (var nta in nonTargetAttributes) 
            {
                var ntaValues = ds.GetPossibleAttributeValues(nta);
                foreach (var ntaValue in ntaValues) 
                {
                    var test = new DLTest();
                    test.Add(nta, ntaValue);
                    tests.Add(test);

                }
            }
            return tests;
        }
    }
}