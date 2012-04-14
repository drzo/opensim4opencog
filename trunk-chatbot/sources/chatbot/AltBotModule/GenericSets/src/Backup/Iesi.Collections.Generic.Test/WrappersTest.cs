#define NUNIT
using System;
using System.Text;
using System.Collections.Generic;
using System.Collections;
#if ! NUNIT
using Microsoft.VisualStudio.TestTools.UnitTesting;
#else
using NUnit.Framework;
using TestClass = NUnit.Framework.TestFixtureAttribute;
using TestInitialize = NUnit.Framework.SetUpAttribute;
using TestCleanup = NUnit.Framework.TearDownAttribute;
using TestMethod = NUnit.Framework.TestAttribute;
#endif


namespace Iesi.Collections.Generic.Test
{
    [TestClass]
    public class WrappersTest
    {
        public static string one = "one";
        public static string two = "two";
        public static string three = "three";
        IList list;
        ICollection<string> cwrapper;
        IEnumerable<string> ewrapper;
        IList<string> lwrapper;
        [TestInitialize]
        public void Setup()
        {
            list = new ArrayList(3);
            list.Add(one);
            list.Add(two);
            list.Add(three);
            this.cwrapper = new CollectionWrapper<string>(list);
            this.ewrapper = new EnumerableWrapper<string>(list);
            this.lwrapper = new ListWrapper<string>(list);
        }
                              
    
        
        [TestMethod]
        public void CollectionWrapperTest()
        {
            Assert.AreEqual(3, cwrapper.Count);
            string notIn = "not in there";
            Assert.IsTrue(cwrapper.Contains(one));
            Assert.IsTrue(cwrapper.Contains(two));
            Assert.IsFalse(cwrapper.Contains(notIn));
        }

        [TestMethod]
        public void EnumerableWrapperTest()
        {
            IList elements = new ArrayList();
            foreach (String s in ewrapper)
                elements.Add(s);
            Assert.AreEqual(3, elements.Count);
            string notIn = "not in there";
            Assert.IsTrue(elements.Contains(one));
            Assert.IsTrue(elements.Contains(two));
            Assert.IsFalse(elements.Contains(notIn));
        }

        [TestMethod]
        public void ListWrapperTest()
        {
            Assert.AreEqual(3, lwrapper.Count);
            string notIn = "not in there";
            Assert.IsTrue(lwrapper.Contains(one));
            Assert.IsTrue(lwrapper.Contains(two));
            Assert.IsFalse(lwrapper.Contains(notIn));
        }
        
        [TestMethod]
        public void EqualTest()
        {
            Assert.AreNotEqual(cwrapper, ewrapper);
            IEnumerable<string> cwrapper2 = new CollectionWrapper<string>(list);
            Assert.AreEqual(cwrapper2,  cwrapper);
            ArrayList list2 = new ArrayList();
            list2.Add("four");
            IEnumerable<string> cwrapper3 = new CollectionWrapper<string>(list2);
            Assert.AreNotEqual(cwrapper3, cwrapper);
        }
    }
}
