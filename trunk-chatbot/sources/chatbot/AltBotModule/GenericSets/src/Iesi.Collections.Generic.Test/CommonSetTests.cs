#define NUNIT
using System;
using System.Collections.Generic;
using System.Collections;

using Iesi.Collections.Generic;

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
	/// <summary>
	/// Summary description for SetFixture.
	/// </summary>
	public abstract class CommonSetTests 
	{
		private IList<string> _aInitValues;
        private IList<string> _bInitValues;
        protected ISet<string> _set;

		public static string one = "one";
		public static string two = "two";
		public static string three = "three";

		public virtual void SetUp()
		{
            _aInitValues = new List<string>();
			_aInitValues.Add( "zero" );
			_aInitValues.Add( "one" );
			_aInitValues.Add( "two" );
			_aInitValues.Add( "three" );

            _bInitValues = new List<string>();
			_bInitValues.Add( "two" );
			_bInitValues.Add( "three" );
			_bInitValues.Add( "four" );

			_set = CreateInstance();
			_set.Add( one );
			_set.Add( two );
			_set.Add( three );
		}

		#region System.IClonable Member Tests


		public virtual void Clone() 
		{
            ISet<string> clonedSet = (ISet<string>)_set.Clone();
			
			Assert.AreEqual( ExpectedType, clonedSet.GetType(), "cloned set should be the same type" );
			Assert.AreEqual( _set.Count, clonedSet.Count, "set and cloned version should be same" );

			clonedSet.Add( "not in original" );
			Assert.IsFalse( _set.Count==clonedSet.Count, "adding to clone should not add to original." );

			foreach( string obj in _set ) 
			{
				Assert.IsTrue( clonedSet.Contains( obj ), "cloned set should have same objects as original set." );
			}
		}

        public virtual void CloneNonGeneric()
        {
            ISet clonedSet = (ISet)_set.Clone();

            Assert.AreEqual( ExpectedType, clonedSet.GetType(), "cloned set should be the same type" );
            Assert.AreEqual( _set.Count, clonedSet.Count, "set and cloned version should be same" );

            clonedSet.Add( "not in original" );
            Assert.IsFalse( _set.Count == clonedSet.Count, "adding to clone should not add to original." );

            foreach ( string obj in _set )
            {
                Assert.IsTrue( clonedSet.Contains( obj ), "cloned set should have same objects as original set." );
            }
        }



		#endregion

		#region System.Collections.ICollection Member Tests
		

		public virtual void CopyTo() 
		{
			string[] dest = new string[3];
			_set.CopyTo( dest, 0 );

			int count = 0;

			foreach( string obj in dest ) 
			{
				Assert.IsTrue( _set.Contains( obj ), "set should contain the object in the array" );
				count++;
			}

			Assert.AreEqual( 3, count, "should have 3 items in array" );
		}

        public virtual void CopyToNonGeneric()
        {
            ISet oSet = (ISet)_set;
            object[] dest = new string[3]; // Should this be object[] or string[]?
            oSet.CopyTo( dest, 0 );

            int count = 0;

            foreach ( object obj in dest )
            {
                Assert.IsTrue( oSet.Contains( obj ), "set should contain the object in the array" );
                count++;
            }

            Assert.AreEqual( 3, count, "should have 3 items in array" );
        }


		public virtual void Count() 
		{
			Assert.AreEqual( 3, _set.Count, "should be 3 items" );
			Assert.AreEqual( 0, CreateInstance().Count, "new set should have nothing in it." );
		}

        public virtual void CountNonGeneric()
        {
            ISet oSet = (ISet)_set;
            Assert.AreEqual( 3, oSet.Count, "should be 3 items" );
            Assert.AreEqual( 0, ((ISet)CreateInstance()).Count, "new set should have nothing in it." );
        }
        
        #endregion

		#region Iesi.Collections.ISet Constructor Tests


        public virtual void CtorWithDefaults() 
		{
            List<string> init = new List<string>( 3 );
			init.Add( "one" );
			init.Add( "two" );
			init.Add( "three" );

            ISet<string> theSet = CreateInstance( init );

			Assert.AreEqual( 3, init.Count, "3 items in set" );

			int index = 0;
			foreach( string obj in init ) 
			{
				Assert.IsTrue( theSet.Contains( obj ), "set should contain obj at index = " + index.ToString() );
				index++;
			}
		}

        public virtual void CtorWithDefaultsNonGeneric()
        {
            IList<string> init = new List<string>( 3 );
            init.Add( "one" );
            init.Add( "two" );
            init.Add( "three" );

            ISet theSet = (Set<string>) CreateInstance( init );

            Assert.AreEqual( 3, init.Count, "3 items in set" );

            int index = 0;
            foreach ( object obj in init )
            {
                Assert.IsTrue( theSet.Contains( obj ), "set should contain obj at index = " + index.ToString() );
                index++;
            }
        }

		#endregion

		#region Iesi.Collections.ISet Member Tests
		

        public virtual void Add() 
		{
			_set.Add( "four" );
			Assert.AreEqual( 4, _set.Count, "should have added 'four'" );

			_set.Add( two );
			Assert.AreEqual( 4, _set.Count, "object already in set" );
		}

        public virtual void AddNonGeneric()
        {
            ISet oSet = (ISet)_set;

            oSet.Add( "four" );
            Assert.AreEqual( 4, _set.Count, "should have added 'four'" );

            oSet.Add( two );
            Assert.AreEqual( 4, _set.Count, "object already in set" );
        }

        public virtual void AddAll() 
		{
            List<string> addAll = new List<string>( 3 );
			addAll.Add( "four" );
			addAll.Add( "five" );
			addAll.Add( "four" );

			Assert.IsTrue( _set.AddAll( addAll ), "should have modified set" );
			Assert.AreEqual( 5, _set.Count, "should have added one 'four' and 'five'" );

			Assert.IsFalse( _set.AddAll( addAll ), "all elements already in set" );
          
		}

        public virtual void AddAllNonGeneric()
        {
            ISet oSet = (ISet)_set;

            IList addAll = new ArrayList( 3 );
            addAll.Add( "four" );
            addAll.Add( "five" );
            addAll.Add( "four" );

            Assert.IsTrue( oSet.AddAll( addAll ), "should have modified set" );
            Assert.AreEqual( 5, oSet.Count, "should have added one 'four' and 'five'" );

            Assert.IsFalse( oSet.AddAll( addAll ), "all elements already in set" );
        }

        public virtual void Clear() 
		{
			_set.Clear();
			Assert.AreEqual( 0, _set.Count, "should have no items in ISet." );
		}


        public virtual void Contains() 
		{
			Assert.IsTrue( _set.Contains( one ), "does contain one" );
			Assert.IsFalse( _set.Contains( "four" ), "does not contain 'four'" );
		}


        public virtual void ContainsAll() 
		{
            List<string> all = new List<string>( 2 );
			all.Add( "one" );
			all.Add( "two" );

			Assert.IsTrue( _set.ContainsAll( all ), "should contain 'one' and 'two'" );

			all.Add( "not in there" );
			Assert.IsFalse( _set.ContainsAll( all ), "should not contain the just added 'not in there'" );
           
		}

        public virtual void ContainsAllNonGeneric()
        {
            ISet oset = (ISet)_set;
            IList all = new ArrayList(2);
            all.Add("one");
            all.Add("two");

            Assert.IsTrue(oset.ContainsAll(all), "should contain 'one' and 'two'");

            all.Add("not in there");
            Assert.IsFalse(oset.ContainsAll(all), "should not contain the just added 'not in there'");
        }


        public virtual void ExclusiveOr() 
		{
            ISet<string> a = CreateInstance( _aInitValues );
            ISet<string> b = CreateInstance( _bInitValues );

            ISet<string> ab = Set<string>.ExclusiveOr( a, b );

			Assert.AreEqual( 3, ab.Count, "contains 3 elements - 'zero', 'one', and 'four'" );
			Assert.IsTrue( ab.Contains( "zero" ), "should contain 'zero'" );
			Assert.IsTrue( ab.Contains( "one" ), "should contain 'one'" );
			Assert.IsTrue( ab.Contains( "four" ), "should contain 'four'" );

			Assert.IsTrue( a.ContainsAll( _aInitValues ), "should not have modified a" );
			Assert.IsTrue( b.ContainsAll( _bInitValues ), "should not have modified b" );

            ISet<string> aNull = Set<string>.ExclusiveOr( a, null );
			Assert.AreEqual( _aInitValues.Count, aNull.Count, "count still same" );
			Assert.IsTrue( aNull.ContainsAll( _aInitValues ), "all A elements kept" );

            ISet<string> bNull = Set<string>.ExclusiveOr( null, b );
			Assert.AreEqual( _bInitValues.Count, bNull.Count, "count still same" );
			Assert.IsTrue( bNull.ContainsAll( _bInitValues ), "all B elements kept" );

            ISet<string> bothNull = Set<string>.ExclusiveOr( null, null );
			Assert.AreEqual( null, bothNull, "two null sets return null set" );
		}


        public virtual void Intersect() 
		{
            ISet<string> a = CreateInstance( _aInitValues );
            ISet<string> b = CreateInstance( _bInitValues );

            ISet<string> ab = Set<string>.Intersect( a, b );

			Assert.AreEqual( 2, ab.Count, "contains 2 elements - 'two', and 'three'" );
			Assert.IsTrue( ab.Contains( "two" ), "should contain 'two'" );
			Assert.IsTrue( ab.Contains( "three" ), "should contain 'three'" );

			Assert.IsTrue( a.ContainsAll( _aInitValues ), "should not have modified a" );
			Assert.IsTrue( b.ContainsAll( _bInitValues ), "should not have modified b" );

            ISet<string> aNull = Set<string>.Intersect( a, null );
			Assert.AreEqual( 0, aNull.Count, "no elements intersected with null set" );

            ISet<string> bNull = Set<string>.Intersect( null, b );
			Assert.AreEqual( 0, bNull.Count, "no elements intersected with null set" );

            ISet<string> bothNull = Set<string>.Intersect( null, null );
			Assert.AreEqual( null, bothNull, "null sets intersect as null set" );
		}


        public virtual void IsEmpty() 
		{
			Assert.IsFalse( _set.IsEmpty, "set should have initial values" );

			Assert.IsTrue( CreateInstance().IsEmpty, "new set is empty" );
		}


        public virtual void Minus() 
		{
            ISet<string> a = CreateInstance( _aInitValues );
            ISet<string> b = CreateInstance( _bInitValues );

            ISet<string> ab = Set<string>.Minus( a, b );

			Assert.AreEqual( 2, ab.Count, "contains 2 elements - 'zero', and 'one'" );
			Assert.IsTrue( ab.Contains( "zero" ), "should contain 'zero'" );
			Assert.IsTrue( ab.Contains( "one" ), "should contain 'one'" );

			Assert.IsTrue( a.ContainsAll( _aInitValues ), "should not have modified a" );
			Assert.IsTrue( b.ContainsAll( _bInitValues ), "should not have modified b" );

            ISet<string> aNull = Set<string>.Minus( a, null );
			Assert.IsTrue( aNull.ContainsAll( _aInitValues ), "should have removed no elements" );

            ISet<string> bNull = Set<string>.Minus( null, b );
			Assert.AreEqual( null, bNull, "null set remained null" );

            ISet<string> bothNull = Set<string>.Minus( null, null );
			Assert.AreEqual( null, bothNull, "both sets are null" );
		}


        public virtual void Remove() 
		{
			Assert.IsTrue( _set.Remove( one ), "should have removed 'one'" );
			Assert.IsFalse( _set.Contains( one ), "one should have been removed" );
			Assert.AreEqual( 2, _set.Count, "should be 2 items after one removed." );

			Assert.IsFalse( _set.Remove( one ), "was already removed." );
		}


        public virtual void RemoveAll() 
		{
            List<string> all = new List<string>( 2 );
			all.Add( one );
			all.Add( "not in there" );

			Assert.IsTrue( _set.RemoveAll( all ), "should have removed an element" );
			Assert.AreEqual( 2, _set.Count, "should be down to 2 elements." );
			Assert.IsFalse( _set.RemoveAll( all ), "all of the elements already removed so set not modified." );
            
		}

        public virtual void RemoveAllNonGeneric()
        {
            ISet oset = (ISet)_set;
            IList all = new ArrayList(2);
            all.Add(one);
            all.Add("not in there");

            Assert.IsTrue(oset.RemoveAll(all), "should have removed an element");
            Assert.AreEqual(2, oset.Count, "should be down to 2 elements.");
            Assert.IsFalse(oset.RemoveAll(all), "all of the elements already removed so set not modified.");
        }


        public virtual void RetainAll() 
		{
            List<string> retain = new List<string>( 2 );
			retain.Add( one );
			retain.Add( "not in there" );

			Assert.IsTrue( _set.RetainAll( retain ), "set was modified" );
			Assert.AreEqual( 1, _set.Count, "only 1 element retained" );

			Assert.IsFalse( _set.RetainAll( retain ), "set was not modified" );
            
		}

        public virtual void RetainAllNonGeneric()
        {
            ISet oset = (ISet)_set;
            IList retain = new ArrayList(2);
            retain.Add(one);
            retain.Add("not in there");

            Assert.IsTrue(oset.RetainAll(retain), "set was modified");
            Assert.AreEqual(1, _set.Count, "only 1 element retained");

            Assert.IsFalse(oset.RetainAll(retain), "set was not modified");
        }


        public virtual void Union() 
		{
            ISet<string> a = CreateInstance( _aInitValues );
            ISet<string> b = CreateInstance( _bInitValues );

            ISet<string> ab = Set<string>.Union( a, b );

			Assert.AreEqual( 5, ab.Count, "contains 5 elements - 'zero' through 'four'" );
			Assert.IsTrue( ab.Contains( "zero" ), "should contain 'zero'" );
			Assert.IsTrue( ab.Contains( "one" ), "should contain 'one'" );
			Assert.IsTrue( ab.Contains( "two" ), "should contain 'two'" );
			Assert.IsTrue( ab.Contains( "three" ), "should contain 'three'" );
			Assert.IsTrue( ab.Contains( "four" ), "should contain 'four'" );

			Assert.IsTrue( a.ContainsAll( _aInitValues ), "should not have modified a" );
			Assert.IsTrue( b.ContainsAll( _bInitValues ), "should not have modified b" );

            ISet<string> aNull = Set<string>.Union( a, null );
			Assert.AreEqual( _aInitValues.Count, aNull.Count, "count not changed" );
			Assert.IsTrue( aNull.ContainsAll( _aInitValues ), "still contains all initial values" );

            ISet<string> bNull = Set<string>.Union( null, b );
			Assert.AreEqual( _bInitValues.Count, bNull.Count, "count not changed" );
			Assert.IsTrue( bNull.ContainsAll( _bInitValues ), "still contains all initial values" );

            ISet<string> bothNull = Set<string>.Union( null, null );
			Assert.AreEqual( null, bothNull, "two nulls intersect as null" );
		}
		
		#endregion

 
		#region Iesi.Collection.ISet Operator Tests

        public virtual void ExclusiveOrOperator() 
		{
            ISet<string> a = CreateInstance( _aInitValues );
            ISet<string> b = CreateInstance( _bInitValues );

            ISet<string> ab = (Set<string>)a ^ (Set<string>)b;

			Assert.AreEqual( 3, ab.Count, "contains 3 elements - 'zero', 'one', and 'four'" );
			Assert.IsTrue( ab.Contains( "zero" ), "should contain 'zero'" );
			Assert.IsTrue( ab.Contains( "one" ), "should contain 'one'" );
			Assert.IsTrue( ab.Contains( "four" ), "should contain 'four'" );

			Assert.IsTrue( a.ContainsAll( _aInitValues ), "should not have modified a" );
			Assert.IsTrue( b.ContainsAll( _bInitValues ), "should not have modified b" );

		
		}


        public virtual void IntersectOperator() 
		{
            ISet<string> a = CreateInstance( _aInitValues );
            ISet<string> b = CreateInstance( _bInitValues );

            ISet<string> ab = (Set<string>)a & (Set<string>)b;

			Assert.AreEqual( 2, ab.Count, "contains 2 elements - 'two', and 'three'" );
			Assert.IsTrue( ab.Contains( "two" ), "should contain 'two'" );
			Assert.IsTrue( ab.Contains( "three" ), "should contain 'three'" );
		}


        public virtual void MinusOperator() 
		{
            ISet<string> a = CreateInstance( _aInitValues );
            ISet<string> b = CreateInstance( _bInitValues );

            ISet<string> ab = (Set<string>)a - (Set<string>)b;

			Assert.AreEqual( 2, ab.Count, "contains 2 elements - 'zero', and 'one'" );
			Assert.IsTrue( ab.Contains( "zero" ), "should contain 'zero'" );
			Assert.IsTrue( ab.Contains( "one" ), "should contain 'one'" );

			Assert.IsTrue( a.ContainsAll( _aInitValues ), "should not have modified a" );
			Assert.IsTrue( b.ContainsAll( _bInitValues ), "should not have modified b" );

		}


		public virtual void UnionOperator() 
		{
            ISet<string> a = CreateInstance( _aInitValues );
            ISet<string> b = CreateInstance( _bInitValues );

            ISet<string> ab = (Set<string>)a | (Set<string>)b;

			Assert.AreEqual( 5, ab.Count, "contains 5 elements - 'zero' through 'four'" );
			Assert.IsTrue( ab.Contains( "zero" ), "should contain 'zero'" );
			Assert.IsTrue( ab.Contains( "one" ), "should contain 'one'" );
			Assert.IsTrue( ab.Contains( "two" ), "should contain 'two'" );
			Assert.IsTrue( ab.Contains( "three" ), "should contain 'three'" );
			Assert.IsTrue( ab.Contains( "four" ), "should contain 'four'" );

			Assert.IsTrue( a.ContainsAll( _aInitValues ), "should not have modified a" );
			Assert.IsTrue( b.ContainsAll( _bInitValues ), "should not have modified b" );

            HybridUnionOperator();
		}
		
	    public virtual void HybridUnionOperator()
	    {
            ISet b = new HashedSet();
            ISet a = CreateInstance() as ISet;
            a.Add("one");
            b.Add("two");
            ISet c = a.Union(b);
            Assert.AreEqual(2, c.Count);
            Assert.IsTrue( c.Contains("one") && c.Contains("two"));
            
	        ISet d = b.Union(a);
            Assert.AreEqual(2, d.Count);
            Assert.IsTrue(d.Contains("one") && d.Contains("two"));   
	    }

		#endregion

        protected abstract ISet<string> CreateInstance();

        protected abstract ISet<string> CreateInstance( ICollection<string> init );

		protected abstract System.Type ExpectedType{ get; }
	}
}
