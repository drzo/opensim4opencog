using System;
using System.Collections;

using Iesi.Collections;
using Iesi.Collections.Generic;
using NUnit.Framework;

namespace Iesi.Collections.Test.Generic
{
	/// <summary>
	/// Summary description for ListSetFixture.
	/// </summary>
	[TestFixture]
	public class ListSetFixture : SetFixture
	{
		protected override ISet CreateInstance()
		{
			return new ListSet<string>();
		}

		protected override ISet CreateInstance(ICollection init)
		{
			return new ListSet<string>( new CollectionWrapper<string> (init) );
		}

		protected override Type ExpectedType
		{
			get { return typeof(ListSet<string>); }
		}
	}
}
