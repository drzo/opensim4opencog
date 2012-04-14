using System;
using System.Collections;

using Iesi.Collections;
using Iesi.Collections.Generic;
using NUnit.Framework;

namespace Iesi.Collections.Test.Generic
{
	/// <summary>
	/// Summary description for HybridSetFixture.
	/// </summary>
	[TestFixture]
	public class HybridSetFixture : SetFixture
	{

		protected override ISet CreateInstance()
		{
			return new HybridSet<string>();
		}

		protected override ISet CreateInstance(ICollection init)
		{
			return new HybridSet<string>( new CollectionWrapper<string>(init)  );
		}

		protected override Type ExpectedType
		{
			get { return typeof(HybridSet<string>); }
		}

	}
}
