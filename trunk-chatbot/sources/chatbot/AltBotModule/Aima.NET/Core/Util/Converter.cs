using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Util
{
    public class Converter<T>
    {

        public IList<T> SetToList(ISet<T> set)
        {
            IList<T> retVal = new List<T>(set);
            return retVal;
        }

        public ISet<T> ListToSet(IList<T> l)
        {
            ISet<T> retVal = new HashedSet<T>(l);
            return retVal;
        }
    }
}
