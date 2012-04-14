using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL
{
    public static class Quantifiers 
    {
        public static readonly String ForAll = "FORALL";
        public static readonly String Exists = "EXISTS";

        public static bool IsForall(String quantifier) 
        {
            return ForAll.Equals(quantifier);
        }

        public static bool IsExists(String quantifier)
        {
            return Exists.Equals(quantifier);
        }
    }
}
