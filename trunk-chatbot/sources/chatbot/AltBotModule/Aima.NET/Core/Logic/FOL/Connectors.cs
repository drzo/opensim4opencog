using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL
{
    public static class Connectors 
    {
        public static readonly string And = "AND";

        public static readonly string Or = "OR";

        public static readonly string Not = "NOT";

        public static readonly string Implies = "=>";

        public static readonly string BiCond = "<=>";

        public static bool IsAnd(string connector) 
        {
            return And.Equals(connector);
        }

        public static bool IsOr(string connector)
        {
            return Or.Equals(connector);
        }

        public static bool IsNot(string connector)
        {
            return Not.Equals(connector);
        }

        public static bool IsImplies(string connector)
        {
            return Implies.Equals(connector);
        }

        public static bool IsBiCond(string connector)
        {
            return BiCond.Equals(connector);
        }
    }
}
