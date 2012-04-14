using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.Common
{
    public static class LogicTokenTypes
    {
        public static readonly int Symbol = 1;

        public static readonly int LParen = 2;

        public static readonly int RParen = 3;

        public static readonly int Comma = 4;

        public static readonly int Connector = 5;

        public static readonly int Quantifier = 6;

        public static readonly int Predicate = 7;

        public static readonly int Function = 8;

        public static readonly int Variable = 9;

        public static readonly int Constant = 10;

        public static readonly int True = 11;

        public static readonly int False = 12;

        public static readonly int LEquals = 13;

        public static readonly int Whitespace = 1000;

        public static readonly int EOI = 9999;
    }
}
