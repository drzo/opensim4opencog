using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Parsing.AST
{
    using Aima.Core.Logic.Common;

    public abstract class Sentence : IParseTreeNode {

        public abstract object Accept(IPLVisitor plv, ISet<Sentence> arg);
    }
}
