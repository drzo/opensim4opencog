using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Parsing.AST
{
    using Aima.Core.Logic.Common;

    public interface IFOLNode : IParseTreeNode 
    {
        string GetSymbolicName();

        bool IsCompound();

        //TODO: verify that "? extends IFOLNode" really means just interface name in C#
        IList<IFOLNode> GetArgs();

        object Accept(IFOLVisitor v, object arg);

        IFOLNode Copy();
    }
}
