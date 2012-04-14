using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Parsing.AST
{
    using System.Collections.ObjectModel;

    public class ConnectedSentence : ISentence 
    {
        public string Connector { get; private set; }

        public ISentence First { get; private set; }

        public ISentence Second { get; private set; }

        private IList<ISentence> args = new List<ISentence>();

        public ConnectedSentence(String connector, ISentence first, ISentence second) 
        {
            this.Connector = connector;
            this.First = first;
            this.Second = second;
            this.args.Add(first);
            this.args.Add(second);
        }

        public string GetSymbolicName() 
        {
            return this.Connector;
        }

        public bool IsCompound() 
        {
            return true;
        }

        public IList<IFOLNode> GetArgs()
        {
            return new ReadOnlyCollection<IFOLNode>(args.Cast<IFOLNode>().ToList());
        }

        public object Accept(IFOLVisitor v, object arg) 
        {
            return v.VisitConnectedSentence(this, arg);
        }

        public IFOLNode Copy() {
            return new ConnectedSentence(this.Connector,(ISentence) this.First.Copy(), (ISentence) this.Second.Copy());
        }

        public override bool Equals(object o) 
        {
            if (ReferenceEquals(null, o))
            {
                return false;
            }
            if (ReferenceEquals(this, o))
            {
                return true;
            }
            if (o.GetType() != typeof(ConnectedSentence))
            {
                return false;
            }
            return Equals((ConnectedSentence)o);
        }

        public override string ToString() 
        {
            var sb = new StringBuilder();
            sb.Append("(");
            sb.Append(this.First);
            sb.Append(" ");
            sb.Append(this.Connector);
            sb.Append(" ");
            sb.Append(this.Second);
            sb.Append(")");

            return sb.ToString();
        }

        public bool Equals(ConnectedSentence other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.Connector, this.Connector) && Equals(other.First, this.First) && Equals(other.Second, this.Second);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                int result = (this.Connector != null ? this.Connector.GetHashCode() : 0);
                result = (result * 397) ^ (this.First != null ? this.First.GetHashCode() : 0);
                result = (result * 397) ^ (this.Second != null ? this.Second.GetHashCode() : 0);
                return result;
            }
        }
    }

}
