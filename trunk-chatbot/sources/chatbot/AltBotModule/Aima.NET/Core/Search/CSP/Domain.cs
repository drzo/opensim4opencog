using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.CSP
{
    using System.Collections;

    /// <summary>
    /// A domain Di consists of a set of allowable values {v1, ... , vk} for the
    /// corresponding variable Xi and defines a default order on those values. This
    /// implementation guarantees, that domains are never changed after they have
    /// been created. Domain reduction is implemented by replacement instead of
    /// modification. So previous states can easily and safely be restored.
    /// </summary>
    public class Domain : IEnumerable<object> 
    {
        private object[] values;

        public Domain(IList<object> values) 
        {
            this.values = new object[values.Count];
            for (int i = 0; i < values.Count; i++)
                this.values[i] = values[i];
        }

        public Domain(object[] values) {
            this.values = new object[values.Length];
            for (int i = 0; i < values.Length; i++)
                this.values[i] = values[i];
        }

        public int Size() {
            return values.Length;
        }

        public object Get(int index) {
            return values[index];
        }

        public bool IsEmpty() {
            return values.Length == 0;
        }

        public bool Contains(object value)
        {
            return this.values.Contains(value);
        }

        public IEnumerator<object> GetEnumerator()
        {
            return values.AsEnumerable().GetEnumerator();
        }
        
        public override bool Equals(object obj) 
        {
            if (obj is Domain)
            {
                return Equals((Domain)obj);
            }
            return false;
        }

        public override string ToString() 
        {
            var result = new StringBuilder("{");
            bool comma = false;
            foreach (object value in values) 
            {
                if (comma)
                    result.Append(", ");
                result.Append(value.ToString());
                comma = true;
            }
            result.Append("}");
            return result.ToString();
        }

        public bool Equals(Domain other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.values, this.values);
        }

        public override int GetHashCode()
        {
            return (this.values != null ? this.values.GetHashCode() : 0);
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }
    }
}
