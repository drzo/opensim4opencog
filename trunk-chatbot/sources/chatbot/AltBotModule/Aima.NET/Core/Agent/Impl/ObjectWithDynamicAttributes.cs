using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Agent.Impl
{
    using System.Collections.ObjectModel;

    public abstract class ObjectWithDynamicAttributes : IEquatable<ObjectWithDynamicAttributes>
    {
        //TODO: can we do something about object references?
        private  Dictionary<object, object> attributes = new Dictionary<object, object>();

        public virtual string DescribeType() {
            return this.GetType().Name;
        }

        public String DescribeAttributes() {
            var sb = new StringBuilder();

            sb.Append("[");
            var first = true;
            foreach (var key in attributes.Keys) 
            {
                if (first) {
                    first = false;
                } else {
                    sb.Append(", ");
                }

                sb.Append(key);
                sb.Append("==");
                sb.Append(attributes[key]);
            }
            sb.Append("]");

            return sb.ToString();
        }
    
        public IEnumerable<object> GetKeySet() {
            return new ReadOnlyCollection<object>(attributes.Keys.ToList());
        }

        public void SetAttribute(object key, object value)
        {
            attributes[key] = value;
        }

        public object GetAttribute(object key)
        {
            return attributes[key];
        }

        public void RemoveAttribute(object key)
        {
            attributes.Remove(key);
        }

        public ObjectWithDynamicAttributes Copy() 
        {
            ObjectWithDynamicAttributes copy = null;

                //TODO:can we get rid of this reflection call? Or is it ok performance wise?
                copy = (ObjectWithDynamicAttributes) Activator.CreateInstance(this.GetType());
                copy.attributes = new Dictionary<object, object>(this.attributes);
            return copy;
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
            if (o.GetType() != typeof(ObjectWithDynamicAttributes))
            {
                return false;
            }
            return Equals((ObjectWithDynamicAttributes)o);
        }

        public override String ToString() {
            var sb = new StringBuilder();

            sb.Append(DescribeType());
            sb.Append(DescribeAttributes());

            return sb.ToString();
        }

        public bool Equals(ObjectWithDynamicAttributes other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.attributes, this.attributes);
        }

        public override int GetHashCode()
        {
            return (this.attributes != null ? this.attributes.GetHashCode() : 0);
        }

        public static bool operator ==(ObjectWithDynamicAttributes left, ObjectWithDynamicAttributes right)
        {
            return Equals(left, right);
        }

        public static bool operator !=(ObjectWithDynamicAttributes left, ObjectWithDynamicAttributes right)
        {
            return !Equals(left, right);
        }

    }
}
