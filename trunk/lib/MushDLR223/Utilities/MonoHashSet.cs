using System;
using System.Collections;
using System.Collections.Generic;

namespace MushDLR223.Utilities
{
    public class ListAsSet<T> : List<T>, IList<T>
    {
        public event Action<T> OnAdd;
        public event Action<T> OnRemove;
        public event Action OnModified;

        void ICollection<T>.Clear()
        {
            lock (this)
            {
                if (Count == 0) return;
                foreach (var set in this)
                {
                    Remove(set);
                }
                //  base.Clear();
            }
            if (OnModified != null) OnModified();
        }

        void IList<T>.RemoveAt(int index)
        {
            T t = this[index];
            if (OnRemove != null) OnRemove(t);
            base.RemoveAt(index);
            if (OnModified != null) OnModified();
        }

        public void RemoveRange(int index, int count)
        {
            int rcount = count;
            int rindex = index;
            while (rcount-->0)
            {
                T t = this[rindex++];
                if (OnRemove != null) OnRemove(t);
            }
            base.RemoveRange(index, count);
            if (OnModified != null) OnModified();
        }

        public void AddRange(System.Collections.Generic.IEnumerable<T> collection)
        {
            bool b = false;

            foreach (var e in collection)
            {
                if (AddToNoNotify(e))
                {
                    b = true;
                    if (OnAdd != null) OnAdd(e);
                }
            }   
            if (b)
            {
                if (OnModified != null) OnModified();
            }
        }

        //TODO 
        public int RemoveAll(Predicate<T> match)
        {
            int c = 0;
            foreach (var set in CopyOf())
            {
                if (match(set))
                {
                    c++;
                    Remove(set);
                }
            }
            return c;
        }

        // synchronization
        bool ICollection<T>.Remove(T item)
        {
            lock (this)
            {
                if (OnRemove != null) OnRemove(item);
                return base.Remove(item);
            }
        }

        // synchronization
        public void ForEach(Action<T> act)
        {
            foreach (T item in CopyOf())
            {
                act(item);
            }
        }

        // synchronization
        public T Find(Predicate<T> act)
        {
            foreach (T item in CopyOf())
            {
                if (act(item)) return item;
            }
            return default(T);
        }

        //public bool AddFirst(T item)
        //{
        //    lock (this)
        //    {
        //        bool found = Remove(item);
        //        Insert(0,item);
        //        return !found;
        //    }
        //}

        public bool AddTo(T item)
        {
            bool b = AddToNoNotify(item);
            if (b)
            {
                if (OnAdd != null) OnAdd(item);
                if (OnModified != null) OnModified();
            }
            return b;
        }

        public bool AddToNoNotify(T item)
        {
            lock (this)
            {
                if (false)
                {
                    {
                        IEnumerator enumer = base.GetEnumerator();
                        while (enumer.MoveNext())
                        {
                            if (item.Equals((T)enumer.Current)) return false;
                        }
                    }
                }
                else
                {
                    if (base.Contains(item)) return false;
                }
                {
                    base.Add(item);
                }
                return true;
            }
        }

        // return a copy
        public Enumerator GetEnumerator()
        {
            return CopyOf().GetEnumerator();
        }

        public class BaseEnumerable : IEnumerable<T>
        {
            readonly IEnumerator<T> be;
            public BaseEnumerable(IEnumerator<T> r)
            {
                be = r;
            }


            #region IEnumerable<T> Members

            IEnumerator<T> IEnumerable<T>.GetEnumerator()
            {
                return be;
            }

            #endregion

            #region IEnumerable Members

            IEnumerator IEnumerable.GetEnumerator()
            {
                return be;
            }

            #endregion
        }
        // return the fast underlying
        public IEnumerable<T> GetBaseEnumerable()
        {
            return new BaseEnumerable(base.GetEnumerator());
        }
        // synchronization
        public List<T> CopyOf()
        {
            List<T> list = new List<T>();
            lock (this)
            {
                IEnumerator enumer = base.GetEnumerator();
                while (enumer.MoveNext())
                {
                    list.Add((T)enumer.Current);
                }
            }
            return list;
        }

        void ICollection<T>.Add(T item)
        {
            AddTo(item);
        }
        public override string ToString()
        {
            List<T> copy = CopyOf();
            switch (copy.Count)
            {
                case 0: return "[]";
                //case 1: return "[" + copy[0] + "]";
                default:
                    {
                        String s = "";
                        foreach (T t in copy)
                        {
                            s += "," + t;
                        }
                        return "[" + s.Substring(1) + "]";
                    }
            }
        }

        #region IList<T> Members

        /// <summary>
        /// Determines the index of a specific item in the <see cref="T:System.Collections.Generic.IList`1"/>.
        /// </summary>
        /// <returns>
        /// The index of <paramref name="item"/> if found in the list; otherwise, -1.
        /// </returns>
        /// <param name="item">The object to locate in the <see cref="T:System.Collections.Generic.IList`1"/>.
        ///                 </param>
        int IList<T>.IndexOf(T item)
        {
            return base.IndexOf(item);
        }

        /// <summary>
        /// Inserts an item to the <see cref="T:System.Collections.Generic.IList`1"/> at the specified index.
        /// </summary>
        /// <param name="index">The zero-based index at which <paramref name="item"/> should be inserted.
        ///                 </param><param name="item">The object to insert into the <see cref="T:System.Collections.Generic.IList`1"/>.
        ///                 </param><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is not a valid index in the <see cref="T:System.Collections.Generic.IList`1"/>.
        ///                 </exception><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.IList`1"/> is read-only.
        ///                 </exception>
        void IList<T>.Insert(int index, T item)
        {
            base.Insert(index, item);
        }

        /// <summary>
        /// Gets or sets the element at the specified index.
        /// </summary>
        /// <returns>
        /// The element at the specified index.
        /// </returns>
        /// <param name="index">The zero-based index of the element to get or set.
        ///                 </param><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is not a valid index in the <see cref="T:System.Collections.Generic.IList`1"/>.
        ///                 </exception><exception cref="T:System.NotSupportedException">The property is set and the <see cref="T:System.Collections.Generic.IList`1"/> is read-only.
        ///                 </exception>
        T IList<T>.this[int index]
        {
            get
            {
                return base[index];
            }
            set
            {
                base[index] = value;
            }
        }

        #endregion

        #region ICollection<T> Members

        /// <summary>
        /// Determines whether the <see cref="T:System.Collections.Generic.ICollection`1"/> contains a specific value.
        /// </summary>
        /// <returns>
        /// true if <paramref name="item"/> is found in the <see cref="T:System.Collections.Generic.ICollection`1"/>; otherwise, false.
        /// </returns>
        /// <param name="item">The object to locate in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param>
        bool ICollection<T>.Contains(T item)
        {
            return base.Contains(item);
        }

        /// <summary>
        /// Copies the elements of the <see cref="T:System.Collections.Generic.ICollection`1"/> to an <see cref="T:System.Array"/>, starting at a particular <see cref="T:System.Array"/> index.
        /// </summary>
        /// <param name="array">The one-dimensional <see cref="T:System.Array"/> that is the destination of the elements copied from <see cref="T:System.Collections.Generic.ICollection`1"/>. The <see cref="T:System.Array"/> must have zero-based indexing.
        ///                 </param><param name="arrayIndex">The zero-based index in <paramref name="array"/> at which copying begins.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="array"/> is null.
        ///                 </exception><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="arrayIndex"/> is less than 0.
        ///                 </exception><exception cref="T:System.ArgumentException"><paramref name="array"/> is multidimensional.
        ///                     -or-
        ///                 <paramref name="arrayIndex"/> is equal to or greater than the length of <paramref name="array"/>.
        ///                     -or-
        ///                     The number of elements in the source <see cref="T:System.Collections.Generic.ICollection`1"/> is greater than the available space from <paramref name="arrayIndex"/> to the end of the destination <paramref name="array"/>.
        ///                     -or-
        ///                     Type <paramref name="T"/> cannot be cast automatically to the type of the destination <paramref name="array"/>.
        ///                 </exception>
        void ICollection<T>.CopyTo(T[] array, int arrayIndex)
        {
            base.CopyTo(array, arrayIndex);
        }

        /// <summary>
        /// Gets the number of elements contained in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <returns>
        /// The number of elements contained in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </returns>
        int ICollection<T>.Count
        {
            get { return base.Count;  }
        }

        /// <summary>
        /// Gets a value indicating whether the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        /// </summary>
        /// <returns>
        /// true if the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only; otherwise, false.
        /// </returns>
        bool ICollection<T>.IsReadOnly
        {
            get { return false; }
        }

        #endregion

        #region IEnumerable<T> Members

        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            return base.GetEnumerator();
        }

        #endregion

        #region IEnumerable Members

        IEnumerator IEnumerable.GetEnumerator()
        {
            return base.GetEnumerator();
        }

        #endregion
    }
}