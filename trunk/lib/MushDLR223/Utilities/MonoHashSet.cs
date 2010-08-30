using System;
using System.Collections;
using System.Collections.Generic;

namespace MushDLR223.Utilities
{
    public class ListAsSet<T> : IList<T>, IList
    {
        public event Action<T> OnAdd;
        public event Action<T> OnRemove;
        public event Action OnModified;
        private readonly IList realList = new List<T>();
        public List<T> RealListT
        {
            get { return (List<T>)realList; }
        }
        /// <summary>
        /// Adds an item to the <see cref="T:System.Collections.IList"/>.
        /// </summary>
        /// <returns>
        /// The position into which the new element was inserted.
        /// </returns>
        /// <param name="value">The <see cref="T:System.Object"/> to add to the <see cref="T:System.Collections.IList"/>. 
        ///                 </param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.IList"/> is read-only.
        ///                     -or- 
        ///                     The <see cref="T:System.Collections.IList"/> has a fixed size. 
        ///                 </exception><filterpriority>2</filterpriority>
        public int Add(object value)
        {
            lock (this)
            {
                int indexOf = realList.IndexOf(value);
                if (indexOf >= 0) return indexOf;
                return realList.Add(value);
            }
        }

        /// <summary>
        /// Determines whether the <see cref="T:System.Collections.IList"/> contains a specific value.
        /// </summary>
        /// <returns>
        /// true if the <see cref="T:System.Object"/> is found in the <see cref="T:System.Collections.IList"/>; otherwise, false.
        /// </returns>
        /// <param name="value">The <see cref="T:System.Object"/> to locate in the <see cref="T:System.Collections.IList"/>. 
        ///                 </param><filterpriority>2</filterpriority>
        public bool Contains(object value)
        {
            lock (this)
            {
                return realList.Contains(value);
            }
        }

        /// <summary>
        /// Removes all items from the <see cref="T:System.Collections.IList"/>.
        /// </summary>
        /// <exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.IList"/> is read-only. 
        ///                 </exception><filterpriority>2</filterpriority>
        public void Clear()
        {
            lock (this)
            {
                realList.Clear();
            }
        }

        /// <summary>
        /// Determines the index of a specific item in the <see cref="T:System.Collections.IList"/>.
        /// </summary>
        /// <returns>
        /// The index of <paramref name="value"/> if found in the list; otherwise, -1.
        /// </returns>
        /// <param name="value">The <see cref="T:System.Object"/> to locate in the <see cref="T:System.Collections.IList"/>. 
        ///                 </param><filterpriority>2</filterpriority>
        public int IndexOf(object value)
        {
            lock (this) return realList.IndexOf(value);
        }

        /// <summary>
        /// Inserts an item to the <see cref="T:System.Collections.IList"/> at the specified index.
        /// </summary>
        /// <param name="index">The zero-based index at which <paramref name="value"/> should be inserted. 
        ///                 </param><param name="value">The <see cref="T:System.Object"/> to insert into the <see cref="T:System.Collections.IList"/>. 
        ///                 </param><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is not a valid index in the <see cref="T:System.Collections.IList"/>. 
        ///                 </exception><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.IList"/> is read-only.
        ///                     -or- 
        ///                     The <see cref="T:System.Collections.IList"/> has a fixed size. 
        ///                 </exception><exception cref="T:System.NullReferenceException"><paramref name="value"/> is null reference in the <see cref="T:System.Collections.IList"/>.
        ///                 </exception><filterpriority>2</filterpriority>
        public void Insert(int index, object value)
        {
            lock (this) realList.Insert(index, value);
        }

        /// <summary>
        /// Removes the first occurrence of a specific object from the <see cref="T:System.Collections.IList"/>.
        /// </summary>
        /// <param name="value">The <see cref="T:System.Object"/> to remove from the <see cref="T:System.Collections.IList"/>. 
        ///                 </param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.IList"/> is read-only.
        ///                     -or- 
        ///                     The <see cref="T:System.Collections.IList"/> has a fixed size. 
        ///                 </exception><filterpriority>2</filterpriority>
        public void Remove(object value)
        {
            RemoveImpl(value);
        }

        public void RemoveImpl(object value)
        {
            lock (this) realList.Remove(value);
        }
        /// <summary>
        /// Removes the <see cref="T:System.Collections.IList"/> item at the specified index.
        /// </summary>
        /// <param name="index">The zero-based index of the item to remove. 
        ///                 </param><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is not a valid index in the <see cref="T:System.Collections.IList"/>. 
        ///                 </exception><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.IList"/> is read-only.
        ///                     -or- 
        ///                     The <see cref="T:System.Collections.IList"/> has a fixed size. 
        ///                 </exception><filterpriority>2</filterpriority>
        public void RemoveAt(int index)
        {
            lock (this) realList.RemoveAt(index);
        }

        /// <summary>
        /// Gets or sets the element at the specified index.
        /// </summary>
        /// <returns>
        /// The element at the specified index.
        /// </returns>
        /// <param name="index">The zero-based index of the element to get or set. 
        ///                 </param><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is not a valid index in the <see cref="T:System.Collections.IList"/>. 
        ///                 </exception><exception cref="T:System.NotSupportedException">The property is set and the <see cref="T:System.Collections.IList"/> is read-only. 
        ///                 </exception><filterpriority>2</filterpriority>
        public object this[int index]
        {
            get { lock (this) return realList[index]; }
            set { lock (this) realList[index] = value; }
        }

        /// <summary>
        /// Gets a value indicating whether the <see cref="T:System.Collections.IList"/> is read-only.
        /// </summary>
        /// <returns>
        /// true if the <see cref="T:System.Collections.IList"/> is read-only; otherwise, false.
        /// </returns>
        /// <filterpriority>2</filterpriority>
        public bool IsReadOnly
        {
            get { return false; }
        }

        /// <summary>
        /// Gets a value indicating whether the <see cref="T:System.Collections.IList"/> has a fixed size.
        /// </summary>
        /// <returns>
        /// true if the <see cref="T:System.Collections.IList"/> has a fixed size; otherwise, false.
        /// </returns>
        /// <filterpriority>2</filterpriority>
        public bool IsFixedSize
        {
            get { return false; }
        }

        void ICollection<T>.Clear()
        {
            ClearImpl();
        }
        void ClearImpl()
        {
            lock (this)
            {
                if (Count == 0) return;
                foreach (var set in realList)
                {
                    Remove(set);
                }
                //  realList.Clear();
            }
            if (OnModified != null) OnModified();
        }

        protected int CountImpl
        {
            get { lock (this) lock (realList) return realList.Count; }
        }
        /// <summary>
        /// Copies the elements of the <see cref="T:System.Collections.ICollection"/> to an <see cref="T:System.Array"/>, starting at a particular <see cref="T:System.Array"/> index.
        /// </summary>
        /// <param name="array">The one-dimensional <see cref="T:System.Array"/> that is the destination of the elements copied from <see cref="T:System.Collections.ICollection"/>. The <see cref="T:System.Array"/> must have zero-based indexing. 
        ///                 </param><param name="index">The zero-based index in <paramref name="array"/> at which copying begins. 
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="array"/> is null. 
        ///                 </exception><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is less than zero. 
        ///                 </exception><exception cref="T:System.ArgumentException"><paramref name="array"/> is multidimensional.
        ///                     -or- 
        ///                 <paramref name="index"/> is equal to or greater than the length of <paramref name="array"/>.
        ///                     -or- 
        ///                     The number of elements in the source <see cref="T:System.Collections.ICollection"/> is greater than the available space from <paramref name="index"/> to the end of the destination <paramref name="array"/>. 
        ///                 </exception><exception cref="T:System.ArgumentException">The type of the source <see cref="T:System.Collections.ICollection"/> cannot be cast automatically to the type of the destination <paramref name="array"/>. 
        ///                 </exception><filterpriority>2</filterpriority>
        public void CopyTo(Array array, int index)
        {
            lock (this)
            {
                realList.CopyTo(array, index);
            }
        }

        public int Count
        {
            get { return CountImpl; }
        }

        /// <summary>
        /// Gets an object that can be used to synchronize access to the <see cref="T:System.Collections.ICollection"/>.
        /// </summary>
        /// <returns>
        /// An object that can be used to synchronize access to the <see cref="T:System.Collections.ICollection"/>.
        /// </returns>
        /// <filterpriority>2</filterpriority>
        public object SyncRoot
        {
            get { return this; }
        }

        /// <summary>
        /// Gets a value indicating whether access to the <see cref="T:System.Collections.ICollection"/> is synchronized (thread safe).
        /// </summary>
        /// <returns>
        /// true if access to the <see cref="T:System.Collections.ICollection"/> is synchronized (thread safe); otherwise, false.
        /// </returns>
        /// <filterpriority>2</filterpriority>
        public bool IsSynchronized
        {
            get { return false; }
        }

        void IList<T>.RemoveAt(int index)
        {
            lock (this)
            {
                T t = ((IList<T>)this)[index];
                if (OnRemove != null) OnRemove(t);
                realList.RemoveAt(index);
                if (OnModified != null) OnModified();
            }
        }

        public void RemoveRange(int index, int count)
        {
            int rcount = count;
            int rindex = index;
            while (rcount-- > 0)
            {
                lock (this)
                {
                    T t = ((IList<T>)this)[rindex++];
                    if (OnRemove != null) OnRemove(t);
                }
            }
            RealListT.RemoveRange(index, count);
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
            int cc = 0;
            foreach (var set in CopyOf())
            {
                if (match(set))
                {
                    c++;
                    if (((ICollection<T>)this).Remove(set)) cc++;
                }
            }
            return c;
        }

        // synchronization
        public bool /*ICollection<T>.*/Remove(T item)
        {
            lock (this)
            {
                if (OnRemove != null) OnRemove(item);
                return RealListT.Remove(item);
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
                        IEnumerator enumer = realList.GetEnumerator();
                        while (enumer.MoveNext())
                        {
                            if (item.Equals((T)enumer.Current)) return false;
                        }
                    }
                }
                else
                {
                    if (realList.Contains(item)) return false;
                }
                {
                    realList.Add(item);
                }
                return true;
            }
        }

        // return a copy
        public IEnumerator<T> GetEnumerator()
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
            return new BaseEnumerable(RealListT.GetEnumerator());
        }
        // synchronization
        public List<T> CopyOf()
        {
            List<T> list = new List<T>();
            lock (this)
            {
                IEnumerator enumer = realList.GetEnumerator();
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
            lock (this) return realList.IndexOf(item);
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
            lock (this) realList.Insert(index, item);
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
                lock (this) return RealListT[index];
            }
            set
            {
                lock (this) RealListT[index] = value;
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
        public bool Contains(T item)
        {
            lock (this) return realList.Contains(item);
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
            lock (this) realList.CopyTo(array, arrayIndex);
        }

        /// <summary>
        /// Gets the number of elements contained in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <returns>
        /// The number of elements contained in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </returns>
        int ICollection<T>.Count
        {
            get { return CountImpl; }
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

        private IEnumerator<T> CopiedEnumerator()
        {
            return CopyOf().GetEnumerator();
        }

        #region IEnumerable<T> Members

        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            return CopiedEnumerator();
        }

        #endregion

        #region IEnumerable Members

        IEnumerator IEnumerable.GetEnumerator()
        {
            return CopiedEnumerator();
        }

        #endregion

        public void Sort(IComparer<T> comparer)
        {
            lock (this) RealListT.Sort(comparer);
        }
        public void Sort(Comparison<T> comparer)
        {
            lock (this) RealListT.Sort(comparer);
        }

        public static void AddIfMissing<TT>(ICollection<TT> col, TT val)
        {
            if (!col.Contains(val))
                col.Add(val);
        }
    }
}