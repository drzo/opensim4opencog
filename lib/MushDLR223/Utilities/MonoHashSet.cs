using System;
using System.Collections;
using System.Collections.Generic;

namespace MushDLR223.Utilities
{
    public class ListAsSet<T> : List<T>
    {
        public event Action<T> OnAdd;
        public event Action<T> OnRemove;
        public event Action OnModified;

        public void Clear()
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

        public void RemoveAt(int index)
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
        public bool Remove(T item)
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

        public new void Add(T item)
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
    }
}