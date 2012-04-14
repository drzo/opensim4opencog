using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Util.Datastructure
{
    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): pg 80.<br>
    /// 
    /// The priority queue, which pops the element of the queue with the highest
    /// priority according to some ordering function.
    /// </summary>
    /// <typeparam name="TItem"></typeparam>
    public class PriorityQueue<TItem> : List<TItem>, IQueue<TItem>
        where TItem : class
    {
        private IComparer<TItem> comparer;

        public PriorityQueue()
        {
        }

        public PriorityQueue(ICollection<TItem> c) :base(c) 
        {
        }

        public PriorityQueue(int initialCapacity): base(initialCapacity) {
        }

        public PriorityQueue(int initialCapacity, IComparer<TItem> comparer) : base(initialCapacity)
        {
            this.comparer = comparer;
        }

        public PriorityQueue(SortedSet<TItem> c) : base(c)
        {
        }

        public int Size()
        {
            return this.Count;
        }

        public bool IsEmpty() 
        {
            return Count == 0;
        }

        public TItem Pop()
        {
            if (this.Count == 0)
            {
                return null;
            }
            var val = this[0];
            this.RemoveAt(0);
            return val;
        }

        public IQueue<TItem> Insert(TItem element) 
        {
            this.Add(element);
            this.Sort(comparer);
            return this;
        }

        public void Push(TItem element)
        {
            this.Add(element);
            this.Sort(comparer);
        }

        bool IQueue<TItem>.Remove(TItem element)
        {
            return this.Remove(element);
        }
    }
}
