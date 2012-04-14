using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Util.Datastructure
{
    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): pg 80.<br>
    ///
    /// Last-in, first-out or LIFO queue (also known as a stack), which pops the newest element of the queue;

    /// </summary>
    /// <typeparam name="TItem"></typeparam>
    public class LIFOQueue<TItem> : List<TItem>, IQueue<TItem> 
        where TItem : class
    {
        public LIFOQueue()
        {
        }

        public LIFOQueue(ICollection<TItem> c) : base(c)
        {
        }

        public int Size()
        {
            return this.Count;
        }

        public bool IsEmpty() {
            return Count == 0;
        }

        public TItem Pop() {
            if (this.Count == 0)
            {
                return null;
            }
            var val = this[0];
            this.RemoveAt(0);
            return val;
        }

        public void Push(TItem element) {
            this.Insert(0, element);
        }

        public IQueue<TItem> Insert(TItem element) 
        {
            //TODO: figure out if there needs to be handling of any kind of an issue during Add
            this.Add(element);
            return this;
        }

        public new void Add(TItem item)
        {
            this.Insert(0, item);
        }

        public new void AddRange(IEnumerable<TItem> c) 
        {
            foreach (var item in c)
            {
                this.Add(item);
            }
        }
    }
}
