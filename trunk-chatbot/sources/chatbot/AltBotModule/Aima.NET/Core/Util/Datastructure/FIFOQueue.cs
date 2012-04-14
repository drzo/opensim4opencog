using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Util.Datastructure
{
    using System.Collections.ObjectModel;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): pg 80.<br/>
    /// First-in, first-out or FIFO queue, which pops the oldest element of the queue;
    /// </summary>
    /// <typeparam name="TItem"></typeparam>
    public class FIFOQueue<TItem> : List<TItem>, IQueue<TItem> 
        where TItem : class
    {
        public FIFOQueue()
        {
        }

        public FIFOQueue(ICollection<TItem> c):base(c)
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

        public virtual TItem Pop()
        {
            if (this.Count == 0)
            {
                return null;
            }
            var val = this[0];
            this.RemoveAt(0);
            return val;
        }

        public void Push(TItem element) 
        {
            this.Add(element);
        }

        bool IQueue<TItem>.Remove(TItem element)
        {
            return this.Remove(element);
        }

        public IQueue<TItem> Insert(TItem element) 
        {
            //TODO: figure out if there needs to be handling of any kind of an issue during Add
            this.Add(element);
            return this;
        }
    }
}
