using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Util.Datastructure
{
    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): pg 79.<br />
    /// The operations on a queue are as follows:<br />
    /// <ul>
    /// <li>EMPTY?(queue) returns true only if there are no elements in the queue</li>
    /// <li>POP(queue) removes teh first element of the queue and returns it.</li>
    /// <li>INSERT(element, queue) inserts and element and returns the resulting queue.</li>
    /// </ul>
    /// Note: This extends the java.util.Queue collections interface in order to take advantage
    /// of pre-existing implementations. The intent of this interface is purely to provide an interface
    /// to Queues that corresponds to what is described in AIMA3e.
    /// </summary>
    /// <typeparam name="TItem"></typeparam>
    public interface IQueue<TItem>
    {
        int Size();

        /// <summary>
        /// 
        /// </summary>
        /// <returns>true only if there are no elements on the queue.</returns>
        bool IsEmpty();

        /// <summary>
        /// POP(queue)
        /// </summary>
        /// <returns>the first element of the queue.</returns>
        TItem Pop();

        /// <summary>
        /// INSERT(element, queue)
        /// 
        /// @param element
        ///           
        /// @return 
        /// </summary>
        /// <param name="element">to be inserted in the queue.</param>
        /// <returns>the resulting queue with the element inserted. null is returned
        /// if the element could not be inserted.</returns>
        IQueue<TItem> Insert(TItem element);

        void Push(TItem element);

        bool Remove(TItem element);
    }
}
