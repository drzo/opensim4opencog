using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Util
{
    using System.Threading;

    /// <summary>
    /// Implements a thread with an additional flag indicating cancellation.
    /// </summary>
    public class CancelableThread //: Thread 
    {
        //TODO: implement proper cancellation of thread
        public static bool CurrIsCanceled() 
        {
            //if (Thread.CurrentThread is CancelableThread)
            //    return ((CancelableThread) Thread.currentThread()).isCanceled;
            return false;
        }

        public bool IsCanceled { get; private set; }

        public void Cancel()
        {
            this.IsCanceled = true;
        }
    }
}
