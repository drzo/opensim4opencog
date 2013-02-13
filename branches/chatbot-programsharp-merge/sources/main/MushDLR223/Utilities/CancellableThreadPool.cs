using System;
using System.Collections;
using System.Threading;
using TASK = System.Threading.ThreadStart;

namespace MushDLR223.Utilities
{
    /// <summary>
    /// A ThreadRunInfo contains info relating to each queued WorkItem. 
    /// </summary>
    /// <remarks>Has to be a class to allow null to be used in references.</remarks>
    public class ThreadRunInfo
    {
        public WaitCallback Callback;
        public DateTime StartTime;
        public object State;

        public ThreadRunInfo(WaitCallback wait, object state)
        {
            Callback = wait;
            State = state;
        }
    }

    /// <summary>
    /// A collection of threads that can be cancelled
    /// </summary>
    public class CancellableThreadPool
    {
        private ThreadStartEvaluator _startEvaluator = null;
        private bool _stopped = false;

        private Hashtable _threads = new Hashtable();
        private ArrayList _workItems = new ArrayList();

        public CancellableThreadPool(ThreadStartEvaluator startEvaluator)
        {
            _startEvaluator = startEvaluator;
        }


        public void QueueWorkItem(WaitCallback callback, object state)
        {
            lock (_workItems)
            {
                //Make sure I don't duplicate this
                //(If its running it qont be in the queue so its not a problem)
                if (_workItems.Contains(callback) == false)
                {
                    //Add the work item into the queue		
                    _workItems.Add(new ThreadRunInfo(callback, state));
                }
            }
            //We never abort from here
            EvaluateThreadMaybeAbort(EvaluationReason.NewWorkItemQueued);
        }

        private void AddNewThread()
        {
            if (_stopped == false)
            {
                var newThread = new Thread(
                    new ThreadStart(ThreadRunner));
                //Add it to the collection
                lock (_threads)
                {
                    _threads.Add(newThread, null);
                }
                newThread.Start();
            }
        }

        private void ThreadRunner()
        {
            ThreadRunInfo runInfo = null;
            bool threadStop = false;

            try
            {
                do
                {
                    //Get a callback to run
                    try
                    {
                        lock (_workItems)
                        {
                            if (_workItems.Count > 0)
                            {
                                runInfo = (ThreadRunInfo) _workItems[0];
                                _workItems.Remove(runInfo);
                            }
                            else
                                runInfo = null;
                        }

                        //Now try to run
                        if (runInfo != null)
                        {
                            lock (_threads)
                            {
                                _threads[Thread.CurrentThread] = runInfo;
                            }

                            //Record when we start
                            runInfo.StartTime = DateTime.Now;

                            //This one line does all the work
                            runInfo.Callback(runInfo.State);
                            //back again
                            //Remove the job from the queue
                            runInfo = null;
                            lock (_threads)
                            {
                                _threads[Thread.CurrentThread] = null;
                            }
                        }
                    }
                    catch (ThreadAbortException)
                    {
                        lock (_workItems)
                        {
                            //requeue the job if it isnt in progress
                            if (runInfo != null)
                                _workItems.Add(runInfo);
                        }
                    }
                    if (_stopped == false)
                        threadStop = EvaluateThreadMaybeAbort(EvaluationReason.WorkItemCompleted);
                } while (_workItems.Count > 0 && _stopped == false && threadStop == false);
            }
            catch (ThreadAbortException)
            {
                lock (_workItems)
                {
                    if (runInfo != null)
                        _workItems.Add(runInfo);
                }
            }
            finally
            {
                //Finally remove this thread from the collection
                lock (_threads)
                {
                    _threads.Remove(Thread.CurrentThread);
                }
            }
        }

        public void Stop() //bool requeue)
        {
            _stopped = true;
            //Cant lock both _threads and _workItems
            //That might cause a deadlock
            lock (_threads)
                foreach (Thread thread in _threads.Keys)
                {
                    thread.Abort();
                }
        }

        public void Start()
        {
            if (_stopped == true)
            {
                _stopped = false;
                //This is called externally. We ignore attempts to abort.
                EvaluateThreadMaybeAbort(EvaluationReason.QueueRestart);
            }
        }

        private bool EvaluateThreadMaybeAbort(EvaluationReason reason)
        {
            int workItemsCount;
            lock (_workItems)
            {
                workItemsCount = _workItems.Count;
            }
            lock (_threads)
            {
                foreach (Thread thread in _threads)
                {
                    if (thread.ThreadState != ThreadState.Running)
                    {
                        _startEvaluator.CurrentlyQueuedWorkItems = workItemsCount;
                        _startEvaluator.CurrentlyRunningThreadCount = _threads.Count;
                        switch (_startEvaluator.EvaluateThreadStartStop(reason))
                        {
                            case EvaluationResult.FinishCurrentThreadWhenWorkItemCompleted:
                                return true;

                            case EvaluationResult.StartNewThread:
                                AddNewThread();
                                break;
                        }
                    }
                }
            }
            return false;
        }
    }

    /// <summary>
    /// Summary description for ThreadStartEvaluator.
    /// </summary>
    public abstract class ThreadStartEvaluator
    {
        /// <summary>
        /// This is the number of WorkItems that are currently waiting to be processed. Changing this value has no effect.
        /// </summary>
        public int CurrentlyQueuedWorkItems;

        /// <summary>
        /// This is the time number of currently running threads. Changing this value has no effect.
        /// </summary>
        public int CurrentlyRunningThreadCount;


        /// <summary>
        /// Override this method to determine whether you wish to start a new thread, stop the current thread, or just leave
        /// the current number as is. Refer to the <see cref="EvaluationReason"/> parameter and the 
        /// <see cref="CurrentlyRunningThreadCount"/> and <see cref="CurrentlyQueuedWorkItems"/> properties to 
        /// work out what you wish to do.
        /// </summary>
        /// <param name="reason"></param>
        /// <returns></returns>
        public abstract EvaluationResult EvaluateThreadStartStop(EvaluationReason reason);
    }

    /// <summary>
    /// The reason that the <see cref="EvaluateThreads"/> method was called.
    /// </summary>
    public enum EvaluationReason
    {
        /// <summary>
        /// A new WorkItem has been added to the ThreadPool
        /// </summary>
        NewWorkItemQueued,

        /// <summary>
        /// A WorkItem has been completed by the thread pool
        /// </summary>
        WorkItemCompleted,

        /// <summary>
        /// The ThreadPool has been Started after being Stopped
        /// </summary>
        QueueRestart
    } ;

    /// <summary>
    /// This is the result that should be returned from the <see cref="EvaluateThreads"/> method.
    /// </summary>
    public enum EvaluationResult
    {
        /// <summary>
        /// Indicates that a new Thread should be added to the thread pool
        /// </summary>
        StartNewThread,
        /// <summary>
        /// Indicates that the current numbers of threads running is sufficient.
        /// </summary>
        NoOperation,
        /// <summary>
        /// Indicates that too many threads are running so we can drop the current one IF AND ONLY IF we are in 
        /// <see cref="EvaluationReason"/> of <see cref="EvaluationReason.WorkItemCompleted"/>
        /// </summary>
        FinishCurrentThreadWhenWorkItemCompleted
    } ;

    /// <summary>
    /// Summary description for ThreadStartEvaluatorByQueueSize.
    /// </summary>
    public class ThreadStartEvaluatorByQueueSize : ThreadStartEvaluator
    {
        private int _MaxDesiredQueued = 2;

        public ThreadStartEvaluatorByQueueSize(int MaxDesiredQueued)
        {
            _MaxDesiredQueued = MaxDesiredQueued;
        }

        public int MaxDesiredQueued
        {
            get { return _MaxDesiredQueued; }
        }

        public override EvaluationResult EvaluateThreadStartStop(EvaluationReason reason)
        {
            //Run another thread if we have a big queue
            if (CurrentlyRunningThreadCount == 0
                || CurrentlyRunningThreadCount < CurrentlyQueuedWorkItems/MaxDesiredQueued)
            {
                return EvaluationResult.StartNewThread;
            }
            if (CurrentlyRunningThreadCount > 1
                && CurrentlyRunningThreadCount > CurrentlyQueuedWorkItems/MaxDesiredQueued)
                return EvaluationResult.FinishCurrentThreadWhenWorkItemCompleted;

            return EvaluationResult.NoOperation;
        }
    }
}