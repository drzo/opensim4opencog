using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading;
using System.Windows.Forms;
using TASK = System.Threading.ThreadStart;

namespace MushDLR223.Utilities
{    
    public class SafeThread
    {

        /// <summary>
        /// ?- cliGet('MushDLR223.Utilities.SafeThread','AllThreads',List),cliGet(List,'Count',C).
        /// </summary>
        public static ListAsSet<Thread> AllThreads = new ListAsSet<Thread>();
        public static Dictionary<Thread, SafeThread> AllThreads2Safe = new Dictionary<Thread, SafeThread>();
        static Thread reaper = new Thread(ReapDeadThreads);
        public static void RegisterCurrentThread()
        {
            Thread thread = Thread.CurrentThread;
            RegisterThread(thread);
        }
        public static void DeregisterCurrentThread()
        {
            DeregisterThread(Thread.CurrentThread);
        }

        static SafeThread()
        {
            SafeThread.ThreadAdded += dummy;
            SafeThread.ThreadRemoved += dummy;
            Application.ThreadException += OnThreadException;
            Application.ThreadExit += OnThreadExit;

        }

        private static void dummy(Thread obj)
        {          
        }
        public static event Action<Thread> ThreadAdded;
        public static event Action<Thread> ThreadRemoved;
        static public void RegisterThread(Thread thread)
        {
            if (!AllThreads.Contains(thread))
            {
                AllThreads.Add(thread);
            }  else
            {
                
            }        
            if (ThreadAdded != null) ThreadAdded(thread);
            AllThreads2Safe.Remove(thread);
        }
        static public void DeregisterThread(Thread thread)
        {
            AllThreads.Remove(thread);
            if (ThreadRemoved != null) ThreadRemoved(thread);
        }
        private static void OnThreadException(object sender, ThreadExceptionEventArgs e)
        {
           //is it on this thread? DeregisterCurrentThread();           
        }

        private static void OnThreadExit(object sender, EventArgs e)
        {
            DeregisterCurrentThread();
        }
        public static implicit operator Thread(SafeThread st)
        {
            return st.AsThread;
        }
        public static implicit operator SafeThread(Thread st)
        {
            lock (AllThreads2Safe)
            {
                SafeThread safe;
                if (AllThreads2Safe.TryGetValue(st, out safe))
                {
                    return safe;
                }
                return new SafeThread(st);
            }
        }
        private static void ReapDeadThreads()
        {
            while (true)
            {
                Thread.Sleep(3000);
                foreach (Thread thread in AllThreads)
                {
                    ThreadState state = thread.ThreadState;
                    if (state == ThreadState.Unstarted) continue;
                    if (state == ThreadState.Suspended) continue;
                    if (state == ThreadState.WaitSleepJoin) continue;
                    if (state == ThreadState.Running) continue;
                    DeregisterThread(thread);
                }                
            }
        }


        public readonly Thread theThread;
        public SafeThread(ThreadStart start)
        {
            theThread = new Thread(() =>
                                       {
                                           RegisterThread(theThread);
                                           start();
                                           DeregisterThread(Thread.CurrentThread);
                                       });
            lock (AllThreads2Safe) AllThreads2Safe[theThread] = this;
        }

        private SafeThread(Thread start)
        {
            theThread = start;
            RegisterThread(theThread);
            AllThreads2Safe[theThread] = this;
        }

        public static SafeThread CurrentThread
        {
            get { return new SafeThread(Thread.CurrentThread); }
        }

        public string Name
        {
            get { return theThread.Name; }
            set
            {
                if (!value.StartsWith("Bless")) value = "Blessed Be" + value;
                theThread.Name = value;
            }
        }

        public bool IsAlive
        {
            get { return theThread.IsAlive; }
        }
        
        public bool IsBackground
        {
            get { return theThread.IsBackground; }
            set { theThread.IsBackground = value; }
        }

        public ApartmentState ApartmentState
        {
            get { return theThread.ApartmentState; }
            set { theThread.ApartmentState = value; }
        }

        public Thread AsThread
        {
            get { return theThread; }
        }

        public ThreadPriority Priority
        {
            get { return theThread.Priority; }
            set { theThread.Priority = value; }
        }

        public ThreadState ThreadState
        {
            get { return theThread.ThreadState; }
        }

        public override bool Equals(object obj)
        {
            SafeThread o = obj as SafeThread;
            return o != null && o.theThread == theThread;
        }
        public static bool operator ==(SafeThread obj1, SafeThread obj2)
        {
            return EqualsTwo(obj1,obj2);
        }

        private static bool EqualsTwo(SafeThread one, SafeThread other)
        {
            if (ReferenceEquals(one, other)) return true;
            if (ReferenceEquals(null, other)) return false;
            if (ReferenceEquals(null, one)) return false;
            return Equals(other.theThread, one.theThread);
        }

        public static bool operator !=(SafeThread obj1, SafeThread obj2)
        {
            return !EqualsTwo(obj1, obj2);
        }

        public void SetApartmentState(ApartmentState state)
        {
            theThread.SetApartmentState(state);
        }

        public void Start()
        {
            if (string.IsNullOrEmpty(theThread.Name) || !theThread.Name.StartsWith("Bless"))
            {
                theThread.Name = "Blessed Be " + theThread.Name;
            }
            theThread.Start();
        }

        public static void Sleep(int i)
        {
            Thread.Sleep(i);
        }
        public static void Sleep(TimeSpan i)
        {
            Thread.Sleep(i);
        }

        public void Abort()
        {
            theThread.Abort();
        }

        public void Join()
        {
            theThread.Join();
        }
        public bool Join(TimeSpan i)
        {
            return theThread.Join(i);
        }

        public static void ResetAbort()
        {
            Thread.ResetAbort();
        }

        public void Resume()
        {
            theThread.Resume();
        }

        public override string ToString()
        {
            return theThread.ToString();
        }

        public bool Equals(SafeThread other)
        {
            if (ReferenceEquals(null, other)) return false;
            if (ReferenceEquals(this, other)) return true;
            return Equals(other.theThread, theThread);
        }

        public override int GetHashCode()
        {
            return (theThread != null ? theThread.GetHashCode() : 0);
        }

        public void Interrupt()
        {
            theThread.Interrupt();
        }

        public void TrySetApartmentState(ApartmentState state)
        {
            theThread.TrySetApartmentState(state);
        }

    }

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