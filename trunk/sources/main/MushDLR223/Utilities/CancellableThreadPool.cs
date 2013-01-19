using System;
using System.Collections;
using System.Collections.Generic;
using System.Windows.Forms;
using MushDLR223.Utilities;
using TASK = System.Threading.ThreadStart;
using SThread = System.Threading.Thread;
using SMonitor = System.Threading.Monitor;
using SThreadPool = System.Threading.ThreadPool;
using RegisteredWaitHandle = System.Threading.RegisteredWaitHandle;
using WaitCallback = System.Threading.WaitCallback;
using WaitHandle = System.Threading.WaitHandle;
using WaitOrTimerCallback = System.Threading.WaitOrTimerCallback;
using ThreadState = System.Threading.ThreadState;
namespace ThreadPoolUtil
{
    public class ThreadPool
    {
        private static IThreadPool _impl;
        private static IThreadPool _dotnet;

        public static IThreadPool Impl
        {
            get
            {
                if (_impl == null)
                {
                    return DotnetImpl;
                }
                return _impl;
            }
            set { _impl = value; }
        }

        public static IThreadPool DotnetImpl
        {
            get
            {
                if (_dotnet == null)
                {
                    _dotnet = new DotNetThreadPool();
                }
                return _dotnet;
            }
        }
        public static RegisteredWaitHandle RegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, TimeSpan timeOutInterval, bool executeOnlyOnce)
        {
            return Impl.RegisterWaitForSingleObject(waitObject, callback, state, timeOutInterval, executeOnlyOnce);
        }
        public static RegisteredWaitHandle RegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, long timeOutInterval, bool executeOnlyOnce)
        {
            return Impl.RegisterWaitForSingleObject(waitObject, callback, state, timeOutInterval, executeOnlyOnce);
        }
        public static RegisteredWaitHandle UnsafeRegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, TimeSpan timeOutInterval, bool executeOnlyOnce)
        {
            return Impl.UnsafeRegisterWaitForSingleObject(waitObject, callback, state, timeOutInterval, executeOnlyOnce);
        }
        public static RegisteredWaitHandle UnsafeRegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, long timeOutInterval, bool executeOnlyOnce)
        {
            return Impl.UnsafeRegisterWaitForSingleObject(waitObject, callback, state, timeOutInterval, executeOnlyOnce);
        }

        public static void QueueUserWorkItem(WaitCallback callback)
        {
            Impl.QueueUserWorkItem(callback);
        }
        public static bool QueueUserWorkItem(WaitCallback callback, object state)
        {
            return Impl.QueueUserWorkItem(callback, state);
        }

        public static void GetMaxThreads(out int workerThreads, out int iocpThreads)
        {
            DotnetImpl.GetMaxThreads(out workerThreads, out iocpThreads);
        }

        public static void SetMaxThreads(int workerThreads, int iocpThreads)
        {
            DotnetImpl.SetMaxThreads(workerThreads, iocpThreads);
        }
    }

    public class DotNetThreadPool : IThreadPool
    {
        virtual public RegisteredWaitHandle RegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, TimeSpan timeOutInterval, bool executeOnlyOnce)
        {
            return SThreadPool.RegisterWaitForSingleObject(waitObject, callback, state, timeOutInterval, executeOnlyOnce);
        }
        virtual public RegisteredWaitHandle RegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, long timeOutInterval, bool executeOnlyOnce)
        {
            return SThreadPool.RegisterWaitForSingleObject(waitObject, callback, state, timeOutInterval, executeOnlyOnce);
        }
        virtual public RegisteredWaitHandle UnsafeRegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, TimeSpan timeOutInterval, bool executeOnlyOnce)
        {
            return SThreadPool.UnsafeRegisterWaitForSingleObject(waitObject, callback, state, timeOutInterval, executeOnlyOnce);
        }
        virtual public RegisteredWaitHandle UnsafeRegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, long timeOutInterval, bool executeOnlyOnce)
        {
            return SThreadPool.UnsafeRegisterWaitForSingleObject(waitObject, callback, state, timeOutInterval, executeOnlyOnce);
        }

        virtual public bool QueueUserWorkItem(WaitCallback callback)
        {
            return SThreadPool.QueueUserWorkItem(callback);
        }
        virtual public bool QueueUserWorkItem(WaitCallback callback, object state)
        {
            return SThreadPool.QueueUserWorkItem(callback, state);
        }
        virtual public void GetMaxThreads(out int workerThreads, out int iocpThreads)
        {
            SThreadPool.GetMaxThreads(out workerThreads, out iocpThreads);
        }
        virtual public void SetMaxThreads(int workerThreads, int iocpThreads)
        {
            SThreadPool.SetMaxThreads(workerThreads, iocpThreads);
        }
    }
    public interface IThreadPool
    {
        RegisteredWaitHandle RegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, TimeSpan timeOutInterval, bool executeOnlyOnce);
        RegisteredWaitHandle RegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, long timeOutInterval, bool executeOnlyOnce);
        RegisteredWaitHandle UnsafeRegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, TimeSpan timeOutInterval, bool executeOnlyOnce);
        RegisteredWaitHandle UnsafeRegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, long timeOutInterval, bool executeOnlyOnce);
        bool QueueUserWorkItem(WaitCallback callback);
        bool QueueUserWorkItem(WaitCallback callback, object state);
        void GetMaxThreads(out int workerThreads, out int iocpThreads);
        void SetMaxThreads(int workerThreads, int iocpThreads);
    }

    /// <summary>
    /// This interface should be implemented by any class whose instances are intended 
    /// to be executed by a SThread.
    /// </summary>
    public interface IThreadRunnable
    {
        /// <summary>
        /// This method has to be implemented in order that starting of the SThread causes the object's 
        /// run method to be called in that separately executing SThread.
        /// </summary>
        void Run();
    }


    public class Monitor
    {
        public static void Exit(object obj)
        {
            SMonitor.Exit(obj);
        }

        public static bool TryEnter(object obj)
        {
            return SMonitor.TryEnter(obj);
        }
        public static bool TryEnter(object obj, int timeout)
        {
            return SMonitor.TryEnter(obj, timeout);
        }
        public static bool TryEnter(object obj, TimeSpan timeout)
        {
            return SMonitor.TryEnter(obj, timeout);
        }

        public static void Enter(object obj)
        {
            SMonitor.Enter(obj);
        }

        public static void Pulse(object obj)
        {
            SMonitor.Pulse(obj);
        }
        public static void PulseAll(object obj)
        {
            SMonitor.PulseAll(obj);
        }
        public static void Wait(object obj)
        {
            SMonitor.Wait(obj);
        }
        public static bool Wait(object obj, TimeSpan timeout)
        {
            return SMonitor.Wait(obj, timeout);
        }
        public static bool Wait(object obj, int timeout)
        {
            return SMonitor.Wait(obj, timeout);
        }
    }
    /// <summary>
    /// Support class used to handle Threads
    /// </summary>
    public class Thread : IThreadRunnable
    {
        /// <summary>
        /// The instance of SThread
        /// </summary>
        private SThread threadField;
        private readonly TASK runnable;


        /// <summary>
        /// Initializes a new instance of the ThreadClass class
        /// </summary>
        public Thread()
        {
            runnable = null;
            threadField = new SThread(new TASK(Run));
        }

        /// <summary>
        /// Initializes a new instance of the SThread class.
        /// </summary>
        /// <param name="Name">The name of the SThread</param>
        public Thread(System.String Name)
        {
            runnable = null;
            threadField = new SThread(new TASK(Run));
            this.Name = Name;
        }

        private Thread(SThread start)
        {
            threadField = start;
            AllThreads2Safe[threadField] = this;
            RegisterThread(threadField);

        }

        /// <summary>
        /// Initializes a new instance of the Thread class.
        /// </summary>
        /// <param name="task">A ThreadStart delegate that references the methods to be invoked when this SThread begins executing</param>
        public Thread(TASK task)
        {
            runnable = task;
            MakeThis();
        }

        private void MakeThis()
        {
            if (size != -2)
            {
                threadField = new SThread(() =>
                                              {
                                                  RegisterThread(threadField);
                                                  Run();
                                                  DeregisterThread(SThread.CurrentThread);
                                              }, size);
            } else
            {
                threadField = new SThread(() =>
                                              {
                                                  RegisterThread(threadField);
                                                  Run();
                                                  DeregisterThread(SThread.CurrentThread);
                                              });
            }
            lock (AllThreads2Safe) AllThreads2Safe[threadField] = this;

        }

        public Thread(TASK task , int size)
        {
            this.size = size;
            runnable = task;
            MakeThis();
        }

        /// <summary>
        /// Initializes a new instance of the SThread class.
        /// </summary>
        /// <param name="Start">A ThreadStart delegate that references the methods to be invoked when this SThread begins executing</param>
        /// <param name="Name">The name of the SThread</param>
        public Thread(TASK task, System.String Name)
        {
            runnable = task;
            MakeThis();
            this.Name = Name;
        }

        /// <summary>
        /// This method has no functionality unless the method is overridden
        /// </summary>
        public virtual void Run()
        {
            try
            {
                runnable();
            }
            catch (Exception e)
            {
                Console.Error.WriteLine(ToString() + " had " + e);
            }   
            finally
            {

            }
        }

        /// <summary>
        /// Causes the operating system to change the state of the current SThread instance to ThreadState.Running
        /// </summary>
        public virtual void Start()
        {
            threadField.Start();
        }

        /// <summary>
        /// Interrupts a SThread that is in the WaitSleepJoin SThread state
        /// </summary>
        public virtual void Interrupt()
        {
            threadField.Interrupt();
        }

        /// <summary>
        /// Gets the current SThread instance
        /// </summary>
        public SThread Instance
        {
            get
            {
                return threadField;
            }
            set
            {
                threadField = value;
            }
        }

        /// <summary>
        /// Gets or sets the name of the SThread
        /// </summary>
        public string Name
        {
            get { return myName ?? threadField.Name; }
            set
            {
                if (threadField.Name == null)
                    threadField.Name = value;
                myName = value;
            }
        }
        private string myName;

        public void SetDaemon(bool isDaemon)
        {
            threadField.IsBackground = isDaemon;
        }

        /// <summary>
        /// Gets or sets a value indicating the scheduling priority of a SThread
        /// </summary>
        public System.Threading.ThreadPriority Priority
        {
            get
            {
                try
                {
                    return threadField.Priority;
                }
                catch
                {
                    return System.Threading.ThreadPriority.Normal;
                }
            }
            set
            {
                try
                {
                    threadField.Priority = value;
                }
                catch { }

            }
        }

        /// <summary>
        /// Gets a value indicating the execution status of the current SThread
        /// </summary>
        public bool IsAlive
        {
            get
            {
                return threadField.IsAlive;
            }
        }

        /// <summary>
        /// Gets or sets a value indicating whether or not a SThread is a background SThread.
        /// </summary>
        public bool IsBackground
        {
            get
            {
                return threadField.IsBackground;
            }
            set
            {
                threadField.IsBackground = value;
            }
        }

        /// <summary>
        /// Blocks the calling SThread until a SThread terminates
        /// </summary>
        public void Join()
        {
            threadField.Join();
        }

        /// <summary>
        /// Blocks the calling SThread until a SThread terminates or the specified time elapses
        /// </summary>
        /// <param name="MiliSeconds">Time of wait in milliseconds</param>
        public void Join(long MiliSeconds)
        {
            threadField.Join(new System.TimeSpan(MiliSeconds * 10000));
        }

        /// <summary>
        /// Blocks the calling SThread until a SThread terminates or the specified time elapses
        /// </summary>
        /// <param name="MiliSeconds">Time of wait in milliseconds</param>
        /// <param name="NanoSeconds">Time of wait in nanoseconds</param>
        public void Join(long MiliSeconds, int NanoSeconds)
        {
            threadField.Join(new System.TimeSpan(MiliSeconds * 10000 + NanoSeconds * 100));
        }

        /// <summary>
        /// Resumes a SThread that has been suspended
        /// </summary>
        public void Resume()
        {
            threadField.Resume();
            SMonitor.PulseAll(threadField);
        }

        /// <summary>
        /// Raises a ThreadAbortException in the SThread on which it is invoked, 
        /// to begin the process of terminating the SThread. Calling this method 
        /// usually terminates the SThread
        /// </summary>
        public void Abort()
        {
            threadField.Abort();
        }

        /// <summary>
        /// Raises a ThreadAbortException in the SThread on which it is invoked, 
        /// to begin the process of terminating the SThread while also providing
        /// exception information about the SThread termination. 
        /// Calling this method usually terminates the SThread.
        /// </summary>
        /// <param name="stateInfo">An object that contains application-specific information, such as state, which can be used by the SThread being aborted</param>
        public void Abort(object stateInfo)
        {
            threadField.Abort(stateInfo);
        }

        /// <summary>
        /// Suspends the SThread, if the SThread is already suspended it has no effect
        /// </summary>
        public void Suspend()
        {
            SMonitor.Wait(threadField);
        }

        /// <summary>
        /// Obtain a String that represents the current object
        /// </summary>
        /// <returns>A String that represents the current object</returns>
        public override System.String ToString()
        {
            return "SThread[" + Name + "," + Priority.ToString() + "]";
        }

        [ThreadStatic]
        static Thread This = null;

        // named as the Java version
        public static Thread CurrentThreadJava()
        {
            return Current();
        }
        public static SThread CurrentThread
        {
            get { return Current(); }
        }

        public static void Sleep(long ms)
        {
            // casting long ms to int ms could lose resolution, however unlikely
            // that someone would want to sleep for that long...
            SThread.Sleep((int)ms);
        }

        /// <summary>
        /// Gets the currently running SThread
        /// </summary>
        /// <returns>The currently running SThread</returns>
        public static Thread Current()
        {
            if (This == null)
            {
                This = new Thread();
                This.Instance = SThread.CurrentThread;
            }
            return This;
        }

        public static bool operator ==(Thread t1, object t2)
        {
            if (((object)t1) == null) return t2 == null;
            return t1.Equals(t2);
        }

        public static bool operator !=(Thread t1, object t2)
        {
            return !(t1 == t2);
        }

        public override bool Equals(object obj)
        {
            if (obj == null) return false;
            if (obj is SThread) return this.threadField.Equals((SThread)obj);
            if (obj is Thread) return this.threadField.Equals(((Thread)obj).threadField);
            return false;
        }

        /// <summary>
        /// ?- cliGet('MushDLR223.Utilities.SafeThread','AllThreads',List),cliGet(List,'Count',C).
        /// </summary>
        public static ListAsSet<Thread> AllThreads = new ListAsSet<Thread>();
        public static Dictionary<SThread, Thread> AllThreads2Safe = new Dictionary<SThread, Thread>();
        static SThread reaper = new SThread(ReapDeadThreads);
        private int size = -2;

        public static void RegisterCurrentThread()
        {
            SThread SThread = SThread.CurrentThread;
            RegisterThread(SThread);
        }
        public static void DeregisterCurrentThread()
        {
            DeregisterThread(SThread.CurrentThread);
        }

        static Thread()
        {
            Thread.ThreadAdded += dummy;
            Thread.ThreadRemoved += dummy;
            Application.ThreadException += OnThreadException;
            Application.ThreadExit += OnThreadExit;
            ThreadPool.Impl = new CancellableThreadPool(null);           
            

        }
        private static void dummy(SThread obj)
        {
        }
        public static event Action<SThread> ThreadAdded;
        public static event Action<SThread> ThreadRemoved;
        static public void RegisterThread(SThread systhread)
        {
            if (!AllThreads.Contains(systhread))
            {
                AllThreads.Add(systhread);
            }
            else
            {

            }
            if (ThreadAdded != null) ThreadAdded(systhread);
            //AllThreads2Safe.Remove(systhread);
        }
        static public void DeregisterThread(SThread SThread)
        {
            AllThreads.Remove(SThread);
            if (ThreadRemoved != null) ThreadRemoved(SThread);
        }
        private static void OnThreadException(object sender, System.Threading.ThreadExceptionEventArgs e)
        {
            //is it on this SThread? DeregisterCurrentThread();           
        }

        private static void OnThreadExit(object sender, EventArgs e)
        {
            DeregisterCurrentThread();
        }
        public static implicit operator SThread(Thread st)
        {
            return st.AsThread;
        }
        public static implicit operator Thread(SThread st)
        {
            lock (AllThreads2Safe)
            {
                Thread safe;
                if (AllThreads2Safe.TryGetValue(st, out safe))
                {
                    return safe;
                }
                return new Thread(st);
            }
        }
        private static void ReapDeadThreads()
        {
            while (true)
            {
                SThread.Sleep(3000);
                foreach (var thread in AllThreads)
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


        public System.Threading.ApartmentState ApartmentState
        {
            get { return threadField.ApartmentState; }
            set { threadField.ApartmentState = value; }
        }

        public SThread AsThread
        {
            get { return threadField; }
        }


        public ThreadState ThreadState
        {
            get { return threadField.ThreadState; }
        }

        public static bool operator ==(Thread obj1, Thread obj2)
        {
            return EqualsTwo(obj1, obj2);
        }

        private static bool EqualsTwo(Thread one, Thread other)
        {
            if (ReferenceEquals(one, other)) return true;
            if (ReferenceEquals(null, other)) return false;
            if (ReferenceEquals(null, one)) return false;
            return Equals(other.threadField, one.threadField);
        }

        public static bool operator !=(Thread obj1, Thread obj2)
        {
            return !EqualsTwo(obj1, obj2);
        }

        public void SetApartmentState(System.Threading.ApartmentState state)
        {
            threadField.SetApartmentState(state);
        }

        public static void Sleep(int i)
        {
            SThread.Sleep(i);
        }
        public static void Sleep(TimeSpan i)
        {
            SThread.Sleep(i);
        }

        public bool Join(TimeSpan i)
        {
            return threadField.Join(i);
        }

        public static void ResetAbort()
        {
            SThread.ResetAbort();
        }


        public bool Equals(Thread other)
        {
            if (ReferenceEquals(null, other)) return false;
            if (ReferenceEquals(this, other)) return true;
            return Equals(other.threadField, threadField);
        }

        public override int GetHashCode()
        {
            return (threadField != null ? threadField.GetHashCode() : 0);
        }

        public void TrySetApartmentState(System.Threading.ApartmentState state)
        {
            threadField.TrySetApartmentState(state);
        }

        public static void MemoryBarrier()
        {
            Unsupported();
            SThread.MemoryBarrier();
        }

        private static void Unsupported()
        {

        }

        public static void SpinWait(int iterations)
        {
            SThread.SpinWait(iterations);
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
    /// A collection of Threads that can be cancelled
    /// </summary>
    public class CancellableThreadPool: IThreadPool
    {
        private ThreadStartEvaluator _startEvaluator = null;
        private bool _stopped = false;

        private Hashtable _Threads = new Hashtable();
        private ArrayList _workItems = new ArrayList();

        public CancellableThreadPool(ThreadStartEvaluator startEvaluator)
        {
            _startEvaluator = startEvaluator;
            SThreadPool.QueueUserWorkItem(new WaitCallback(o => Start()));
        }


        public bool QueueWorkItem(WaitCallback callback, object state)
        {
            bool workItemsContains = false;
            lock (_workItems)
            {
                workItemsContains = _workItems.Contains(callback);
                //Make sure I don't duplicate this
                //(If its running it qont be in the queue so its not a problem)
                if (workItemsContains == false)
                {
                    //Add the work item into the queue		
                    _workItems.Add(new ThreadRunInfo(callback, state));

                }
            }
            //We never abort from here
            EvaluateThreadMaybeAbort(EvaluationReason.NewWorkItemQueued);
            return workItemsContains;
        }

        private void AddNewThread()
        {
            if (_stopped == false)
            {
                var newThread = new SThread(
                    new TASK(ThreadRunner));
                //Add it to the collection
                lock (_Threads)
                {
                    _Threads.Add(newThread, null);
                }
                newThread.Start();
            }
        }

        private void ThreadRunner()
        {
            ThreadRunInfo runInfo = null;
            bool ThreadStop = false;

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
                                runInfo = (ThreadRunInfo)_workItems[0];
                                _workItems.Remove(runInfo);
                            }
                            else
                                runInfo = null;
                        }

                        //Now try to run
                        if (runInfo != null)
                        {
                            lock (_Threads)
                            {
                                _Threads[SThread.CurrentThread] = runInfo;
                            }

                            //Record when we start
                            runInfo.StartTime = DateTime.Now;

                            //This one line does all the work
                            runInfo.Callback(runInfo.State);
                            //back again
                            //Remove the job from the queue
                            runInfo = null;
                            lock (_Threads)
                            {
                                _Threads[SThread.CurrentThread] = null;
                            }
                        }
                    }
                    catch (System.Threading.ThreadAbortException)
                    {
                        lock (_workItems)
                        {
                            //requeue the job if it isnt in progress
                            if (runInfo != null)
                                _workItems.Add(runInfo);
                        }
                    }
                    if (_stopped == false)
                        ThreadStop = EvaluateThreadMaybeAbort(EvaluationReason.WorkItemCompleted);
                } while (_workItems.Count > 0 && _stopped == false && ThreadStop == false);
            }
            catch (System.Threading.ThreadAbortException)
            {
                lock (_workItems)
                {
                    if (runInfo != null)
                        _workItems.Add(runInfo);
                }
            }
            finally
            {
                //Finally remove this SThread from the collection
                lock (_Threads)
                {
                    _Threads.Remove(SThread.CurrentThread);
                }
            }
        }

        public void Stop() //bool requeue)
        {
            _stopped = true;
            //Cant lock both _Threads and _workItems
            //That might cause a deadlock
            lock (_Threads)
                foreach (SThread SThread in _Threads.Keys)
                {
                    SThread.Abort();
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
            lock (_Threads)
            {
                foreach (SThread SThread in _Threads)
                {
                    if (SThread.ThreadState != ThreadState.Running)
                    {
                        _startEvaluator.CurrentlyQueuedWorkItems = workItemsCount;
                        _startEvaluator.CurrentlyRunningThreadCount = _Threads.Count;
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

        virtual public RegisteredWaitHandle RegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, TimeSpan timeOutInterval, bool executeOnlyOnce)
        {
            return SThreadPool.RegisterWaitForSingleObject(waitObject, callback, state, timeOutInterval, executeOnlyOnce);
        }
        virtual public RegisteredWaitHandle RegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, long timeOutInterval, bool executeOnlyOnce)
        {
            return SThreadPool.RegisterWaitForSingleObject(waitObject, callback, state, timeOutInterval, executeOnlyOnce);
        }
        virtual public RegisteredWaitHandle UnsafeRegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, TimeSpan timeOutInterval, bool executeOnlyOnce)
        {
            return SThreadPool.UnsafeRegisterWaitForSingleObject(waitObject, callback, state, timeOutInterval, executeOnlyOnce);
        }
        virtual public RegisteredWaitHandle UnsafeRegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, long timeOutInterval, bool executeOnlyOnce)
        {
            return SThreadPool.UnsafeRegisterWaitForSingleObject(waitObject, callback, state, timeOutInterval, executeOnlyOnce);
        }

        virtual public bool QueueUserWorkItem(WaitCallback callback)
        {
            if (true) return ThreadPool.DotnetImpl.QueueUserWorkItem(AddSafety(callback));
            return QueueWorkItem(callback, null);
        }
        virtual public bool QueueUserWorkItem(WaitCallback callback, object state)
        {
            if (true) return ThreadPool.DotnetImpl.QueueUserWorkItem(AddSafety(callback), state);
            return QueueWorkItem(callback, state);
        }

        private WaitCallback AddSafety(WaitCallback callback)
        {
            return new WaitCallback((o) =>
                                        {
                                            try
                                            {
                                                callback(o);
                                            }
                                            catch (Exception e)
                                            {

                                                Issue(SThread.CurrentThread, e);
                                            } 
                                        });
        }

        private void Issue(SThread thread, Exception exception)
        {

        }

        virtual public void GetMaxThreads(out int workerThreads, out int iocpThreads)
        {
            SThreadPool.GetMaxThreads(out workerThreads, out iocpThreads);
        }
        virtual public void SetMaxThreads(int workerThreads, int iocpThreads)
        {
            SThreadPool.SetMaxThreads(workerThreads, iocpThreads);
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
        /// This is the time number of currently running Threads. Changing this value has no effect.
        /// </summary>
        public int CurrentlyRunningThreadCount;


        /// <summary>
        /// Override this method to determine whether you wish to start a new SThread, stop the current SThread, or just leave
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
        /// A WorkItem has been completed by the SThread pool
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
        /// Indicates that a new SThread should be added to the SThread pool
        /// </summary>
        StartNewThread,
        /// <summary>
        /// Indicates that the current numbers of Threads running is sufficient.
        /// </summary>
        NoOperation,
        /// <summary>
        /// Indicates that too many Threads are running so we can drop the current one IF AND ONLY IF we are in 
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
            //Run another SThread if we have a big queue
            if (CurrentlyRunningThreadCount == 0
                || CurrentlyRunningThreadCount < CurrentlyQueuedWorkItems / MaxDesiredQueued)
            {
                return EvaluationResult.StartNewThread;
            }
            if (CurrentlyRunningThreadCount > 1
                && CurrentlyRunningThreadCount > CurrentlyQueuedWorkItems / MaxDesiredQueued)
                return EvaluationResult.FinishCurrentThreadWhenWorkItemCompleted;

            return EvaluationResult.NoOperation;
        }
    }
}
#if false
//
// Copyright (c) Microsoft Corporation.   All rights reserved.
//

namespace System.Threading
{

    using System.Runtime.CompilerServices;
    using System.Runtime.InteropServices;

    using Microsoft.Bartok.Runtime;

    using Microsoft.Singularity;

    /// <summary>
    /// A monitor is used for synchronization. Only a single thread can
    /// hold the monitor at any given time.
    ///
    /// The monitor maintains two lists of threads: one for threads waiting
    /// to enter the monitor, and one for threads waiting for a pulse within
    /// the monitor.
    /// </summary>
    public sealed partial class Monitor {
        /// <summary>
        /// Private so that only we can create instances.
        /// </summary>
        internal Monitor()
        {
            this.mutex = new Mutex();
            this.depth = 0;
        }

        /// <summary>
        /// Wake up a thread waiting on the monitor.
        /// </summary>
        public static void Pulse(Object obj)
        {
            Monitor monitor = GetMonitorFromObject(obj);
            monitor.Pulse();
        }

        /// <summary>
        /// Wake up all threads waiting on the monitor.
        /// </summary>
        public static void PulseAll(Object obj)
        {
            Monitor monitor = GetMonitorFromObject(obj);
            monitor.PulseAll();
        }

        /// <summary>
        /// Attempt to enter the monitor, returning immediately if it is
        /// already held by another thread.
        /// </summary>
        public static bool TryEnter(Object obj)
        {
            Monitor monitor = GetMonitorFromObject(obj);
            return monitor.TryEnter();
        }

        /// <summary>
        /// Attempt to enter the monitor, returning if it can not be taken
        /// within the specified timeout.
        /// </summary>
        public static bool TryEnter(Object obj, TimeSpan timeout)
        {
            Monitor monitor = GetMonitorFromObject(obj);
            return monitor.TryEnter(SchedulerTime.Now + timeout);
        }

        /// <summary>
        /// Wait to be woken up by a holder of the monitor.
        /// </summary>
        public static bool Wait(Object obj)
        {
            Monitor monitor = GetMonitorFromObject(obj);
            return monitor.Wait(SchedulerTime.MaxValue);
        }

        /// <summary>
        /// Wait to be woken up by a holder of the monitor. Give up after
        /// a specified timeout.
        /// </summary>
        public static bool Wait(Object obj, TimeSpan timeout)
        {
            Monitor monitor = GetMonitorFromObject(obj);
            return monitor.Wait(SchedulerTime.Now + timeout);
        }

        /// <summary>
        /// Wait to be woken up by a holder of the monitor. Give up after
        /// a specified timeout.
        ///
        /// Overload exists to match the CLR. Exit Context not supported.
        /// </summary>
        public static bool Wait(Object obj,
                                TimeSpan timeout,
                                bool exitContext)
        {
            if (exitContext) {
                DebugStub.Break();
                throw new NotSupportedException("exitContext not supported!");
            }
            Monitor monitor = GetMonitorFromObject(obj);
            return monitor.Wait(SchedulerTime.Now + timeout);
        }

        /// <summary>
        /// Enter the monitor, blocking until it is held.
        /// </summary>
        internal void Enter()
        {
            TryEnter(SchedulerTime.MaxValue);
        }

        /// <summary>
        /// Exit the monitor.
        /// </summary>
        internal void Exit()
        {
            if (!mutex.IsOwnedByCurrentThread()) {
                DebugStub.Break();
                throw new SynchronizationLockException("Monitor not held on Exit");
            }

            depth--;
            if (depth == 0) {
                mutex.ReleaseMutex();
            }
        }

        /// <summary>
        /// Wake up a single thread waiting on the monitor.
        /// </summary>
        internal void Pulse()
        {
            if (!mutex.IsOwnedByCurrentThread()) {
                DebugStub.Break();
                throw new SynchronizationLockException("Monitor not held on Pulse");
            }

            // Wake up thread at the head of the wait list.
            if (waitListHead != null) {
                Thread t = Dequeue();
                if (t != null) {
                    t.nextThread = null;
                    t.SignalMonitor();
                }
            }
        }

        /// <summary>
        /// Wake up all threads waiting on the monitor.
        /// </summary>
        internal void PulseAll()
        {
            if (!mutex.IsOwnedByCurrentThread()) {
                DebugStub.Break();
                throw new SynchronizationLockException("Monitor not held on PulseAll");
            }

            // Wake up all threads the wait list.
            if (waitListHead != null) {
                Thread t = waitListHead;
                while (t != null) {
                    Thread next = t.nextThread;
                    t.nextThread = null;
                    t.SignalMonitor();
                    t = next;
                }
                waitListHead = null;
                waitListTail = null;
            }
        }

        /// <summary>
        /// Try to enter the monitor, returning immediately if it is
        /// already held.
        /// </summary>
        internal bool TryEnter()
        {
            return TryEnter(new SchedulerTime(0));
        }

        /// <summary>
        /// Try to enter the monitor, giving up if it cannot be
        /// entered after a timeout.
        /// </summary>
        internal bool TryEnter(SchedulerTime stop)
        {
            if (mutex.IsOwnedByCurrentThread()) {
                depth++;
                return true;
            }

            if (mutex.AcquireMutex(stop)) {
                depth = 1;
                return true;
            }
            return false;
        }

        /// <summary>
        /// Wait within the monitor for a Pulse.
        /// </summary>
        internal bool Wait(SchedulerTime stop)
        {
            Thread currentThread = Thread.CurrentThread;
            if (!mutex.IsOwnedByCurrentThread()) {
                DebugStub.Break();
                throw new SynchronizationLockException("Monitor not held on Wait");
            }

            int rememberedDepth = depth;
            depth = 0;

            // Add me onto the waiting list.
            Enqueue(currentThread);

            // Exit the monitor
            mutex.ReleaseMutex();

            // Wait
            currentThread.WaitForMonitor(stop);

            // Re-enter the monitor
            mutex.AcquireMutex();
            depth = rememberedDepth;

            bool success = !Remove(currentThread);

            if (!success && stop == SchedulerTime.MaxValue) {
                VTable.DebugBreak();
            }

            return success;
        }

        /// <summary>
        /// Ensure that the passed object has a monitor (and associated
        /// SyncBlock) allocated.
        /// </summary>
        internal static void CreateMonitor(Object obj)
        {
            GetMonitorFromObject(obj);
        }

        // BUGBUG: The garbage collectors will not collect monitors that are
        // in use.  Use a very defensive strategy for now.
        internal bool IsInUse() {
            return true;
        }

        /// <summary>
        /// Internal Type conversion method.
        /// Note: we don't use VTable.fromAddress because we
        /// cannot do a checked cast from Object to Monitor during GC
        /// (because the GC may be using the vtable word)
        /// </summary>
        ///
        internal static Monitor FromAddress(UIntPtr v) {
            return Magic.toMonitor(Magic.fromAddress(v));
        }

        /// <summary>
        /// Look up the Monitor for the specified object in the SyncBlock
        /// tables. If no Monitor exists for the object then one is created.
        /// </summary>
        private static Monitor GetMonitorFromObject(Object obj)
        {
            if (obj == null) {
                DebugStub.Break();
                throw new ArgumentNullException("obj");
            }
            Monitor result = MultiUseWord.GetMonitor(obj);
            return result;
        }

        //////////////////////////////////////////////////////////////////////
        //
        // Linked list of threads waiting for a Pulse in a monitor.
        private Thread waitListHead;
        private Thread waitListTail;

        /// <summary>
        /// Dequeue a thread from the singly linked list from head to tail,
        /// acquiring the ListLock if necessary.
        ///
        /// If the list is empty then this method returns null.
        /// </summary>
        [Inline]
        private Thread Dequeue()
        {
            Thread result;
            if (waitListHead == null) {
                // Empty list
                result = null;
            }
            else if (waitListHead == waitListTail) {
                // Single entry on list
                VTable.Assert(waitListHead.nextThread == null);
                result = waitListHead;
                waitListHead = waitListTail = null;
            }
            else {
                // Multiple entries on list
                result = waitListHead;
                waitListHead = waitListHead.nextThread;
            }
            return result;
        }

        /// <summary>
        /// Search the linked list and remove the specified thread if
        /// it is linked in.
        ///
        /// Acquires the ListLock if necessary.
        /// </summary>
        [Inline]
        private bool Remove(Thread target)
        {
            if (waitListHead == null) {
                return false;
            }

            if (waitListHead == waitListTail) {
                // Single entry on list
                VTable.Assert(waitListHead.nextThread == null);

                if (waitListHead != target) {
                    // Not on list
                    return false;
                }

                waitListHead = waitListTail = null;
            }
            else if (waitListHead == target) {
                // At waitListHead of list
                waitListHead = target.nextThread;
                target.nextThread = null;
            }
            else {
                // Multiple entries on list
                Thread next = waitListHead;
                while (next != null && next.nextThread != target) {
                    next = next.nextThread;
                }

                if (next == null) {
                    // Not on list
                    return false;
                }

                if (waitListTail == target) {
                    // Update the waitListTail
                    waitListTail = next;
                }

                next.nextThread = target.nextThread;
                target.nextThread = null;
            }
            return true;
        }

        /// <summary>
        /// Append a thread at the tail of a queue. If the queue is
        /// currently null this method initializes it.
        ///
        /// Acquires the ListLock if necessary.
        /// </summary>
        [Inline]
        private void Enqueue(Thread target)
        {
            if (waitListHead == null) {
                waitListHead = waitListTail = target;
            }
            else {
                waitListTail.nextThread = target;
                waitListTail = target;
            }
        }

        /// <summary>
        /// The recursion depth of the current holder of the monitor.
        /// </summary>
        private int depth;

        /// <summary>
        /// The mutex that is held by the thread that holds the monitor
        /// </summary>
        private Mutex mutex;
    }
}
#endif