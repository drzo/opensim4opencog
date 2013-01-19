using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using System.Windows.Forms;
using MushDLR223.Utilities;

namespace ThreadPoolUtil
{
    /// <summary>
    /// Support class used to handle Threads
    /// </summary>
    public class Thread : IThreadRunnable
    {
        /// <summary>
        /// The instance of SThread
        /// </summary>
        private System.Threading.Thread threadField;
        private readonly ThreadStart runnable0;
        private readonly ParameterizedThreadStart runnable1;
        private readonly object param;

        /// <summary>
        /// Initializes a new instance of the ThreadClass class
        /// </summary>
        protected Thread()
        {
            threadField = new System.Threading.Thread(new ThreadStart(RunIt));
            MakeThis(false);
        }

        /// <summary>
        /// Initializes a new instance of the SThread class.
        /// </summary>
        /// <param name="Name">The name of the SThread</param>
        protected Thread(System.String Name)
        {
            threadField = new System.Threading.Thread(new ThreadStart(RunIt));
            this.Name = Name;
            MakeThis(false);
        }

        private Thread(System.Threading.Thread start)
        {
            threadField = start;
            MakeThis(false);
        }

        /// <summary>
        /// Initializes a new instance of the Thread class.
        /// </summary>
        /// <param name="task">A ThreadStart delegate that references the methods to be invoked when this SThread begins executing</param>
        public Thread(ThreadStart task)
        {
            runnable0 = task;
            MakeThis(true);
        }
        public Thread(ParameterizedThreadStart task, object obj)
        {
            runnable1 = task;
            param = obj;
            MakeThis(true);
        }
        public Thread(ThreadStart task, int size)
        {
            this.size = size;
            runnable0 = task;
            MakeThis(true);
        }
        public Thread(ParameterizedThreadStart task, object obj, int size)
        {
            this.size = size;
            runnable1 = task;
            param = obj;
            MakeThis(true);
        }

        private void MakeThis(bool needMakeThread)
        {
            if (needMakeThread)
            {
                threadField = size != -2 ? new System.Threading.Thread(RunIt, size) : new System.Threading.Thread(RunIt);
            }
            lock (AllThreads2Safe) AllThreads2Safe[threadField] = this;
        }

        /// <summary>
        /// Initializes a new instance of the SThread class.
        /// </summary>
        /// <param name="Start">A ThreadStart delegate that references the methods to be invoked when this SThread begins executing</param>
        /// <param name="Name">The name of the SThread</param>
        public Thread(ThreadStart task, System.String Name)
        {
            runnable0 = task;
            MakeThis(true);
            this.Name = Name;
        }

        public void RunIt()
        {
            This = this;
            try
            {
                RegisterThread(threadField);
                SafeThreadPool.AddSafety((o) => Run())(param);
            }
            catch (Exception e)
            {
                SafeThreadPool.Issue(threadField, e);
            }
            finally
            {
                DeregisterThread(System.Threading.Thread.CurrentThread);
            }
        }

        /// <summary>
        /// This method has no functionality unless the method is overridden
        /// </summary>
        public virtual void Run()
        {
            try
            {
                if (runnable1 != null)
                {
                    runnable1(param);
                }
                if (runnable0 != null)
                {
                    runnable0();
                }
            }
            catch (Exception e)
            {
                SafeThreadPool.Issue(threadField, e);
            }
        }

        /// <summary>
        /// Causes the operating system to change the state of the current SThread instance to ThreadState.Running
        /// </summary>
        public virtual void Start()
        {
//            savedStartUp = GetStackString();
            threadField.Start();
        }

        public string GetStackString()
        {
            TextWriter sb = new StringWriter();
            return sb.ToString();
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
        public System.Threading.Thread Instance
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
            System.Threading.Monitor.PulseAll(threadField);
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
            System.Threading.Monitor.Wait(threadField);
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
        public static System.Threading.Thread CurrentThread
        {
            get { return Current(); }
        }

        public static void Sleep(long ms)
        {
            // casting long ms to int ms could lose resolution, however unlikely
            // that someone would want to sleep for that long...
            System.Threading.Thread.Sleep((int)ms);
        }

        /// <summary>
        /// Gets the currently running SThread
        /// </summary>
        /// <returns>The currently running SThread</returns>
        public static Thread Current()
        {
            if (This == null)
            {
                This = new Thread(System.Threading.Thread.CurrentThread);
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
            if (obj is System.Threading.Thread) return this.threadField.Equals((System.Threading.Thread)obj);
            if (obj is Thread) return this.threadField.Equals(((Thread)obj).threadField);
            return false;
        }

        /// <summary>
        /// ?- cliGet('MushDLR223.Utilities.SafeThread','AllThreads',List),cliGet(List,'Count',C).
        /// </summary>
        public static ListAsSet<Thread> AllThreads = new ListAsSet<Thread>();
        public static Dictionary<System.Threading.Thread, Thread> AllThreads2Safe = new Dictionary<System.Threading.Thread, Thread>();
        static System.Threading.Thread reaper = new System.Threading.Thread(ReapDeadThreads);
        private int size = -2;

        public static void RegisterCurrentThread()
        {
            System.Threading.Thread SThread = System.Threading.Thread.CurrentThread;
            RegisterThread(SThread);
        }
        public static void DeregisterCurrentThread()
        {
            DeregisterThread(System.Threading.Thread.CurrentThread);
        }

        static Thread()
        {
            Thread.ThreadAdded += dummy;
            Thread.ThreadRemoved += dummy;
            Application.ThreadException += OnThreadException;
            Application.ThreadExit += OnThreadExit;
            ThreadPool.Impl = new SafeThreadPool();           
            

        }
        private static void dummy(System.Threading.Thread obj)
        {
        }
        public static event Action<System.Threading.Thread> ThreadAdded;
        public static event Action<System.Threading.Thread> ThreadRemoved;
        static public void RegisterThread(System.Threading.Thread systhread)
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
        static public void DeregisterThread(System.Threading.Thread SThread)
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
        public static implicit operator System.Threading.Thread(Thread st)
        {
            return st.AsThread;
        }
        public static implicit operator Thread(System.Threading.Thread st)
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
                System.Threading.Thread.Sleep(3000);
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

        public System.Threading.Thread AsThread
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
            System.Threading.Thread.Sleep(i);
        }
        public static void Sleep(TimeSpan i)
        {
            System.Threading.Thread.Sleep(i);
        }

        public bool Join(TimeSpan i)
        {
            return threadField.Join(i);
        }

        public static void ResetAbort()
        {
            System.Threading.Thread.ResetAbort();
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
            System.Threading.Thread.MemoryBarrier();
        }

        private static void Unsupported()
        {

        }

        public static void SpinWait(int iterations)
        {
            System.Threading.Thread.SpinWait(iterations);
        }
    }

}