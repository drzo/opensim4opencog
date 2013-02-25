using System;
using System.Diagnostics;
using System.IO;
using System.Windows.Forms;
using NativeThread = System.Threading.Thread;
using SMonitor = System.Threading.Monitor;
using SThreadPool = System.Threading.ThreadPool;
using RegisteredWaitHandle = System.Threading.RegisteredWaitHandle;
using WaitCallback = System.Threading.WaitCallback;
using WaitHandle = System.Threading.WaitHandle;
using WaitOrTimerCallback = System.Threading.WaitOrTimerCallback;
using ThreadExceptionEventArgs = System.Threading.ThreadExceptionEventArgs;
using MushDLR223.Utilities;

namespace ThreadPoolUtil
{
    public class SafeThreadPool : IThreadPool
    {
        private IThreadPool dotnet
        {
            get { return ThreadPool.DotnetImpl; }
        }
        /// <summary>
        /// Get notices this level or higher
        /// </summary>
        public static int noticeLevel = 1;
        static SafeThreadPool()
        {
            Application.ThreadException += ThreadException;
            Application.ThreadExit += ThreadExit;
            Application.ApplicationExit += ApplicationExit;
        }

        private static void ThreadException(object sender, ThreadExceptionEventArgs e)
        {
            Notice(2, "ThreadException {0} {1}", sender, e.Exception);
        }

        private static void ThreadExit(object sender,  EventArgs e)
        {
            Notice(1, "ThreadExit {0} {1}", sender, e);
        }

        private static void ApplicationExit(object sender, EventArgs e)
        {
            Notice(0, "ApplicationExit {0} {1}", sender, e);
        }

        public static void Notice(int level, string fmt, params object[] args)
        {
            if (level < noticeLevel) return;
            DLRConsole.DebugLevel = 9;
            DLRConsole.DebugWriteLine(fmt, args);
        }

        public static void Issue(object thread, Exception e)
        {
            Notice(2, "Thread issue " + thread + " had " + e);
        }

        public static string StackTraceString()
        {
            var rs = new System.Text.StringBuilder();
            var fs = new System.Diagnostics.StackTrace(true).GetFrames();
            if (fs != null) foreach (StackFrame frame in fs)
                {
                    rs.Append(frame.ToString());
                }
            return rs.ToString();
        }

        public static WaitCallback AddSafety(WaitCallback callback)
        {
            var creationTrace = StackTraceString();
            return ((o) => SafelyInvoke(creationTrace, callback, o));
        }

        public static WaitOrTimerCallback AddSafetyTC(WaitOrTimerCallback callback)
        {
            var creationTrace = StackTraceString();
            return ((o, b) => SafelyInvoke(creationTrace, callback, o, b));
        }
        public static void SafelyAct(string creationTrace, Action action)
        {
            SafelyInvoke(creationTrace, action);
        }
        public static T SafelyFunc<T>(string creationTrace, Func<T> action)
        {
            T[] res = new T[1];
            Action act = () =>
                             {
                                 res[0] = action();
                             };
            SafelyInvoke(creationTrace, act);
            return res[0];
        }

        public static bool NeverCrash = true;

        private static void SafelyInvoke(string creationTrace, Delegate action, params object[] args)       
        {
            Thread thread = Thread.CurrentThread;
            thread.LastException = null;
            thread.CreationStack = creationTrace;
            try
            {
                if (thread.StartStack == null)
                {
                    thread.StartStack = StackTraceString();
                }
                action.DynamicInvoke(args);
            }
            catch (Exception e)
            {
                thread.LastException = e;
                Issue(thread, e);
                if (!thread.HandleException(e))
                {
                    if (!NeverCrash)
                    {
                        throw thread.LastException;
                    }
                }
            }
            finally
            {
                Notice(0, "Leaving callbackthread " + thread);
                thread.RunOnExit();
            }
        }

        public static T Safely<T>(Func<T> func)
        {
            try
            {
                return func();
            }
            catch (Exception e)
            {
                Issue(NativeThread.CurrentThread, e);
                return default(T);

            }
        }
        public static WaitHandle AddSafetyWH(WaitHandle handle)
        {
            if (true) return handle;
            return new SafeWaitHandle(handle);
        }

        virtual public RegisteredWaitHandle RegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, TimeSpan timeOutInterval, bool executeOnlyOnce)
        {
            return dotnet.RegisterWaitForSingleObject(AddSafetyWH(waitObject), AddSafetyTC(callback), state, timeOutInterval, executeOnlyOnce);
        }
        virtual public RegisteredWaitHandle RegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, long timeOutInterval, bool executeOnlyOnce)
        {
            return dotnet.RegisterWaitForSingleObject(AddSafetyWH(waitObject), AddSafetyTC(callback), state, timeOutInterval, executeOnlyOnce);
        }

        virtual public RegisteredWaitHandle UnsafeRegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, TimeSpan timeOutInterval, bool executeOnlyOnce)
        {
            return dotnet.UnsafeRegisterWaitForSingleObject(AddSafetyWH(waitObject), AddSafetyTC(callback), state, timeOutInterval, executeOnlyOnce);
        }
        virtual public RegisteredWaitHandle UnsafeRegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, long timeOutInterval, bool executeOnlyOnce)
        {
            return dotnet.UnsafeRegisterWaitForSingleObject(AddSafetyWH(waitObject), AddSafetyTC(callback), state, timeOutInterval, executeOnlyOnce);
        }

        virtual public bool QueueUserWorkItem(WaitCallback callback)
        {
            return dotnet.QueueUserWorkItem(AddSafety(callback));
        }
        virtual public bool QueueUserWorkItem(WaitCallback callback, object state)
        {
            return dotnet.QueueUserWorkItem(AddSafety(callback), state);
        }

        virtual public void GetMaxThreads(out int workerThreads, out int iocpThreads)
        {
            dotnet.GetMaxThreads(out workerThreads, out iocpThreads);
        }
        virtual public void SetMaxThreads(int workerThreads, int iocpThreads)
        {
            dotnet.SetMaxThreads(workerThreads, iocpThreads);
        }
    }
    public static class ThreadPool
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

        public static bool QueueUserWorkItem(WaitCallback callback)
        {
            return Impl.QueueUserWorkItem(callback);
        }
        public static bool QueueUserWorkItem(WaitCallback callback, object state)
        {
            return Impl.QueueUserWorkItem(callback, state);
        }
        public static System.Threading.ManualResetEvent WaitableQueueUserWorkItem(WaitCallback callback)
        {
            var waitObject = new System.Threading.ManualResetEvent(false);
            if (!QueueUserWorkItem((o) =>
                                      {
                                          callback(o);
                                          waitObject.Set();
                                      }))
            {
                return null;
            }
            return waitObject;
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
    /// to be executed by a NativeThread.
    /// </summary>
    public interface IThreadRunnable
    {
        /// <summary>
        /// This method has to be implemented in order that starting of the NativeThread causes the object's 
        /// run method to be called in that separately executing NativeThread.
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

    public class SafeWaitHandle : WaitHandle
    {

        public static T Safely<T>(Func<T> func)
        {
            return SafeThreadPool.Safely(func);
        }

        private static void SafelyV(Action func)
        {
            Safely(() =>
                       {
                           func();
                           return true;
                       });
        }

        private WaitHandle wrapped;
        public SafeWaitHandle(WaitHandle wrapped0)
        {
            wrapped = wrapped0;
        }
        public override void Close()
        {
            SafelyV(wrapped.Close);
        }
        public override System.Runtime.Remoting.ObjRef CreateObjRef(Type requestedType)
        {
            return Safely(() => wrapped.CreateObjRef(requestedType));
        }
        public override bool WaitOne()
        {
            return Safely(() => wrapped.WaitOne());
        }

        [Obsolete("Use the SafeWaitHandle property instead.")]
        public override IntPtr Handle
        {
            get
            {
                return Safely(() => wrapped.Handle);
            }
            set
            {
                wrapped.Handle = value;
            }
        }
        protected override void Dispose(bool explicitDisposing)
        {
            base.Dispose(explicitDisposing);
            wrapped = null;
        }
        public override bool Equals(object obj)
        {
            return Safely(() => wrapped.Equals(obj));
        }

        public override int GetHashCode()
        {
            return Safely(() => wrapped.GetHashCode());
        }
        public override bool WaitOne(int millisecondsTimeout)
        {
            return Safely(() => wrapped.WaitOne(millisecondsTimeout));
        }
        public override object InitializeLifetimeService()
        {
            return Safely(() => wrapped.InitializeLifetimeService());
        }
        public override string ToString()
        {
            return Safely(() => wrapped.ToString());
        }
        public override bool WaitOne(int millisecondsTimeout, bool exitContext)
        {
            return Safely(() => wrapped.WaitOne(millisecondsTimeout, exitContext));
        }
        public override bool WaitOne(TimeSpan timeout)
        {
            return Safely(() => wrapped.WaitOne(timeout));
        }
        public override bool WaitOne(TimeSpan timeout, bool exitContext)
        {
            return Safely(() => wrapped.WaitOne(timeout, exitContext));
        }
    }
}
