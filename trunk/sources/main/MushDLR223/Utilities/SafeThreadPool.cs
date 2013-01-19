using System;
using SThread = System.Threading.Thread;
using SMonitor = System.Threading.Monitor;
using SThreadPool = System.Threading.ThreadPool;
using RegisteredWaitHandle = System.Threading.RegisteredWaitHandle;
using WaitCallback = System.Threading.WaitCallback;
using WaitHandle = System.Threading.WaitHandle;
using WaitOrTimerCallback = System.Threading.WaitOrTimerCallback;

namespace ThreadPoolUtil
{
    public class SafeThreadPool : IThreadPool
    {

        public static void Issue(SThread thread, Exception e)
        {
            Console.Error.WriteLine("Thread issue " + thread + " had " + e);
        }

        public static WaitCallback AddSafety(WaitCallback callback)
        {
            return ((o) =>
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

        public static WaitOrTimerCallback AddSafetyTC(WaitOrTimerCallback callback)
        {
            return ((o, b) =>
            {
                try
                {
                    callback(o, b);
                }
                catch (Exception e)
                {
                    Issue(SThread.CurrentThread, e);
                }
            });
        }

        public static WaitHandle AddSafetyWH(WaitHandle handle)
        {
            return handle;
        }

        virtual public RegisteredWaitHandle RegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, TimeSpan timeOutInterval, bool executeOnlyOnce)
        {
            return SThreadPool.RegisterWaitForSingleObject(AddSafetyWH(waitObject), AddSafetyTC(callback), state, timeOutInterval, executeOnlyOnce);
        }
        virtual public RegisteredWaitHandle RegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, long timeOutInterval, bool executeOnlyOnce)
        {
            return SThreadPool.RegisterWaitForSingleObject(AddSafetyWH(waitObject), AddSafetyTC(callback), state, timeOutInterval, executeOnlyOnce);
        }

        virtual public RegisteredWaitHandle UnsafeRegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, TimeSpan timeOutInterval, bool executeOnlyOnce)
        {
            return SThreadPool.UnsafeRegisterWaitForSingleObject(AddSafetyWH(waitObject), AddSafetyTC(callback), state, timeOutInterval, executeOnlyOnce);
        }
        virtual public RegisteredWaitHandle UnsafeRegisterWaitForSingleObject(WaitHandle waitObject, WaitOrTimerCallback callback, object state, long timeOutInterval, bool executeOnlyOnce)
        {
            return SThreadPool.UnsafeRegisterWaitForSingleObject(AddSafetyWH(waitObject), AddSafetyTC(callback), state, timeOutInterval, executeOnlyOnce);
        }

        virtual public bool QueueUserWorkItem(WaitCallback callback)
        {
            return ThreadPool.DotnetImpl.QueueUserWorkItem(AddSafety(callback));
        }
        virtual public bool QueueUserWorkItem(WaitCallback callback, object state)
        {
            return ThreadPool.DotnetImpl.QueueUserWorkItem(AddSafety(callback), state);
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

    public class SafeWaitHandle : WaitHandle
    {
        private WaitHandle wrapped;
        public SafeWaitHandle(WaitHandle wrapped0)
        {
            wrapped = wrapped0;
        }
        public override void Close()
        {
            wrapped.Close();
        }
        public override System.Runtime.Remoting.ObjRef CreateObjRef(Type requestedType)
        {
            return wrapped.CreateObjRef(requestedType);
        }
        public override bool WaitOne()
        {
            return wrapped.WaitOne();
        }
        public override IntPtr Handle
        {
            get
            {
                return wrapped.Handle;
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
            return wrapped.Equals(obj);
        }
        public override int GetHashCode()
        {
            return wrapped.GetHashCode();
        }
        public override bool WaitOne(int millisecondsTimeout)
        {
            return wrapped.WaitOne(millisecondsTimeout);
        }
        public override object InitializeLifetimeService()
        {
            return wrapped.InitializeLifetimeService();
        }
        public override string ToString()
        {
            return wrapped.ToString();
        }
        public override bool WaitOne(int millisecondsTimeout, bool exitContext)
        {
            return wrapped.WaitOne(millisecondsTimeout, exitContext);
        }
        public override bool WaitOne(TimeSpan timeout)
        {
            return wrapped.WaitOne(timeout);
        }
        public override bool WaitOne(TimeSpan timeout, bool exitContext)
        {
            return wrapped.WaitOne(timeout, exitContext);
        }
    }
}
