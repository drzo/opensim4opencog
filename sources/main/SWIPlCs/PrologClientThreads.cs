using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Threading;
#if USE_IKVM
using IKVM.Internal;
using ikvm.runtime;
using java.net;
using jpl;
#endif
using SbsSW.SwiPlCs.Callback;
using SbsSW.SwiPlCs.Exceptions;
using SbsSW.SwiPlCs.Streams;
using System.Windows.Forms;
#if USE_IKVM
using Hashtable = java.util.Hashtable;
using ClassLoader = java.lang.ClassLoader;
using Class = java.lang.Class;
using sun.reflect.misc;
#endif
using CycFort = SbsSW.SwiPlCs.PlTerm;
using PrologCli = SbsSW.SwiPlCs.PrologClient;

namespace SbsSW.SwiPlCs
{
    public partial class PrologClient
    {
        public static Dictionary<Thread, int> ThreadRegisterations = new Dictionary<Thread, int>();

        internal static void IncrementUseCount(Thread thread)
        {
            lock (ThreadRegisterations)
            {
                int regs;
                if (!ThreadRegisterations.TryGetValue(thread, out regs))
                {
                    ThreadRegisterations[thread] = 1;
                }
                else
                {
                    ThreadRegisterations[thread] = regs + 1;
                }
            }
        }
        internal static void DecrementUseCount(Thread thread)
        {
            lock (ThreadRegisterations)
            {
                int regs;
                if (!ThreadRegisterations.TryGetValue(thread, out regs))
                {
                    ThreadRegisterations[thread] = 0;
                }
                else
                {
                    ThreadRegisterations[thread] = regs - 1;
                }
            }
        }



        public static Dictionary<Thread, IntPtr> SafeThreads = new Dictionary<Thread, IntPtr>();
        public static Dictionary<int, Thread> engineToThread = new Dictionary<int, Thread>();
        public static Dictionary<int, int> threadToEngine = new Dictionary<int, int>();


        public static Dictionary<int, PlMtEngine> ThreadEngines = new Dictionary<int, PlMtEngine>();
        public static List<IntPtr> FreeEngines = new List<IntPtr>();
        public static bool UseEnginePool = true;

        public static void RegisterMainThread()
        {
            PingThreadFactories();
            lock (SafeThreads)
            {
                Application.ThreadExit += new EventHandler(OnThreadExit);
                var t = Thread.CurrentThread;
                SafeThreads.Add(t, IntPtr.Zero);
                int self = libpl.PL_thread_self();
                engineToThread.Add(self, t);
            }
        }

        public static bool NoTestThreadFActory = false;
        public static void PingThreadFactories()
        {
            try
            {

                if (NoTestThreadFActory) return;
                Assembly assem = AssemblyLoad("MushDLR223");
                if (assem != null)
                {
                    Type type = assem.GetType("MushDLR223.Utilities.SafeThread");
                    if (type != null)
                    {
                        NoTestThreadFActory = true;
                        type.GetEvent("ThreadAdded").GetAddMethod().Invoke(null,
                                                                           new[] { new Action<Thread>(RegisterThread) });
                        type.GetEvent("ThreadRemoved").GetAddMethod().Invoke(null,
                                                                             new[] { new Action<Thread>(DeregisterThread) });
                    }
                }
            }
            catch (Exception)
            {
            }
        }

        private static void OnThreadExit(object sender, EventArgs e)
        {

        }
        public static void RegisterCurrentThread()
        {
            RegisterThread(Thread.CurrentThread);
        }


        public static bool OneToOneEnginesPeThread = true;


        public static void RegisterThread(Thread thread)
        {
            if (thread == CreatorThread) return;
            if (OneToOneEnginesPeThread)
            {
                // leaks!
                RegisterThread121(thread);
            }
            else
            {
                RegisterThread12Many(thread);
            }
        }

        public static void RegisterThread121(Thread thread)
        {
            if (thread == CreatorThread) return;
            lock (SafeThreads)
            {
                int oldSelf;
                bool threadHasSelf = threadToEngine.TryGetValue(thread.ManagedThreadId, out oldSelf);
                if (threadHasSelf)
                {
                    return;
                }
                //if (thread == CreatorThread) return;
                IncrementUseCount(thread);
                lock (unregisteredThreads) unregisteredThreads.Remove(thread);
                //libpl.PL_thread_attach_engine(IntPtr.Zero);
                threadToEngine[thread.ManagedThreadId] = libpl.PL_thread_attach_engine(IntPtr.Zero);
                if (threadToEngine.Count % 20 == 0)
                {


                }
            }
        }

        public static void RegisterThread121Leak(Thread thread)
        {
            lock (SafeThreads)
            {
                IncrementUseCount(thread);
                lock (unregisteredThreads) unregisteredThreads.Remove(thread);
                int self = libpl.PL_thread_self();
                int oldSelf;
                Thread otherThread;
                bool plthreadHasThread = engineToThread.TryGetValue(self, out otherThread);
                bool threadHasSelf = threadToEngine.TryGetValue(thread.ManagedThreadId, out oldSelf);
                bool plThreadHasDifferntThread = false;
                GCHandle.Alloc(thread, GCHandleType.Normal);
                if (plthreadHasThread)
                {
                    plThreadHasDifferntThread = otherThread != thread;
                }
                if (threadHasSelf)
                {
                    if (self < 1)
                    {
                        Debug("self < 1: " + thread);
                        return; //maybe mnot fine                       
                    }
                    if (plThreadHasDifferntThread)
                    {
                        Debug("plThreadHasDifferntThread " + thread);
                        return; //maybe mnot fine       
                    }
                    if (thread == CreatorThread) return;
                    int ret0 = libpl.PL_thread_attach_engine(IntPtr.Zero);
                    //int iRet = CheckEngine();
                    return; // all was fine;
                }
                // thread never had engine
                int ret = libpl.PL_thread_attach_engine(IntPtr.Zero);
                int self0 = libpl.PL_thread_self();
                engineToThread[self0] = thread;
                threadToEngine[thread.ManagedThreadId] = self0;
                RegisterThread121Leak(thread);
                return;
            }
        }

        /// <summary>
        /// FIX ME!!
        /// </summary>
        /// <param name="thread"></param>
        public static void RegisterThread12Many(Thread thread)
        {
            lock (SafeThreads)
            {
                IncrementUseCount(thread);
                lock (unregisteredThreads) unregisteredThreads.Remove(thread);
                PlMtEngine oldSelf;
                if (ThreadEngines.TryGetValue(thread.ManagedThreadId, out oldSelf))
                {
                    oldSelf.PlSetEngine();
                    return;
                }
                try
                {
                    //var _iEngineNumber = libpl.PL_create_engine(IntPtr.Zero);
                    oldSelf = new PlMtEngine();
                    oldSelf.PlSetEngine();
                    ThreadEngines.Add(thread.ManagedThreadId, oldSelf);
                }
                catch (Exception)
                {                    
                    throw;
                }
            }
        }

        public static void RegisterThreadOrig(Thread thread)
        {
            lock (SafeThreads)
            {
                int regs;
                if (!ThreadRegisterations.TryGetValue(thread, out regs))
                {
                    ThreadRegisterations[thread] = 1;
                }
                else
                {
                    ThreadRegisterations[thread] = regs + 1;
                }
                lock (unregisteredThreads) unregisteredThreads.Remove(thread);
                int self = libpl.PL_thread_self();
                IntPtr _iEngineNumber;
                IntPtr _oiEngineNumber;
                Thread otherThread;
                bool threadOnceHadEngine = SafeThreads.TryGetValue(thread, out _iEngineNumber);
                bool plthreadHasThread = engineToThread.TryGetValue(self, out otherThread);
                bool plThreadHasDifferntThread = false;
                GCHandle.Alloc(thread, GCHandleType.Normal);
                if (plthreadHasThread)
                {
                    plThreadHasDifferntThread = otherThread != thread;
                }
                if (self < 0 || threadOnceHadEngine)
                {
                    if (self < 1)
                    {
                        Debug("self < 1: " + thread);
                        return; //maybe mnot fine                       
                    }
                    if (plThreadHasDifferntThread)
                    {
                        Debug("plThreadHasDifferntThread " + thread);
                        return; //maybe mnot fine       
                    }
                    //return; // all was fine;
                    //  if (thread == CreatorThread || true) return;

                    int iRet = CheckEngine();

                    return; // all was fine;
                }
                else
                {
                    // thread never had engine
                    int ret = libpl.PL_thread_attach_engine(IntPtr.Zero);
                    int self0 = libpl.PL_thread_self();
                    if (ret == self0)
                    {
                        SafeThreads.Add(thread, IntPtr.Zero);
                        engineToThread[self0] = thread;
                        //RegisterThread(thread);
                        return;
                    }
                    _iEngineNumber = GetFreeEngine();
                    SafeThreads.Add(thread, _iEngineNumber);
                    int self2 = libpl.PL_thread_self();
                    if (self2 == -1)
                    {
                        if (libpl.PL_is_initialised(IntPtr.Zero, IntPtr.Zero) == libpl.PL_fail)
                        {
                            try
                            {
                                ret = libpl.PL_thread_attach_engine(_iEngineNumber);
                                int self3 = libpl.PL_thread_self();
                                engineToThread.Add(self3, thread);
                                return;
                            }
                            catch (Exception ex)
                            {
                                throw (new PlException("PL_create_engine : " + ex.Message));
                            }
                        }
                        else
                        {
                            //int ret = libpl.PL_thread_attach_engine(_iEngineNumber);
                            IntPtr pNullPointer = IntPtr.Zero;
                            int iRet = libpl.PL_set_engine(_iEngineNumber, ref pNullPointer);
                            switch (iRet)
                            {
                                case libpl.PL_ENGINE_SET:
                                    {
                                        int self4 = libpl.PL_thread_self();
                                        engineToThread.Add(self4, thread);
                                        return; // all is fine!
                                    }
                                case libpl.PL_ENGINE_INVAL: throw (new PlLibException("PlSetEngine returns Invalid")); //break;
                                case libpl.PL_ENGINE_INUSE: throw (new PlLibException("PlSetEngine returns it is used by an other thread")); //break;
                                default: throw (new PlLibException("Unknown return from PlSetEngine"));
                            }
                            int self3 = libpl.PL_thread_self();
                            engineToThread.Add(self3, thread);
                        }
                        return;
                    }

                    engineToThread.Add(self2, thread);
                }
                /*
                //
                if (self != ret)
                {
                    engineToThread[ret] = thread;
                }

                if (engineToThread.TryGetValue(self, out otherThread))
                {
                    // All good!
                    if (otherThread == thread)
                        return;
                    bool othreadOnceHadEngine = SafeThreads.TryGetValue(otherThread, out _oiEngineNumber);
                    int ret = libpl.PL_thread_attach_engine(_iEngineNumber);
                    if (self != ret)
                    {
                        engineToThread[ret] = thread;
                        //what does this mean?
                        SafeThreads.TryGetValue(thread, out _iEngineNumber);
                    }
                }
                libpl.PL_set_engine(libpl.PL_ENGINE_CURRENT, ref oldEngine);
                if (!OneToOneEnginesPeThread)
                {
                }
                SafeThreads.Add(thread, _iEngineNumber);
                  */
            }
        }

        private static IntPtr GetFreeEngine()
        {
            lock (FreeEngines)
            {
                if (FreeEngines.Count > 0)
                {
                    var fe = FreeEngines[0];
                    FreeEngines.RemoveAt(0);
                    return fe;
                }
            }
            return libpl.PL_create_engine(IntPtr.Zero);
        }

        private static void Debug(object plthreadhasdifferntthread)
        {

        }

        public static int CheckEngine()
        {
            IntPtr _iEngineNumber;
            IntPtr pNullPointer = IntPtr.Zero;
            IntPtr PL_ENGINE_CURRENT_PTR = new IntPtr(libpl.PL_ENGINE_CURRENT); // ((PL_engine_t)0x2)
            int iRet = libpl.PL_set_engine(PL_ENGINE_CURRENT_PTR, ref pNullPointer);
            if (libpl.PL_ENGINE_SET == iRet) return iRet;
            switch (iRet)
            {
                case libpl.PL_ENGINE_SET:
                    {
                        break; // all is fine!
                    }
                case libpl.PL_ENGINE_INVAL:
                    throw (new PlLibException("PlSetEngine returns Invalid")); //break;
                case libpl.PL_ENGINE_INUSE:
                    throw (new PlLibException("PlSetEngine returns it is used by an other thread")); //break;
                default:
                    throw (new PlLibException("Unknown return from PlSetEngine"));
            }

            return iRet;
        }
        public static int CheckEngine(IntPtr PL_ENGINE_CURRENT_PTR)
        {
            IntPtr _iEngineNumber;
            IntPtr pNullPointer = IntPtr.Zero;
            int iRet = libpl.PL_set_engine(PL_ENGINE_CURRENT_PTR, ref pNullPointer);
            if (libpl.PL_ENGINE_SET == iRet) return iRet;
            switch (iRet)
            {
                case libpl.PL_ENGINE_SET:
                    {
                        break; // all is fine!
                    }
                case libpl.PL_ENGINE_INVAL:
                    throw (new PlLibException("PlSetEngine returns Invalid")); //break;
                case libpl.PL_ENGINE_INUSE:
                    throw (new PlLibException("PlSetEngine returns it is used by an other thread")); //break;
                default:
                    throw (new PlLibException("Unknown return from PlSetEngine"));
            }

            return iRet;
        }

        static readonly List<Thread> unregisteredThreads = new List<Thread>();
        public static void DeregisterThread(Thread thread)
        {
            lock (SafeThreads)
            {
                lock (unregisteredThreads) unregisteredThreads.Add(thread);
                int regs = -1;
                if (!ThreadRegisterations.TryGetValue(thread, out regs))
                {
                    ThreadRegisterations[thread] = 0;
                }
                else
                {
                    ThreadRegisterations[thread] = regs - 1;
                }
                if (regs == 1)
                {
                    if (OneToOneEnginesPeThread)
                    {
                      //  libpl.PL_thread_destroy_engine();
                    }
                    else
                    {

                    }
                    //ExitThread(thread);
                }
            }
        }

        public static void ExitThread(Thread thread)
        {
            lock (SafeThreads)
            {
                int self = libpl.PL_thread_self();
                IntPtr _iEngineNumber;
                if (!SafeThreads.TryGetValue(thread, out _iEngineNumber))
                {
                    return;
                }
                //  if (_iEngineNumber == IntPtr.Zero) return;
                SafeThreads.Remove(thread);
                var rnull = IntPtr.Zero;
                if (libpl.PL_set_engine(IntPtr.Zero, ref rnull) != 0)
                {
                    lock (FreeEngines)
                    {
                        if (_iEngineNumber != IntPtr.Zero) FreeEngines.Add(_iEngineNumber);
                    }
                    return;
                }
                if (libpl.PL_destroy_engine(_iEngineNumber) != 0)
                {
                    try
                    {
                        _iEngineNumber = libpl.PL_create_engine(IntPtr.Zero);
                        lock (FreeEngines)
                        {
                            FreeEngines.Add(_iEngineNumber);
                        }
                    }
                    catch (Exception ex)
                    {
                        throw (new PlException("PL_create_engine : " + ex.Message));
                    }
                }
            }
        }

        private static void Thread_Exit(object sender, EventArgs e)
        {

        }
    }
}