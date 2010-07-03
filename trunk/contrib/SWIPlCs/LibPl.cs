/*********************************************************
* 
*  Author:        Uwe Lesta
*  Copyright (C): 2008, Uwe Lesta SBS-Softwaresysteme GmbH
*
*  This library is free software; you can redistribute it and/or
*  modify it under the terms of the GNU Lesser General Public
*  License as published by the Free Software Foundation; either
*  version 2.1 of the License, or (at your option) any later version.
*
*  This library is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  Lesser General Public License for more details.
*
*  You should have received a copy of the GNU Lesser General Public
*  License along with this library; if not, write to the Free Software
*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*********************************************************/

using System;
using System.Collections.Generic;
using System.Text;
using SbsSW.SwiPlCs.Exceptions;         // in PlHalt

using System.Runtime.InteropServices;	// marscall

// ReSharper disable InconsistentNaming
namespace SbsSW.SwiPlCs
{
    /**********************************
    * Wrapper libpl(cs) - DllFileName - D:\\Lesta\\swi-pl\\pl\\bin\\LibPlD.dll *
    **********************************/
    #region private Wraper classe libpl - MyLibPl mit constanten
    public static class libpl
    {

        public const int PL_Q_NORMAL = 0x02;
        public const int PL_Q_NODEBUG = 0x04;
        public const int PL_Q_CATCH_EXCEPTION = 0x08;
        public const int PL_Q_PASS_EXCEPTION = 0x10;
        /*******************************
         *	    CHAR BUFFERS	        *
         *	    from include/SWI-Prolog.h
         *******************************/
        public const int CVT_ATOM = 0x0001;
        public const int CVT_STRING = 0x0002;
        public const int CVT_LIST = 0x0004;
        public const int CVT_INTEGER = 0x0008;
        public const int CVT_FLOAT = 0x0010;
        public const int CVT_VARIABLE = 0x0020;
        public const int CVT_NUMBER = (CVT_INTEGER | CVT_FLOAT);
        public const int CVT_ATOMIC = (CVT_NUMBER | CVT_ATOM | CVT_STRING);
        public const int CVT_WRITE = 0x0040;		// as of version 3.2.10
        public const int CVT_WRITE_CANONICAL = 0x0080;		// As CVT_WRITE, but use write_canonical/2. 
        public const int CVT_ALL = (CVT_ATOMIC | CVT_LIST);
        public const int CVT_MASK = 0x00ff;

        public const int BUF_DISCARDABLE = 0x0000;
        public const int BUF_RING = 0x0100;
        public const int BUF_MALLOC = 0x0200;

        public const int REP_ISO_LATIN_1 = 0x0000;		/* output representation */
        public const int REP_UTF8 = 0x1000;
        public const int REP_MB = 0x2000;

        //	 ENGINES (MT-ONLY)
        public const int PL_ENGINE_MAIN = 0x1;			//	  ((PL_engine_t)0x1)
        public const int PL_ENGINE_CURRENT = 0x2;		// ((PL_engine_t)0x2)

        public const int PL_ENGINE_SET = 0;			// engine set successfully 
        public const int PL_ENGINE_INVAL = 2;			// engine doesn'termRef exist
        public const int PL_ENGINE_INUSE = 3;			// engine is in use 

        public const int PL_fail = 0;
        public const int PL_succeed = 3;


        /////////////////////////////
        /// libpl
        ///

        #region helper for initialize and cleanUp halt

        // Unmanaged resource. CLR will ensure SafeHandles get freed, without requiring a finalizer on this class.
        static SafeLibraryHandle m_hLibrary;


        private static bool IsValid
        {
            get { return m_hLibrary != null && !m_hLibrary.IsInvalid; }
        }

        private static void LoadUnmanagedLibrary(string fileName)
        {
            if (m_hLibrary == null)
            {
                m_hLibrary = NativeMethods.LoadLibrary(fileName);
                if (m_hLibrary.IsInvalid)
                {
                    int hr = Marshal.GetHRForLastWin32Error();
                    Marshal.ThrowExceptionForHR(hr);
                }
            }
        }

        public static void UnLoadUnmanagedLibrary()
        {
            if (!m_hLibrary.IsClosed)
            {
                m_hLibrary.Close();
                m_hLibrary.UnLoad();
                m_hLibrary.Dispose();
                m_hLibrary = null;
            }
        }


        #endregion helper for initialize and cleanUp halt

        // PL_is_initialised is the *only* function which may called befor PL_initialise


        /// <summary>
        /// The standard SWI-Prolog streams ( inout output error )
        /// </summary>
        internal enum StreamsFunction
        {
            /// <summary>0 - Sread_function.</summary>
            Read = 0,
            /// <summary>1 - Swrite_function.</summary>
            Write = 1
        }


        internal static void SetStreamFunction(Streams.PlStreamType streamType, StreamsFunction functionType, Delegate function)
        {
            //int size_of_IOSTREAM = 136;

#if _PL_X64
#warning _PL_X64 is defined
            int size_of_IOSTREAM = 232;
            int offset_to_poninter_of_IOFUNCTIONS = 104;
            int size_of_pointer = 8;
#else
#warning _PL_X64 is NOT defined
            int size_of_IOSTREAM = 144;
            int offset_to_poninter_of_IOFUNCTIONS = 72;
            int size_of_pointer = 4;
#endif

            IntPtr callbackFunctionPtr = Marshal.GetFunctionPointerForDelegate(function);

            IntPtr address_std_stream_array = NativeMethods.GetProcAddress(m_hLibrary, "S__iob");
            IntPtr function_array_out = Marshal.ReadIntPtr(address_std_stream_array, (size_of_IOSTREAM * (int)streamType) + offset_to_poninter_of_IOFUNCTIONS);


#if _PL_X64
            Marshal.WriteIntPtr(new IntPtr(function_array_out.ToInt64() + (size_of_pointer * (int)functionType)), callbackFunctionPtr);
#else
            Marshal.WriteIntPtr(new IntPtr(function_array_out.ToInt32() + (size_of_pointer * (int)functionType)), callbackFunctionPtr);
#endif
        }

        internal static void LoadLibPl()
        {
            LoadUnmanagedLibrary(SafeNativeMethods.DllFileName1);
        }

        internal static int PL_initialise(int argc, String[] argv)
        {
            LoadLibPl();
            return SafeNativeMethods.PL_initialise(argc, argv);
        }


        /// <summary>
        /// Does NOT work correct if engine is_initialised
        /// int PL_is_initialised(int *argc, char ***argv) 
        /// </summary>
        /// <param name="argc"></param>
        /// <param name="argv"></param>
        /// <returns></returns>
        internal static int PL_is_initialised(ref int argc, ref String[] argv)
        {
            int iRet = 0;
            if (IsValid)
            {
                iRet = SafeNativeMethods.PL_is_initialised(ref argc, ref argv);
            }
            return iRet;
        }

        internal static int PL_is_initialised(IntPtr argc, IntPtr argv)
        {
            int iRet = 0;
            if (IsValid)
            {
                iRet = SafeNativeMethods.PL_is_initialised(argc, argv);
            }
            return iRet;
        }

        internal static int PL_halt(int i)
        {
            int iRet = 0;
            if (IsValid)
            {
                iRet = SafeNativeMethods.PL_halt(i);
                if (0 == iRet)
                    throw new PlLibException("PL_halt returned false");
                UnLoadUnmanagedLibrary();
            }
            return iRet;
        }

        internal static int PL_cleanup(int status)
        {
            int iRet = 0;
            if (IsValid)
            {
                // Only call it since PL_cleanup return void
                //iRet = SafeNativeMethods.PL_cleanup(status);
                //if (0 == iRet)
                //    throw new PlLibException("PL_cleanup returned false");
                SafeNativeMethods.PL_cleanup(status);
                UnLoadUnmanagedLibrary();
            }
            return iRet;
        }

        // see http://www.codeproject.com/KB/dotnet/Cdecl_CSharp_VB.aspx
        internal static int PL_register_foreign_in_module(string module, string name, int arity, Delegate function, int flags)
        { return SafeNativeMethods.PL_register_foreign_in_module(module, name, arity, function, flags); }


        internal static IntPtr PL_create_engine(IntPtr attr)
        { return SafeNativeMethods.PL_create_engine(attr); }

        internal static int PL_set_engine(IntPtr engine, ref IntPtr old)
        { return SafeNativeMethods.PL_set_engine(engine, ref old); }

        internal static int PL_destroy_engine(IntPtr engine)
        { return SafeNativeMethods.PL_destroy_engine(engine); }



        internal static uint PL_new_atom(string text)
        { return SafeNativeMethods.PL_new_atom(text); }

        internal static String PL_atom_chars(uint t_atom)
        {
            // see http://www.mycsharp.de/wbb2/thread.php?threadid=51100
            return Marshal.PtrToStringAnsi(SafeNativeMethods.PL_atom_chars(t_atom));
        }



        /********************************
        *         QUERY PROLOG          *
        *********************************/

        // see http://gollem.science.uva.nl/SWI-Prolog/Manual/foreigninclude.html#PL_query()

        public const int PL_QUERY_ARGC = 1;	    /* return main() argc */
        public const int PL_QUERY_ARGV = 2;	    /* return main() argv */
        /* 3: Obsolete PL_QUERY_SYMBOLFILE */
        /* 4: Obsolete PL_QUERY_ORGSYMBOLFILE*/
        public const int PL_QUERY_GETC = 5;	            /* Read character from terminal */
        public const int PL_QUERY_MAX_INTEGER = 6;	    /* largest integer */
        public const int PL_QUERY_MIN_INTEGER = 7;	    /* smallest integer */
        public const int PL_QUERY_MAX_TAGGED_INT = 8;	/* largest tagged integer */
        public const int PL_QUERY_MIN_TAGGED_INT = 9;	/* smallest tagged integer */
        public const int PL_QUERY_VERSION = 10;	        /* 207006 = 2.7.6 */
        public const int PL_QUERY_MAX_THREADS = 11;	    /* maximum thread count */
        public const int PL_QUERY_ENCODING = 12;	    /* I/O encoding */
        public const int PL_QUERY_USER_CPU = 13;	    /* User CPU in milliseconds */


        // get information from Prolog
        internal static uint PL_query(uint query_type)
        { return SafeNativeMethods.PL_query(query_type); }


        // PlFrame
        internal static uint PL_open_foreign_frame()
        { return SafeNativeMethods.PL_open_foreign_frame(); }

        internal static void PL_close_foreign_frame(uint fid_t)
        {
            if (IsValid)
                SafeNativeMethods.PL_close_foreign_frame(fid_t);
        }

        internal static void PL_rewind_foreign_frame(uint fid_t)
        { SafeNativeMethods.PL_close_foreign_frame(fid_t); }

        // record erase
        internal static uint PL_record(uint term_t)
        { return SafeNativeMethods.PL_record(term_t); }

        internal static void PL_recorded(uint record_t, [Out]uint term_t)    // term_t - ( ausgabe )
        { SafeNativeMethods.PL_recorded(record_t, term_t); }

        internal static void PL_erase(uint record_t)
        { SafeNativeMethods.PL_erase(record_t); }

        // PlQuery
        internal static int PL_next_solution(uint qid_t)
        { return SafeNativeMethods.PL_next_solution(qid_t); }

        internal static IntPtr PL_predicate(string name, int arity, string module)
        { return SafeNativeMethods.PL_predicate(name, arity, module); }

        internal static uint PL_open_query(IntPtr module, int flags, IntPtr pred, uint term)
        { return SafeNativeMethods.PL_open_query(module, flags, pred, term); }

        internal static void PL_cut_query(uint qid)
        {
            if (IsValid)
                SafeNativeMethods.PL_cut_query(qid);
        }
        internal static void PL_close_query(uint qid)
        {
            if (IsValid)
                SafeNativeMethods.PL_close_query(qid);
        }

        // PlTerm
        internal static void PL_put_atom_chars(uint term, string chars)
        { SafeNativeMethods.PL_put_atom_chars(term, chars); }

        internal static uint PL_new_term_ref()
        { return SafeNativeMethods.PL_new_term_ref(); }

        internal static void PL_put_integer(uint term, long i)
        { SafeNativeMethods.PL_put_integer(term, i); }

        internal static void PL_put_float(uint term, double i)
        { SafeNativeMethods.PL_put_float(term, i); }

        internal static void PL_put_atom(uint term, uint atom_handle)
        { SafeNativeMethods.PL_put_atom(term, atom_handle); }

        //internal static int PL_get_chars(uint term, ref string s, uint flags)
        //{ return SafeNativeMethods.PL_get_chars(term, ref s, flags); }
        internal static int PL_get_chars(uint term, ref string s, uint flags)
        {
            IntPtr ps = IntPtr.Zero;
            int iRet = SafeNativeMethods.PL_get_chars(term, ref ps, flags);
            s = Marshal.PtrToStringAnsi(ps);
            return iRet;
        }

        internal static int PL_get_long(uint term, ref int i)
        { return SafeNativeMethods.PL_get_long(term, ref i); }

        internal static int PL_get_long(uint term, ref long i)
        { return SafeNativeMethods.PL_get_long(term, ref i); }

        internal static int PL_get_float(uint term, ref double i)
        { return SafeNativeMethods.PL_get_float(term, ref i); }

        internal static int PL_get_atom(uint term, ref uint atom_t)
        { return SafeNativeMethods.PL_get_atom(term, ref atom_t); }

        internal static int PL_term_type(uint t)
        { return SafeNativeMethods.PL_term_type(t); }

        // COMPARE
        internal static int PL_compare(uint term1, uint term2)
        { return SafeNativeMethods.PL_compare(term1, term2); }



        // PlTermV
        internal static uint PL_new_term_refs(int n)
        { return SafeNativeMethods.PL_new_term_refs(n); }

        internal static void PL_put_term(uint t1, uint t2)
        { SafeNativeMethods.PL_put_term(t1, t2); }

        // PlCompound
        internal static int PL_chars_to_term(string chars, uint term)
        { return SafeNativeMethods.PL_chars_to_term(chars, term); }

        internal static void PL_cons_functor_v(uint term, uint functor_t, uint term_a0)
        { SafeNativeMethods.PL_cons_functor_v(term, functor_t, term_a0); }

        internal static uint PL_new_functor(uint atom_a, int a)
        { return SafeNativeMethods.PL_new_functor(atom_a, a); }

        internal static void PL_put_string_chars(uint term_t, string chars)
        { SafeNativeMethods.PL_put_string_chars(term_t, chars); }

        internal static void PL_put_string_nchars(uint term_t, int len, string chars)
        { SafeNativeMethods.PL_put_string_nchars(term_t, len, chars); }

        internal static void PL_put_list_codes(uint term_t, string chars)
        { SafeNativeMethods.PL_put_list_codes(term_t, chars); }

        internal static void PL_put_list_chars(uint term_t, string chars)
        { SafeNativeMethods.PL_put_list_chars(term_t, chars); }

        internal static void PL_put_list(uint term_t)
        { SafeNativeMethods.PL_put_list(term_t); }

        // Testing the type of a term
        // all return non zero if condition succeed
        internal static int PL_is_variable(uint term_t)
        { return SafeNativeMethods.PL_is_variable(term_t); }

        internal static int PL_is_ground(uint term_t)
        { return SafeNativeMethods.PL_is_ground(term_t); }

        internal static int PL_is_atom(uint term_t)
        { return SafeNativeMethods.PL_is_atom(term_t); }

        internal static int PL_is_string(uint term_t)
        { return SafeNativeMethods.PL_is_string(term_t); }

        internal static int PL_is_integer(uint term_t)
        { return SafeNativeMethods.PL_is_integer(term_t); }

        internal static int PL_is_float(uint term_t)
        { return SafeNativeMethods.PL_is_float(term_t); }

        internal static int PL_is_compound(uint term_t)
        { return SafeNativeMethods.PL_is_compound(term_t); }

        internal static int PL_is_list(uint term_t)
        { return SafeNativeMethods.PL_is_list(term_t); }

        internal static int PL_is_atomic(uint term_t)
        { return SafeNativeMethods.PL_is_atomic(term_t); }

        internal static int PL_is_number(uint term_t)
        { return SafeNativeMethods.PL_is_number(term_t); }

        // LISTS (PlTail)
        internal static uint PL_copy_term_ref(uint term_t)
        { return SafeNativeMethods.PL_copy_term_ref(term_t); }

        internal static int PL_unify_list(uint term_t_l, uint term_t_h, uint term_t_t)
        { return SafeNativeMethods.PL_unify_list(term_t_l, term_t_h, term_t_t); }

        internal static int PL_unify_nil(uint term_t)
        { return SafeNativeMethods.PL_unify_nil(term_t); }

        internal static int PL_get_list(uint term_t_l, uint term_t_h, uint term_t_t)
        { return SafeNativeMethods.PL_get_list(term_t_l, term_t_h, term_t_t); }

        internal static int PL_get_nil(uint term_t)
        { return SafeNativeMethods.PL_get_nil(term_t); }

        internal static int PL_unify(uint t1, uint t2)
        { return SafeNativeMethods.PL_unify(t1, t2); }

        internal static int PL_unify_atom_chars(uint t1, string atom)
        { return SafeNativeMethods.PL_unify_atom_chars(t1, atom); }

        internal static int PL_unify_integer(uint t1, Int32 n)
        { return SafeNativeMethods.PL_unify_integer(t1, n); }

        internal static int PL_unify_integer(uint t1, Int64 n)
        { return SafeNativeMethods.PL_unify_integer(t1, n); }

        internal static int PL_unify_float(uint t1, double n)
        { return SafeNativeMethods.PL_unify_float(t1, n); }



        // Exceptions
        // Handling exceptions
        internal static uint PL_exception(uint qid)
        { return SafeNativeMethods.PL_exception(qid); }

        internal static int PL_raise_exception(uint exception_term)
        { return SafeNativeMethods.PL_raise_exception(exception_term); }

        internal static int PL_get_arg(int index, uint t, uint a)
        { return SafeNativeMethods.PL_get_arg(index, t, a); }

        internal static int PL_get_name_arity(uint t, ref uint name, ref int arity)
        { return SafeNativeMethods.PL_get_name_arity(t, ref name, ref arity); }

        // ******************************
        // *	  PROLOG THREADS		*
        // ******************************
        internal static int PL_thread_self()
        { return SafeNativeMethods.PL_thread_self(); }

        internal static int PL_thread_attach_engine(IntPtr attr)
        { return SafeNativeMethods.PL_thread_attach_engine(attr); }

        internal static int PL_thread_destroy_engine()
        { return SafeNativeMethods.PL_thread_destroy_engine(); }


        // ******************************
        // *	  PROLOG STREAM's		*
        // ******************************

        internal static int Slinesize()
        { return SafeNativeMethods.Slinesize(); }

        internal static IntPtr S__getiob()
        { return SafeNativeMethods.S__getiob(); }


        /// <summary>
        /// 
        /// </summary>
        /// <returns> a SWI-PROLOG IOSTREAM defined in spl-stream.h</returns>
        internal static IntPtr Snew()
        { return SafeNativeMethods.Snew(IntPtr.Zero, 0, IntPtr.Zero); }


        // from pl-itf.h
        // PL_EXPORT(int)  	PL_unify_stream(term_t t, IOSTREAM *s);
        internal static int PL_unify_stream(uint term_t, IntPtr iostream)
        { return SafeNativeMethods.PL_unify_stream(term_t, iostream); }

        public const int PL_WRT_QUOTED = 0x01;	// quote atoms 
        public const int PL_WRT_IGNOREOPS = 0x02;	// ignore list/operators 
        public const int PL_WRT_NUMBERVARS = 0x04;	// print $VAR(N) as a variable
        public const int PL_WRT_PORTRAY = 0x08;	// call portray 
        public const int PL_WRT_CHARESCAPES = 0x10;	// Output ISO escape sequences 
        public const int PL_WRT_BACKQUOTED_STRING = 0x20;	// Write strings as `...` 
        // Write attributed variables 
        public const int PL_WRT_ATTVAR_IGNORE = 0x040;	// Default: just write the var 
        public const int PL_WRT_ATTVAR_DOTS = 0x080;	// Write as Var{...} 
        public const int PL_WRT_ATTVAR_WRITE = 0x100;	// Write as Var{Attributes} 
        public const int PL_WRT_ATTVAR_PORTRAY = 0x200;	// Use Module:portray_attrs/2 

        public const int PL_WRT_ATTVAR_MASK =
            (PL_WRT_ATTVAR_IGNORE |
             PL_WRT_ATTVAR_DOTS |
             PL_WRT_ATTVAR_WRITE |
             PL_WRT_ATTVAR_PORTRAY);
        /*
        

PL_EXPORT(int) PL_write_term(IOSTREAM *s,
                  term_t term,
                  int precedence,
                  int flags);
         * 
         */
        internal static int PL_write_term(IntPtr iostream, uint term_t, int precedence, int flags)
        {
            return SafeNativeMethods.PL_write_term(iostream, term_t, precedence, flags);
        }

        ///<summary>
        /// int PL_foreign_control(control_t)
        ///  Extracts the type of call from the control argument. 
        /// The return values are described above. 
        /// Note: that the function should be prepared to handle the PL_CUTTED case and 
        /// Note: should be aware that the other arguments are not valid in this case.
        ///</summary>
        ///<param name="control"></param>
        ///<returns></returns>
        public static FRG PL_foreign_control(IntPtr control)
        {
            return SafeNativeMethods.PL_foreign_control(control);
        }
        ///<summary>
        /// long PL_foreign_context(control_t)
        ///  Extracts the context from the context argument. 
        /// In the call type is PL_FIRST_CALL the context value is 0L. 
        /// Otherwise it is the value returned by the last PL_retry() associated 
        /// with this goal (both if the call type is PL_REDO as PL_CUTTED).
        ///</summary>
        ///<param name="control"></param>
        ///<returns></returns>
        public static int PL_foreign_context(IntPtr control)
        {
            return SafeNativeMethods.PL_foreign_context(control);
        }

        ///<summary>
        /// void * PL_foreign_context_address(control_t)
        ///    Extracts an address as passed in by PL_retry_address(). 
        ///</summary>
        ///<param name="control"></param>
        ///<returns></returns>
        public static IntPtr PL_foreign_context_address(IntPtr control)
        {
            return SafeNativeMethods.PL_foreign_context_address(control);
        }

        ///<summary>
        /// void PL_retry(long)
        /// The foreign function succeeds while leaving a choice point. 
        /// On backtracking over this goal the foreign function will be called again, 
        /// but the control argument now indicates it is a `Redo' call and the macro 
        /// PL_foreign_context() will return the handle passed via PL_retry(). 
        /// This handle is a 30 bits signed value (two bits are used for status indication).
        ///</summary>
        ///<param name="control"></param>
        public static void PL_retry(int control)
        {
            SafeNativeMethods._PL_retry(control);
        }

        ///<summary>
        /// void PL_retry_address(void *)
        /// As PL_retry(), but ensures an address as returned by malloc() 
        /// is correctly recovered by PL_foreign_context_address().
        ///</summary>
        ///<param name="control"></param>
        public static void PL_retry_address(IntPtr control)
        {
            SafeNativeMethods._PL_retry_address(control);
        }

        public static int PL_toplevel()
        {
            return SafeNativeMethods.PL_toplevel();
        }

        /*******************************
		 *	MEMORY ALLOCATION	*
		 *******************************/

        //PL_EXPORT(void *)	PL_malloc(size_t size);
        //PL_EXPORT(void *)	PL_realloc(void *mem, size_t size);
        //PL_EXPORT(void)		PL_free(void *mem);

    } // libpl
    #endregion

    // *********************************
    // * NON-DETERMINISTIC CALL/RETURN *
    // *********************************

    public enum FRG : int
    {
        PL_FIRST_CALL = 0,
        PL_CUTTED = 1,
        PL_REDO = 2,

    }

    struct foreign_context
    {
        IntPtr context;        /* context value */
        FRG control;   /* FRG_* action */
        IntPtr engine; /* invoking engine */
    }

} // namespace SbsSW.SwiPlCs
// ReSharper reenable InconsistentNaming
