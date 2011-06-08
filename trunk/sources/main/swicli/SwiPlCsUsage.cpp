// DummyLibraryUsage.cpp: определяет точку входа для консольного приложения.
//

#include "stdafx.h"

//#define PROLOG_MODULE "math"
//#include <windows.h>
//#include <SWI-Prolog.h>

//#include <_vcclrit.h>

//using namespace SbsSW::SwiPlCs; // Process class lives here



/*
static foreign_t
pl_say_hello(term_t to)
{ char *a;

  if ( PL_get_atom_chars(to, &a) )
  { MessageBox(NULL, a, "DLL test", MB_OK|MB_TASKMODAL);

    PL_succeed;
  }

  PL_fail;
}

install_t
install_mylib()
{ PL_register_foreign("say_hello", 1, pl_say_hello, 0);
}


 void __cdecl load_install_swicli()
{
	System::Object^ theDLL = gcnew System::Object();
	SbsSW::SwiPlCs::swipl_win::install();
	wprintf(L"Hello from unmanaged code!\n");
}
*/

extern "C" {
	__declspec( dllexport ) void install()
	{
		SbsSW::SwiPlCs::swipl_win::install();
	}
	/*
	int main(int argc, _TCHAR* argv[])
	{
	load_install_swicli();
	return 0;
	}
	*/
}

