// DummyLibraryUsage.cpp: определяет точку входа для консольного приложения.
//

#include "stdafx.h"

//#define PROLOG_MODULE "math"
//#include <windows.h>
#include "SWI-Prolog.h"

//#include <_vcclrit.h>

//using namespace SbsSW::SwiPlCs; // Process class lives here



/*


install_t
install_mylib()
{
}


 void __cdecl load_install_swicli()
{
	System::Object^ theDLL = gcnew System::Object();
	SbsSW::SwiPlCs::swipl_win::install();
	wprintf(L"Hello from unmanaged code!\n");
}
*/

extern "C" {

	/// Assembly + ClassName + StaticMethodName
	/// Such as  ?- cli_load_lib('SwiPlCs.dll','SbsSW.SwiPlCs.swipl_win','install').
	static foreign_t  cli_load_lib(term_t aname, term_t cname, term_t mname) 	
	{ 
		char *anamestr;
		char *cnamestr;
		char *mnamestr;
		if ( PL_get_atom_chars(aname, &anamestr) && PL_get_atom_chars(cname, &cnamestr) && PL_get_atom_chars(mname, &mnamestr) )
		{
			System::Reflection::Assembly^ assembly = System::Reflection::Assembly::Load(gcnew System::String(anamestr));
			System::Type^ type = assembly->GetType(gcnew System::String(cnamestr));
			System::Reflection::MethodInfo^ method = type->GetMethod(gcnew System::String(mnamestr));
			method->Invoke(nullptr, gcnew cli::array<System::Object^,1>(0));
			PL_succeed;
		}
		PL_fail;
	}
	__declspec( dllexport ) void install()
	{
		PL_register_foreign("cli_load_lib", 3, cli_load_lib, 0);
		//SbsSW::SwiPlCs::swipl_win::install();
	}
	//int loadFramework

	/*
	int main(int argc, _TCHAR* argv[])
	{
	load_install_swicli();
	return 0;
	}
	*/
}

