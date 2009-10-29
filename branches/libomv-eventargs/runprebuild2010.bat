REM Targets available in Prebuild: autotools,makefile,monodev,nant,sharpdev,sharpdev2,vs2002,vs2003,vs2005,vs2008,xcode
bin\Prebuild2010.exe  prebuild.xml /target vs2010
copy lib\Radegast\assemblies\*.* bin\
copy lib\Radegast\*.dll bin\
copy lib\Radegast\*.pdb bin\
copy lib\Radegast\*.config bin\
copy lib\Radegast\*.so bin\
copy lib\Radegast\*.dylib bin\
@rem del lib\LookingGlass-svn\bin\Prebuild.exe
@rem del lib\LookingGlass-svn\bin\HttpServer.dll
@rem del lib\LookingGlass-svn\bin\PrimMesher.dll
@rem del lib\LookingGlass-svn\bin\Radegast.exe
@rem del lib\LookingGlass-svn\bin\OpenMetav*.*
@rem copy lib\LookingGlass-svn\bin\*.* bin\
pause