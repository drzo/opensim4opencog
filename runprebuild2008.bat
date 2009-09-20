REM Targets available in Prebuild: autotools,makefile,monodev,nant,sharpdev,sharpdev2,vs2002,vs2003,vs2005,vs2008,xcode
bin\Prebuild.exe  prebuild.xml /target vs2008
copy lib\Radegast\assemblies\*.dll bin\
copy lib\Radegast\*.dll bin\
copy lib\Radegast\*.pdb bin\
copy lib\Radegast\*.config bin\
copy lib\Radegast\*.so bin\
copy lib\Radegast\*.dylib bin\
del lib\LookingGlass-svn\bin\Prebuild.exe
del lib\LookingGlass-svn\bin\HttpServer.dll
del lib\LookingGlass-svn\bin\PrimMesher.dll
del lib\LookingGlass-svn\bin\Radegast.exe
copy lib\LookingGlass-svn\bin\*.* bin\
pause