REM Targets available in Prebuild: autotools,makefile,monodev,nant,sharpdev,sharpdev2,vs2002,vs2003,vs2005,vs2008,xcode
SET PREBUILDTARGET=%1

REM IF NO COMAND LINE ARG IS SPECIFIED use Vsiaul Studio 2008
IF "%1"=="" SET PREBUILDTARGET=vs2008

bin\Prebuild.exe  prebuild.xml /target %PREBUILDTARGET%

copy lib\Radegast\assemblies\*.* bin\
copy lib\Radegast\*.dll bin\
copy lib\Radegast\*.pdb bin\
copy lib\Radegast\*.config bin\
copy lib\Radegast\*.so bin\
copy lib\Radegast\*.dylib bin\
del lib\LookingGlass-svn\bin\Prebuild.exe
del lib\LookingGlass-svn\bin\HttpServer.dll
del lib\LookingGlass-svn\bin\PrimMesher.dll
del lib\LookingGlass-svn\bin\Radegast.exe
del lib\LookingGlass-svn\bin\OpenMetav*.*
copy lib\LookingGlass-svn\bin\*.* bin\
pause

exit /b 0

@REM REST OF THESE ARE FOR IKVM REBUILDIGN STUFF 

del "c:\Program Files (x86)\pl\bin\SWIPlCs.*"
del "c:\Program Files (x86)\pl\bin\SWIJPL.*"
del "bin\SWIPlCs.*"
del "bin\SWIJPL.*"

xcopy /f /y lib\ikvm\bin-x86\*.* lib\ikvm\bin\
xcopy /f /y lib\ikvm\bin\*.* bin\
xcopy /f /y lib\ikvm\bin\*.* "C:\Program Files (x86)\pl\bin"
xcopy /f /y lib\ikvm\bin\*.* lib\ABCL.Net\bin\

dir /s IKVM.OpenJDK.ClassLibrary.dll

SET IKVMVER=0.43.3817.0
SET SWI_PROLOG_HOME="C:\Program Files (x86)\pl"
REM  -classloader:ikvm.runtime.ClassPathAssemblyClassLoader 
SET IKVMOPTIONS=-sharedclassloader -time -debug -compressresources -version:%IKVMVER%

del Opencyc.dll /s


bin\ikvmc.exe %IKVMOPTIONS% -target:library bin\OpenCyc.jar
move OpenCyc.dll bin\

@REM copy C:\development\opensim4opencog\lib\jpl\src\java\jpl.jar %SWI_PROLOG_HOME%\lib\


bin\ikvmc %IKVMOPTIONS% -target:library -out:SWIJPL.dll %SWI_PROLOG_HOME%\lib\jpl.jar

bin\ikvmstub SWIJPL.dll -lib:bin\

move /y SWIJPL.* bin\ 



xcopy /y /f SWIJPL.dll %SWI_PROLOG_HOME%\lib\
xcopy /y /f SWIJPL.dll %SWI_PROLOG_HOME%\bin\
