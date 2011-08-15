REM Targets available in Prebuild: autotools,makefile,monodev,nant,sharpdev,sharpdev2,vs2002,vs2003,vs2005,vs2008,xcode
SET PREBUILDTARGET=%1

REM IF NO COMAND LINE ARG IS SPECIFIED use Vsiaul Studio 2008
IF "%1"=="" SET PREBUILDTARGET=vs2008

xcopy /f /y /c sources\external\Radegast\assemblies\*.* bin\
xcopy /f /y /c sources\external\Radegast\*.dll bin\
xcopy /f /y /c sources\external\Radegast\*.pdb bin\
xcopy /f /y /c sources\external\Radegast\*.config bin\
xcopy /f /y /c sources\external\Radegast\*.so bin\
xcopy /f /y /c sources\external\Radegast\*.dylib bin\
del sources\external\LookingGlass-svn\bin\Prebuild.exe
del sources\external\LookingGlass-svn\bin\HttpServer.dll
del sources\external\LookingGlass-svn\bin\PrimMesher.dll
del sources\external\LookingGlass-svn\bin\Radegast.exe
del sources\external\LookingGlass-svn\bin\OpenMetav*.*
xcopy /f /y /c sources\external\LookingGlass-svn\bin\*.* bin\
REM from cygwin
svn revert bin/*.dll
svn revert bin/*.exe
svn revert bin/*.pdb
svn revert bin/*.so

del bin\swicli.dll

mkdir bin\character
xcopy /f /y /c sources\external\Radegast\character bin\character

mkdir bin\shader_data
xcopy /f /y /c sources\external\Radegast\shader_data bin\shader_data

move XML.dll XML.dll.XGLoreWindowsOnly
move GraphvizDot.dll GraphvizDot.dll.XGLoreWindowsOnly

xcopy /f /y /c sources\external\LAIR.ResourceAPIs\PennBank\TreeBankGrapher\bin\Release\LAIR.Misc.dll bin\


bin\Prebuild.exe  prebuild.xml /target %PREBUILDTARGET%

del Cogbot.sln

pause

exit /b 0

@REM REST OF THESE ARE FOR IKVM REBUILDIGN STUFF 

del "c:\Program Files (x86)\pl\bin\SWIPlCs.*"
del "c:\Program Files (x86)\pl\bin\SWIJPL.*"
del "bin\SWIPlCs.*"
del "bin\SWIJPL.*"


xcopy /f /y /c sources\external\ikvm\bin-x86\*.* sources\external\ikvm\bin\
xcopy /f /y /c sources\external\ikvm\bin\*.* bin\
xcopy /f /y /c sources\external\ikvm\bin\*.* "C:\Program Files (x86)\pl\bin"
xcopy /f /y /c sources\external\ikvm\bin\*.* sources\external\ABCL.Net\bin\

dir /s IKVM.OpenJDK.ClassLibrary.dll

SET IKVMVER=0.43.3817.0
SET SWI_PROLOG_HOME="C:\Program Files (x86)\pl"
REM  -classloader:ikvm.runtime.ClassPathAssemblyClassLoader 
SET IKVMOPTIONS=-sharedclassloader -time -debug -compressresources -version:%IKVMVER%

del Opencyc.dll /s


bin\ikvmc.exe %IKVMOPTIONS% -target:library bin\OpenCyc.jar
move OpenCyc.* bin\

bin\ikvmc.exe %IKVMOPTIONS% -target:library bin\jnaerator-0.9.3.jar
move jnaerator-0.9.3.* bin\

bin\ikvmc.exe %IKVMOPTIONS% -target:library bin\bsh-2.0b4.jar
move bsh-2.0b4.* bin\


@REM copy C:\development\opensim4opencog\lib\jpl\src\java\jpl.jar %SWI_PROLOG_HOME%\lib\


bin\ikvmc %IKVMOPTIONS% -target:library -out:SWIJPL.dll %SWI_PROLOG_HOME%\lib\jpl.jar

bin\ikvmstub SWIJPL.dll -lib:bin\

move /y SWIJPL.* bin\ 



xcopy /y /f SWIJPL.dll %SWI_PROLOG_HOME%\lib\
xcopy /y /f SWIJPL.dll %SWI_PROLOG_HOME%\bin\
