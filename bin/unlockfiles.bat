@echo  sometime Visual Studio while debuggign locks files.. this batch file unlocks them all

mkdir deletables\
mkdir deletables\foo\
del /F /Q deletables\foo\*.*

move /y deletables\*.* deletables\foo\

del /F /Q deletables\foo\*.*
del /F /Q deletables\*.*

mkdir deletables\
mkdir deletables\foo

SET FILEMASK=*.exe *.dll *.pdb


@echo ignore anthing that happened above!
@echo  START Trying to move local files

move /y %FILEMASK% deletables\


@echo END Trying to move local files

SET XCO=xcopy /R /H /C /Y /Q /X

@echo START copy locked files (Again)

%XCO% *.exe  deletables\
%XCO% *.pdb  deletables\
%XCO% *.dll  deletables\

@echo END copy locked files (Again)

@echo . 
@echo .
@echo .

@echo START restore local files

%XCO% deletables\*.* .\
%XCO% ..\bin-chatbot\*.* .\
svn update

@echo END restore local files
