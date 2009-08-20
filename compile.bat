@echo off 
call runprebuild2008.bat
C:\WINDOWS\Microsoft.NET\Framework\v3.5\msbuild Cogbot.sln 
:SUCCESS 
echo Build Successful! 
pause
exit /B 0 
:FAIL 
echo Build Failed, check log for reason 
pause
exit /B 1 
