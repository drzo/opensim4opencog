@echo off 
call runprebuild.bat
C:\WINDOWS\Microsoft.NET\Framework\v3.5\msbuild Cogbot_VS9.sln 
:SUCCESS 
echo Build Successful! 
pause
exit /B 0 
:FAIL 
echo Build Failed, check log for reason 
pause
exit /B 1 
