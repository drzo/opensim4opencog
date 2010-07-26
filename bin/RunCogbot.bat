del *.bak /s

setlocal
cd %CD%
if NOT EXIST ABuildStartup.exe jar x Built.zip
cmd /c ABuildStartup.exe --httpd --aiml BinaBot Daxeline
endlocal
