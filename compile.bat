@echo off 
@rem call runprebuild.bat
set dobuild="C:\WINDOWS\Microsoft.NET\Framework\v3.5\msbuild"
%dobuild% "sources\external\LibOpenMetaverse\OpenMetaverse.StructuredData\OpenMetaverse.StructuredData.csproj"
%dobuild% "sources\external\LibOpenMetaverse\OpenMetaverse\OpenMetaverse.csproj"
%dobuild% Cogbot_VS9_Pass_01.sln 
%dobuild% "sources\external\LibOpenMetaverse\OpenMetaverse.Rendering.Meshmerizer\OpenMetaverse.Rendering.Meshmerizer.csproj"
%dobuild% "sources\external\Radegast\Radegast.csproj"
%dobuild% sources\main\PathSystem3D\PathSystem3D.csproj
%dobuild% sources\external\Radegast.Plugin.Speech\RadSpeech\Radegast.Plugin.Speech.csproj
%dobuild% sources\main\LibCogbot\Cogbot.Library.csproj
%dobuild% sources\modules\CycWorldModule\CycWorldModule.csproj
%dobuild% sources\modules\CogbotRadegastPluginModule\CogbotRadegastPluginModule.csproj
%dobuild% sources\modules\FashionBotModule\FashionBotModule.csproj
%dobuild% sources\modules\IrcRegionModule\IrcRegionModule.csproj
%dobuild% sources\modules\SwiPrologBotModule\PrologBotModule.csproj
%dobuild% sources\modules\TheSimiansModule\TheSimiansModule.csproj
%dobuild% Cogbot_VS9_Pass_02.sln
%dobuild% sources\external\Radegast.Plugin.Speech\RadSpeechLin\RadSpeechLin.csproj
%dobuild% sources\external\Radegast.Plugin.Speech\RadSpeechMac\RadSpeechMac.csproj
%dobuild% sources\external\Radegast.Plugin.Speech\RadSpeechWin\RadSpeechWin.csproj
%dobuild% sources\main\Cogbot\Cogbot.csproj
%dobuild% sources\main\Cogbot32\Cogbot32.csproj
%dobuild% Cogbot_VS9.sln


:SUCCESS 
echo Build Successful! 
pause
exit /B 0 
:FAIL 
echo Build Failed, check log for reason 
pause
exit /B 1 
