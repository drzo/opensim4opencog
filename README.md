Readme for OpenSim4OpenCog/Cogbot, a bot interface for external AI systems.

--+-----------------------+<pre>
=        OVERVIEW         =<br>
</pre>--+-----------------------+<pre>

OpenSim4OpenCog/Cogbot is a server written in C# that connects to a avatar acting<br>
as "bot" in OpenSim/Second Life/RealXtend. The basic shell is based on the<br>
TextSL client for visually impared users. The system can accept in XML<br>
(or a pointer to a file with XML commands). The system is updated to<br>
use the latest Openmetaverse library to interface with sims. It also has<br>
an internal LISP interperter (based on DotLisp) and task management queue.<br>
This allows operations to be persistent.<br>
<br>
</pre>--+-----------------------+<pre>
=   COMPILING 4 WINDOWS   =<br>
</pre>--+-----------------------+<pre>

Cogbot was developed using Windows XP and Visual Studio 2008.<br>
Some plugins have dependancies of:<br>
<br>
Managed DirectX components<br>
URL: http://www.microsoft.com/downloads/details.aspx?displaylang=en&FamilyID=2da43d38-db71-4c1b-bc6a-9b6652cd92a3<br>
<br>
Microsoft Speech SDK 5.1<br>
URL: http://www.microsoft.com/downloads/details.aspx?FamilyID=5e86ec97-40a7-453f-b0ee-6583171b4530&displaylang=en<br>
msttss22L.exe<br>
SpeechSDK51.exe<br>
<br>
You can use Visual Studio C# Express which is free from Microsoft.<br>
URL: http://microsoft.com/express<br>
<br>
To compile,<br>
Open Command Prompt (cmd.exe)<br>
<br>
Go into your source path<br>
cd C:\development\opensim4opencog\<br>
<br>
Run the Project creation (This will Generate the "Cogbot.sln")<br>
runprebuild2008.bat<br>
<br>
Open Visual Studio and open the cogbot.sln file<br>
located in the project's root directory.<br>
This should open Cogbot in the IDE and show the several sub projects.<br>
<br>
In the Solution Explorer, make sure that the "Radegast" project<br>
is bold (the startup project)  and if it is not make it so by right<br>
click the project and select "Set as Startup Project"<br>
<br>
To build, go to Build -> Build Solution (or just press F6)<br>
<br>
On the status bar at the bottom of the Visual Studio window - you should see<br>
"Build succeeded", once the system is finished compiling.<br>
<br>
Time to Configure the system.<br>
<br>
</pre>--+-----------------------+<pre>
=    COMPILING - LINUX    =<br>
</pre>--+-----------------------+<pre>
We do know the follwing will be needed:<br>
<br>
* nANT<br>
* Mono 1.2.4 or better (with .NET 2.0 package)<br>
* gmcs (known as the mono-gmcs package on Debian)<br>
<br>
Here is a typical build session:<br>
<br>
[root@titan trunk]# pwd<br>
/development/Opensim4OpenCog/trunk<br>
[root@titan trunk]# ./runprebuild.sh<br>
Prebuild v2.0.3<br>
Copyright (c) 2004-2008<br>
Matthew Holmes (matthew@wildfiregames.com),<br>
Dan Moorehead (dan05a@gmail.com),<br>
David Hudson (jendave@yahoo.com),<br>
Rob Loach (http://www.robloach.net),<br>
C.J. Adams-Collier (cjac@colliertech.org),<br>
See 'prebuild /usage' for help<br>
<br>
Creating NAnt build files<br>
...Creating project: RTParser<br>
...Creating project: PathSystem3D<br>
...Creating project: OpenMetaverse.Utilities<br>
...Creating project: Cogbot<br>
...Creating project: OpenMetaverseTypes<br>
...Creating project: OpenMetaverse.Rendering.Meshmerizer<br>
...Creating project: OpenMetaverse<br>
...Creating project: OpenMetaverse.StructuredData<br>
...Creating project: OpenMetaverse.Http<br>
...Creating project: DotLisp<br>
<br>
[root@titan trunk]# nant<br>
...<br>
...<br>
After 10-50 seconds of compiling you should see "BUILD SUCCEEDED".<br>
....<br>
<br>
[root@titan trunk]# cd bin<br>
[root@titan bin]# mono Cogbot.exe<br>
<br>
<br>
</pre>--+-----------------------+<pre>
=      CONFIGURATION      =<br>
</pre>--+-----------------------+<pre>

To configure the system you will need to provide or modify the botconfig.xml<br>
file. In particular you will want to have an avatar already defined on the<br>
sim you will be using. The "tcp" paramaters define the IP address of the<br>
Cogbot service. The other parameters define the login for the virtual viewer<br>
used by the sytem to connect to the avatar in the sim.<br>
You can find the botconfig.xml file in<br>
.\cogbot\bin\Debug\botconfig.xml<br>
.\cogbot\bin\Release\botconfig.xml<br>
.\cogbot\bin\botconfig.xml<br>
Depending on the build or run options you use.<br>
<br>
The key parameters to change are<br>
<br>
<tcpPort>5555<br>
<br>
Unknown end tag for </tcpPort><br>
<br>
<br>
<tcpIPAddress>127.0.0.1<br>
<br>
Unknown end tag for </tcpIPAddress><br>
<br>
<br>
<br>
<firstName>My<br>
<br>
Unknown end tag for </firstName><br>
<br>
<br>
<lastName>Bot<br>
<br>
Unknown end tag for </lastName><br>
<br>
<br>
<password>MyBotPassword<br>
<br>
Unknown end tag for </password><br>
<br>
<br>
<simURL>http://localhost:8002/<br>
<br>
Unknown end tag for </simURL><br>
<br>
<br>
<br>
<startupLisp>(thisClient.ExecuteCommand "login")<br>
<br>
Unknown end tag for </startupLisp><br>
<br>
<br>
<br>
Once started you can fill in the login parameters using the login form. Any<br>
changes there will be saved to the config file (which will be created if it<br>
does not already exist).<br>
<br>
New: <startupLisp> allows an initial Lisp expression to be evaluated after the<br>
task manager starts executing and has loaded its boot files.<br>
<br>
</pre>--+-----------------------+<pre>
=      MANUAL OPERATION   =<br>
</pre>--+-----------------------+<pre>
Since Cogbot is based on TextSL, you can drive things manually.<br>
You can start the system by executing:<br>
\cogbot\bin\cogbot.exe<br>
which uses \cogbot\bin\botconfig.xml<br>
<br>
The menu system is fairly simple for now<br>
File -> Exit<br>
Client->Login<br>
Client->Logout<br>
<br>
Under "Client->Login" is a form for modifying the username and password.<br>
Information on the status of commands and messages are provided in the<br>
central text box. User input is in the white text entry box and executed<br>
by pressing the submit button.<br>
<br>
An example of the report of logging in :<br>
<br>
--------------------------------------------<br>
About to initialize port.<br>
Listening for a connection... port=5555<br>
LoginForm Start Attempt 0<br>
TextForm Network_OnLogin : [ConnectingToSim] Connecting to simulator...<br>
LoginForm NOT logged in for reason:0 Timed out<br>
TextForm Network_OnLogin : [ConnectingToSim] Connecting to simulator...<br>
You see 0 people.<br>
TextForm client_OnLogMessage: Info <Kotoko Irata>: Connecting to (127.0.0.1:9011)<br>
TextForm Objects_OnNewAvatar:<br>
TextForm Objects_OnNewAvatar:<br>
TextForm client_OnLogMessage: Info <Kotoko Irata>: Received a region handshake for Citadel (127.0.0.1:9011)<br>
TextForm Network_OnSimConnected: Citadel (127.0.0.1:9011)<br>
TextForm Network_OnLogin : [Success] Welcome to OGS<br>
TextForm Objects_OnNewAvatar:<br>
TextForm Avatars_OnLookAt: 7dbd61c6-90cf-49df-bf77-94f5a7223c19 to 7dbd61c6-90cf-49df-bf77-94f5a7223c19 at 7dbd61c6-90cf-49df-bf77-94f5a7223c19 with type FreeLook duration 2<br>
You see the objects 1: DIR, 2: Deck, 3: Desk, 4: StucoBeachHouse, 5: WallSectionSolid, 6: FW_Steps, 7: Landscape, 8: HellBox, 9: Blue, 10: marble, 11: six, 12: one, 13: DaxSymbol, 14: 2_Walls, 15: ML866, and 16: Window02_Tall,4-Pane.<br>
TextForm Avatars_OnLookAt: 7dbd61c6-90cf-49df-bf77-94f5a7223c19 to 7dbd61c6-90cf-49df-bf77-94f5a7223c19 at 7dbd61c6-90cf-49df-bf77-94f5a7223c19 with type FreeLook duration 2<br>
Logged in successfully.<br>
-----------------------------------------------<br>
<br>
The results of typing help:<br>
-----------------------------------------------<br>
login: Login to Secondlife<br>
logout: Logout from Secondlife<br>
stop: Cancels a particular action<br>
teleport: Teleport to a location.<br>
describe: Describe location, people, objects, or buildings.<br>
say: Say a message for everyone to hear.<br>
whisper: Whisper a message to a user.<br>
help: Print this help message.<br>
sit: Sit on the ground or on an object.<br>
stand: Stand up.<br>
jump: Jump.<br>
crouch: Crouch.<br>
mute: Toggle Mute or unmute a user<br>
move: Move to a person or object, or in a direction.<br>
use: Use an item from inventory.<br>
fly: You start flying.<br>
stop-flying: You stop flying.<br>
where: Finds out in which direction an object or a building or a person is.<br>
locate: Gives the coordinates of where you are.<br>
follow: Start or stop following a user.<br>
stop following: Start or stop following a user.<br>
stop-following: Start or stop following a user.<br>
tutorial1: Teaches you how to navigate using basic commands move, sit, stand<br>
--------------------------------------------------<br>
<br>
describe<br>
------------------<br>
You are in Citadel.<br>
You see 2 people.<br>
You see the objects 1: CEMA, 2: Sit, 3: Clever, 4: 5_Flooring, 5: Boardman Bedroom, 6: Wood, 7: Keyboard, 8: Medical, 9: House03_PostHickory, 10: Low, 11: CLEAR, 12: marble end, 13: Banana, 14: Banana Plant, 15: Clay, and 16: Imperial.<br>
You see 2 buildings.<br>
You see 2 people.<br>
------------------<br>
describe people<br>
------------------<br>
You see one person: 1: Daxxon Kinoc.<br>
------------------<br>
describe Daxxon<br>
------------------<br>
Daxxon Kinoc is standing in Citadel.<br>
Daxxon Kinoc is 2.112267 distant.<br>
------------------<br>
describe Clever<br>
------------------<br>
Clever Zebra Small Office (left): http://www.cleverzebra.com/<br>
This object is for sale for L10<br>
------------------<br>
----------------------------------------------------------<br>
and so on. To get the inventory you can issue "describe inventory".<br>
The system also accepts<br>
"use <inventory-item-name> to wear"<br>
"use <inventory-item-name> to animation-start"<br>
"use <inventory-item-name> to animation-stop"<br>
<br>
The system recives events from the sim like what is being looked at.<br>
<br>
TextForm Avatars_OnLookAt: 7dbd61c6-90cf-49df-bf77-94f5a7223c19<br>
to da717612-e98f-469b-b6c3-f9145ca84e64<br>
at da717612-e98f-469b-b6c3-f9145ca84e64<br>
with type Focus duration 1.701412E+38<br>
(TARGET IS SELF)<br>
<br>
meaning that the user is looking at the bot.<br>
<br>
<br>
</pre>--+-----------------------+<pre>
=   External Access       =<br>
</pre>--+-----------------------+<pre>

The whole point is to be something an AI running as an external process<br>
could use. To do this the system  accepts commands via the socket<br>
it is listening to.<br>
<br>
For testing we setup a file on a server with the commands to the lisp<br>
interperter XML encoded.<br>
<br>
The construct (thisClient.ExecuteCommand “<command-string>”) will execute<br>
any command-string you could execute manually from the client.<br>
<br>
Another useful fragment to know is:<br>
(set thisTask.requeue (to-bool false))<br>
which means not to requeue this code fragment for later execution.<br>
<br>
If you did want this fragment to be constantly requeued you would use<br>
(set thisTask.requeue (to-bool true))<br>
<br>
So the dotlisp equivelent of "Hello World" would be:<br>
<br>
(block<br>
(thisClient.ExecuteCommand “say Hello World with a L I S P 2 …”)<br>
(thisClient.msgClient "(knows world (exists me))" )<br>
(set thisTask.requeue (to-bool-false))<br>
)<br>
<br>
Which we then translate into XML and stick in a URL accessible file.<br>
<br>
--------------------------------------------------<br>
testlisp2.xlsp<br>
--------------------------------------------------<br>
<?xml version="1.0" encoding="utf-8" ?><br>
<op name="block"><br>
<op name="thisClient.ExecuteCommand"><br>
"say Hello With a L I S P 2..."<br>
<br>
<br>
Unknown end tag for </op><br>
<br>
<br>
<op name="thisClient.msgClient"><br>
"(knows world (exists me))"<br>
<br>
<br>
Unknown end tag for </op><br>
<br>
<br>
<op name="set"><br>
<arg name="1">thisTask.requeue<br>
<br>
Unknown end tag for </arg><br>
<br>
<br>
<arg name="2">(to-bool false)<br>
<br>
Unknown end tag for </arg><br>
<br>
<br>
<br>
<br>
Unknown end tag for </op><br>
<br>
<br>
<br>
<br>
Unknown end tag for </op><br>
<br>
<br>
--------------------------------------------------<br>
<br>
<br>
Using Putty we connect via a raw socket to the Cogbot server<br>
(in our case localhost:5555).<br>
<br>
And assuming we have a command stored as an XML file on a server<br>
we can simply type in the URL<br>
http://pandor6/temp/testlisp2.xlsp<br>
<br>
The system will return<br>
'(enqueued)(knows world (exists me))<br>
execute the command and the bot will say "Hello With a L I S P 2..."<br>
in-world.<br>
<br>
----------------------------<br>
SockClient:http://pandor6/temp/testlisp2.xlsp<br>
EvaluateXmlCommand :http://pandor6/temp/testlisp2.xlsp<br>
XML2Lisp =>'(block(thisClient.ExecuteCommand<br>
"say Hello With a L I S P 2..."<br>
)(thisClient.msgClient<br>
"(knows world (exists me))"<br>
) )'<br>
taskTick Results>nil<br>
taskTick continueTask=False<br>
Kotoko Irata says, "Hello With a L I S P 2...".<br>
------------------------------<br>
<br>
To find out more about the lisp system see \cogbot\dotlisp\dotlisp.html<br>
The system should automatically load<br>
\cogbot\bin\boot.lisp<br>
\cogbot\bin\extra.lisp<br>
\cogbot\bin\cogbot.lisp<br>
<br>
</pre>--+-----------------------+<pre>
= Cogbot.lisp and events  =<br>
</pre>--+-----------------------+<pre>
The simulator sends update messages to clients like TextSL or the<br>
Second Life viewer. Cogbot hooks reception of these messages<br>
and updates the appropriate structures. The system can also add<br>
lisp code fragements to the queue for processing. The defintion<br>
of these functions/methods are in the cogbot.lisp file. The<br>
initial version simply maps the parameters into an appropriate<br>
message sent to the client AI.<br>
<br>
(on-chat agent message)<br>
(on-instantmessage agent message)<br>
(on-meanCollision perp victim)<br>
(on-avatar-dist agent dist)<br>
(on-avatar-pos agent vector)<br>
(on-avatar-posture agent sitstand)<br>
(on-prim-description  obj description)<br>
<br>
EXPANDED SET 2008-09-18<br>
(on-login-fail  login description)<br>
(on-login-success  login description)<br>
(on-network-disconnected reason message)<br>
(on-network-connected reason message)<br>
(on-simulator-connected simulator)<br>
(on-new-avatar  avatar-name avatar-uuid)<br>
(on-new-prim  prim-name prim-uuid prim-description)<br>
(on-new-foliage  foliage-name foliage-uuid foliage-description)<br>
(on-self-look-target  source description)<br>
(on-self-point-target  source description)<br>
(on-avatar-point  source  dest description)<br>
(on-avatar-look  source  dest description)<br>
(on-prim-dist prim-name prim-ID dist)<br>
(on-prim-pos prim-name prim-ID vector)<br>
<br>
Extra sensory input simply require adding more hooks and defining<br>
the functions. Note that reflexes could be implemented at this<br>
level as well as filtering.<br>
<br>
Event log of Cogbot seeing "Daxxon Kinoc" logging in:<br>
TextForm Objects_OnNewAvatar:<br>
taskcode =(on-avatar-dist (@"Daxxon Kinoc") 2.217624 )<br>
taskTick Results>nil<br>
taskTick continueTask=False<br>
taskcode =(on-avatar-pos (@"Daxxon Kinoc") (@"<127.2048, 129.4689, 21.47487>") )<br>
taskTick Results>nil<br>
taskTick continueTask=False<br>
taskcode =(on-avatar-posture (@"Daxxon Kinoc") (@"standing") )<br>
taskTick Results>nil<br>
taskTick continueTask=False<br>
TextForm Avatars_OnLookAt: 7dbd61c6-90cf-49df-bf77-94f5a7223c19<br>
to 7dbd61c6-90cf-49df-bf77-94f5a7223c19 at 7dbd61c6-90cf-49df-bf77-94f5a7223c19<br>
with type FreeLook duration 2<br>
Daxxon Kinoc says, "How are you ?".<br>
taskcode =(on-chat (@"Daxxon Kinoc") (@"How are you ?") )<br>
taskTick Results>nil<br>
taskTick continueTask=False<br>
<br>
What was sent to the AI socket:<br>
(distance-from ("Daxxon Kinoc") 2.217624)<br>
(position ("Daxxon Kinoc") '"<127.2048, 129.4689, 21.47487>"')<br>
(posture ("Daxxon Kinoc") '"standing"')<br>
(heard ("Daxxon Kinoc") '"How are you ?"')<br>
<br>
The Avatars_OnLookAt need to be captured and transformed. Then the<br>
system would be able to track objects being pointed to, like itself.<br>
<br>
</pre>--+-----------------------+<pre>
=   TODO: 2008-08-24      =<br>
</pre>--+-----------------------+<pre>
At this time all the subprojects "aligned" to the point to allow Cogbot to<br>
function. OpenSim and OpenMetaverse are currently moving and OpenCog is being<br>
written. So there are many things that are needed besides just keeping up with<br>
new releases.<br>
<br>
* (DONE) Provide feedback to the client. This is as simple as adding a<br>
"msgClient" method to the "thisClient" object.<br>
<br>
* (INWORK) Patch the events hooks through using the "msgClient" function.<br>
Each event would simply post a lisp code fragment to the task queue.<br>
<br>
Working on the "heard" listener first by adding to Chat.cs:<br>
parent.enqueueLispTask("(thisClient.msgClient \"(heard (" + fromName + ") '" + message + "' )\" )");<br>
<br>
when I say 'hi there' to the bot in-world cogbot returns to the tcp client:<br>
(heard (Daxxon Kinoc) 'hi there' )<br>
<br>
This could of course be changed into calls like "(on_chat (fromName) message)"<br>
where "on_chat" is a lisp function, and could be redefined by the AI using<br>
Cogbot. Also such a function could translate into something like XML or<br>
use system provided methods to do so.<br>
<br>
In general define all the events that occur, map them to function calls,<br>
then have a file for each type of message each type of client AI expects.<br>
So OpenCog would have one, OpenCyc its own, Soar its own, etc...<br>
<br>
- Progress (2008-08-27): implemented outline of the above idea.<br>
see "Cogbot.lisp and events" above<br>
<br>
* (SEMI-DONE) Set the system up for auto-login. The command line does have<br>
login and logout. However setting the other parameters would be nice. Also<br>
the sim may report that the system is already logged on, and may require a<br>
second attempt.<br>
<br>
*(DONE) Lisp initialization string in the config file, to be executed<br>
on start up.<br>
<br>
Adding<br>
<startupLisp>(thisClient.ExecuteCommand "login")<br>
<br>
Unknown end tag for </startupLisp><br>
<br>
<br>
to the startup file causes the system to automatically login as it's first<br>
action after booting. This addresses the previous TODO but requires more lisp.<br>
<br>
Something like:<br>
<br>
<startupLisp><br>
(block<br>
(set thisClient.config.simURL "http://myothersim.org:8002/")<br>
(set thisClient.config.firstName "EvilTwin")<br>
(thisClient.ExecuteCommand "login")<br>
)<br>
<br>
<br>
Unknown end tag for </startupLisp><br>
<br>
<br>
<br>
or load additional config or operational files.<br>
<br>
* Having multiple bots. Currently the system provides single bot access,<br>
with each socket serving one bot. Being able to run multiple bots would<br>
be nice.<br>
<br>
* Time based requeuing . The system can requeue a task but it is not timed on<br>
an individual task basis, using a simple round robin scheduler.<br>
<br>
*Document the objects the lisp system has access to. These are basically the<br>
same as the client object in Cogbot since "lisp thisClinent" == "cogbot client"<br>
in the code. However the method set is still evolving.<br>
<br>
* More graceful socket shutdown.<br>
<br>
* RealXtend functions. The system uses OpenMetaverse and thus should work with<br>
the systems it works with. However some RealXtend function may go beyond the<br>
set supported by Secondlife/Opensim. This is more of a wish for OpenMeteverse<br>
extension.<br>
<br>
* Intelligently support lisp over the socket. Ideally, lisp, XML-encoded lisp,<br>
and the current pointer to XML files.<br>
<br>
* Port dotLisp to Opensim. I already ported Yield Prolog, and dotLisp is<br>
"safely dead", meaning it works but is not being rapidly extended (like<br>
everything else). So it would provide an AI-ish scripting language which<br>
can access sim methods, being both simple, dynamic and complete. Might<br>
provide a method to let AI's script objects...<br>
<br>
* Connecting up OpenCog. In general create the set of "mapping files" in lisp<br>
described above for any set of external AI programs.<br>
<br>
NOTES:<br>
· Simply affordable: With the general ability to evaluate lisp functions in strings or<br>
use the strings as URL’s to functions, one can implement a form of affordance<br>
processing similar to The Sims. In The Sims new objects transmitted to passing<br>
agents code fragments which included conditions checked against the agents<br>
internal state blackboard. A message from a refrigerator might in effect say “if<br>
your hunger is high and you like fruit then open me by grasping handle 3234” or<br>
some such. While most learning AI systems might consider it “cheating” it might<br>
be used to simulate extra perceptions like smell.<br>
<br>
Links:<br>
Initial CogBot at Google Code<br>
http://code.google.com/p/opensim4opencog/<br>
<br>
TextSL at Google Code<br>
http://code.google.com/p/textsl/<br>
<br>
DotLisp<br>
http://dotlisp.sourceforge.net/dotlisp.htm<br>
<br>
OpenMetaverse<br>
http://openmv.org/wiki/Main_Page<br>
http://www.libsecondlife.org/wiki/Main_Page<br>
<br>
OpenSimulator<br>
http://www.opensimulator.org<br>
http://osgrid.org/forums/<br>
<br>
Second Life<br>
http://secondlife.com/<br>
<br>
RealXtend<br>
http://www.realxtend.org/<br>
http://www.rexdeveloper.org/forum/