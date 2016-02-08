# botcmd #

Bot commands are one of the first things that a person can do in Cogbot without learning how to program.  They allow a person to control Cogbot in the virtual world imperatively like "say hi" .. and the bot will say "hi".  They can be used from graphical user interface, the command line, botscript language, AIML, Prolog, Lisp, over the socket, and even httpd.   Best way to learn them is to start using them to do simple things in the world.


## starting ##

So we began with the command line version of cogbot. In Linux
```
mono Cogbot.exe --nogui
```

or, in Windows start a Command Prompt
```
cd \pathtocogbot\
Cogbot.exe --nogui
```

If you have already configured your botconfig.xml with a secondlife acct then this may automatically log you on.  Read [botconfig.xml] page for more info.  Quite a bit of debuggish output will happen after entering the above command line that for now we'll forgo the analysis of it.  You can wait for a few seconds for the output to fly past and eventually stop with:
```
0 avatars online: 
```


For this example we will assume you have not configured that so that we can show how to log one in fresh.

First we login
```
0 avatars online: login ExampleBot Resident pass1234 agni last
```

We had very much debug above but for now we are going to concentrate on commands so we will set Debug level to None
```
1 avatars online: debug none
Logging is set to None
Success debug
1 avatars online:
```
to set less output (we'll still see some having to do things going on in the simulator such as other people taking.

Speaking of which, it is very useful for you to have a second acct logged into the simulator using a graphical client like Imprudence for watching your bot execute the commands.

Setting the buffer width and height to 8000 in the windows cmd box (right click the title bar, Properties, the Layout tab, set Screen buffer size, Width and Height, to 8000) is also useful in cutting down on debug spam.

Lets compare the two commands we used above
"login" and "debug"

Debug was a modal command and executed instantly returning the words:
```
Logging is set to None
Success debug
```

Then showed the prompt again
```
1 avatars online:
```

whereas the "login" command started the login to the server but might take a while to complete.

We might typed
```
waitforlogin 30
```

after the login command.  Which would have told it to stop reading commands for up to 30 seconds until the bot is logged in.

```
1 avatars online: waitforlogin 30
ExampleBot Resident: Korean EduIsland (216.82.44.118:13002)
waitforlogin 0 failures and 1 successes
 1 successes
1 avatars online:
```

Since the bot was already logged in the command will complete immediately.

The bot may be in an inconvenient place. To move it, you'll need to be it's master.

When the bot logs in, it should IM you with 'hello master'. If not, you can set yourself as master with

```
setmaster ExampleMaster Resident
Master set to ExampleMaster Resident (9eda1cfc-e0c4-41ed-b2d2-e62bb70366df)
```

Now simply send the bot a TP request. The bot automatically accepts.

When you're ready to quit, (not now), you can type

```
quit
```

## Simple Actions ##


We've got a bot standing there, lets make it do things in the world.

type

```
say hello world
```

and the bot says hello world in local chat

Lets have a sit.

Rez a prim near the bot for it to sit on, and type

```
sit
```

The bot will sit on it.

```
stand
```

Makes the bot stand back up.

If you move the prim a little ways away, and type

```
sit
```

the bot will sit on the ground instead.

You can specify what the bot should sit on.  Name the prim **bot chair**, stand the bot up, and try
Specify Named prim (Notice that you need quotes if there are space)
```
sit "bot chair"
```

Specify nth closest (the chair 2nd closet to you)
```
sit 2 "bot chair"
```

You can use UUIDs
```
sit ff45-f45-f55-454-f-f45f-fff
```

You can use simulator local IDs
```
sit lid345345345345
```

### Command Argument reader Impl ###

Prims are read in into a command from three starting points

  * PrimsArg - Reads one argument for a group or single prim
  * ListOfPrimsArgs - Reads each argument like a PrimsArg and concats them together (priminfo select)
  * FilterList (Braces `[`])  - Reads all arguments inside and does a union on all args

Here are examples of a PrimsArg denoting one Prim

```
primid345345345
```
```
1 [attachments]
```
```
1 "bot chair"
```
```
$self
```
```
[$none +primid345345345 ]
```


A PrimsArg denoting possibly more than one Prim

```
$selected
```

these three are all the same
```
[$none +primid345345345 +[objects bydist max 2]]
```
```
[+primid345345345 +[objects bydist max 2]]
```
```
[objects bydist max 2 +primid345345345]
```
"objects" above is treated as "$objects"
So unless "+" or "$" is the first char, it will prepend "$" to the body




This would match Zero
```
[+ primid345345345 $self [objects bydist max 2 ]]
```



Special prefixes that change how command arguments are parsed:
  * + = append to list
  * $ = use a botvar group
  * [ = Start reading a Braced FilterList
  * ( = eval a lisp expression and insert it into arg
  * ! = remove the elements denoted by the next arg
  * integer = read the next args and get the "nth" item
  * @ = read a location and convert to a prim


A Braced FilterList Starts with either a $group or a +PrimsArg

"$" Single groups are:

  * $master
  * $objects
  * $avatars
  * $accounts
  * $attachments
  * $self
  * $none
  * $selected (items selected by current bot)

For the complete list type:
```
/paramhelp 
```



```
[ $objects mindist 1 maxdist 3 [ not match "red" ] bydist nth 1 ]
```

  * distfrom `<loc`>  - sort group by distance to `<loc`>
  * bydist - sort group by distance to avatar (same as "distfrom $self")
  * nth `<1-based`>  - grab only one element (negative numbers search from end)
  * !nth `<1-based`> - remove only one element (negative numbers search from end)
  * keep `<number`>  - retain so many elements (negative numbers retain from end)
  * !keep `<number`> - discard so many elements (negative numbers discard from end)
  * reverse - reverses list order
  * +SingleObject - adds the SingleObject to the group
  * and `<body>` - all preceding elements will be retained `<body`> restarts the group
  * not `<body`> - switches from normally filtering things for retention to removal

Example
```
[ not match "red" ]
```
is the same as
```
!match "red"
```


Some commands that take multiple objects such as "look"

```
look [ objects maxdist 3 ]
look primid34343434
```

some commands can take multiple objects per single argument

```
/priminfo [ $selected maxdist 2 ]
```
the selected prims (that is, the prims in my 'selected' grasper) tha are within 2 meters of my AV

See all this stuff starting at line 445 of WorldCommandParsing

```
/priminfo [ $selected maxdist 2.1 mindist 1.9 max 4 nth2 matches Window !matches "-like" ownedby $master ]
```

@ coerces prim position to 3d location @lid152345234

## Locations ##

[13:19] BinaBot Daxeline: location formats      simname/x/y/z
[13:19] BinaBot Daxeline: location formats      x/y/z
[13:20] BinaBot Daxeline: location formats      "Annie Obscure"
[13:20] BinaBot Daxeline: location formats      "Annie"
[13:21] BinaBot Daxeline: location formats    "ObjectNAme spec" 1
[13:21] bungiecord Burnstein: if it matches more than one
[13:21] BinaBot Daxeline: location formats    nth 1 "ObjectNAme spec"
[13:23] bungiecord Burnstein: at symbol
[13:23] bungiecord Burnstein: angle and distance
[13:33] BinaBot Daxeline: ok /moveto @0\*1
[13:33] BinaBot Daxeline: ok /moveto @90\*2


See  [issue 9](https://code.google.com/p/opensim4opencog/issues/detail?id=9)  for a bit more info about format of args

Douglas - feel free to blow this away if you integrate the info
in here

nice to have:
match LSL's preferred representation

[13:20] BinaBot Daxeline: location formats      <x,y,z>
[13:20] bungiecord Burnstein: with a space
[13:21] bungiecord Burnstein: so <4.0, 5.0, 18.0>

## Ways of sending bot commands to the bot ##

need section for multiple av's

@: Restrict the following commands to one or all avatars.

quit logs the bot off, and kills cogbot if no more are online.


## Inventory ##

Inventory is referred to by a syntax similar to paths in unix file systems.

Inventory paths start at 'My Inventory' as /.  So a typical set of clothing might be in

/Clothing/My Party Dress

so

wear /Clothing/My Party Dress

The botcmd parser tries to parse correctly without quotes, but you can force it to accept spaces with double quotes

wear "/Clothing/My Party Dress"

It's possible to change the current directory in inventory.

cd /Clothing

and discover the current directory with

pwd

now you can refer to local path by omitting the leading /

wear "My Party Dress"

. is the current folder

.. is the parent folder, so

if you have

cd /Clothing/theater/costumes/victorian/ball gown

then you can refer to

/Clothing/theater/costumes/edwardian

as

../../edwardian



## Sources for us editors ##

see Cogbot.listeners.WorldCommandParsing  for list of prepositions and variables


! and $ in commands
$master
$selected   (group of selected prims)
primid is local id
@ coerces prim position to 3d location @primid 15

max is maximum prims in filterset
dist is maximum dist to avatar
/priminfo $selected maxdist 2
the selected prims (that is, the prims in my 'selected' grasper) tha are within 2 meters of my AV

See all this stuff starting at line 445 of WorldCommandParsing


## Help ##
```
@'C#505366584':"!crouch: Starts or stops crouching."
@'C#505366584':"!fly: Starts or stops flying."
@'C#505366584':"!help: Print this help message."
@'C#505366584':"!jump: Jumps or flies up"
@'C#505366584':"!pay: Pays a prim."
@'C#505366584':"!say: Say something."
@'C#505366584':"!sit: Attempt to sit on the closest prim"
@'C#505366584':"!stand: Stand"
@'C#505366584':"!wear: Wear an outfit folder from inventory."
@'C#505366584':"!whisper: Whisper something."
@'C#505366584':"@: Restrict the following commands to one or all avatars."
@'C#505366584':"activategroup: Set a group as active."
@'C#505366584':"addfriend: Add avatar friend."
@'C#505366584':"agentlocations: Downloads all of the agent locations in a specified region."
@'C#505366584':"anim: Do a animation or gesture."
@'C#505366584':"animinfo: Show debug info about anims."
@'C#505366584':"appearance: Set your current appearance to your last saved appearance"
@'C#505366584':"astargoto:   Usage: Use A* Pathfinding to get to object"
@'C#505366584':"astarpath:   Usage: Use A* Pathfinding to get to object"
@'C#505366584':"attach: attach a prim to specified (or default) attachment point from the world"
@'C#505366584':"attachments: Prints a list of the currently known agent attachments."
@'C#505366584':"autopilot: Moves the avatar to the specified global position using simulator autopilot."
@'C#505366584':"avatarinfo: Print out information on a nearby avatar."
@'C#505366584':"back: Sends the move back command to the server for a single packet or a given number of seconds."
@'C#505366584':"backuptext: Backup inventory to a folder on your hard drive."
@'C#505366584':"balance: Shows the amount of L$."
@'C#505366584':"blockpath: Puts one minute temp blocks toward objects"
@'C#505366584':"botact: Invoke a command a bot interuptable action (interupts previous foreground action)."
@'C#505366584':"botperms: Sets the bot"
@'C#505366584':"botvar: Maniputlates bot vars."
@'C#505366584':"buy: Buys from a prim."
@'C#505366584':"cd: Changes the current working inventory folder."
@'C#505366584':"changeperms: Recursively changes all of the permissions for child and task inventory objects."
@'C#505366584':"clone: Clone the appearance of a nearby avatar."
@'C#505366584':"connectionscommand: shows simulator connections"
@'C#505366584':"copy: Copys from a prim."
@'C#505366584':"createnotecard: Creates a notecard from a local text file and optionally embed an inventory item."
@'C#505366584':"createscript: Creates a script in your inventory from a local .lsl file."
@'C#505366584':"crouch: crouch [on|off] 'no argumennt=for 500ms' "
@'C#505366584':"debug: Turn debug messages on or off."
@'C#505366584':"deed: Recursively changes all of the permissions for child and task inventory objects."
@'C#505366584':"deletefolder: Moves a folder to the Trash Folder"
@'C#505366584':"derez: De-Rezes a specified prim."
@'C#505366584':"describe:   Usage: \"describe\": describes everything around you \r\n you can also type \"describe location\", \"describe people\", \"describe objects\", or \"describe buildings\" to describe them respectively."
@'C#505366584':"dilation: Shows time dilation for current sim."
@'C#505366584':"do: Tell a bot to do an action on an object"
@'C#505366584':"download: Downloads the specified asset."
@'C#505366584':"downloadscript: Downloads the specified stript."
@'C#505366584':"downloadterrain: Download the RAW terrain file for this estate."
@'C#505366584':"downloadtexture: Downloads the specified texture."
@'C#505366584':"drop: drops a specified attachment into the world"
@'C#505366584':"dumpoutfit: Dumps all of the textures from an avatars outfit to the hard drive."
@'C#505366584':"echomaster: Repeat everything that master says."
@'C#505366584':"emptylostandfound: Empty inventory Lost And Found folder"
@'C#505366584':"emptytrash: Empty inventory Trash folder"
@'C#505366584':"eval: Enqueue a lisp task on a bot."
@'C#505366584':"evalsys: Enqueue a lisp task."
@'C#505366584':"evalwith: Evals a command with a scripting interpretor."
@'C#505366584':"evinfo: Shows the events that have been associated with an object."
@'C#505366584':"exportparticles: Reverse engineers a prim with a particle system to an LSL script."
@'C#505366584':"findobjects: Finds all objects, which name contains search-string."
@'C#505366584':"findsim: Searches for a simulator and returns information about it."
@'C#505366584':"findtexture: Checks if a specified texture is currently visible on a specified face."
@'C#505366584':"fly: To start flying type: \"fly\""
@'C#505366584':"flyto: Fly the avatar toward the specified position for a maximum of seconds."
@'C#505366584':"follow: Start or stop following a"
@'C#505366584':"follow*: Start or stop following a"
@'C#505366584':"forward: Sends the move forward command to the server for a single packet or a given number of seconds."
@'C#505366584':"friendslist: List avatar friends."
@'C#505366584':"gesture: Do a gesture."
@'C#505366584':"give: Gives items from the current working directory to an avatar."
@'C#505366584':"giveallmoney: Gives you all it's money."
@'C#505366584':"gohome: Teleports home"
@'C#505366584':"goto: Teleport to a location (e.g. \"goto Hooper/100/100/30\")"
@'C#505366584':"goto_landmark: Teleports to a Landmark."
@'C#505366584':"gridhealth: Runs a TP check to make sure ALL sims are"
@'C#505366584':"gridlayer: Downloads all of the layer chunks for the grid object map"
@'C#505366584':"gridmap: Downloads all visible information about the grid map"
@'C#505366584':"groupinfo: Shows the group UI."
@'C#505366584':"groupmembers: Dump group members to console."
@'C#505366584':"grouproles: Dump group roles to console."
@'C#505366584':"groups: List avatar groups."
@'C#505366584':"help: Print this help message."
@'C#505366584':"httpget: Do an http get."
@'C#505366584':"httppost: Do an http post."
@'C#505366584':"i: Prints out inventory."
@'C#505366584':"ilint: Prints out inventory."
@'C#505366584':"im: Instant message someone."
@'C#505366584':"imgroup: Send an instant message to a group."
@'C#505366584':"invitegroup: invite an avatar into a group."
@'C#505366584':"jarexec: Do an java exec."
@'C#505366584':"joingroup: join a group."
@'C#505366584':"jump: Jump for 500ms."
@'C#505366584':"leavegroup: Leave a group."
@'C#505366584':"left: Sends the move left command to the server for a single packet or a given number of seconds."
@'C#505366584':"lindenfollow: Follow another avatar."
@'C#505366584':"linkset: Takes from a prim."
@'C#505366584':"load: Loads commands from a dll. ("
@'C#505366584':"locate: Finds out in which direction yourself, an object or a building or a person is."
@'C#505366584':"location: Finds out in which direction yourself, an object or a building or a person is."
@'C#505366584':"log: Filters out console messages"
@'C#505366584':"login: Login to World Server"
@'C#505366584':"logout: Logout from Secondlife"
@'C#505366584':"look:   Usage: \"describe\": describes everything around you \r\n you can also type \"describe location\", \"describe people\", \"describe objects\", or \"describe buildings\" to describe them respectively."
@'C#505366584':"lowersim: Lowers all parent prims on a simulator."
@'C#505366584':"ls: Lists the contents of the current working inventory folder."
@'C#505366584':"lure: Send a lure to a"
@'C#505366584':"mapfriend: Show a friends location."
@'C#505366584':"mapimagepaths: Reads the sim map for improving routes"
@'C#505366584':"md5: Creates an MD5 hash from a given password."
@'C#505366584':"me: Emote something.  ("
@'C#505366584':"meshinfo: Shows meshinfo"
@'C#505366584':"move: Move to a person or object, or in a direction: west, east, north or south."
@'C#505366584':"moveprim: move prim to the relative specified position."
@'C#505366584':"moveto: Moves the avatar to the specified global position using robot turnto and walk."
@'C#505366584':"mute: Maniputate hte MuteList on the server"
@'C#505366584':"n/s/e/w: Move to a person or object, or in a direction: west, east, north or south."
@'C#505366584':"objectinventory: Retrieves a listing of items inside an object (task inventory)."
@'C#505366584':"orphans: Finds objects without locations [prim]"
@'C#505366584':"packetlog: Logs a given number of packets to an xml file."
@'C#505366584':"paramhelp: Scans the documentation for cogbot commands using the 'Parameters' field."
@'C#505366584':"parceldetails: Displays parcel details from the ParcelTracker dictionary."
@'C#505366584':"parcelinfo: Prints out info about all the parcels in this simulator"
@'C#505366584':"parrot: Make a bot parrot all actions by another avatar."
@'C#505366584':"pay: Pays a prim."
@'C#505366584':"pfdebug: Starts the pathfinder debuger"
@'C#505366584':"pointat: PointAts from a prim."
@'C#505366584':"primcount: Shows the number of objects currently being tracked."
@'C#505366584':"priminfo: Dumps information about a specified prim."
@'C#505366584':"primowners: Displays a list of prim owners and prim counts on a parcel."
@'C#505366584':"primregex: Find prim by text predicat."
@'C#505366584':"primworkshop: Runs PrimWorkshop on a prim."
@'C#505366584':"profile: Shows the Avatars profile in a UI component."
@'C#505366584':"profileclone: Clones another avatars profile as closely as possible. WARNING: This command will destroy your existing profile!"
@'C#505366584':"quietly: Invoke a console command with no return results."
@'C#505366584':"quit: Log all avatars out and shut down"
@'C#505366584':"regioninfo: Prints out info about all the current region"
@'C#505366584':"remeshprim: Reads the sim prims for improving routes then bakes the region (was called srprim)."
@'C#505366584':"removefriend: Remove avatar friend."
@'C#505366584':"repeat: Repeats a command in its own thread."
@'C#505366584':"returnobjectsowned: Returns all prims with a specific owner."
@'C#505366584':"rezitem: Rezs items from the current working directory to an avatar."
@'C#505366584':"right: Sends the move right command to the server for a single packet or a given number of seconds."
@'C#505366584':"rotate: The rotate command changes the BodyRotation on the server with a single packet."
@'C#505366584':"rotateprim: Rotate prim to the relative specified position."
@'C#505366584':"saleinfo: sets or prints SaleInfo on a prim."
@'C#505366584':"saveuuids: Saves resolution of UUID types."
@'C#505366584':"say: Say a message for everyone to hear."
```

```
/say <message>
```

say a message on channel 0

```
/say #<integer> <message>
```

say a message on channel 

&lt;integer&gt;



```
/say <....xml...>  
```

Passes text to a text-to-speech generator.
The bot then says this speech on the voice channel

The argument should be a valid ssml document, as defined in
[The SSML Specification](http://www.w3.org/TR/speech-synthesis/), wrapped in 

&lt;sapi&gt;

...

&lt;/sapi&gt;

 tags.

DOUG - I'm giving up here. What's with this?

I see it should be surrounded with 

&lt;sapi&gt;

... 

&lt;/sapi&gt;

 and am puzzled that SSML's root tag is 

&lt;speak&gt;

, and that ultimately it chats rather than voicing the message.


```
@'C#505366584':"script: Reads BotClient commands from a file. One command per line, arguments separated by spaces."
@'C#505366584':"searchclassifieds: Searches Classified Ads."
@'C#505366584':"searchevents: Searches Events list."
@'C#505366584':"searchgroups: Searches groups."
@'C#505366584':"searchland: Searches for land for sale. for"
@'C#505366584':"searchpeople: Searches for other avatars."
@'C#505366584':"searchplaces: Searches Places."
@'C#505366584':"select: selects one or more object in world."
@'C#505366584':"selectobject: Re selectobject [re|de] [prim]"
@'C#505366584':"selectobjects: Displays a list of prim localIDs on a given parcel with a specific owner."
@'C#505366584':"setbot: Sets one current bot for subsequent textform commands"
@'C#505366584':"sethome: Sets home to the current location."
@'C#505366584':"setmaster: Sets the"
@'C#505366584':"setmasterkey: Sets the key of the master"
@'C#505366584':"setperm: Recursively changes all of the permissions for child and task inventory objects."
@'C#505366584':"settexture: Set appearance texture of avatar."
@'C#505366584':"shellexec: Do an shell exec."
@'C#505366584':"shout: Shout something."
@'C#505366584':"showeffects: Prints out information for every viewer effect that is received."
@'C#505366584':"showevent: Shows an Events details."
@'C#505366584':"showgui: Shows the Radegast UI"
@'C#505366584':"showimage: Shows the specified image."
@'C#505366584':"simbot: Start theOpenSims type AI."
@'C#505366584':"simcatchup: Catches up the pathfinder"
@'C#505366584':"simexport: Exports an object to an xml file."
@'C#505366584':"simhinfo: Calculates the Height (Z) level of walking at point."
@'C#505366584':"simimport: Import prims from an exported xml file."
@'C#505366584':"simtype: Manipulates the SimType typesystem"
@'C#505366584':"simzinfo: Calculates the Z level of walking at point."
@'C#505366584':"sit: Sit on the ground or on an object."
@'C#505366584':"siton: Attempt to sit on a particular prim, with specified UUID"
@'C#505366584':"sleep:   Usage: sleep [seconds]"
@'C#505366584':"srdebug: Starts the waypoint debuger"
@'C#505366584':"stand: Stand up."
@'C#505366584':"stats: Provide connection figures and statistics"
@'C#505366584':"stop: Cancels a particular action"
@'C#505366584':"stop following: Start or stop following a"
@'C#505366584':"stopflying: You stop flying."
@'C#505366584':"stop-flying: You stop flying."
@'C#505366584':"stop-following: Start or stop following a"
@'C#505366584':"stopmoving: stops all movement threads"
@'C#505366584':"swip: runs swi-prolog commands on current sim."
@'C#505366584':"sysvar: Manipulates system vars."
@'C#505366584':"take: Takes from a prim."
@'C#505366584':"taskrunning: Retrieves or set IsRunning flag on items inside an object (task inventory)."
@'C#505366584':"tasks: Shows the list of task queue stat"
@'C#505366584':"tcpserver:   Usage: "
@'C#505366584':"teleport: Teleport to a location."
@'C#505366584':"textures: Turns automatic texture downloading on or off."
@'C#505366584':"thread: executes a command in its own thread."
@'C#505366584':"tobot: Send a command only to one bot."
@'C#505366584':"touch: Attempt to touch a prim with specified UUID"
@'C#505366584':"tree: Rez a tree 3 meters overhead."
@'C#505366584':"turnto: turn the avatar toward the specified position for a maximum of seconds. turnto [prim | [x y [z]]"
@'C#505366584':"unmeshprim: Unmeshes all prims and removes collision planes."
@'C#505366584':"unmute: Maniputate hte MuteList on the server"
@'C#505366584':"uploadimage: Upload an image to your inventory."
@'C#505366584':"uploadscript: Upload a local .lsl file file into your inventory."
@'C#505366584':"uploadterrain: Upload a raw terrain file to a simulator."
@'C#505366584':"uptime: Shows the login name, login time and length of time logged on."
@'C#505366584':"use:   Usage: Use an item from inventory or world."
@'C#505366584':"uuidtype: Resolve the type of Object the UUID represents."
@'C#505366584':"viewnote: Downloads and displays a notecard asset"
@'C#505366584':"voiceaccount: obtain voice account info."
@'C#505366584':"voiceparcel: obtain parcel voice info."
@'C#505366584':"voicetest: VoiceTest [firstname] [lastname] [password] [loginuri]"
@'C#505366584':"waitevent: wait until a certain event takes place."
@'C#505366584':"waitforlogin: Waits until all bots that are currently attempting to login have succeeded or failed"
@'C#505366584':"waitpos: Block until the robot gets to a certain position for a certain maxwait"
@'C#505366584':"walkto: Go to the avatar toward the specified position for a maximum of seconds."
@'C#505366584':"wear:   Usage: wear [outfit name]\r\n you can type  'wear [bake] /My Outfit/Dance Party"
@'C#505366584':"wearprim: Takes (derez to inventory) and wears a prim."
@'C#505366584':"where: Finds out in which direction yourself, an object or a building or a person is."
@'C#505366584':"whisper: Whisper a message to a"
@'C#505366584':"who: Lists seen avatars."
@'C#505366584':"wind: Displays current wind data"
@'C#505366584':"xfer: Downloads the specified asset using the Xfer system."
@'C#505366584':"Help complete"
"Success help"
```


## Getting events ##

This needs done

this is what ChatFromSim looks like..
require\_chat\_hook :-
> gridclient\_ref(Obj),
> cli\_get(Obj , 'Self' , S),
> > asserta(chat\_hook\_installed),

> cli\_add\_event\_handler(S , 'ChatFromSimulator' , onChatTSHook(_,_,_)).
[11:36:03 PM] Douglas R. Miles: the im would be.,..
[11:36:05 PM] Anne Ogborn: yes, looking at that
[11:36:57 PM | Edited 11:37:04 PM] Douglas R. Miles:  cli\_add\_event\_handler(S , 'IM' , onIMHook(_,_,_)).
[11:37:36 PM] Douglas R. Miles: you'll only care about the 3rd arg
[11:37:43 PM] Douglas R. Miles: oh first arg is BotID
[11:37:57 PM | Edited 11:38:00 PM] Douglas R. Miles: of onIMHook
[11:38:30 PM] Anne Ogborn: I **love** shared media
[11:38:34 PM] Douglas R. Miles: third will be a struvct you need to print out a couple times
[11:38:38 PM] Anne Ogborn: I got a 1 hour loop of fire sound
[11:38:57 PM] Douglas R. Miles: print out to know the scructs shape