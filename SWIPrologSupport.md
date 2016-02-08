> Support for the swi-Prolog language is built into Cogbot's Prolog integration module.

> API's for swipl integration with Cogbot are in

```
C:\pathtocogbot\bin\prolog\simulator\cogrobot.pl
```

# Starting with a prolog top level #

## Setup ##

### remove excess debugging ###

You will probably want to remove a lot of debugging verbiage
So, in

```
C:\pathtocogbot\bin\prolog\simulator\cogrobot.pl
```

On line 286
Uncomment

```
% onSimEvent(_A,_B,_C):-!. % comment out this first line to print them
```


### Set Bot Credentials ###

To set up your bot's credentials, on line 139 of this file modify the code to your own user name, password, and loginuri:

```
C:\pathtocogbot\bin\prolog\cogbot.pl
```


## Start ##

### With normal start ###

```
cd c:\pathtoCogBot\bin
swipl-win.exe  -f prolog/cogbot.pl
```

and query

```
ike.
```


### Sample Module ###

A sample example module is in

```
c:\pathtoCogBot\bin\runcogbot.pl
```

Double clicking it launches cogbot, brings up Radegast, and logs the bot on.

Notice that it has an end\_of\_file at line 35.

### From Visual Studio ###

Select **swicli** as startup project
Run with the VS **play** button

# Including cogbot in your swi-Prolog program #

Add a file search path to cogbot/bin/prolog directory as root of cogbot

:-ensure\_loaded(cogbot('simulator/cogrobot')).

# Logging the bot on #

If you didn't log the bot on automatically at startup,

```
runSL.
```

Makes sure the bot is running. Logs bot on using the botconfig.xml
parameters if necessary.

# Showing Radegast #

```
?-botClientCmd(showgui).
```

Notice that closing the Radegast window kills the bot and the prolog process.

# Making the bot execute a bot command #

[botcmd language](BotCommands.md) is the basic command language for ordering the bot to do things in world. You can execute bot commands from Prolog.

```
3 ?- botClientCall(executeCommand("jump"),X),cli_writeln(X).
"Success Jump"
X = @'C#186521916'.
```

If the command takes args

```
botClientCmd(say('hello world')).
```

The term is, in general, variadic.  So, the equivilent of

```
/waitpos 30 tree 1
```

is

```
botClientCmd(waitpos(30, tree, 1)).
```

The args can also be passed as a list, so

```
botClientCmd(waitpos([30, tree, 1])).
```

# AIML #

You can directly call prolog from AIML via the system tag.

```
<system lang="swi">some_pred(X)</system>
```

DOUGLAS TODO - need explanation of how to load init file

# [SwiCLI](SwiCLI.md) #

Cogbot's core is written in C#. Access to the underlying C# objects is through [SwiCLI](SwiCLI.md), a bridge between swi-Prolog and the CLI runtime that C# is built upon.

For example, SimAvatar is the underlying representation of agents the simulator knows about. The predicate  simAvatar/1 unifies if it's argument is a reference to a known SimAvatar object.

```
[1] 58 ?- simAvatar(X).
X = @'C#638101232';
X = @'C#638101728';
X = @'C#638111960';
false.

```

The term @atom is a reference to a C# object, in this case an instance of SimAvatar.

With a reference we can use cli\_get to extract information from the reference.

The shorthand method cli\_to\_str/2 calls to\_string on the object.

```
[1] 58 ?- simAvatar(X),cli_to_str(X,S).
X = @'C#638101232',
S = "BinaBot Daxeline" ;
X = @'C#638101728',
S = "Nephrael Rajesh" ;
X = @'C#638111960',
S = "Trollerblades Wasp" ;
false.
```



# API's #

> runSL/0,
> listMembs/0,
> worldSystem/1,
> worldSystem/2,
> botClient/1,
> botClient/2,
> botClientCall/1,
> botClientCall/2,
> botClientCmd/1,
> botClientCmd/2,
> botClientCmd/3,
> simObject/1,
> simAvatar/1,
> simAvDistance/3,
> simAsset/1,
> simAccount/1,
> gridCliient/1,
> resolveObjectByName/2,
> vectorAdd/3,
> distanceTo/2,
> toGlobalVect/2,
> toLocalVect/2,
> onSimEvent/3,wasSimEvent/3,
> obj2Npl/2,
> npl2Obj/2,
> chat/1,
> chat/2,
> chat/3,
> createWritelnDelegate/2,
> createWritelnDelegate/1,
> textureIDToImage/2,
> textureIDToImageParts/2,
> requestTexture/1,
> simObjectColor/2