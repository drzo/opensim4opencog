#description of botvars

# Introduction #

Cogbot is composed of a number of interacting plugins (yay, we love plugins!). The primary semantic way of interchanging information among plugins is botvars.


## Producing botvars from prolog _**(requires SWI-Prolog 6.1.5 or above)**_ ##

For your Prolog code to provide a botvar you must define 3
dynamic hook predicates.

Three hook predicates start in the module **bv**:

```
bv:hook_botvar_get/4
bv:hook_botvar_set/4
bv:hook_botvar_key/3
bv:hook_botvar_desc/4
```

Arg 1 = The BotID ref (like current\_bot/1)

Arg 2 = The Namespace submitted ( there is a special namespace called "bot" )

Arg 3 = The Key Name = "is Good" matches "is\_good"

Arg 4 = The Value for Set or Get (bv:hook\_botvar\_key does not use this). For hook\_botvar\_desc this unifies with a string description of the botvar

These 3 predicates may be registered from any module merely by including them (in the prolog source files) like below as long as they use the module prefix "bv:"

_These are working examples_
```
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BOTVAR EXAMPLE: provide isNight, a readonly botvar that
%%% is "Yes" if the bot thinks it's night
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-dynamic isNight/0.

bv:hook_botvar_get(_,bot,'isNight',Value):- isNight -> Value="Yes" ; Value="No".

bv:hook_botvar_set(_,bot,'isNight',Value):- 'format'(user_error,'Cannot set isNight=~w~n',Value).

bv:hook_botvar_key(_,bot,'isNight').

bv:hook_botvar_desc(_,bot,'isNight',"Readonly- Yes if bot thinks it is night").

%... other Prolog code asserts and retracts isNight...
```

Some AIML that uses isNight to say something appropriate to the
time of day

```
<category>
    <pattern>IDLE COMMENT</pattern>
    <template>
        <condition name="isNight">
         <li value="Yes">The stars look pretty tonight</li>
         <li>Nice day today</li>
    </template>
</category>
```

Example that affects the physical world of the bot by making the
bot sit on the ground

```
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BOTVAR EXAMPLE: Set up sitting on ground based botvar 
%%% Setting this variable makes the bot sit on the ground
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bv:hook_botvar_get(BotID,bot,'isSittingGround',Value):- 
     wbotget(BotID,['Self','Movement','SitOnGround'],Result),!,
        (cli_is_true(Result)-> Value="Yes" ; Value="No").

bv:hook_botvar_set(BotID,bot,'isSittingGround',Value):- 
   Value="Yes" -> wbotcall(BotID,['Self','SitOnGround'],_) ; wbotcall(BotID,['Self','Stand'],_).

bv:hook_botvar_key(_,bot,'isSittingGround').

b:hook_botvar_desc(_,bot,'isSittingGround', "ReadWrite - setting Yes makes the bot sit on the ground, setting No makes the bot stand up").
```

Now the connecting AIML code:
```
<category>
    <pattern>SITTING</pattern>
    <template>
        <condition name="isSittingGround">
        <li value="Yes">Yes I am sitting tell me DO STAND</li>
        <li>Nope, tell me DO SIT</li>
        </condition>
    </template>
</category>

<category>
    <pattern>DO SIT</pattern>
    <template>I will try to sit <think><set dict="bot" name="isSittingGround">Yes</set></think> <srai>SITTING</srai></template>
</category>

<category>
    <pattern>DO STAND</pattern>
    <template>I will try to stand <think><set dict="bot" name="isSittingGround">No</set></think> <srai>SITTING</srai></template>
</category>

```

This is an example of a generally useful pattern for using botvars.

Sometimes you want to dynamically add botvars. So, for instance, if you want to allow the user to tell the bot facts about itself, you don't want to have to pre-define what the facts are in Prolog.

We might also want to have a lower friction way to define botvars in prolog. This snippet lets us set an undeclared botvar and it will automatically be declared.

Bit of Prolog advice:

If you are implementing something like this snippet, be aware that assert/1 retract/1 uses the current **context\_module** so you want to make sure you are asserting/retracting your dynamic predicates somewhere you might find them again. (Every dynamic predicate in prolog works this way (not related to Cogbot)).

```
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BOTVAR EXAMPLE: Dynamic predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(mymodule, [do_my_thing/0]).

:-dynamic(dyn_store_botvar/3).
dyn_store_botvar(_,"a",1).
dyn_store_botvar(_,"b",2).

bv:hook_botvar_get(_BotID,NS,Key,Value):-dyn_store_botvar(NS,Key,Value).
bv:hook_botvar_set(_BotID,NS,Key,Value):-
    retractall(dyn_store_botvar(NS,Key,_)),assert(dyn_store_botvar(NS,Key,Value)).
bv:hook_botvar_key(_BotID,NS,Key):-dyn_store_botvar(NS,Key,_).

.... elsewhere in prolog code ...

realize_we_are_doomed :-
      botvar_set(bot, 'doomed', "Yes").

panic :-
      botvar_get(bot, 'doomed', "Yes"),
      format('We are doomed~n', []).


... from botcmd ...

/botvar doomed Yes

/botvar doomed
=>Yes

```

## Using botvars from prolog ##

```
1 ?- botvar_get(bot,'isSittingGround',Z).
Z = "No".

2 ?- botvar_set(bot,'isSittingGround',"Yes").
true.

3 ?- botvar_get(bot,'isSittingGround',Z).
Z = "Yes".

```

## Important Note ##

All get/set/key hooks are called in _any_ _unexpected_ order.  This means the use of cuts to stop prolog from exploring more sources is not possible.

Here is a example of this problem:

```
bv:hook_botvar_get(BotID, bot, current_action, X) :-
 botID(Name, BotID),
 tribal_dyn:current_action(Name, X),!.
bv:hook_botvar_get(_, bot, current_action, "nothing").
```

The code could be rewritten as thus:

```
bv:hook_botvar_get(BotID, bot, current_action, X) :-
 botID(Name, BotID), 
  once(tribal_dyn:current_action(Name, X) ; X = "nothing")
```

or as:

```
bv:hook_botvar_get(BotID, bot, current_action, X) :- 
  hook_current_action(BotID,X).


hook_current_action(BotID,X):- botID(Name, BotID),
  tribal_dyn:current_action(Name, X),!.

hook_current_action(_BotID,"nothing").

```





## using botvars from botcmd ##

```
/say $isNight
```

### Command output of //botvars --vars ###
```
[12:30] Success: fromprolog.a .1 = 1
[12:30] Success: fromprolog.b .1 = 2
[12:30] Success: otopopo_dougstribe.selected .Count == 0
[12:30] Success: otopopo_dougstribe.accounts .1 = Otopopo Dougstribe
[12:30] Success: otopopo_dougstribe.accounts .2 = Douglas Miles
[12:30] Success: otopopo_dougstribe.accounts .3 = Dogbert Miles
[12:30] Success: otopopo_dougstribe.accounts .4 = Anniebot Ogborn
[12:30] Success: otopopo_dougstribe.accounts .5 = Anne Ogborn
[12:30] Success: otopopo_dougstribe.accounts .Count == 7
[12:30] Success: otopopo_dougstribe.master .1 = Douglas Miles
[12:30] Success: otopopo_dougstribe.self .1 = Otopopo Dougstribe
[12:30] Success: otopopo_dougstribe.regionprims .1 = Otopopo Dougstribe
[12:30] Success: otopopo_dougstribe.regionprims .2 = Douglas Miles
[12:30] Success: otopopo_dougstribe.regionprims .3 = bowl2 Torus 90cbb984-00c3-4ed0-8d27-bbb61b63d3f9 (localID 2944258497)(ch0)(PrimFlagsFalse InventoryEmpty, ObjectOwnerModify)[](!IsPassable)(hollow 0.99)
[12:30] Success: otopopo_dougstribe.regionprims .4 = Primitive Cylinder 6a9e9cb7-05ad-4d20-9660-49c598775c84 (localID 2944258471)(ch0)(PrimFlagsTrue Scripted)(PrimFlagsFalse InventoryEmpty, ObjectOwnerModify)[](!IsPassable)
[12:30] Success: otopopo_dougstribe.regionprims .5 = Primitive Cylinder 55596f84-05ee-42c6-b46d-41861d902c73 | Listening on 100
touch to stop (localID 2944258470)(ch0)(PrimFlagsTrue Scripted, Touch)(PrimFlagsFalse InventoryEmpty, ObjectOwnerModify)[](IsTouchDefined)(!IsPassable)
[12:30] Success: otopopo_dougstribe.regionprims .Count == 454
[12:30] Success: otopopo_dougstribe.allprims .1 = Otopopo Dougstribe
[12:30] Success: otopopo_dougstribe.allprims .2 = Douglas Miles
[12:30] Success: otopopo_dougstribe.allprims .3 = bowl2 Torus 90cbb984-00c3-4ed0-8d27-bbb61b63d3f9 (localID 2944258497)(ch0)(PrimFlagsFalse InventoryEmpty, ObjectOwnerModify)[](!IsPassable)(hollow 0.99)
[12:30] Success: otopopo_dougstribe.allprims .4 = Primitive Cylinder 6a9e9cb7-05ad-4d20-9660-49c598775c84 (localID 2944258471)(ch0)(PrimFlagsTrue Scripted)(PrimFlagsFalse InventoryEmpty, ObjectOwnerModify)[](!IsPassable)
[12:30] Success: otopopo_dougstribe.allprims .5 = Primitive Cylinder 55596f84-05ee-42c6-b46d-41861d902c73 | Listening on 100
touch to stop (localID 2944258470)(ch0)(PrimFlagsTrue Scripted, Touch)(PrimFlagsFalse InventoryEmpty, ObjectOwnerModify)[](IsTouchDefined)(!IsPassable)
[12:30] Success: otopopo_dougstribe.allprims .Count == 742
[12:30] Success: otopopo_dougstribe.selfknownprims .1 = Atmosphere Tube bb12f9bc-1273-4005-934d-cbc167011a60 | 0.780000 (localID 2944258688)(ch0)(PrimFlagsTrue Scripted, Touch)(PrimFlagsFalse InventoryEmpty, ObjectOwnerModify)[](IsTouchDefined)(!IsPassable)
[12:30] Success: otopopo_dougstribe.selfknownprims .2 = Primitive Box c3ef7796-b41f-4107-9dd0-8aac2544ce26 | 0.680000 (localID 2944258692)(ch0)(PrimFlagsTrue Scripted, Touch)(PrimFlagsFalse InventoryEmpty, ObjectOwnerModify)[](IsTouchDefined)(!IsPassable)
[12:30] Success: otopopo_dougstribe.selfknownprims .3 = yuppie skirt Torus a2ad0a23-c1f3-40a9-9a04-3ad57157321c (localID 2944259031)(childs 8)(PrimFlagsTrue Phantom)(PrimFlagsFalse InventoryEmpty, ObjectOwnerModify)[]
[12:30] Success: otopopo_dougstribe.selfknownprims .4 = Douglas Miles
[12:30] Success: otopopo_dougstribe.selfknownprims .5 = Primitive Cylinder 6a9e9cb7-05ad-4d20-9660-49c598775c84 (localID 2944258471)(ch0)(PrimFlagsTrue Scripted)(PrimFlagsFalse InventoryEmpty, ObjectOwnerModify)[](!IsPassable)
[12:30] Success: otopopo_dougstribe.selfknownprims .Count == 207
[12:30] botvar: Success: otopopo_dougstribe.lasteventprim= NULL
[12:30] Success: otopopo_dougstribe.selectedobjects .Count == 0
[12:30] Success: otopopo_dougstribe.favfood .1 = corn
[12:30] botvar 0 failures and 32 successes
[12:30] botvar 0 failures and 32 successes
```


## AIML botvars ##

AIML users will be familiar with get and set. Get and set are used to record information about the user during an interaction- e.g.

bot: what is your favorite movie?
user: it is Castaway On The Moon
bot: I haven't seen Castaway On The Moon

The bot remembers the user's favorite movie via the set tag in AIML, defined in [AIML 1.0.1 spec, section 7.4.1](http://www.alicebot.org/TR/2001/WD-aiml/#section-set), which looks like

```
<set name="favorite_movie" >
```

and recovers it via the [get tag](http://www.alicebot.org/TR/2001/WD-aiml/#section-get), so the category might look like

```
<category>
   <pattern>_ is <set name="favorite_movie">*</set></pattern>
   <template>I haven't seen <get name="favorite_movie" /></template>
</category>
```

(Note that we also could have used the `<that />` tag, but this is an example)

Notice that favorite\_movie refers to the user's favorite movie. If we have several users we might need to keep separate name spaces for them.
Fortunately, Cogbot's AIMLbot keeps a separate "dictionary" for each user. The set and get tags use the dictionary called 'user' by default. This dictionary matches the AV who made the request.

We might also want to store information about the bot itself. For example, suppose we want a bot to be able to remember the flavor of the day. So one user can tell the bot the flavor of the day, and another ask about it. The bot itself has a dictionary.



This of course has the user's name hard coded, a deficiency we'll correct in a moment.

We might also want to record information about the bot itself. This is done in the dictionary 'bot'. AIML contains a shorthand equivilent `<bot name="foo" />`  for `<get dict="bot" name="foo" />`. The dictionary 'user' is the default dict.

Cogbot botvars are effectively AIML vars. So, for example, we can introduce the bot's master's name with   `<get dict="


&lt;bot dict="$you"..&gt;

 == <get ..>
<bot..> == <get dict="bot" ..>

of course we're more likely to want to store

now explain how bot is slightly different

dict="user"   dict="bot"

dict and user names

dict and bot names

world name space

world/local bots

$somedict.somesubdict.somevar

every request in aiml has local dictionary, which is the name of the person making the request to the bot (usually the person who spoke to the bot).


name with dot doesn't use local dictionary

$vars allowed anywhere in aiml


## Cogbot defined botvars ##

you  - author of current request (the bot may be both 'user' and 'bot')

selected - set of currently selected objects

accounts - all known accounts, logged on or not

master - this bot's master(s)  (might have more than one)

self - the current bot
regionprims - list of all av's, attachments, and objects in this region
> used in "moveprim $regionprims <0,0,-1>
allprims - list of all av's, attachments, and objects known to system
selfknownprims - list of all objects that have an affordance or
> > makes sense for the bot to know about

lasteventprim - the 'current object' - the current object is

> determined in a complex way but is generally the last
> object acted upon by the av.
> Can be changed by AIML or other plugins.
> if the bot's head is free (not being animated)
> it will look at target


## botvars in the default AIML files ##

[here they are](http://code.google.com/p/opensim4opencog/source/browse/data-chatbot/alicebot/config/bot.xml)