#Description how to do various things

# Introduction #

> This page is arranged by thing a programmer might want to do in Cogbot

# Overall #

## Log in a bot ##

```
:-dynamic(raphe1Ran).
raphe1:- raphe1Ran,!.
raphe1:-assert(raphe1Ran),fail.
raphe1:-logon_bot('FirstName','LastName','sekretpassword', "http://www.pathwayslms.com:9000/","home",_Y).
raphe:-thread_create(raphe1,_,[]).

?-raphe.

```

## switch commands to a bot ##

DOUG -  ????

```
37 ?- botcmd('setbot raphe Testbot').
true.

38 ?- botcmd(jump).
true.

39 ?- botcmd('setbot ike Testbot').
true.

40 ?- botcmd(jump).
true.
```

## attach/detach radegast ##

DOUG - how to control which I attach to?
DOUG - detach

## log bots out ##

DOUG

## shut down and exit cleanly ##

DOUG

== Execute prolog from botcmd

```
/swip X is 1 + 1, botcmd(say(X)).
```

# Work with assets #

How do I set permissions when uploading?
How do I set name and description?

How do I save assets?

## Images ##

### Upload ###

```
17 ?- botdo('help uploadimage').
"uploadimage: Upload an image to your inventory."
"Help complete"
"Success help"
true.
```

### Get Image from UUID ###

get as what?

Can I manipulate the image data?

## Sound ##

### Upload ###

### Determine Length ###

## Animation ##

### Upload ###

Loop?  In/out %?
Ease?
Hand pose/expression
priority

### Characteristics ###

What info can I get?

# snapshots #

## Taking a snapshot ##

### To disk ###

### To Inventory ###

# attachments #

## attaching inventory ##

How to set attachment point

## detaching inventory ##

## Touching an attachment ##

### on self ###

### on other ###

# rezzed objects #

## rez standard objects ##

### rez plants ###

## rez from inventory ##

## save to inventory ##

## change rezzed object ##

### move, scale, rotate, change cut and so on ###

### link ###

### change texture ID and UV mapping ###

alpha? colors? glow? fullbright? bumpmap?

### change contents ###

### change permissions ###

### set for sale ###

### share wtih group ###

deed, etc. on front tab of edit panel

### Set locked, physical, phantom, temp ###

### Flexi and light ###

# inventory #

### move items ###

### delete items ###

### modify permissions ###

### wear from inventory ###

# clothing #

## wear ##

## remove ##

## modify (appearance mode) ##

## bake appearance ##

# land #

## modify terrain ##

## parcel controls ##

## reading parcel info ##

### banning/ ejecting ###

## region/estate controls ##

## reading region/estate info ##

# gestures #

## create gesture ##

## activate gesture ##

## play gesture ##

# animations #

## play animation ##

## get list of animations playing ##

## stop animation ##

# profile #

## get picks iterator ##

## get rez date ##

# preferences #

## network settings ##

# friends #

???

# groups #

## Activate a group ##

botdo('activategroup MyGroup').

DOUG - set role?

# getting information from the environment #

## objects rezzed in world ##

### prim params ###

### face params ###

texture animation?

### media params ###

## attachments and clothing on other av's ##

## attachments and clothing on self ##

## listening to chat ##

## sun, wind, clouds ##

## payment event ##

## determining ground height ##

## collisions ##

## detecting newly rezzed/deleted objects ##

## detect sound play ##

## detect having been ejected ##

# money #

## pay lindens ##

# camera -do we even have one? #

# using assets #

## play sound ##

## notecard ##

### read contents ###

### create ###

## script ##

### create ###

### start/stop ###

### add/remove from object inventory ###

### save to/ load from my inventory ###

# voice #

# sit #

## determine what I'm sitting on ##

## sit on something ##

## unsit ##

# point, look, touch #

## set point at/ look at ##

## select an object(s) ##

## touch an object ##

## grab a physical object ##

## detect what another av is selecting ##

# movement #

waitpos

"autopilot: Moves the avatar to the specified global position using simulator autopilot."


## walk ##

botcmd(crouch).
botcmd(jump).

## run ##

## fly ##

## stop flying / fall ##

## jump ##

```
?- botcmd(crouch).
?- botcmd(jump).
```

# media/ browser interaction #

# mute #

## mute/unmute an av ##

## mute/ unmute an object ##

# get help #

```
botdo(help).
... much help text ...


15 ?- botdo('help activategroup').
"activategroup: Set a group as active."
"Help complete"
"Success help"
true.

6 ?- botcmd('debug Debug').
"Logging is set to Debug"
"Success debug"
true.

Usage: //debug [level] where level is one of None, Debug, Error, Info, Warn
```

# Interrupt current action #

"botact: Invoke a command a bot interuptable action (interupts previous foreground action)."