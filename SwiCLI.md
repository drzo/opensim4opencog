# SWI-Prolog 2-Way interface to Common Language Infrastructure (.NET) #

## [Download](http://code.google.com/p/opensim4opencog/downloads/list) ##
> The File named (SWICLI-xxx-DIST-xxxx.zip)
## [Sourcecode](https://github.com/logicmoo/swicli) ##

## [Documentation](http://logicmoo.github.com/swicli/api.html) ##

## [Old Docs](http://logicmoo.github.com/swicli/documentation.html) ##

## Introduction ##

  * Provides SWI-Prolog full control of the Common Language Infrastructure.
  * SwiCLI is a module that works on Linux, OS/X and MS Windows.
  * cli_preds loosely based on jpl_ interface of JPL
  * _Pasted much code from SwiPlCS by Uwe Lesta_
  * See library/swicli.pl for predicate list/documentaton

## Installation ##

### MS windows _requires .NET 2.0 or above_ ###
Copy these two directories onto your Prolog Install Dir.
```
pl//bin/   
pl/library/
```

### Linux OS/X _requires Mono (2.10.8+)_ ###
Copy these two directories onto your Prolog Install Dir
```
pl/lib/  
pl/library/
```

## Running / Examples ##

```
[root@titan bin]# . mono_sysvars.sh
[root@titan bin]# swipl
Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 6.0.2)
Copyright (c) 1990-2011 University of Amsterdam, VU Amsterdam
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
Please visit http://www.swi-prolog.org for details.

For help, use ?- help(Topic). or ?- apropos(Word).

?- use_module(library(swicli)).
SetupProlog

RegisterPLCSForeigns
done RegisterPLCSForeigns
Swicli.Library.Embedded.install suceeded
% library(swicli) compiled into swicli 2.00 sec, 1,411 clauses
true.

?- cli_call('System.Threading.ThreadPool','GetAvailableThreads'(X,Y),_).
X = 200,
Y = 8.
```

```
?- cli_new('System.Collections.Generic.List'('System.String'),[int],[10],Obj).
Obj = @'C#516939544'.
```
```
?- cli_get($Obj,'Count',Out).
Out = 0.
```
```
?- cli_call($Obj,'Add'("foo"),Out).
Out = @void.
```
```
?- cli_call($Obj,'Add'("bar"),Out).
Out = @void.
```
```
?- cli_get($Out,'Count',Out).
Out = 2.
```
```
?- cli_col($Obj,E).
E = "foo" ;
E = "bar" ;
false.
```
```
?- cli_get_type($Obj,Type),cli_get_typename(Type,Name).
Type = @'C#516939520',
Name = 'System.Collections.Generic.List`1[[System.String, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]]'.
```
```
?- cli_get_type($Obj,Type), cli_to_typespec(Type,Name).
Type = @'C#516939520',
Name = 'System.Collections.Generic.List'('String').

?- cli_get_typespec($Obj,Type).
Type = 'System.Collections.Generic.List'('String').


```
```
?- cli_shorttype(stringl,'System.Collections.Generic.List'('String')).
true.
```
```
?- cli_new(stringl,[],O).
O = @'C#516939472'.
```
```
?- cli_get_type($O,Type),cli_typespec(Type,Name).
Type = @'C#516939520',
Name = 'System.Collections.Generic.List'('String').
```

cli\_add\_event\_handler( +Class\_or\_Object, +EventName, +PredicateIndicator) :-

## ADDING A NEW EVENT HOOK ##

We already at least know that the object we want to hook is found via our call to
```
?- botget(['Self'],AM).
```
So we ask for the e/7 (event handlers of the members)
```
?- botget(['Self'],AM),cli_memb(AM,e(A,B,C,D,E,F,G)). 
```
> Press ;;;; a few times until you find the event Name you need (in the B var)
```
A = 6,                                          % index number
B = 'IM',                                       % event name
C = 'System.EventHandler'('InstantMessageEventArgs'),   % the delegation type
D = ['Object', 'InstantMessageEventArgs'],      % the parameter types (2)
E = [],                                         % the generic paramters
F = decl(static(false), 'AgentManager'),        % the static/non static-ness.. the declaring class
G = access_pafv(true, false, false, false)      % the PAFV bits
```

So reading the parameter types  "['Object', 'InstantMessageEventArgs']" lets you know the predicate needs at least two arguments

And "F = decl(static(false), 'AgentManager')" says add on extra argument at start for Origin

> handle\_im(Origin,Obj,IM)

So registering the event is done:
```
?- botget(['Self'],AM), cli_add_event_handler(AM,'IM',handle_im(_Origin,_Object,_InstantMessageEventArgs))
```
To target a predicate such as:
```
handle_im(Origin,Obj,IM):-writeq(handle_im(Origin,Obj,IM)),nl.
```

# Release notes #

## TODO ##
  1. Publish the autoload examples (to website - outside of this package)

## 0.7 ##
  1. Started making release notes
  1. The PL\_agc\_hook (Atom GC) tracker for deciding when to GC foriegn objects
  1. Added the dynamic registrations for exit and abort hooks


