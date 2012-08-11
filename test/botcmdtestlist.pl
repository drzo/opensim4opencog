:- module(botcmdtestlist, [test/4]).
:-use_module(library(swicli)).
:-use_module(library('simulator/cogrobot')).
:- use_module(test(testsupport)).

:- discontiguous test/4.

test('I will make the radegast GUI appear',
     botdo(showgui),
    10,
    'Did the Radegast GUI appear?').

test('I will teleport to the location marked TELEPORT TEST',
     botdo(teleport('annies haven',237,19,23)),
     2,
     'Is the Bot at the teleport target?').

test('I will sit on the botchair',
     test_sit,
     1,
     'Did the bot sit on chair and then stand up?').

test_sit :-
	botapi(teleport('annies haven',232,21,23)),
	botapi(sit(botchair)),
	sleep(10.0),
	botapi(stand).

test('I will move to the big yellow donut',
     botapi(moveto(bigyellowdonut)),
     1,
     'Did the bot move to the donut?').

test('I will describe objects around me',
     describe_stuff,
     1,
     'Did the bot describe the objects, and only the objects, near itself?').

describe_stuff :-
     botapi(teleport('annies haven',215.7, 18.5, 22)),
     botapi(describe(maxdist(4),family,parentof)).

/*
test('I will friend you',
     friend_nearby,
     1,
     'Did the bot friend you?').

friend_nearby :-
	world_avatar(Bot),
	cli_get(
*/






