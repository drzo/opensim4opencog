:- module(hillpeople, [
		       logon_bots/0,
		       botID/2,
		       age/2,
		       sex/2,
		       husband_of/2,
		       hill_person/1,
		       home/2
		      ]).

%--------------------------------------------------------
%
%  hillpeople.pl
%
%     Example module that runs the hill people.
%
%     The hill people is a simple demo of some traditional people who
%     demonstrate management-by-felt-need in a manner similar to the
%     sims.
%
%     Instructions:
%     1. Load the hillpeople.oar into an empty sim
%     2. Create 6 bot accounts. You can do this with the TODO script
%     3. If this file isn't in cogbot/bin/examples/hillpeople modify the
%     line below to cd into cogbot/bin
%     4. change loginuri to your login uri
%     5. change the names, passwords for the bots in this file
%     6. comment out any bots you automatically log in with
%     botconfig.xml
%     7. Consult this file
%     8. Query logon_bots.  This logs on the bots
%     9. Query ebt.  This starts the bots doing their thing.
%
%
%---------------------------------------------------------------------

:-set_prolog_flag(double_quotes,string).

%
% Change this line to cd to cogbot/bin directory on your system
%  the exists_file is so it doesn't do it again when reconsulted
:- exists_file('hillpeople.pl') -> cd('../..') ; true.

%% add to search paths
assertIfNewRC(Gaf):-catch(call(Gaf),_,fail),!.
assertIfNewRC(Gaf):-asserta(Gaf).

%
%  These lines need adjusted if you've moved this file
%
:- assertIfNewRC(user:file_search_path(library, '.')).
:- assertIfNewRC(user:file_search_path(library, '../test')).
:- assertIfNewRC(user:file_search_path(cogbot, './prolog/simulator')).
:- assertIfNewRC(user:file_search_path(cogbot, './prolog')).
:- assertIfNewRC(user:file_search_path(library, './prolog')).
:- assertIfNewRC(user:file_search_path(hillpeople, './examples/hillpeople')).

:- use_module(cogbot(cogrobot)).
:- use_module(hillpeople(slow_planner)).
:- use_module(hillpeople(actions)).
:- use_module(hillpeople(tribal)).

:-dynamic
	botID/2,
	bot_ran/1.

:- discontiguous
	hill_person/1,
	hill_credentials/4,
	sex/2,
	age/2,
	home/2,
	husband_of/2.

%
%  Log on the bots and start the simulation. This is
%  the main entry point for the simulation.
%
%  The bot logon is staggered at 30 sec intervals as a
%  workaround for a bug caused by logging too many bots on
%  too fast.
%
logon_bots :-
	repeat,
	(
	    hill_person(Name),
	    logon_by_name(Name),
	    sleep(1),
	    fail
	;
	    true
	).

%
% Using dynamic preds allows us to reconsult this file
% without relogging the bots
%
logon_by_name(Name) :- bot_ran(Name),!.
logon_by_name(Name) :- assert(bot_ran(Name)),fail.
logon_by_name(Name) :-
	    format('making a bot for ~w~n', [Name]),
	    thread_create(logon_a_bot(Name), _, []).

%
%%	This logs in a single bot
%
%    It should be called in a new thread.
%    So most users are looking for logon_by_name
%
logon_a_bot(Name) :-
	loginuri(Loginuri),
	hill_credentials(Name, First, Last, Password),
	format('before clientManager ~w~n', [Name]),
	cogrobot:clientManager(CM),
	format('after clientManager ~w ~w~n', [Name, CM]),
	cli_call(CM,
		 'CreateBotClient'(First, Last, Password, Loginuri, "home"),
		 BotID),
        cli_call(BotID,'Login',_),
	format('made botID ~w~n', [BotID]),
	assert(botID(Name, BotID)).


loginuri("http://www.pathwayslms.com:9000/").

%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            bot credentials
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% if you have all the passwords the same this saves some typing
%
pw('hillpeople').
tribe('Hillperson').

hill_person(otopopo).
hill_credentials(otopopo, 'Otopopo', Tribe, PW) :-
    pw(PW),
    tribe(Tribe).
age(otopopo, 17).
sex(otopopo, m).
home(otopopo, hut1).
husband_of(otopopo, yuppie).

hill_person(yuppie).
hill_credentials(yuppie, 'Yuppie', Tribe, PW) :-
    pw(PW),
    tribe(Tribe).
age(yuppie, 21).
sex(yuppie, f).
home(yuppie, hut1).

hill_person(bignose).
hill_credentials(bignose, 'Bignose', Tribe, PW) :-
    pw(PW),
    tribe(Tribe).
age(bignose, 42).
sex(bignose, m).
home(bignose, hut2).
husband_of(bignose, onosideboard).

hill_person(onosideboard).
hill_credentials(onosideboard,
		 'Onosideboard', Tribe, PW) :-
    pw(PW),
    tribe(Tribe).
age(onosideboard, 35).
sex(onosideboard, f).
home(onosideboard, hut2).

hill_person(lemonaide).
hill_credentials(lemonaide, 'Lemonaide', Tribe, PW) :-
    pw(PW),
    tribe(Tribe).
age(lemonaide, 7).
sex(lemonaide, f).
parent_of(onosideboard, lemonaide).
home(lemonaide, hut2).

hill_person(opthamologist).
hill_credentials(opthamologist, 'Opthamologist', Tribe, PW) :-
    pw(PW),
    tribe(Tribe).
age(opthamologist, 62).
sex(opthamologist, f).
home(opthamologist, hut3).


home(_, hut3). % fallback, stay in 3 if you don't know

everybody_be_tribal :-
	hill_person(Name),
	thread_create(
	    be_tribal(Name),
	    _, []),
	fail.
everybody_be_tribal.

ebt :- everybody_be_tribal.






