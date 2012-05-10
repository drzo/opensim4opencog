%------------------------------------------------------------------------------
%
%  hillpeople.pl
%
%     Example module that runs the hill people
%
%     Instructions:
%     1. Load the hillpeople.oar into an empty sim
%     2. Create 6 bot accounts. You can do this with the TODO script
%     3. If this file isn't in cogbot/bin/examples/hillpeople modify the
%     line below to cd into cogbot/bin
%     4. change loginuri to your login uri
%     5. change the names, passwords for the bots in this file
%     6. Consult this file
%     7. Query logon_bots.
%
%
%------------------------------------------------------------------------------

:-set_prolog_flag(double_quotes,string).

%
% Change this line to cd to cogbot/bin directory on your system
%  the exists_file is so it doesn't do it again when reconsulted
:- exists_file('hillpeople.pl') -> cd('../..') ; true.

%% add to search paths
assertIfNewRC(Gaf):-catch(call(Gaf),_,fail),!.
assertIfNewRC(Gaf):-asserta(Gaf).


:- assertIfNewRC(user:file_search_path(library, '.')).
:- assertIfNewRC(user:file_search_path(library, '../test')).
:- assertIfNewRC(user:file_search_path(cogbot, './prolog/simulator')).
:- assertIfNewRC(user:file_search_path(cogbot, './prolog')).
:- assertIfNewRC(user:file_search_path(library, './prolog')).

:- use_module(cogbot(cogrobot)).

:-dynamic
	botID/2,
	bot_ran/1.

:- discontiguous
	hill_person/1,
	hill_credentials/4.

logon_bots :-
	repeat,
	(
	    hill_person(Name),
	    logon_by_name(Name),
	    fail
	;
	    true
	).

logon_by_name(Name) :- bot_ran(Name),!.
logon_by_name(Name) :- assert(bot_ran(Name)),fail.
logon_by_name(Name) :-
	    format('making a bot for ~w~n', [Name]),
	    thread_create(logon_a_bot(Name), _, []).

logon_a_bot(Name) :-
	loginuri(Loginuri),
	hill_credentials(Name, First, Last, Password),
	format('before clientManager ~w~n', [Name]),
	cogrobot:clientManager(CM),
	format('after clientManager ~w ~w~n', [Name, CM]),
	cli_call(CM,
		 'CreateBotClient'(First, Last, Password, Loginuri, "home"),
		 BotID),
	format('made botID ~w~n', [BotID]),
	assert(botID(Name, BotID)).

%
% if you have all the passwords the same htis saves some typing
%
pw('hillpeople').

hill_person(otopopo).
hill_credentials(otopopo, 'Otopopo', 'Hillperson', PW) :- pw(PW).

hill_person(bignose).
hill_credentials(bignose, 'Bignose', 'Hillperson', PW) :- pw(PW).

hill_person(yuppie).
hill_credentials(yuppie, 'Yuppie', 'Hillperson', PW) :- pw(PW).

hill_person(onosideboard).
hill_credentials(onosideboard, 'Onosideboard', 'Hillperson', PW) :- pw(PW).

hill_person(lemonaide).
hill_credentials(lemonaide, 'Lemonaide', 'Hillperson', PW) :- pw(PW).

hill_person(opthamologist).
hill_credentials(opthamologist, 'Opthamologist', 'Hillperson', PW) :- pw(PW).

/*
:-dynamic(raphe1Ran).
raphe1:- raphe1Ran,!.
raphe1:-assert(raphe1Ran),fail.
raphe1:-cogrobot:clientManager(X),cli_call(X,'CreateBotClient'('raphe','Testbot','060788', "http://www.pathwayslms.com:9000/","home"),_Y).
raphe:-thread_create(raphe1,_,[]).

*/

loginuri("http://www.pathwayslms.com:9000/").
