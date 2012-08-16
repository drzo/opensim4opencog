:- module(hillpeople, [
		       logon_bots/0,
		       botID/2,
		       tribal_land/1,
                       set_num_bots/1,
                       tribe_size/1,
                       set_tribe/1
		      ]).

%--------------------------------------------------------
%
%  botcmdtests.pl
%
%     Batch of integeration tests of botcmds
%
%
%---------------------------------------------------------------------
%

% workaround for swipl bug
user:cli_fmt(A,B,C):-swicli:cli_fmt(A,B,C).



:-set_prolog_flag(double_quotes,string).

dbgfmt(F,A):-'format'(F,A).


%
% Change this line to cd to cogbot/bin directory on your system
%  the exists_file is so it doesn't do it again when reconsulted
:- exists_file('testpathfind.pl') -> cd('../bin') ; true.

%% add to search paths
assertIfNewRC(Gaf):-catch(call(Gaf),_,fail),!.
assertIfNewRC(Gaf):-asserta(Gaf).

%
%  These lines need adjusted if you've moved this file
%
:- assertIfNewRC(user:file_search_path(library, '.')).
:- assertIfNewRC(user:file_search_path(test, '../test')).
:- assertIfNewRC(user:file_search_path(cogbot, './prolog/simulator')).
:- assertIfNewRC(user:file_search_path(cogbot, './prolog')).
:- assertIfNewRC(user:file_search_path(library, './prolog')).


:- ensure_loaded(test(testsupport)).
:-ensure_loaded(library(swicli)).
:-ensure_loaded(library('simulator/cogrobot')).
:- ensure_loaded(test(botcmdtestlist)).

:-dynamic
	botID/2,
	bot_ran/1.

:- discontiguous
	hill_person/1,
	hill_credentials/4,
	bot_startup/1.

%
%  Log on the bots and start the simulation. This is
%  the main entry point for the simulation.
%
%  The bot logon is staggered at 1 sec intervals as a
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
	    botdo('setmaster Anne Ogborn')
	).

%
% Using dynamic preds allows us to reconsult this file
% without relogging the bots
%
logon_by_name(Name) :- bot_ran(Name),!.
logon_by_name(Name) :- assert(bot_ran(Name)),fail.
logon_by_name(Name) :-
	    dbgfmt('making a bot for ~w~n', [Name]),
            logon_a_bot(Name),!.

%
%	This logs in a single bot
%
logon_a_bot(Name) :-
	loginuri(Loginuri),
	hill_credentials(Name, First, Last, Password),
        logon_bot(First, Last, Password, Loginuri, "home", BotID),
        assert(botID(Name, BotID)),
	bot_startup(Name),
        dbgfmt('made botID ~w~n', [BotID]),
        (thread_self(main)->true;thread_exit(true)).

loginuri("http://www.pathwayslms.com:9000/").
tribal_land('annies haven II/164/135/21').

:-dynamic(tribe_size/1).

set_num_bots(Value):-retractall(tribe_size(_)),assert(tribe_size(Value)).
:-set_num_bots(2).


%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            bot credentials
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% if you have all the passwords the same this saves some typing
%
pw('hillpeople').
% Last name of the bots

:-dynamic(tribe/1).

%tribe('Hillperson').
tribe('Dougstribe').

set_tribe(Value):-retractall(tribe(_)),assert(tribe(Value)).

hill_person(otopopo) :- tribe_size(X), X > 0.
hill_credentials(otopopo, 'Otopopo', Tribe, PW) :-
    pw(PW),
    tribe(Tribe).
bot_startup(otopopo) :- !,
	require_chat_hook.

%  otopopo is the test bot, and hears the chat
%  yuppie does things like be the target for otopopo to find
%
hill_person(yuppie) :- tribe_size(X), X > 1.
hill_credentials(yuppie, 'Yuppie', Tribe, PW) :-
    pw(PW),
    tribe(Tribe).

bot_startup(_).

:- dynamic  current_botcmd_test/2.

bv:hook_botvar_get(BotID, bot, 'dotest', X) :-
	current_botcmd_test(BotID, X),!.

bv:hook_botvar_get(_BotID, bot, dotest, nothing).

bv:hook_botvar_set(BotID, bot, 'dotest', X) :-
	retractall(current_botcmd_test(BotID, _)),
	assert(current_botcmd_test(BotID, X)),
	ignore(X).

bv:hook_botvar_key(_, bot, 'dotest').

bv:hook_botvar_desc(_, bot, 'dotest',
     "ReadWrite - The last test performed by the bot").







