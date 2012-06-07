:- module(acctsupport, [
		       logon_bots/0,
		       botID/2,
		       test_bot/1,
                       logon_bots/1,
                       last_name/1,
                       set_tribe/1,
                       set_num_bots/1
		      ]).

%--------------------------------------------------------
%
%  acctsupport.pl
%
%    Testing of multi bot logon and movement
%
%    This requires a set of the 'hillpeople' bots
%
%---------------------------------------------------------------------

:-set_prolog_flag(double_quotes,string).

dbgfmt(F,A):-'format'(F,A).

%
% Change this line to cd to cogbot/bin directory on your system
%  the exists_file is so it doesn't do it again when reconsulted
:- exists_file('simplemove.pl') -> cd('../bin') ; true.

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
% :- assertIfNewRC(user:file_search_path(hillpeople,
% './examples/hillpeople')).

:- use_module(cogbot(cogrobot)).

:-dynamic
	botID/2,
        last_name/1,
        num_bots_to_run/1,
	bot_ran/1.


:-module_transparent
        num_bots_to_run/1.

:- discontiguous
	test_bot/1,
	test_bot_credentials/4.


%
%  Log on the bots and start the test. This is
%  the main entry point for the test.
%
%  workaround for a bug caused by logging too many bots on
%  too fast.
%
logon_bots :-
        num_bots_to_run(Max),
	repeat,
	(
	    test_bot(Name),
	    logon_by_name(Name),
	    sleep(1),
	    fail
	;
	    true
	).


logon_bots(N):-set_num_bots(N),logon_bots.

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
%%	This logs in a single bot
%
%    It should be called in a new thread.
%    So most users are looking for logon_by_name
%
logon_a_bot(Name) :-
	loginuri(Loginuri),
	test_bot_credentials(Name, First, Last, Password),
        logon_bot(First, Last, Password, Loginuri, "last", BotID),
        assert(botID(Name, BotID)),
        dbgfmt('made botID ~w~n', [BotID]).

loginuri("http://www.pathwayslms.com:9000/").

%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            bot credentials
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% if you have all the passwords the same this saves some typing
%
pw('hillpeople').
last_name('Hillperson').
%%last_name('Dougstribe').
%

set_tribe(Lastname):-retractall(last_name(_)),assert(last_name(Lastname)).


set_num_bots(Value):-retractall(num_bots_to_run(_)),assert(num_bots_to_run(Value)).

num_bots_to_run(1).

%test_bot(dogbert) :- num_bots_to_run(X), X >= 1,last_name('Dougstribe').
test_bot_credentials(dogbert, 'Dogbert', 'Miles', 'tek123').

test_bot(otopopo) :- num_bots_to_run(X), X >= 1.
test_bot_credentials(otopopo, 'Otopopo', Tribe, PW) :-
    pw(PW),
    last_name(Tribe).

test_bot(yuppie) :- num_bots_to_run(X), X >= 2.
test_bot_credentials(yuppie, 'Yuppie', Tribe, PW) :-
    pw(PW),
    last_name(Tribe).

test_bot(bignose) :- num_bots_to_run(X), X >= 3.
test_bot_credentials(bignose, 'Bignose', Tribe, PW) :-
    pw(PW),
    last_name(Tribe).

test_bot(onosideboard) :- num_bots_to_run(X), X >= 4.
test_bot_credentials(onosideboard,
		 'Onosideboard', Tribe, PW) :-
    pw(PW),
    last_name(Tribe).

test_bot(lemonaide) :- num_bots_to_run(X), X >= 5.
test_bot_credentials(lemonaide, 'Lemonaide', Tribe, PW) :-
    pw(PW),
    last_name(Tribe).


test_bot(opthamologist) :- num_bots_to_run(X), X >= 6.
test_bot_credentials(opthamologist, 'Opthamologist', Tribe, PW) :-
    pw(PW),
    last_name(Tribe).
   
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                    A default test
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

