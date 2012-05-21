:- module(simplemove, [
		       logon_bots/0,
		       botID/2,
		       test_bot/1,
		       run_test/0,
                       move_bots/0
		      ]).

%--------------------------------------------------------
%
%  simplemove.pl
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
	bot_ran/1.

:- discontiguous
	test_bot/1,
	test_bot_credentials/4,
	bot_waypoints/3.

%
%  Log on the bots and start the test. This is
%  the main entry point for the test.
%
%  workaround for a bug caused by logging too many bots on
%  too fast.
%
logon_bots :-
	repeat,
	(
	    test_bot(Name),
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
        dbgfmt('made botID ~w~n', [BotID]),
        (thread_self(main)->true;thread_exit(true)).

loginuri("http://www.pathwayslms.com:9000/").

%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            bot credentials
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% if you have all the passwords the same this saves some typing
%
pw('hillpeople').
last_name('Hillperson').
%last_name('Dougstribe').
%

num_bots_to_run(6).

test_bot(otopopo) :- num_bots_to_run(X), X >= 1.
test_bot_credentials(otopopo, 'Otopopo', Tribe, PW) :-
    pw(PW),
    last_name(Tribe).
bot_waypoints(otopopo, otopopo1, otopopo2).

test_bot(yuppie) :- num_bots_to_run(X), X >= 2.
test_bot_credentials(yuppie, 'Yuppie', Tribe, PW) :-
    pw(PW),
    last_name(Tribe).
bot_waypoints(yuppie, yuppie1, yuppie2).


test_bot(bignose) :- num_bots_to_run(X), X >= 3.
test_bot_credentials(bignose, 'Bignose', Tribe, PW) :-
    pw(PW),
    last_name(Tribe).
bot_waypoints(bignose, bignose1, bignose2).

test_bot(onosideboard) :- num_bots_to_run(X), X >= 4.
test_bot_credentials(onosideboard,
		 'Onosideboard', Tribe, PW) :-
    pw(PW),
    last_name(Tribe).
bot_waypoints(onosideboard, onosideboard1, onosideboard2).

test_bot(lemonaide) :- num_bots_to_run(X), X >= 5.
test_bot_credentials(lemonaide, 'Lemonaide', Tribe, PW) :-
    pw(PW),
    last_name(Tribe).
bot_waypoints(lemonaide, lemonaide1, lemonaide2).

test_bot(opthamologist) :- num_bots_to_run(X), X >= 6.
test_bot_credentials(opthamologist, 'Opthamologist', Tribe, PW) :-
    pw(PW),
    last_name(Tribe).
bot_waypoints(opthamologist, opthamologist1, opthamologist2).

move_bots :-
	test_bot(Name),
	thread_create(
	    march_up_down(Name),
	    _, [alias(Name)]),
	fail.
move_bots.

run_test :- logon_bots, move_bots.


%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  move the bots around
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
march_up_down(Name) :-
	botID(Name, ID),
	set_current_bot(ID),
	march_up(Name).

march_up(Name) :-
	bot_waypoints(Name, H, _),
	botcmd(moveto(H, 1), MoveStat),
        say_ref('Move', MoveStat),
	botcmd(waitpos(20, H , 1), WaitStat),
        say_ref('Wait', WaitStat),
	say_format('went to ~w', [H]),
	march_down(Name).

march_down(Name) :-
	bot_waypoints(Name, _, H),
	botcmd(moveto(H, 1), MoveStat),
        say_ref('Move', MoveStat),
	botcmd(waitpos(20, H , 1), WaitStat),
        say_ref('Wait', WaitStat),
	say_format('went to ~w', [H]),
	march_down(Name).


%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                    utilities
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

say_format(Format, Args) :-
    'format'(string(Contents), Format, Args),
    'format'(string(QContents), '"~s"', [Contents]),
    botcmd(say(QContents)).

say_ref(Prompt, Ref) :-
    once(cli_to_str(Ref, S) ; S = "Darn cli_to_str failed"),
    say_format('~w = ~s', [Prompt, S]).








