:- module(hillpeople, [
		       logon_bots/0,
		       botID/2,
		       age/2,
		       sex/2,
		       husband_of/2,
		       start_wearing/2,
		       hill_person/1,
		       home/2,
		       ebt/0,
		       tribal_land/1,
		       join_the_tribe/0,
                       set_num_bots/1,
                       tribe_size/1,
                       set_tribe/1,
		       bed_for_name/2
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
%     2. Create 6 bot accounts. You can do this with the
%        createusers script
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
%

% workaround for swipl bug
user:cli_fmt(A,B,C):-swicli:cli_fmt(A,B,C).



:-set_prolog_flag(double_quotes,string).

dbgfmt(F,A):-'format'(F,A).

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
:- use_module(hillpeople(actions)).
:- use_module(hillpeople(tribal)).
:- use_module(hillpeople(aimlbot)).

:-dynamic
	botID/2,
	bot_ran/1.

:- discontiguous
	hill_person/1,
	hill_credentials/4,
	sex/2,
	age/2,
	home/2,
	husband_of/2,
	start_wearing/2.


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
%%	This logs in a single bot
%
logon_a_bot(Name) :-
	loginuri(Loginuri),
	hill_credentials(Name, First, Last, Password),
        logon_bot(First, Last, Password, Loginuri, "home", BotID),
        assert(botID(Name, BotID)),
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

tribe('Hillperson').
%tribe('Dougstribe').

set_tribe(Value):-retractall(tribe(_)),assert(tribe(Value)).

hill_person(otopopo) :- tribe_size(X), X > 0.
hill_credentials(otopopo, 'Otopopo', Tribe, PW) :-
    pw(PW),
    tribe(Tribe).
age(otopopo, 17).
sex(otopopo, m).
home(otopopo, hut1).
husband_of(otopopo, yuppie).
start_wearing(otopopo, [
			'otopopo necklace',
			'otopopo hair',
			'otopopo bone',
			'otopopo diaper',
			'R Tribal sandal',
			'L Tribal sandal',
			'otopopo shape',
			'bald cap',
			'otopopo skin',
			'otopopo eyes']).

hill_person(yuppie) :- tribe_size(X), X > 1.
hill_credentials(yuppie, 'Yuppie', Tribe, PW) :-
    pw(PW),
    tribe(Tribe).
age(yuppie, 21).
sex(yuppie, f).
home(yuppie, hut1).
start_wearing(yuppie, [
		       'yuppie skirt',
		       'R Tribal sandal',
		       'L Tribal sandal',
		       'yuppie smarter shape',
		       'bald cap',
		       'yuppie eyes',
		       'yuppie skin',
		       'yuppietopknot',
		       'yuppieao']).

hill_person(bignose) :- tribe_size(X), X > 2.
hill_credentials(bignose, 'Bignose', Tribe, PW) :-
    pw(PW),
    tribe(Tribe).
age(bignose, 42).
sex(bignose, m).
home(bignose, hut2).
husband_of(bignose, onosideboard).
start_wearing(bignose, [
			'bignose outfit Eyes',
			'bald cap',
			'bignose outfit Shape',
			'bignose outfit Skin',
			'bignose diaper',
			'bignose bracelet',
			'bignose diaper',
			'R Tribal sandal',
			'L Tribal sandal'
		       ]).

hill_person(onosideboard) :- tribe_size(X), X > 3.
hill_credentials(onosideboard,
		 'Onosideboard', Tribe, PW) :-
    pw(PW),
    tribe(Tribe).
age(onosideboard, 35).
sex(onosideboard, f).
home(onosideboard, hut2).
start_wearing(onosideboard, [
			     'R Tribal sandal',
			     'L Tribal sandal',
			     'onosideboard R earring',
			     'onosideboard L earring',
			     'onosideboard skirt',
			     'onosideboard shirt',
			     'onosideboard hair',
			     'onosideboard outfit Eyes',
			     'bald cap',
			     'onosideboard outfit Shape',
			     'onosideboard outfit Skin'
			    ]).

hill_person(lemonaide) :- tribe_size(X), X > 4.
hill_credentials(lemonaide, 'Lemonaide', Tribe, PW) :-
    pw(PW),
    tribe(Tribe).
age(lemonaide, 7).
sex(lemonaide, f).
parent_of(onosideboard, lemonaide).
home(lemonaide, hut2).
start_wearing(lemonaide, [
			     'lemonaide outfit Eyes',
			     'bald cap',
			     'lemonaide outfit Shape',
			     'lemonaide outfit Skin'
			    ]).


hill_person(opthamologist) :- tribe_size(X), X > 5.
hill_credentials(opthamologist, 'Opthamologist', Tribe, PW) :-
    pw(PW),
    tribe(Tribe).
age(opthamologist, 62).
sex(opthamologist, f).
home(opthamologist, hut3).
start_wearing(opthamologist, [
			     'opthamologist Eyes',
			     'opthamologist hair',
			     'opthamologist Shape',
			     'opthamologist Skin'
			    ]).

% fallback, people's beds are named af_otopopo_bed
bed_for_name(Name, BedName) :-
	format(string(BedName), 'af_~w_bed', [Name]).

everybody_be_tribal :-
	hill_person(Name),
	ensure_tribal(Name),
	fail.
everybody_be_tribal.

ensure_tribal(Name):-thread_property(T,_),T=Name,!.
ensure_tribal(Name):-thread_create(be_tribal(Name),_, [alias(Name)]).

join_the_tribe :-
	hill_person(Name),
	thread_join(Name, _),
	fail.
join_the_tribe.

ebt :- everybody_be_tribal.






