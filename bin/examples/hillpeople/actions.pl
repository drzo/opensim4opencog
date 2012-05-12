:- module(actions , [
		     botcmd_with_bot/2,
		     call_with_bot/2,
		     with_all_bots/1
		    ]).

:- use_module(cogbot(cogrobot)).
:- use_module(hillpeople(hillpeople)).
:- use_module(hillpeople(navigation)).

% doug - is there a more elegant way than generating botcmd to
% do setbot?
/*
with_bot(Name, Goal) :-
	botID(Name, ID),
	cli_get(ID, name, SLName),
	format(string(S), 'setbot ~w', [SLName]),
        string_to_atom(S, A),
	botClientCmd(A),
	call(Goal). % in an ideal world I'd set it back,
                    %but can't find current bot
*/

botcmd_with_bot(Name, BotCmd) :-
	botID(Name, BotID),
	cli_call(BotID, 'ExecuteCommand'(BotCmd), _Ret).


call_with_bot(_Name, _Goal).  % TODO :-
/*	botID(Name, BotID),
	cli_call(BotID, 'ExecuteCommand'('jump'), _Ret).
*/

%
%  call goal on all bots
%
with_all_bots(Goal) :-
	hill_person(Name),
	call_with_bot(Name, Goal).




