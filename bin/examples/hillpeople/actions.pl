:- module(actions , [
		     with_bot/2
		    ]).

:- use_module(cogbot(cogrobot)).
:- use_module(hillpeople(hillpeople)).

% doug - is there a more elegant way than generating botcmd to
% do setbot?
with_bot(Name, Goal) :-
	botID(Name, ID),
	cli_get(ID, name, SLName),
	format(string(S), 'setbot ~w', [SLName]),
        string_to_atom(S, A),
	botClientCmd(A),
	call(Goal). % in an ideal world I'd set it back, but can't find current bot




