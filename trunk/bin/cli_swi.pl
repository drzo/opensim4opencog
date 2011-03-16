

:-use_module(library(jpl)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%% cli_debug/[1,2]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

cli_debug(format(Format,Args)):-atom(Format),sformat(S,Format,Args),cli_debug(S).
cli_debug(Data):-format(user_error,'~n %% CLI-DEBUG: ~q~n',[Data]),flush_output(user_error).

%%cli_debug(Engine,Data):- format(user_error,'~n %% ENGINE-DEBUG: ~q',[Engine]),cli_debug(Data).

to_string(Object,String):-jpl_is_ref(Object),!,jpl_call(Object,toString,[],String).
to_string(Object,String):-Object=String.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%% cli_Intern/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
:-dynamic(cli_Interned/3).
:-multifile(cli_Interned/3).
:-module_transparent(cli_Interned/3).
cli_Intern(Engine,Name,Value):-retractall(cli_Interned(Engine,Name,_)),assert(cli_Interned(Engine,Name,Value)),cli_debug(cli_Interned(Name,Value)),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%% cli_Eval/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

:-dynamic(cli_Eval_Hook/3).
:-multifile(cli_Eval_Hook/3).
:-module_transparent(cli_Eval_Hook/3).

cli_Eval(Engine,Name,Value):- cli_Eval_Hook(Engine,Name,Value),!,cli_debug(cli_Eval(Engine,Name,Value)),!.
cli_Eval(Engine,Name,Value):- Value=cli_Eval(Engine,Name),cli_debug(cli_Eval(Name,Value)),!.

cli_Eval_Hook(Engine,In,Out):- Out = foobar(Engine,In).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%% cli_IsDefined/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

cli_IsDefined(_Engine,Name):-cli_debug(cli_not_IsDefined(Name)),!,fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%% cli_GetSymbol/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

cli_GetSymbol(Engine,Name,Value):- (cli_Interned(Engine,Name,Value);Value=cli_UnDefined(Name)),!,cli_debug(cli_GetSymbol(Name,Value)),!.






:-cli_debug('I am swi_cli.pl !!').

:-use_module(library(jpl)).