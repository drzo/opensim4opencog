
:- module(clipl,
          [ cli_debug/1,
            cli_Intern/3,
            cli_Eval/3,
            cli_GetSymbol/3,
            cli_IsDefined/2,
            cliAddEventHandler/3,
            cliAddLayout/2,
            cliCall/3,
            cliCall/4,
            cliCollection/2,
            cliFindClass/2,
            cliFindMethod/3,
            cliFindType/2,
            cliGet/3,
            cliGetS/3,
            cliLoadAssembly/1,
            cliMemb/2,
            cliMemb/3,
            cliMembers/2,
            cliNew/3,
            cliNew/4,
            cliSet/3,
            cliShortType/2,
            cliToString/2,
            cliToStringS/2,

            link_swiplcs/1,
            to_string/2
          ]).

:-dynamic(shortTypeName/2).

:-module_transparent(shortTypeName/2).
:-module_transparent(cliGet/3).

:-load_foreign_library(swicli35).
:-cli_load_lib('SwiPlCs','SbsSW.SwiPlCs.swipl_win','install').
:-cliLoadAssembly('SwiPlCs.dll').


%% old version:s cliCollection(Obj,Ele):-cliCall(Obj,'ToArray',[],Array),cliArrayToTerm(Array,Vect),!,arg(_,Vect,Ele).
cliCollection(Obj,Ele):-  
      cliCall(Obj,'GetEnumerator',[],Enum),
      call_cleanup(cli_enumerator_element(Enum,Ele),cliFree(Enum)).
   
cliWriteln(S):-cliToString(S,W),writeq(W),nl.


cliMemb(O,X):-cliMembers(O,Y),member(X,Y).
cliMemb(O,F,X):-cliMemb(O,X),member(F,[f,p, c,m ,e]),functor(X,F,_).

%% ?- cliNew('java.lang.Long'(long),[44],Out),cliToString(Out,Str).
%% same as..
%% ?- cliNew('java.lang.Long',[long],[44],Out),cliToString(Out,Str).
%% arity 4 exists to specify generic types
cliNew(Clazz,ConstArgs,Out):-Clazz=..[BasicType|ParmSpc],cliNew(BasicType,ParmSpc,ConstArgs,Out).

cliCall(Obj,CallTerm,Out):-CallTerm=..[MethodName|Args],cliCall(Obj,MethodName,Args,Out).



%type   jpl_iterator_element(object, datum)

% jpl_iterator_element(+Iterator, -Element) :-

cli_iterator_element(I, E) :- %%cliIsInstance('java.util.Iterator',I),!,
	(   cliCall(I, hasNext, [], @(true))
	->  (   cliCall(I, next, [], E)        % surely it's steadfast...
	;   cli_iterator_element(I, E)
	)
	).
cli_enumerator_element(I, E) :- %%cliIsInstance('System.Collections.IEnumerator',I),!,
	(   cliCall(I, 'MoveNext', [], @(true))
	->  (   cliGet(I, 'Current', E)        % surely it's steadfast...
	;   cli_enumerator_element(I, E)
	)
	).


cliIsObject(X):- \+ cliIsNull(X),functor(X,F,_),F='@'.

cliIsNull(Obj):-(var(Obj);Obj='@'(null);Obj='@'(void)),!.

cliGetS(Obj,_,_):-cliIsNull(Obj),!,fail.
cliGetS(Obj,[P],Value):-!,cliGet(Obj,P,Value).
cliGetS(Obj,[P|N],Value):-cliGet(Obj,P,M),cliGetS(M,N,Value),!.


cliToStringS(Term,Term):- (var(Term);atomic(Term);not(compound(Term))),!.
cliToStringS(Term,String):-cliIsObject(Term),!,cliToString(Term,String).
cliToStringS([A|B],[AS|BS]):-!,cliToStringS(A,AS),cliToStringS(B,BS).
cliToStringS(Term,String):-Term=..[F|A],cliToStringS(A,AS),String=..[F|AS],!.

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






:-cli_debug('I am swi_cli.pl in BIN DIR!!').

:-use_module(library(jpl)).
%:-use_module(library(pce)).

%%:-interactor.
