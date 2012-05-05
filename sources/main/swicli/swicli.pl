:- module(swicli,
          [
            cli_debug/1,
            cli_eval/3,
            cli_getSymbol/3,
            cli_intern/3,
            cli_is_defined/2,
            cli_add_event_handler/3,
            cli_add_layout/2,
            cli_array_to_term/2,
            cli_array_to_termlist/2,
            cli_call/3,
            cli_call/4,
            cli_col/2,
            cli_cast/3,
            cli_collection/2,
            cli_find_class/2,
            cli_find_method/3,
            cli_find_type/2,
            cli_get/3,
            cli_get_raw/3,
            cli_get_type/2,
            cli_is_null/1,
            cli_non_null/1,
            cli_is_object/1,
            cli_is_type/2,            
            cli_load_assembly/1,
            cli_memb/2,
            cli_memb/3,
            cli_members/2,
            cli_new/3,
            cli_new/4,
            cli_props_for_type/2,
            cli_set/3,
            cli_set_raw/3,
            cli_subproperty/2,
            cli_shorttype/2,
            cli_subclass/2,
            cli_to_data/2,
            cli_to_data/3,
            cli_to_from_layout/3,
            cli_to_str/2,
            cli_to_str_raw/2,
            cli_to_tagged/2,
            cli_typespec/2,
            cli_write/1,
            cli_writeln/1,
            cli_unify/2,
            cli_with_lock/2,
            cli_lock_enter/1,
            cli_lock_exit/1,
            cli_new_delegate/3,
            link_swiplcs/1,
            to_string/2,
            cli_to_from_recomposer/4,
            cli_fmt/3,
            cli_fmt/2,

            cli_with_gc/1,
            cli_tracker_begin/1,
            cli_tracker_free/1,

            cli_free/1
          ]).



:-dynamic(shortTypeName/2).
:-dynamic(cli_subproperty/2).

:-set_prolog_flag(double_quotes,string).

:-module_transparent(shortTypeName/2).
%%:-module_transparent(cli_get/3).

%------------------------------------------------------------------------------
% Load C++ DLL
%------------------------------------------------------------------------------
:-dynamic(loadedcli_Assembly/0).

foName1(X):- current_prolog_flag(address_bits,32) -> X = swicli32 ;  X= swicli.
foName(Y):-foName1(X), (current_prolog_flag(unix,true) -> Y= foreign(X); Y =X).

loadcli_Assembly:-loadedcli_Assembly,!.
loadcli_Assembly:-assert(loadedcli_Assembly),fail.
loadcli_Assembly:- foName(SWICLI),strip_module(SWICLI,_,DLL),load_foreign_library(DLL).
:-loadcli_Assembly.


%------------------------------------------------------------------------------
% The C++ DLL should have given us cli_load_lib/4
%  ?- cli_load_lib(+AppDomainName, +AssemblyPartialName, +FullClassName, +StaticMethodName).
%------------------------------------------------------------------------------
onWindows:-current_prolog_flag(arch,ARCH),atomic_list_concat([_,_],'win',ARCH).

:- cli_load_lib('SWIProlog','Swicli.Library','Swicli.Library.Embedded','install').

%% remember to: export LD_LIBRARY_PATH=/development/opensim4opencog/bin:/development/opensim4opencog/lib/x86_64-linux:$LD_LIBRARY_PATH

%------------------------------------------------------------------------------
%% cli_load_lib/4 should have given us 
%   cli_load_assembly/1
%------------------------------------------------------------------------------
:- cli_load_assembly('Swicli.Library').
% the cli_load_assembly/1 should have give us a few more cli_<Predicates>



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%% cli_is_type(+Impl,+Type) tests to see if the Impl Object is assignable to Type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
cli_is_type(Impl,Type):-cli_find_type(Type,RealType),cli_call(RealType,'IsInstanceOfType'(object),[Impl],'@'(true)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%% cli_subclass(+Subclass,+Superclass) tests to see if the Subclass is assignable to Superclass
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
cli_subclass(Sub,Sup):-cli_find_type(Sub,RealSub),cli_find_type(Sup,RealSup),cli_call(RealSup,'IsAssignableFrom'('System.Type'),[RealSub],'@'(true)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%% cli_col(+Col,-Elem) iterates out Elems for Collection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
% old version:s cli_collection(Obj,Ele):-cli_call(Obj,'ToArray',[],Array),cli_array_to_term(Array,Vect),!,arg(_,Vect,Ele).
cli_collection(Error,_Ele):-cli_is_null(Error),!,fail.
cli_collection([S|Obj],Ele):-!,member(Ele,[S|Obj]).
cli_collection(Obj,Ele):-
      cli_memb(Obj,m(_, 'GetEnumerator', _, [], [], _, _)),
      cli_call(Obj,'GetEnumerator',[],Enum),!,
      call_cleanup(cli_enumerator_element(Enum,Ele),cli_free(Enum)).
cli_collection(Obj,Ele):-cli_array_to_term(Obj,Vect),!,arg(_,Vect,Ele).
cli_collection(Obj,Ele):-cli_memb(Obj,m(_, 'ToArray', _, [], [], _, _)),cli_call(Obj,'ToArray',[],Array),cli_array_to_term(Array,Vect),!,arg(_,Vect,Ele).
cli_collection(Obj,Ele):-cli_array_to_termlist(Obj,Vect),!,member(Ele,Vect).

cli_col(X,Y):-cli_collection(X,Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%% cli_writeln(+Obj) writes an object out
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
cli_write(S):-cli_to_str(S,W),writeq(W).
cli_writeln(S):-cli_write(S),nl.


cli_fmt(WID,String,Args):-writeq(WID),write(':'),cli_fmt(String,Args),cli_free(WID). %% WID will be made again each call
cli_fmt(String,Args):-cli_call('System.String','Format'('string','object[]'),[String,Args],Result),cli_writeln(Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%% cli_to_str(+Obj,-String) writes an object out to string
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
cli_to_str(Term,Term):- not(compound(Term)),!.
cli_to_str(Term,String):-cli_is_object(Term),!,cli_to_str_raw(Term,String).
cli_to_str([A|B],[AS|BS]):-!,cli_to_str(A,AS),cli_to_str(B,BS).
cli_to_str(Term,String):-Term=..[F|A],cli_to_str(A,AS),String=..[F|AS],!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%% cli_is_null(+Obj) is Object null or void or variable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
cli_is_null(Obj):-notrace(var(Obj);Obj='@'(null);Obj='@'(void)),!.
cli_non_null(Obj):-not(cli_is_null(Obj)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%% cli_is_object(+Obj) is Object a CLR object and not null or void
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
cli_is_object([_|_]):-!,fail.
cli_is_object('@'(O)):-!,O\=void,O\=null.
cli_is_object(enum(_,_)):-!.
cli_is_object(O):-functor(O,F,_),memberchk(F,[struct,object]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%% cli_is_taggedObject(+Obj) is Object a tagged object and not null or void
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
cli_is_taggedObject([_|_]):-!,fail.
cli_is_taggedObject('@'(O)):-!, O\=void,O\=null.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% cli_memb(O,F,X) Object to the member infos of it
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
cli_memb(O,X):-cli_members(O,Y),member(X,Y).
cli_memb(O,F,X):-cli_memb(O,X),member(F,[f,p, c,m ,e]),functor(X,F,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% cli_Preserve(TF,Calls)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
cli_Preserve(TF,Calls):-
   cli_get('Swicli.Library.PrologClient','PreserveObjectType',O),
   call_cleanup((cli_set('Swicli.Library.PrologClient','PreserveObjectType',TF),Calls),
   cli_set('Swicli.Library.PrologClient','PreserveObjectType',O)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% cli_with_collection(Calls).  
%%%%%% as tagged objects are created they are tracked .. when the call is complete any new object tags are released
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
cli_with_collection(Calls):-cli_tracker_begin(O),call_cleanup(Calls,cli_tracker_free(O)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% cli_new(+X, +Params, -V).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%% ?- cli_load_assembly('IKVM.OpenJDK.Core')
%% ?- cli_new('java.lang.Long'(long),[44],Out),cli_to_str(Out,Str).
%% same as..
%% ?- cli_new('java.lang.Long',[long],[44],Out),cli_to_str(Out,Str).
%% arity 4 exists to specify generic types
%% ?- cli_new('System.Int64',[int],[44],Out),cli_to_str(Out,Str).
%% ?- cli_new('System.Text.StringBuilder',[string],["hi there"],Out),cli_to_str(Out,Str).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%
%   X can be:
%    * an atomic classname
%       e.g. 'java.lang.String'
%    * an atomic descriptor
%       e.g. '[I' or 'Ljava.lang.String;'
%    * a suitable type
%       i.e. any class(_,_) or array(_)
%
%   if X is an object (non-array)  type   or  descriptor and Params is a
%   list of values or references, then V  is the result of an invocation
%   of  that  type's  most  specifically-typed    constructor  to  whose
%   respective formal parameters the actual   Params are assignable (and
%   assigned)
%
%   if X is an array type or descriptor   and Params is a list of values
%   or references, each of which is   (independently)  assignable to the
%   array element type, then V is a  new   array  of as many elements as
%   Params has members,  initialised  with   the  respective  members of
%   Params;
%
%   if X is an array type  or   descriptor  and Params is a non-negative
%   integer N, then V is a new array of that type, with N elements, each
%   initialised to Java's appropriate default value for the type;
%
%   If V is {Term} then we attempt to convert a new jpl.Term instance to
%   a corresponding term; this is of  little   obvious  use here, but is
%   consistent with jpl_call/4 and jpl_get/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

cli_new(Clazz,ConstArgs,Out):-Clazz=..[BasicType|ParmSpc],cli_new(BasicType,ParmSpc,ConstArgs,Out).
%%cli_new(ClazzConstArgs,Out):-ClazzConstArgs=..[BasicType|ConstArgs],cli_new(BasicType,ConstArgs,ConstArgs,Out).

/*
 NOTES

 ?- cli_new('System.Int32'(int),[44],Out),cli_to_str(Out,Str).
ERROR: Trying to constuct a primitive type
ERROR: Cant find constructor [int] on System.Int32
   Fail: (8) swicli:cli_new('System.Int32', [int], [44], _G731) ? abort
% Execution Aborted
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% cli_call(+Obj, +CallTerm, -Result).
%%% cli_call(+X, +MethodSpec, +Params, -Result).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%
%   X should be:
%     an object reference
%       (for static or instance methods)
%     a classname, descriptor or type
%       (for static methods of the denoted class)
%
%   MethodSpec should be:
%     a method name (as an atom)
%       (may involve dynamic overload resolution based on inferred types of params)
%
%   Params should be:
%     a proper list (perhaps empty) of suitable actual parameters for the named method
%
%   finally, an attempt will be made to unify Result with the returned result
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

cli_call(Obj,[Prop|CallTerm],Out):-cli_get(Obj,Prop,Mid),!,cli_call(Mid,CallTerm,Out).
cli_call(Obj,CallTerm,Out):-CallTerm=..[MethodName|Args],cli_call(Obj,MethodName,Args,Out).
cli_call(Obj,[Prop|CallTerm],Params,Out):-cli_get(Obj,Prop,Mid),!,cli_call(Mid,CallTerm,Params,Out).
cli_call(Obj,MethodSpec,Params,Out):-cli_call_raw(Obj,MethodSpec,Params,Out_raw),!,Out=Out_raw.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% cli_libCall(+CallTerm, -Out).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
cli_libCall(CallTerm,Out):-cli_call('Swicli.Library.PrologClient',CallTerm,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% cli_get(+X, +Fspec, -V).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%
%   X can be:
%     * a classname, a descriptor, or an (object or array) type
%       (for static fields);
%     * a non-array object
%       (for static and non-static fields)
%     * an array
%       (for 'length' pseudo field, or indexed element retrieval),
%   but not:
%     * a String
%       (clashes with class name; anyway, String has no fields to retrieve)
%
%   Fspec can be:
%       * an atomic field name,
%       * or an integral array index (to get an element from an array,
%	* or a pair I-J of integers (to get a subrange (slice?) of an
%	  array)
%       * A list of  [a,b(1),c] to denoate cli_getting X.a.b(1).c
%
%   finally, an attempt will be made to unify V with the retrieved value
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

cli_get(Obj,_,_):-cli_is_null(Obj),!,fail.
cli_get(Obj,[P],Value):-!,cli_get(Obj,P,Value).
cli_get(Obj,[P|N],Value):-!,cli_get(Obj,P,M),cli_get(M,N,Value),!.
cli_get(Obj,P,ValueOut):-cli_getOverloaded(Obj,P,Value),!,cli_unify(Value,ValueOut).

cli_getOverloaded(Obj,_,_):-cli_is_null(Obj),!,fail,throw(cli_is_null(Obj)).
cli_getOverloaded(Obj,P,Value):-cli_getHook(Obj,P,Value),!.
cli_getOverloaded(Obj,P,Value):-cli_get_raw(Obj,P,Value),!.
cli_getOverloaded(Obj,P,Value):-not(atom(Obj)),cli_get_type(Obj,CType),!,cli_get_typeSubProps(CType,Sub),cli_get_rawS(Obj,Sub,SubValue),cli_getOverloaded(SubValue,P,Value),!.

cli_get_rawS(Obj,[P],Value):-!,cli_get_rawS(Obj,P,Value).
cli_get_rawS(Obj,[P|N],Value):-!,cli_get_rawS(Obj,P,M),cli_get_rawS(M,N,Value),!.
cli_get_rawS(Obj,P,Value):-cli_get_raw(Obj,P,Value),!.

%%cli_get_typeSubProps(CType,Sub):-cli_ProppedType(
cli_get_typeSubProps(CType,Sub):-cli_subproperty(Type,Sub),cli_subclass(CType,Type).

:-dynamic(cli_getHook/3).
:-multifile(cli_getHook/3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% cli_set(+Obj, +PropTerm, +NewValue).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
cli_set(Obj,_,_):-cli_is_null(Obj),!,fail.
cli_set(Obj,[P],Value):-!,cli_set(Obj,P,Value).
cli_set(Obj,[P|N],Value):-!,cli_get(Obj,P,M),cli_set(M,N,Value),!.
cli_set(Obj,P,Value):-cli_setOverloaded(Obj,P,Value).

cli_setOverloaded(Obj,_,_):-cli_is_null(Obj),!,fail.
cli_setOverloaded(Obj,P,Value):-cli_setHook(Obj,P,Value),!.
cli_setOverloaded(Obj,P,Value):-cli_subproperty(Type,Sub),cli_is_type(Obj,Type),cli_get_rawS(Obj,Sub,SubValue),cli_setOverloaded(SubValue,P,Value),!.
cli_setOverloaded(Obj,P,Value):-cli_set_raw(Obj,P,Value),!.

:-dynamic(cli_setHook/3).
:-multifile(cli_setHook/3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% cli_unify(OE,PE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
cli_unify(OE,PE):-OE=PE,!.
cli_unify(enum(_,O1),O2):-!,cli_unify(O1,O2).
cli_unify(O2,enum(_,O1)):-!,cli_unify(O1,O2).
cli_unify(O1,O2):-atomic(O1),atomic(O2),string_to_atom(S1,O1),string_to_atom(S2,O2),!,S1==S2.
cli_unify([O1|ARGS1],[O2|ARGS2]):-!,cli_unify(O1,O2),cli_unify(ARGS1,ARGS2).
cli_unify(O1,O2):-cli_is_taggedObject(O1),cli_to_str(O1,S1),!,cli_unify(O2,S1).
cli_unify(O1,O2):-O1=..[F|[A1|RGS1]],!,O2=..[F|[A2|RGS2]],cli_unify([A1|RGS1],[A2|RGS2]).

%type   jpl_iterator_element(object, datum)

% jpl_iterator_element(+Iterator, -Element) :-

cli_iterator_element(I, E) :- cli_is_type(I,'java.util.Iterator'),!,
	(   cli_call(I, hasNext, [], @(true))
	->  (   cli_call(I, next, [], E)        % surely it's steadfast...
	;   cli_iterator_element(I, E)
	)
	).
cli_enumerator_element(I, E) :- %%cli_is_type('System.Collections.IEnumerator',I),!,
	(   cli_call(I, 'MoveNext', [], @(true))
	->  (   cli_get(I, 'Current', E)        % surely it's steadfast...
	;   cli_enumerator_element(I, E)
	)
	).



cli_to_data(Term,String):- cli_new('System.Collections.Generic.List'(object),[],[],Objs),cli_to_data(Objs,Term,String).
cli_to_data(_,Term,Term):- not(compound(Term)),!.
%%cli_to_data(_Objs,[A|B],[A|B]):-!.
cli_to_data(_Objs,[A|B],[A|B]):-'\+' '\+' A=[_=_],!.
cli_to_data(Objs,[A|B],[AS|BS]):-!,cli_to_data(Objs,A,AS),cli_to_data(Objs,B,BS).
cli_to_data(Objs,Term,String):-cli_is_taggedObject(Term),!,cli_gettermData(Objs,Term,Mid),(Term==Mid-> true; cli_to_data(Objs,Mid,String)).
cli_to_data(Objs,Term,FAS):-Term=..[F|A],cli_to_data1(Objs,F,A,Term,FAS).

cli_to_data1(_Objs,struct,_A,Term,Term):-!.
cli_to_data1(_Objs,object,_A,Term,Term):-!.
cli_to_data1(_Objs,enum,_A,Term,Term):-!.

cli_to_data1(Objs,F,A,_Term,String):-cli_to_data(Objs,A,AS),!,String=..[F|AS].

cli_gettermData(Objs,Term,String):-cli_get_type(Term,Type),cli_props_for_type(Type,Props),cli_getMap(Objs,Term,Props,Name,Value,Name=Value,Mid),!,cli_to_data(Objs,Mid,String).
cli_gettermData(Objs,Term,Mid):-cli_getterm(Objs,Term,Mid),!.


cli_getMap(Objs,Term,_,_,_,_,List):- cli_is_type(Term,'System.Collections.IEnumerable'),findall(ED,(cli_col(Term,E),cli_to_data(Objs,E,ED)),List),!.
cli_getMap(Objs,Term,Props,Name,Value,NameValue,List):-cli_getMap1(Objs,Term,Props,Name,Value,NameValue,List).

cli_getMap1(Objs,Term,Props,Name,Value,NameValue,List):-findall(NameValue,(member(Name,Props),cli_get_raw(Term,Name,ValueM),cli_to_data(Objs,ValueM,Value)),List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% cli_with_lock(Lock,Call)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
cli_with_lock(Lock,Call):-setup_call_cleanup(cli_lock_enter(Lock),Call,cli_lock_exit(Lock)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% cli_with_gc/1 use Forienly defined cli_tracker_begin/1 and cli_tracker_free/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

cli_with_gc(Call):-setup_call_cleanup(cli_tracker_begin(Mark),Call,cli_tracker_free(Mark)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% cli_debug/[1,2]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

cli_debug(format(Format,Args)):-atom(Format),sformat(S,Format,Args),cli_debug(S).
cli_debug(Data):-format(user_error,'~n %% cli_-DEBUG: ~q~n',[Data]),flush_output(user_error).

%%cli_debug(Engine,Data):- format(user_error,'~n %% ENGINE-DEBUG: ~q',[Engine]),cli_debug(Data).

%%to_string(Object,String):-jpl_is_ref(Object),!,jpl_call(Object,toString,[],String).
to_string(Object,String):-cli_to_str(Object,String).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% cli_intern/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
:-dynamic(cli_interned/3).
:-multifile(cli_interned/3).
:-module_transparent(cli_interned/3).
cli_intern(Engine,Name,Value):-retractall(cli_interned(Engine,Name,_)),assert(cli_interned(Engine,Name,Value)),cli_debug(cli_interned(Name,Value)),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% cli_eval/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

:-dynamic(cli_eval_Hook/3).
:-multifile(cli_eval_Hook/3).
:-module_transparent(cli_eval_Hook/3).

cli_eval(Engine,Name,Value):- cli_eval_Hook(Engine,Name,Value),!,cli_debug(cli_eval(Engine,Name,Value)),!.
cli_eval(Engine,Name,Value):- Value=cli_eval(Engine,Name),cli_debug(cli_eval(Name,Value)),!.

cli_eval_Hook(Engine,In,Out):- catch(call((In,Out=In)),E,Out= foobar(Engine,In,E)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% cli_is_defined/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

cli_is_defined(_Engine,Name):-cli_debug(cli_not_is_defined(Name)),!,fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% cli_getSymbol/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

cli_getSymbol(Engine,Name,Value):- (cli_interned(Engine,Name,Value);Value=cli_UnDefined(Name)),!,cli_debug(cli_getSymbol(Name,Value)),!.

%:-use_module(library(jpl)).
%:-use_module(library(pce)).

%%:-interactor.

ensureExported:-current_predicate(swicli:F/A),atom_concat(cli_,_,F),export(F/A),fail.
ensureExported.

:-ensureExported.


end_of_file.



15 ?- cli_to_tagged(sbyte(127),O),cli_get_type(O,T),cli_writeln(O is T).
"127"is"System.SByte"
O = @'C#283319280',
T = @'C#283324332'.

16 ?- cli_to_tagged(long(127),O),cli_get_type(O,T),cli_writeln(O is T).
"127"is"System.Int64"
O = @'C#283345876',
T = @'C#283345868'.

17 ?- cli_to_tagged(ulong(127),O),cli_get_type(O,T),cli_writeln(O is T).
"127"is"System.UInt64"
O = @'C#283346772',
T = @'C#283346760'.

15 ?- cli_to_tagged(sbyte(127),O),cli_get_type(O,T),cli_writeln(O is T).
"127"is"System.SByte"
O = @'C#283319280',
T = @'C#283324332'.

16 ?- cli_to_tagged(long(127),O),cli_get_type(O,T),cli_writeln(O is T).
"127"is"System.Int64"
O = @'C#283345876',
T = @'C#283345868'.

18 ?- cli_to_tagged(343434127,O),cli_get_type(O,T),cli_writeln(O is T).
"343434127"is"System.Int32"
O = @'C#281925284',
T = @'C#281925280'.

19 ?- cli_to_tagged(3434341271,O),cli_get_type(O,T),cli_writeln(O is T).
"3434341271"is"System.UInt64"
O = @'C#281926616',
T = @'C#283346760'.

21 ?- cli_to_tagged(343434127111,O),cli_get_type(O,T),cli_writeln(O is T).
"343434127111"is"System.UInt64"
O = @'C#281930092',
T = @'C#283346760'.

28 ?- cli_to_tagged(34343412711111111111111111111111111111,O),cli_get_type(O,T),cli_writeln(O is T).
"34343412711111111111111111111111111111"is"java.math.BigInteger"
O = @'C#281813796',
T = @'C#281810860'.

