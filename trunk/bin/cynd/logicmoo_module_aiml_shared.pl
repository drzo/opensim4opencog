% ===================================================================
% File 'logicmoo_module_aiml_loader.pl'
% Purpose: An Implementation in SWI-Prolog of AIML
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================

%:-module()
%:-include('logicmoo_utils_header.pl'). %<?
%:- style_check(-singleton).
%%:- style_check(-discontiguous).
:- style_check(-atom).
:- style_check(-string).

throw_safe(Exc):-trace,throw(Exc).

atom_contains(F,C):-notrace((atom(F),atom(C),sub_atom(F,_,_,_,C))).

prolog_must(Call):-Call,!.
prolog_must(Call):-!,aiml_error(Call).


assert_new(NEW):-ignore(retract(NEW)),asserta(NEW).
writeqnl(NEW):-(format('~q.~n',[NEW])),!.


%%%retractall(E):- retractall(E),functor(E,File,A),dynamic(File/A),!.

pp_listing(_Pred):-!. %%functor(Pred,File,A),functor(FA,File,A),listing(File),nl,findall(NV,predicate_property(FA,NV),LIST),writeq(LIST),nl,!.

% =================================================================================
% Utils
% =================================================================================

printPredCount(Msg,Pred,N1):- arg(_,Pred,NG),nonvar(NG),!,
   findall(Pred,Pred,LEFTOVERS),length(LEFTOVERS,N1),debugFmt(num_clauses(Msg,Pred,N1)),!.

printPredCount(Msg,Pred,N1):-!,functor(Pred,File,A),functor(FA,File,A), predicate_property(FA,number_of_clauses(N1)),debugFmt(num_clauses(Msg,File/A,N1)),!.

% =================================================================================
% Loader Utils
% =================================================================================

dynamic_load(Key,PLNAME):- creating_aiml_file(Key,PLNAME),throw_safe(creating_aiml_file(Key,PLNAME)).
dynamic_load(Key,PLNAME):- not(ground(dynamic_load(Key,PLNAME))),throw_safe(not(ground(dynamic_load(Key,PLNAME)))).
dynamic_load(Key,PLNAME):- loaded_aiml_file(Key,PLNAME),trace,throw_safe(loaded_aiml_file(Key,PLNAME)).
dynamic_load(Key,PLNAME):- assert(loaded_aiml_file(Key,PLNAME)),fail.

dynamic_load(Key,_PLNAME):- nonvar(Key), once(cateForFile(_,Key,Cate)),Cate,!.

dynamic_load(_Key,PLNAME):- consult(PLNAME),!.
dynamic_load(_Key,PLNAME):- %% unload_file(PLNAME),
     open(PLNAME, read, In, []),
     repeat,
      line_count(In,Lineno),
      %% double_quotes(_DQBool)
      Options = [variables(_Vars),variable_names(_VarNames),singletons(_Singletons),comment(_Comment)],
      catch((read_term(In,Term,[syntax_errors(error)|Options])),E,(debugFmt(E),fail)),      
      load_term(Term,[line_count(Lineno),file(PLNAME),stream(In)|Options]),
     Term==end_of_file,
     close(In).

load_term(end_of_file,_Options):-!.
load_term(Term,Options):-catch(load_term2(Term,Options),E,(debugFmt(error(load_term(Term,Options,E))),throw_safe(E))).

load_term2(':-'(Term),Options):-!,load_dirrective(Term,Options),!.
load_term2(:-(H,B),Options):-!,load_assert(H,B,Options).
load_term2(Fact,Options):-!,load_assert(Fact,true,Options).

load_assert(H,B,_Options):-assert((H:-B)),!.

load_dirrective(include(PLNAME),_Options):-  (atom_concat_safe(Key,'.pl',PLNAME);Key=PLNAME),!,dynamic_load(Key,PLNAME).
load_dirrective(module(M,Preds),_Options):-!,module(M),call(module(M,Preds)).
load_dirrective(Term,_Options):-!,Term.

showProfilerStatistics(FileMatch):-
   statistics(global,Mem), MU is (Mem / 1024 / 1024),
   printPredCount('showProfilerStatistics: '(MU),FileMatch,_N1).

aimlPredCount:-printAll(aimlPredCount(_,_,0)),printAll((aimlPredCount(Pred,File,Count),Count>0),aimlPredCount(Pred,File,Count)).
aimlPredCount(Pred,File,Count):-source_file(File),atom_contains(File,'aiml'),source_file(Pred,File),functor(Pred,F,A),current_predicate(F/A),
    predicate_property(Pred,dynamic),predicate_property(Pred,number_of_clauses(Count)).

% =================================================================================
% Utils
% =================================================================================

unify_listing(FileMatch):-functor(FileMatch,F,A),unify_listing(FileMatch,F,A),!.

unify_listing(FileMatch,F,A):- (format('~n/* Prediate:  ~q / ~q  ~n',[F,A,FileMatch])),fail.
unify_listing(FileMatch,_F,_A):- printAll(predicate_property(FileMatch,PP),PP),fail.
unify_listing(FileMatch,_F,_A):- (format('~n ~q. ~n */ ~n',[FileMatch])),fail.
unify_listing(FileMatch,F,A):-predicate_property(FileMatch,dynamic),(format(':-dynamic(~q).~n',[F/A])),fail.
unify_listing(FileMatch,F,A):-predicate_property(FileMatch,multifile),(format(':-multifile(~q).~n',[F/A])),fail.
unify_listing(FileMatch,_F,_A):- printAll(FileMatch).

printAll(FileMatch):-printAll(FileMatch,FileMatch).
printAll(Call,Print):-forall(Call,(format('~q.~n',[Print]))).

% =================================================================================
% Utils
% =================================================================================

global_pathname(B,A):-absolute_file_name(B,A),!.
global_pathname(B,A):-relative_pathname(B,A).
relative_pathname(Path,Relative):-absolute_file_name(Path,[relative_to('./')],Absolute),member(Rel,['./','../','../../']),absolute_file_name(Rel,Clip),
   canonical_pathname(Absolute,AbsoluteA),
   canonical_pathname(Clip,ClipA),
   atom_concat(ClipA,RelativeA,AbsoluteA),!,atom_concat(Rel,RelativeA,Relative),!.
relative_pathname(Path,Relative):-canonical_pathname(Path,Relative),!.

canonical_pathname(Absolute,AbsoluteB):-prolog_to_os_filename(AbsoluteA,Absolute),expand_file_name(AbsoluteA,[AbsoluteB]),!.

alldiscontiguous:-!.


% =================================================================================
% Utils
% =================================================================================

% ===============================================================================================
% UTILS
% ===============================================================================================

callInteractive(Term,Var):-catch(callInteractive0(Term,Var),E,aiml_error(E)),!.

%callInteractive0(Term,_):-atom(Term),!,Term,!,writeln(called(Term)),!.
callInteractive0(Term,Var):- call(Term),writeq(Term:Var),nl,fail.
callInteractive0(_,_):-!.

atomsSameCI(Name1,Name2):-downcase_atom(Name1,D1),downcase_atom(Name2,D2),!,D1=D2.
currentContext(Name,X):-makeAimlContext(Name,X),!.


%getWordTokens(WORDS,TOKENS):-concat_atom(TOKENS,' ',WORDS).
%is_string(S):-string(S).

convert_to_string(I,ISO):-
                term_to_string(I,IS),!,
		string_to_list(IS,LIST),!,
		list_replace(LIST,92,[92,92],LISTM),
		list_replace(LISTM,34,[92,34],LISTO),
		string_to_atom(ISO,LISTO),!.

list_replace(List,Char,Replace,NewList):-
	append(Left,[Char|Right],List),
	append(Left,Replace,NewLeft),
	list_replace(Right,Char,Replace,NewRight),
	append(NewLeft,NewRight,NewList),!.
list_replace(List,_Char,_Replace,List):-!.


term_to_string(I,IS):- catch(string_to_atom(IS,I),_,(term_to_atom(I,A),string_to_atom(IS,A))),!.
%well i played with a couple few differnt environment impls.. they have their pros cons.. one impl.. that was unique is that an array of "binding pairs" live in an arraylist.. to be "in" an environment it meant that you held an "index" into the arry list that as you went backwards you'd find your bindings.. each symbol had a java int field "lastBindingIndex" .. that was a "hint" to where you could fastforward the backwards search .. end named binding context also had a "index" to when you leave a named block.. you could quickly reset the top of an index.

atomSplit(A,B):-cyc:atomSplit(A,B).

removePMark(UCase,Atoms):-append(AtomsPre,[Last],UCase),member(Last,[?,('.'),(',')]),!,removePMark(AtomsPre,Atoms).
removePMark(Atoms,Atoms).

randomPick(List,Ele):-length(List,Len),Pick is random(Len),nth0(Pick,List,Ele),!.

all_upper_atom(X):-toUppercase(X,N),N=X.


ifThen(When,Do):-When->Do;true.

atom_concat_safe(L,R,A):- ((atom(A),(atom(L);atom(R))) ; ((atom(L),atom(R)))), !, atom_concat(L,R,A),!.
concat_atom_safe(List,Sep,[Atom]):-atom(Atom),!,concat_atom(List,Sep,Atom),!.
concat_atom_safe(List,Sep,Atom):-atom(Atom),!,concat_atom(ListM,Sep,Atom),!,List = ListM.
concat_atom_safe(List,Sep,Atom):- concat_atom(List,Sep,Atom),!.

upcase_atom_safe(A,B):-atom(A),upcase_atom(A,B).

%appendPred(Pred,X,NEWPRED):- Pred=..ARGS, append(ARGS,[X],NEWARGS),NEWPRED=..NEWARGS,!.

/*
maplist_safe(Pred,LIST):-prolog_must(nonvar(Pred)),prolog_must(nonvar(LIST)),
      findall(E, ((member(E,LIST),debugOnFailureAiml(apply(Pred,[E])))),LISTO),!,debugOnFailureAiml(LIST=LISTO).

maplist_safe(Pred,LIST):-findall(E,(member(E,LIST),apply(Pred,[E])),LISTO),LIST=LISTO.

*/

maplist_safe(_Pred,[]):-!.
maplist_safe(Pred,LIST):-findall(E,(member(E,LIST),debugOnFailureAiml(apply(Pred,[E]))),LISTO),!,debugOnFailureAiml(LIST=LISTO),!.
% maybe needs copy_term? 
%maplist_safe(Pred,[X|L]):- debugOnFailureAiml((appendPred(Pred,X,NEWPRED),copy_term(a(Pred,X,NEWPRED),a(_Pred0,X0,NEWPRED0)))),
%   debugOnFailureAiml(NEWPRED0),!,debugOnFailureAiml(X=X0), debugOnFailureAiml(maplist_safe(Pred,L)),!.


maplist_safe23(Pred,List,List2):-maplist_safe(Pred,List),!,debugOnFailure(List=List2).

maplist_safe(_Pred,[],[]):-!.
maplist_safe(Pred,[A|B],Out):- debugOnFailureAiml(apply(Pred,[A,AA])),!,maplist_safe(Pred,B,BB),!, Out =[AA|BB].

/*

maplist_safe(Pred,[A|B],Out):- apply(Pred,[A,AA]),maplist_safe(Pred,B,BB), Out=[AA|BB].

*/

clean_codes(X,Y):-trim(X,Y),!.
clean_codes(X,X).

%clean_out_atom(X,Y):-atomSplit(X,C),delete(C,'',O),concat_atom_safe(C,' ',Y).
clean_out_atom(X,Y):-atom_codes(X,C),clean_codes(C,D),!,atom_codes(X,D),!,Y=X.

mapWords(_,PATTERN,[PATTERN]):- (var(PATTERN);number(PATTERN)),!.
mapWords(_,[],OUT):-!,[]=OUT.
mapWords(Pred,IN,OUT):- once(call(Pred,IN,MID)), debugOnFailureAiml((MID=IN -> flatten([MID],OUT) ; mapWords(Pred,MID,OUT))).
mapWords(Pred,[I|IN],OUT):-!,debugOnFailureAiml((mapWords(Pred,I,O1),mapWords(Pred,IN,O2),!,append(O1,O2,OUT))),!.
mapWords(Pred,IN,OUT):-atom(IN),debugOnFailureAiml((atomSplit(IN,MID),!,mapWords(Pred,MID,OUT))),!.
mapWords(Pred,IN,[OUT]):-debugOnFailureAiml((compound(IN), IN=..INP, append(Left,[Last],INP), mapWords(Pred,Last,UT),!, append(Left,[UT],OUTP),!, OUT =.. OUTP)).
mapWords(_,IN,[IN]):-trace,!.


toCodes(B,A):-cyc:stringToCodelist(B,AO),(is_list(A) -> A=AO ; string_to_list(AO,A)),!.


dumpList(B):-currentContext(dumpList,Ctx),dumpList(Ctx,B).
dumpList(_,AB):-debugFmt(dumpList(AB)),!.

dumpList(_,[]):-!.
%dumpList(Ctx,[A|B]):-!,say(Ctx,A),dumpList(Ctx,B),!.
%dumpList(Ctx,B):-say(Ctx,dumpList(B)).


traceCall(A):-trace(A,[-all,+fail]),A,!.

/*
debugOnFailureAiml(Call):- clause(Call,(_A,_B)),!,clause(Call,Body),trace,debugOnFailureAiml(Body),!.
debugOnFailureAiml((A,B)):- !,debugOnFailureAiml(A),!,debugOnFailureAiml(B),!.
*/
debugOnFailureAiml(Call):-!,(Call;(trace,Call)),!.
%debugOnFailureAiml(Call):- debugOnFailureAimlTrace(Call),!.
debugOnFailureAiml(Call):- beenCaugth(Call),!.

debugOnFailureAimlTrace(debugOnFailureAiml(Call)):-!,debugOnFailureAimlTrace(Call),!.
debugOnFailureAimlTrace((A,B)):- !,debugOnFailureAimlTrace(A),!,debugOnFailureAimlTrace(B),!.
debugOnFailureAimlTrace(Call):- debugOnFailureAimlTrace1(Call),debugFmt(success(Call)),!.
debugOnFailureAimlTrace(Call):- debugFmt(faild(Call)),!,fail.


debugOnFailureAimlTrace1((A,B)):- !,debugOnFailureAimlTrace1(A),!,debugOnFailureAimlTrace1(B),!.
debugOnFailureAimlTrace1(Call):- functor(Call,F,A),member(F/A,[retract/_,retractall/_]),!,debugFmt(fakingCall(Call)),numbervars(Call,0,_),!.
debugOnFailureAimlTrace1(Call):- Call,!.

beenCaugth(debugOnFailureAiml(Call)):- !, beenCaugth(Call).
beenCaugth((A,B)):- !,debugOnFailureAiml(A),!,beenCaugth(B).
%beenCaugth(Call):- catch(once(Call),E,(debugFmt(caugth(Call,E)),beenCaugth(Call))),!.
beenCaugth(Call):- traceAll,debugFmt(tracing(Call)),debug,trace,Call.


takeout(_,[],[]):-!.
takeout(X,[Y|R],RR):-not(not(X=Y)),!,takeout(X,R,RR),!.
takeout(X,[F|R],[F|S]) :- takeout(X,R,S),!.
takeout(_,X,X).

local_predicate(_,_/0):-!,fail.
local_predicate(_,_/N):-N>7,!,fail.
local_predicate(P,_):-predicate_property(P,built_in),!,fail.
local_predicate(P,_):-predicate_property(P,imported_from(_)),!,fail.
local_predicate(P,_):-predicate_property(P,file(F)),!,atom_contains(F,'aiml_'),!.
local_predicate(P,F/N):-functor(P,F,N),!,fail.


time_file_safe(F,INNER_XML):-exists_file(F),time_file(F,INNER_XML).


%%:- current_predicate(F/N),trace(F/N, -all),fail.
/*
traceAll:- current_predicate(user:F/N),
   functor(P,F,N),
   local_predicate(P,F/N),
   trace(F/N, +fail),fail.
traceAll:- not((predicate_property(clearCateStack/1,_))),!.
traceAll:-findall(_,(member(F,[member/2,debugFmt/1,takeout/3,findall/3,clearCateStack/1]),trace(F, -all)),_).
*/
traceAll:-!.



%%% peekAttributes/2,pushAttributes/2,pushCateElement/2.


lengthAtLeast(N,GE):-atom(N),atom_length(N,L),L>=GE.

