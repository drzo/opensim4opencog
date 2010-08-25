% ===================================================================
% File 'logicmoo_module_aiml.pl'
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
:- style_check(-discontiguous).
:- style_check(-atom).
:- style_check(-string).





global_pathname(B,A):-absolute_file_name(B,A),!.
global_pathname(B,A):-relative_pathname(B,A).
relative_pathname(Path,Relative):-absolute_file_name(Path,[relative_to('./')],Absolute),member(Rel,['./','../','../../']),absolute_file_name(Rel,Clip),
   canonical_pathname(Absolute,AbsoluteA),
   canonical_pathname(Clip,ClipA),
   atom_concat(ClipA,RelativeA,AbsoluteA),!,atom_concat(Rel,RelativeA,Relative),!.
relative_pathname(Path,Relative):-canonical_pathname(Path,Relative),!.

canonical_pathname(Absolute,AbsoluteB):-prolog_to_os_filename(AbsoluteA,Absolute),expand_file_name(AbsoluteA,[AbsoluteB]),!.




alldiscontiguous:-!.

:-multifile(what/3).
:-multifile(response/2).
%:-dynamic(aimlCate/1).

atom_contains(F,C):-notrace((atom(F),sub_atom(F,_,_,_,C))).

run_chat_tests:-
   test_call(alicebot('Hi')),
   test_call(alicebot('What is your name')),
   test_call(alicebot('My name is Fred.')),
   test_call(alicebot('what is my name?')).

test_call(G):-writeln(G),ignore(once(catch(G,E,writeln(E)))).

main_loop1(Atom):- current_input(In),!,
            read_line_to_codes(In,Codes),!,
            atom_codes(Atom,Codes),!,
            alicebot(Atom),!.

main_loop:-repeat,main_loop1(_),fail.
%:-abolish(aimlCate/10).

% ===================================================================
%  aimlCate database decl
% ===================================================================

:-dynamic(aimlCateSigCached/1).
aimlCateSig(X):-aimlCateSigCached(X),!.
aimlCateSig(Pred):-aimlCateOrder(List),length(List,L),functor(Pred,aimlCate,L),asserta(aimlCateSigCached(Pred)),!.

aimlCateOrder([graph,precall,topic,that,[pattern,input],flags,call,guard,template,userdict,lineno,filename]).

% [graph,precall,topic,that,pattern,flags,call,guard,template,userdict]
cateMemberTags(Result):- aimlCateOrder(List), findall(E,(member(E0,List),once((E0=[E|_];E0=E))), Result).


makeAimlCateSig(Ctx,ListOfValues,Pred):-aimlCateSig(Pred),!,makeAimlCate(Ctx,ListOfValues,Pred,current_value),!.

:- aimlCateOrder(List),length(List,L),dynamic(aimlCate/L),multifile(aimlCate/L). 

% ===================================================================
%  AimlContexts
%   They hold name-values in
%     -- assoc/1 lists
%     -- open tailed lists
%     -- frame/1 contains one or more of the above

%% v/3s 
%  = v(Value,Setter,KeyDestructor)

%% frame/3s
%  = frame(Named,Destructor,Ctx)


% ===================================================================

makeAimlContext(Name,Ctx):-makeAimlContext1(Name,Ctx),!,setCtxValue(ctx,Ctx,Name).

makeAimlContext1(Gensym_Key, [frame(Gensym_Key,throw(no_destructor),[assoc(AL)|_])|_]):-    
   list_to_assoc([a-v(is_a),a-v(is_a2),b-v(is_b)],AL).


unwrapValue(HValue,TValue):-TValue==deleted,!,not(unwrapValue1(HValue,_)),!.
unwrapValue(HValue,TValue):-unwrapValue1(HValue,Value),!,TValue=Value.

unwrapValue1(v(ValueHolder,_SetterFun,_KeyDestroyer),Value):-!,unwrapValue1(ValueHolder,Value).
unwrapValue1(deleted,_):-!,fail.
unwrapValue1(Value,Value):-!.

bestSetterFn(v(_,Setter,_),_OuterSetter,Setter):-!.
bestSetterFn(_Value,OuterSetter,OuterSetter).

getCtxValue(Name,Ctx,Value):-notrace(get_ctx_holder(Ctx,Holder)),get_o_value(Name,Holder,HValue,_Setter),!, unwrapValue(HValue,Value).

setCtxValue(Name,Ctx,Value):-get_ctx_holder(Ctx,Holder),get_o_value(Name,Holder,HValue,Setter),unwrapValue(HValue,CurrentValue),!,(CurrentValue=Value;call(Setter,Value)),!.
setCtxValue(Name,Ctx,Value):-addCtxValue1(Name,Ctx,Value),!.

addCtxValue(Name,Ctx,Value):-addCtxValue1(Name,Ctx,Value),!.
addCtxValue1(Name,Ctx,Value):-get_ctx_holderFreeSpot(Ctx,Name=v(Value,_,Destructor),Destructor),!.

remCtxValue(Name,Ctx,_Value):-setCtxValue(Name,Ctx,deleted),!.


pushCtxFrame(Name,Ctx,NewValues):-get_ctx_holderFreeSpot(Ctx,Holder,GuestDest),!,Holder=frame(Name,GuestDest,NewValues).

popCtxFrame(Name,Ctx,PrevValues):-get_ctx_frame_holder(Ctx,Name,Frame),Frame = frame(Name,Destructor,PrevValues),Destructor,!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% get the frame holder
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_ctx_frame_holder(Ctx,Name,R):-compound(Ctx),get_ctx_frame_holder1(Ctx,Name,R).
get_ctx_frame_holder1(v(_Ctx),_Name,_R):-!,fail.
get_ctx_frame_holder1(frame(Name,Dest,Ctx),Name,R):- R = frame(Name,Dest,Ctx),!.
get_ctx_frame_holder1([H|T],Name,R):- nonvar(H), !, ( get_ctx_frame_holder(T,Name,R);get_ctx_frame_holder1(H,Name,R)) .
%%get_ctx_frame_holder1(Ctx,Name,Ctx):-!,get_ctx_frame_holder1(Ctx,Name,R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% get the holders areas last in first out %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% get_ctx_holder(+Ctx, -PlaceToSearch),

get_ctx_holder(Ctx,R):-compound(Ctx),get_ctx_holder1(Ctx,R).
get_ctx_holder1([H|T],R):- nonvar(H), !, ( get_ctx_holder(T,R);get_ctx_holder1(H,R)) .
get_ctx_holder1(v(_Ctx),_R):-!,fail.%% get_ctx_holder(Ctx,R).
get_ctx_holder1(frame(_N,_Dest,Ctx),R):-!,get_ctx_holder(Ctx,R).
%get_ctx_holder1(Ctx,R):- functor(Ctx,F,A),A<3,!,fail.
get_ctx_holder1(assoc(Ctx),assoc(Ctx)):-!.
get_ctx_holder1(Ctx,Ctx).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% find a free area to place a: vv(name,val) %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% get_ctx_holderFreeSpot(+Ctx, -Put_NV, -CallToRemoveNV)

get_ctx_holderFreeSpot(Ctx,NamedValue,Destruct):-compound(Ctx),get_ctx_holderFreeSpot1(Ctx,NamedValue,Destruct).

get_ctx_holderFreeSpot1(assoc(_Ctx),_,_):-!,fail.
get_ctx_holderFreeSpot1(frame(Key,_Inner_Dest,Ctx),NamedValue,Destruct):- nonvar(Key), !, get_ctx_holderFreeSpot1(Ctx,NamedValue,Destruct).
get_ctx_holderFreeSpot1(Ctx,NamedValue,Destruct):-functor(Ctx,F,A),!,get_ctx_holderFreeSpot1(Ctx,F,A,NamedValue,Destruct).

get_ctx_holderFreeSpot1(Ctx,'.',2,NamedValue,nb_setarg(Ctx,2,NEXT)):-arg(2,Ctx,Try1), var(Try1),!, Try1 = [NamedValue|NEXT].
get_ctx_holderFreeSpot1(Ctx,'.',2,NamedValue,Destruct):-arg(2,Ctx,Try2),get_ctx_holderFreeSpot(Try2,NamedValue,Destruct).

%%get_ctx_holderFreeSpot1(Ctx,_,_,NamedValue,_):-!,fail.
%%get_ctx_holderFreeSpot1(Ctx,_,_,NamedValue,nb_setarg(Ctx,N,NEXT)):-arg(N,Ctx,Try3),var(Try3),!, Try3 = [NamedValue|NEXT].
%%get_ctx_holderFreeSpot1(Ctx,_,_,NamedValue,Destruct):-arg(N,Ctx,Try4),get_ctx_holderFreeSpot(Try4,NamedValue,Destruct).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% find the value holder associated with a keyname
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_ctx_value(Name,Ctx,Value,Setter):-nonvar(Name),var(Value),get_o_value(Name,Ctx,Value,OuterSetter),bestSetterFn(Value,OuterSetter,Setter).

get_o_value(Name,Ctx,Value,Setter):-compound(Ctx),get_o_value1(Name,Ctx,Value,Setter).
get_o_value1(Name,assoc(Ctx),Value,throw(cannot_set(Name))):- get_assoc(Name,Ctx,Value),!.
get_o_value1(Name,frame(Key,_Inner_Dest,Ctx),Value,Setter):- nonvar(Key), get_o_value(Name,Ctx,Value,Setter),!.
get_o_value1(Name,[H|T],Value,Setter):- !,(get_o_value(Name,T,Value,Setter);get_o_value1(Name,H,Value,Setter)).
get_o_value1(Name,Pred,Value,Setter):-functor(Pred,F,A),!,get_n_value(Name,Pred,F,A,Value,Setter).

get_n_value(Name,Name,_F,_A,_Value,_):-!,fail.
get_n_value(Name,Pred,Name,1,Value,nb_setarg(1,Pred)):-arg(1,Pred,Value).
get_n_value(Name,Pred,Name,_,Value,Setter):- arg(1,Pred,Value),!,arg(2,Pred,Setter). %% value can actually be 'Pred'
get_n_value(Name,Pred,Dash,2,Value,nb_setarg(2,Pred)):-arg(1,Pred,Name),member(Dash,[=,-,vv]),!, arg(2,Pred,Value).
%%get_n_value(Name,Pred,'.',2,Value,Setter):-arg(2,Pred,Try1), get_o_value(Name,Try1,Value,Setter);(arg(1,Pred,Try2),get_o_value(Name,Try2,Value,Setter)).
%%get_n_value(Name,Pred,_,_,Value,Setter):- !, arg(_,Pred,Try2),get_o_value(Name,Try2,Value,Setter).


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


:-dynamic(default_channel/1).
:-dynamic(default_user/1).

%default_channel( "#logicmoo").
%default_user(    "default_user").         

% say(Say):-writeq(Say),nl.
toCodes(B,A):-cyc:stringToCodelist(B,AO),(is_list(A) -> A=AO ; string_to_list(AO,A)),!.


dumpList(B):-currentContext(dumpList,Ctx),dumpList(Ctx,B).
dumpList(_,AB):-debugFmt(dumpList(AB)),!.

dumpList(_,[]):-!.
%dumpList(Ctx,[A|B]):-!,say(Ctx,A),dumpList(Ctx,B),!.
%dumpList(Ctx,B):-say(Ctx,dumpList(B)).

% ===============================================================================================
% ALICE IN PROLOG
% ===============================================================================================

say(X):-currentContext(say(X),Ctx),say(Ctx,X),!.
say(Ctx,X):- aiml_eval(Ctx,X,Y),!,debugFmt(Y),!.

alicebot:-repeat,
	read_line_with_nl(user,Codes,[]),
        makeAimlContext(alicebot,Ctx),
        once((atom_codes(Atom,Codes),alicebotCTX(Ctx,Atom))),fail.

alicebot(Input):- currentContext(alicebot(Input),Ctx),!,alicebotCTX(Ctx,Input).

alicebotCTX(_Ctx,Input):- atom(Input),catch(((atom_to_term(Input,Term,Vars),callInteractive0(Term,Vars))),_,fail),!.
alicebotCTX(Ctx,Input):- once((alicebotCTX(Ctx,Input,Resp),say(Ctx,Resp))),!.
alicebotCTX(Ctx,_):- say(Ctx,'-no response-').

alicebotCTX(_Ctx,[],_):-debugFmt('no input'),!,fail.
alicebotCTX(Ctx,Input,Resp):- atom(Input),!,
      getWordTokens(Input,TokensO),!,Tokens=TokensO,
      alicebotCTX(Ctx,Tokens,Resp),!.

alicebotCTX(Ctx,[TOK|Tokens],Output):- atom(TOK),atom_concat('@',_,TOK),!,systemCall(Ctx,'bot',[TOK|Tokens],Output),debugFmt(Output).

alicebotCTX(Ctx,Tokens,Resp):-
   toUppercase(Tokens,UCase),!,
   removePMark(UCase,Atoms),!,
   alicebot2(Ctx,Atoms,Resp),!.

alicebot2(Atoms,Resp):- currentContext(alicebot2(Atoms),Ctx),!,alicebot2(Ctx,Atoms,Resp).

alicebot2(Ctx,Atoms,Resp):-	
   retractall(posibleResponse(_,_)),
   flag(a_answers,_,0),!,
   ignore((

   call_with_depth_limit(computeAnswer(Ctx,1,srai(Atoms),O,N),8000,_DL),
	 ignore((nonvar(N),nonvar(O),savePosibleResponse(N,O))),flag(a_answers,X,X+1),X>3)),!,
   findall(NR-OR,posibleResponse(NR,OR),L),!,
   (format('~n-> ~w~n',[L])),
   keysort(L,S),
   dumpList(S),
   reverse(S,[Resp|_RR]),
   degrade(Resp),!,
   rememberSaidIt(Resp),!.


% ===============================================================================================
% Save Possible Responses (Degrade them as well)
% ===============================================================================================
:-dynamic(posibleResponse/2).

savePosibleResponse(_N,O):-posibleResponse(_,O),!.
savePosibleResponse(N,O):-
   findall(1,degraded(O),L),!,
   length(L,K),
   SN is N - (K * 0.6)  , !,
   asserta(posibleResponse(SN,O)).


% ===============================================================================================
% Expand Answers
% ===============================================================================================
expandVariables(_Ctx,Votes,[],[],Votes):-!.
expandVariables(Ctx,Votes,[A|B],[AA|BB],Votes):-expandVar(Ctx,A,AA),!,expandVariables(Ctx,Votes,B,BB,Votes).
expandVariables(Ctx,Votes,[A|B],[A|BB],Votes):-atomic(A),!,expandVariables(Ctx,Votes,B,BB,Votes).
expandVariables(Ctx,VotesM,O,OO,VotesM):-expandVar(Ctx,O,OO),!.
expandVariables(_Ctx,VotesM,O,O,VotesM):-!.


expandVar(_Ctx,nick,A):-!,default_user(B),!,from_atom_codes(A,B),!.
expandVar(_Ctx,person,A):-!,default_user(B),!,from_atom_codes(A,B),!.
expandVar(_Ctx,botnick,'jllykifsh'):-!.
expandVar(_Ctx,mynick,'jllykifsh'):-!.
expandVar(_Ctx,name=name,'jllykifsh'):-!.
expandVar(_Ctx,mychan,A):-!,default_channel(B),!,from_atom_codes(A,B),!.
expandVar(_Ctx,Resp,Resp):-atomic(Resp),!.
expandVar(Ctx,In,Out):-computeAnswer(Ctx,1,In,Out,_VotesO).


from_atom_codes(Atom,Atom):-atom(Atom),!.
from_atom_codes(Atom,Codes):-convert_to_string(Codes,Atom),!.
from_atom_codes(Atom,Codes):-atom_codes(Atom,Codes).


:-dynamic(recursiveTag/1).


%%notRecursiveTag(system).

recursiveTag(random).
recursiveTag(srai).
recursiveTag(_).
isAimlTag(result):-!,fail.
isAimlTag(get).
isAimlTag(_).

computeInner(Ctx,Votes, In, Out) :- computeAnswer(Ctx,Votes, In, Out, _VoteMid),!.
computeInner(Ctx,Votes, In, VoteMid-Out) :- computeAnswer(Ctx,Votes, In, Out, VoteMid),!.
computeInner(Ctx,Votes, In, Out) :- expandVariables(Ctx,Votes, In, Out, _VoteMid),!.


unused_computeElement(Ctx,Votes, TAG, ATTRIBS, [DO|IT], OUT, NEWVOTE) :- recursiveTag(TAG),!,
        withAttributes(_Ctx,filelevel,ATTRIBS,((findall(Out,((member(In,[DO|IT]),expandVariables(Ctx,Votes, In, Out, _VoteMid))),INNERDONE),
         NOUT=..[TAG,ATTRIBS,INNERDONE],!,
         computeAnswer(Ctx,Votes,NOUT,OUT,NEWVOTE)))).


computeElement(Ctx,Votes,TAG,ATTRIBS,INNER,Resp,VotesO):- !,computeElement(Ctx,Votes,TAG,ATTRIBS,INNER,Resp,VotesO).
computeElement(Ctx,Votes,TAG,[ATTRIBS],INNER,Resp,VotesO):- !,computeElement(Ctx,Votes,TAG,ATTRIBS,INNER,Resp,VotesO).
computeElement(Ctx,Votes,TAG,name=Name,INNER,Resp,VotesO):- !,computeElement(Ctx,Votes,TAG,[],[name=Name|INNER],Resp,VotesO).
computeElement(Ctx,Votes,TAG,ATTRIBS,[name=Name],Resp,VotesO):- !,computeElement(Ctx,Votes,TAG,ATTRIBS,Name,Resp,VotesO).
computeElement(Ctx,Votes,TAG,ATTRIBS,name=Name,Resp,VotesO):- !,computeElement(Ctx,Votes,TAG,ATTRIBS,Name,Resp,VotesO).

computeElement(Ctx,Votes,GET,ATTRIBS,INNER,Resp,VotesO):- computeAnswer(Ctx,Votes,result(GET,ATTRIBS,INNER),Resp,VotesO).
%%computeElement(Ctx,Votes,element,result,ATTRIBS,INNER,Resp,VotesO):-!.


% ===============================================================================================
% Compute Answer Probilities
% ===============================================================================================

% element inner reductions
computeAnswer(Ctx,Votes, element(Tag, ATTRIBS, [DO|IT]), OUT, NEWVOTE) :- recursiveTag(Tag),not(DO=(_-_)),!,         
        withAttributes(_Ctx,template,ATTRIBS,((findall(OutVoteMid,((member(In,[DO|IT]),computeInner(Ctx,Votes, In, OutVoteMid))),INNERDONE),
         NOUT=..[Tag,ATTRIBS,INNERDONE],!,
         computeAnswer(Ctx,Votes,NOUT,OUT,NEWVOTE)))).

computeAnswer(Ctx,Votes,GETATTRIBS, Resp,VotesO):- convert_element(Ctx,GETATTRIBS,GETATTRIBS0),
      GETATTRIBS \== GETATTRIBS0,!, computeAnswer(Ctx,Votes,GETATTRIBS0, Resp,VotesO).

computeAnswer(Ctx,Votes,element(GET, ATTRIBS, INNER),Resp,VotesO):- !, computeElement(Ctx,Votes,GET,ATTRIBS,INNER,Resp,VotesO).

computeAnswer(_,Votes,IN,_,_):-debugFmt(computeAnswer(_,Votes,IN,_,_)),fail.

% <srai>s   
computeAnswer(Ctx,Votes,srai(Input),O,VotesO):- !,flatten([Input],Flat),
   debugOnFailureAiml(computeSRAI(Ctx,Votes,Flat,Mid,VotesM)),
   debugOnFailureAiml(expandVariables(Ctx,VotesM,Mid,O,VotesO)).


computeAnswer(Ctx,Votes,get(ATTRIBS),Resp,VotesO):- !,computeAnswer(Ctx,Votes,get(user,ATTRIBS),Resp,VotesO).
computeAnswer(Ctx,Votes,get(WHO,[X]),Resp,VotesO):- !,computeAnswer(Ctx,Votes,get(WHO,X),Resp,VotesO).
computeAnswer(Ctx,Votes,get([WHO],X),Resp,VotesO):- !,computeAnswer(Ctx,Votes,get(WHO,X),Resp,VotesO).
computeAnswer(Ctx,Votes,get(name=NAME,MORE),Resp,VotesO):- !, computeAnswer(Ctx,Votes,get(user,[name=NAME|MORE]),Resp,VotesO).
computeAnswer(Ctx,Votes,get(ATTRIBS),Resp,VotesO):- delete(ATTRIBS,type=bot,NEW),!,computeAnswer(Ctx,Votes,get(bot,NEW),Resp,VotesO).
computeAnswer(Ctx,Votes,get(TYPE),Resp,VotesO):- !,computeAnswer(Ctx,Votes,get(user,TYPE),Resp,VotesO).

computeAnswer(Ctx,Votes,MidVote - In,Out,VotesO):- computeAnswer(Ctx,Votes,In,Out,VotesA), VotesO is VotesA * MidVote.

computeAnswer(_Ctx,Votes,[],[],Votes):-!.
computeAnswer(Ctx,Votes,[A|B],[A|Resp],VotesO):-atomic(A),!,expandVariables(Ctx,Votes,B,Resp,VotesO).

computeAnswer(_Ctx,Votes,['.'],[],Votes):-!.
computeAnswer(_Ctx,Votes,_I,_,_):-(Votes>20;Votes<0.3),!,fail.

computeAnswer(_Ctx,_Votes,['*'],_,_):- !,fail.
/*
computeAnswer(Ctx,Votes,['*'],sTAR,3):- !.
computeAnswer(Ctx,Votes,['*'],_,_):- !,trace.
*/

% atomic 
computeAnswer(Ctx,Votes,randomsentence,O,VotesO):-!, choose_randomsentence(X),!,computeAnswer(Ctx,Votes,X,O,VotesO).
computeAnswer(Ctx,Votes,In,Out,Votes):-atomic(In),expandVar(Ctx,In,Out).

computeAnswer(_Ctx,Votes,Resp,Resp,Votes):-atomic(Resp),!.

computeAnswer(Ctx,Votes,li(List),AA,VotesO):-!,computeAnswer(Ctx,Votes,List,AA,VotesO).

computeAnswer(Ctx,Votes,random(List),AA,VotesO):-!,randomPick(List,Pick),computeAnswer(Ctx,Votes,Pick,AA,VotesO).
computeAnswer(Ctx,Votes,condition(List),AA,VotesO):-!,member(Pick,List),computeAnswer(Ctx,Votes,Pick,AA,VotesO).
computeAnswer(_Ctx,Votes,String,Atom,Votes):-string(String),!,string_to_atom(String,Atom).
computeAnswer(_Ctx,Votes,String,Atom,Votes):-is_string(String),toCodes(String,Codes),!,from_atom_codes(Atom,Codes).
computeAnswer(_Ctx,Votes,'$stringCodes'(List),AA,Votes):-!,from_atom_codes(AA,List).
computeAnswer(Ctx,Votes,gossip(Thought),O,VotesO):-!,computeAnswer(Ctx,Votes,Thought,O,VotesO).
computeAnswer(Ctx,Votes,think(Thought),[],VotesO):-!,computeAnswer(Ctx,Votes,Thought,_Hidden,VotesO).

computeAnswer(Ctx,Votes,get(user,X),Resp,VotesO):-getAliceMem(X,E),!,computeAnswer(Ctx,Votes,E,Resp,VotesM),VotesO is VotesM * 1.1.
computeAnswer(_Ctx,_Votes,get(_,_),_,_):-!,fail.


computeAnswer(Ctx,Votes,cycrandom(RAND),O,VotesO):-!, computeAnswer(Ctx,Votes,cyceval(RAND),RO,VotesO),randomPick(RO,O).
computeAnswer(_Ctx,Votes,cyceval(RAND),O,Votes):-!,  RAND=O. %%computeAnswer(Ctx,Votes,cyceval(RAND),RO,VotesO),randomPick(RO,O).

computeAnswer(Ctx,Votes,template([], DOIT), OUT, NEWVOTE) :- !,computeAnswer(Ctx,Votes,DOIT,OUT,NEWVOTE).


computeAnswer(Ctx,Votes,set(X,E),Resp,VotesO):-!,computeAnswer(Ctx,Votes,E,Resp,VotesM),setAliceMem(X,Resp),!,VotesO is VotesM * 1.1.

%computeAnswer(Ctx,Votes,B,Out,VotesO):-append(BB,['.','.'|BBB],B),append(BB,['.'|BBB],RB),!,computeAnswer(Ctx,Votes,RB,Out,VotesO).

computeAnswer(Ctx,Votes,B,Out,VotesO):-append(BB,['.'|BBB],B),append(BB,BBB,RB),!,computeAnswer(Ctx,Votes,RB,Out,VotesO).


computeAnswer(Ctx,Votes,[A|L],OO,VotesO):-!,
	 computeAnswer(Ctx,Votes,A,AA,VotesM),
	 computeAnswer(Ctx,VotesM,L,LL,VotesO),
	 once(flatten([AA,LL],OO)).

computeAnswer(_Ctx,Votes,Resp,Resp,Votes):-!.

computeAnswer(Ctx,Votes,GETATTRIBS, Resp,VotesO):- GETATTRIBS=..[GET], isAimlTag(GET), !, computeElement(Ctx,Votes,GET,[],[],Resp,VotesO).
computeAnswer(Ctx,Votes,GETATTRIBS, Resp,VotesO):- GETATTRIBS=..[GET,ATTRIBS], isAimlTag(GET), !, computeElement(Ctx,Votes,GET,ATTRIBS,[],Resp,VotesO).
computeAnswer(Ctx,Votes,GETATTRIBS, Resp,VotesO):- GETATTRIBS=..[GET,ATTRIBS,INNER], isAimlTag(GET), !, computeElement(Ctx,Votes,GET,ATTRIBS,INNER,Resp,VotesO).





% ===============================================================================================
% Apply Input Match
% ===============================================================================================

computeSRAI(_Ctx,_Votes,[],_,_):- !, fail.
computeSRAI(Ctx,Votes,Input,TopicStarO,VotesO):-
  findall(TopicStarM=VotesM,notrace(((computeSRAI2(Ctx,Votes,Input,TopicStarM,VotesM)))),FOUND),
  FOUND=[_|_], !, member(TopicStarO=VotesO,FOUND), 
  TopicStarO \= [*].

computeSRAI(Ctx,Votes,Input,_,_):- debugFmt(computeSRAI2(Ctx,Votes,Input)),fail.
%computeSRAI(Ctx,Votes,Input,TopicStarO,VotesO):- computeSRAI2(Ctx,Votes,Input,TopicStarO,VotesO),TopicStarO \= [*].

% this next line is what it does on fallback
%computeSRAI(Ctx,Votes,[B|Flat],[B|TopicStarO],VotesO):-computeSRAI(Ctx,Votes,Flat,TopicStarO,VotesO).

computeSRAI2(Ctx,Votes,Input,TopicStarO,VotesO):-
	 getLastSaid(ThatSaid),
	 set_matchit(Input,MatchIt),
         trace,
         get_aiml_that(Ctx,SaidThat,MatchIt,Out),
	 rateMatch(SaidThat,ThatSaid,SaidThat,NewTopic,TopicVote,TopicStar), 
	 rateMatch(MatchIt,Input,Out,Next,Voted,_), 
	 flatten([Next],NextO),
	 subst(NextO,topicstar,TopicStar,TopicStarO),
	 VotesO is Votes * (Voted + TopicVote).

set_matchit([Input|_],[Input|_]).
set_matchit([Input|_],[_,Input|_]).
set_matchit([_,Input|_],[_,Input|_]).

%get_aiml_that(Ctx,SaidThat,Match,OOut):-get_aiml_cyc(SaidThat,Match,Out),(([srai(Out)] = OOut);OOut=Out).
get_aiml_that(_Ctx,SaidThat,Match,Out):-what(SaidThat, Match,Out).
get_aiml_that(_Ctx,[*],Match,Out):-response(Match,Out).

%%%%aimlCate(graph,topic,that,pattern,flags,call,guard,template,userdict).
get_aiml_that(_Ctx,[T|HAT],Match,Out):-aimlCate(_Graph,_Precall,_Topic,[T|HAT],Match,_Flags,_Call,_Guard,Out,_userdict,_,_).
get_aiml_that(_Ctx,[*],Match,Out):-aimlCate(_Graph,_Precall,_Topic,that,Match,_Flags,_Call,_Guard,Out,_userdict,_,_).

%get_aiml_cyc([*],[String|ListO],[Obj,*]):-poStr(Obj,[String|List]),append(List,[*],ListO).
%get_aiml_cyc([*],[String,*],[Obj,*]):-poStr(Obj,String).


% ===============================================================================================
% Rate Match -  rateMatch(Markup,Original,InputBefore,InputAfter,Cost,Consumed)
% ===============================================================================================
rateMatch([],[],Out,Out,1,[]):-!.

rateMatch(Match,Match,Out,Out,1.3,[]):-!.

rateMatch([This|More],[This|More2],Out,OOut,Vote2,Grabbed):-!,
      rateMatch(More,More2,Out,OOut,Vote,Grabbed),!,
      Vote2 is Vote * (1.11).

rateMatch([*],More,Out,OOut,0.77,More):-!,subst(Out,*,More,OOut),!.
rateMatch(['_'],More,Out,OOut,0.87,More):-!,subst(Out,*,More,OOut),!.

rateMatch([*|Rest],More,Out,OOut,VoteO,Grabbed):-!,
      append(Grabbed,Rest,More),
      rateMatch(More,_More2,Out,OOut,Vote,_),
      subst(Out,*,Grabbed,OOut),
      VoteO is Vote * (0.72),!.

rateMatch(['_'|Rest],More,Out,OOut,VoteO,Grabbed):-!,
      append(Grabbed,Rest,More),
      rateMatch(More,_More2,Out,OOut,Vote,_),
      subst(Out,*,Grabbed,OOut),
      VoteO is Vote * (0.82),!.
            


% ===============================================================================================
% Run answer procs
% ===============================================================================================

choose_randomsentence(X):-
	repeat,
		retract(random_sent(Y)),
		assertz(random_sent(Y)),
		4 is random(10),!,
		Y=X.

getAliceMem(X,E):-isAliceMem(X,E),!.
 
setAliceMem(X,[E]):-!,setAliceMem(X,E),!.
setAliceMem(_X,'nick').
setAliceMem(X,E):-retractall(isAliceMem(X,_)),asserta(isAliceMem(X,E)),writeln(debug(isAliceMem(X,E))),!.
:-dynamic(isAliceMem/2).

% ===============================================================================================
% Get and rember Last Said
% ===============================================================================================

:-dynamic(getLastSaid/1).
getLastSaid(['where',am,'I']).

rememberSaidIt([]):-!.
rememberSaidIt(_-R1):-!,rememberSaidIt(R1).
rememberSaidIt(R1):-append(New,'.',R1),!,rememberSaidIt(New).
rememberSaidIt(R1):-ignore(retract(getLastSaid(_))),toUppercase(R1,SR1),!,asserta(getLastSaid(SR1)).

% ===============================================================================================
% Degrade Response
% ===============================================================================================

:-dynamic(degraded/1).

degrade(_-OR):-!,degrade(OR).
degrade(OR):-asserta(degraded(OR)).
   
:- exists_file('logicmoo_module_aiml_eval.pl')-> cd('..') ; true.



aimlDebugFmt(X):-debugFmt(X),!.

traceCall(A):-trace(A,[-all,+fail]),A,!.

/*
debugOnFailureAiml(Call):- clause(Call,(_A,_B)),!,clause(Call,Body),trace,debugOnFailureAiml(Body),!.
*/
debugOnFailureAiml((A,B)):- !,debugOnFailureAiml(A),!,debugOnFailureAiml(B),!.
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

:-traceAll.

:-['cynd/logicmoo_module_aiml_loader.pl'].
:-['cynd/logicmoo_module_aiml_eval.pl'].


%:-['bootstrap.aiml.pl'].

%:-load_aiml_files.

%:-debug,run_chat_tests.

%:-main_loop.

% :- tell(listing1),listing,told.

%:- guitracer.

%:- do.



