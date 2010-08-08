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
:- style_check(-singleton).
:- style_check(-discontiguous).
:- style_check(-atom).
:- style_check(-string).

alldiscontiguous:-!.

:-multifile(what/3).
:-multifile(response/2).

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

main_loop:-repeat,main_loop1(Atom),catch(atom_to_term(Atom,Term,Vars),_,fail),
      once(callInteractive(Term,Vars)),fail.

% callInteractive(Term,V):-
callInteractive(Term,Var):-callInteractive0(Term,Var).

callInteractive0(Term,Var):-atom(Term),!,catch(((Term,writeln(called(Term)))),E,aiml_error(E)),!.
callInteractive0(Term,_):-catch((call(Term),writeq(Term),nl,fail),E,aiml_error(E)).
callInteractive0(Term,_):-!.

% ===============================================================================================
% ALICE IN PROLOG
% ===============================================================================================

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

alicebot:-repeat,
	read_line_with_nl(user,Codes,[]),
	once((atom_codes(Atom,Codes),alicebot(Atom))),fail.

say(X):-format(user_output,'~q~n',X),flush_output(user_output),fail.
say(X):-aiml_eval(X,Calls),callEachList(Calls).

callEachList(Calls):-member(C,Calls),callInteractive(once(call(C)),C),fail.
callEachList(_Calls).

alicebot(Input):- atom(Input),atom_concat('call ',Rest,Input),catch((atom_to_term(Rest,Term,Vars),ignore(callInteractive(Term,Vars))),_,true),!.
alicebot(Input):-
   alicebot(Input,Resp),
   say(Resp),!.
alicebot(Input):-say('-no response-').


atomSplit(A,B):-cyc:atomSplit(A,B).

removePMark(UCase,Atoms):-append(AtomsPre,[Last],UCase),member(Last,[?,('.'),(',')]),!,removePMark(AtomsPre,Atoms).
removePMark(Atoms,Atoms).

randomPick(List,Ele):-length(List,Len),Pick is random(Len),nth0(Pick,List,Ele),!.

all_upper_atom(X):-toUppercase(X,N),N=X.

:-dynamic(default_channel/1).
:-dynamic(default_user/1).

%default_channel( "#logicmoo").
%default_user(    "default_user").         

% say(Say):-writeq(Say),nl.
toCodes(B,A):-cyc:stringToCodelist(B,AO),(is_list(A) -> A=AO ; string_to_list(AO,A)),!.


dumpList([]):-!.
dumpList([A|B]):-say(A),dumpList(B),!.

dumpList(B):-say(  dumpList(B)).

alicebot(Input,Resp):-
   getWordTokens(Input,Tokens),
   toUppercase(Tokens,UCase),
   removePMark(UCase,Atoms),
   alicebot2(Atoms,Resp).

alicebot2(Atoms,Resp):-	
   retractall(posibleResponse(_,_)),
   flag(a_answers,_,0),!,
   ignore((
   call_with_depth_limit(computeAnswer(1,srai(Atoms),O,N),8000,DL),
	 ignore((nonvar(N),nonvar(O),savePosibleResponse(N,O))),flag(a_answers,X,X+1),X>3)),!,
   findall(NR-OR,posibleResponse(NR,OR),L),!,
   %format('~n-> ~w~n',[L]),
   keysort(L,S),
   dumpList(S),
   reverse(S,[Resp|RR]),
   degrade(Resp),
   rememberSaidIt(Resp).


% ===============================================================================================
% Save Possible Responses (Degrade them as well)
% ===============================================================================================
:-dynamic(posibleResponse/2).

savePosibleResponse(N,O):-posibleResponse(_,O),!.
savePosibleResponse(N,O):-
   findall(1,degraded(O),L),!,
   length(L,K),
   SN is N - (K * 0.6)  , !,
   asserta(posibleResponse(SN,O)).

% ===============================================================================================
% Compute Answer Probilities
% ===============================================================================================
from_atom_codes(Atom,Atom):-atom(Atom),!.
from_atom_codes(Atom,Codes):-convert_to_string(Codes,Atom),!.
from_atom_codes(Atom,Codes):-atom_codes(Atom,Codes).

computeAnswer(Votes,[],[],Votes):-!.
computeAnswer(Votes,['.'],[],Votes):-!.
computeAnswer(Votes,I,_,_):-(Votes>20;Votes<0.3),!,fail.
computeAnswer(Votes,nick,A,Votes):-!,default_user(B),!,from_atom_codes(A,B),!.
computeAnswer(Votes,person,A,Votes):-!,default_user(B),!,from_atom_codes(A,B),!.
computeAnswer(Votes,botnick,'jllykifsh',Votes):-!.
computeAnswer(Votes,mynick,'jllykifsh',Votes):-!.
computeAnswer(Votes,name=name,'jllykifsh',Votes):-!.
computeAnswer(Votes,mychan,A,Votes):-!,default_channel(B),!,from_atom_codes(A,B),!.
computeAnswer(Votes,randomsentence,O,VotesO):-!, choose_randomsentence(X),!,computeAnswer(Votes,X,O,VotesO).
computeAnswer(Votes,Resp,Resp,Votes):-atomic(Resp),!.
computeAnswer(Votes,srai(Input),O,VotesO):- !,flatten([Input],Flat),computeSRAI(Votes,Flat,Mid,VotesM),computeAnswer(VotesM,Mid,O,VotesO).
computeAnswer(Votes,li(List),AA,VotesO):-!,computeAnswer(Votes,List,AA,VotesO).
computeAnswer(Votes,random(List),AA,VotesO):-!,randomPick(List,Pick),computeAnswer(Votes,Pick,AA,VotesO).
computeAnswer(Votes,condition(List),AA,VotesO):-!,member(Pick,List),computeAnswer(Votes,Pick,AA,VotesO).
computeAnswer(Votes,String,Atom,Votes):-string(String),!,string_to_atom(String,Atom).
computeAnswer(Votes,String,Atom,Votes):-is_string(String),toCodes(String,Codes),!,from_atom_codes(Atom,Codes).
computeAnswer(Votes,'$stringCodes'(List),AA,Votes):-!,from_atom_codes(AA,List).
computeAnswer(Votes,gossip(Thought),O,VotesO):-!,computeAnswer(Votes,Thought,O,VotesO).
computeAnswer(Votes,think(Thought),[],VotesO):-!,computeAnswer(Votes,Thought,O,VotesO).
computeAnswer(Votes,get([X]),Resp,VotesO):-getAliceMem(X,E),!,computeAnswer(Votes,E,Resp,VotesM),VotesO is VotesM * 1.1.
computeAnswer(Votes,get(X),Resp,VotesO):-getAliceMem(X,E),!,computeAnswer(Votes,E,Resp,VotesM),VotesO is VotesM * 1.1.
computeAnswer(Votes,get(_),_,_):-!,fail.
computeAnswer(Votes,_-A,A,Votes):-!.
computeAnswer(Votes,set(X,E),Resp,VotesO):-!,computeAnswer(Votes,E,Resp,VotesM),setAliceMem(X,Resp),!,VotesO is VotesM * 1.1.
%computeAnswer(Votes,B,Out,VotesO):-append(BB,['.','.'|BBB],B),append(BB,['.'|BBB],RB),!,computeAnswer(Votes,RB,Out,VotesO).
computeAnswer(Votes,B,Out,VotesO):-append(BB,['.'|BBB],B),append(BB,BBB,RB),!,computeAnswer(Votes,RB,Out,VotesO).
computeAnswer(Votes,[A|L],OO,VotesO):-!,
	 computeAnswer(Votes,A,AA,VotesM),
	 computeAnswer(VotesM,L,LL,VotesO),
	 once(flatten([AA,LL],OO)).
computeAnswer(Votes,Resp,Resp,Votes).





% ===============================================================================================
% Apply Input Match
% ===============================================================================================

computeSRAI(Votes,[],_,_):-!,fail.
computeSRAI(Votes,Input,TopicStarO,VotesO):-
	 getLastSaid(WhatSaid),
	 set_matchit(Input,MatchIt),get_aiml_what(What,MatchIt,Out),
	 rateMatch(What,WhatSaid,What,NewTopic,TopicVote,TopicStar), 
	 rateMatch(MatchIt,Input,Out,Next,Voted,_), 
	 flatten([Next],NextO),
	 subst(NextO,topicstar,TopicStar,TopicStarO),
	 VotesO is Votes * (Voted + TopicVote).

% this next line is what it does on fallback
%computeSRAI(Votes,[B|Flat],[B|TopicStarO],VotesO):-computeSRAI(Votes,Flat,TopicStarO,VotesO).

set_matchit([Input|_],[Input|_]).
set_matchit([Input|_],[_,Input|_]).
set_matchit([_,Input|_],[_,Input|_]).

%get_aiml_what(What,Match,OOut):-get_aiml_cyc(What,Match,Out),(([srai(Out)] = OOut);OOut=Out).
get_aiml_what(What,Match,Out):-what(What, Match,Out).
get_aiml_what([*],Match,Out):-response(Match,Out).
get_aiml_what([A|L],Match,Out):-aimlCate(AIML),member(that=[A|L],AIML),member(pattern=Match,AIML),member(template=Out,AIML).
get_aiml_what([*],Match,Out):-aimlCate(AIML),member(that=(*),AIML),member(pattern=Match,AIML),member(template=Out,AIML).

%get_aiml_cyc([*],[String|ListO],[Obj,*]):-poStr(Obj,[String|List]),append(List,[*],ListO).
%get_aiml_cyc([*],[String,*],[Obj,*]):-poStr(Obj,String).


% ===============================================================================================
% Rate Match
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
      rateMatch(More,More2,Out,OOut,Vote,_),
      subst(Out,*,Grabbed,OOut),
      VoteO is Vote * (0.72),!.

rateMatch(['_'|Rest],More,Out,OOut,VoteO,Grabbed):-!,
      append(Grabbed,Rest,More),
      rateMatch(More,More2,Out,OOut,Vote,_),
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
setAliceMem(X,'nick').
setAliceMem(X,E):-retractall(isAliceMem(X,_)),asserta(isAliceMem(X,E)),writeln(debug(isAliceMem(X,E))),!.
:-dynamic(isAliceMem/2).

% ===============================================================================================
% Get and rember Last Said
% ===============================================================================================

:-dynamic(getLastSaid/1).
getLastSaid([*]).

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
   

:-['logicmoo_module_aiml_loader.pl'].
:-['logicmoo_module_aiml_eval.pl'].

%:-['bootstrap.aiml.pl'].

%:-load_aiml_files.

%:-debug,run_chat_tests.

%:-main_loop.


