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
:-dynamic(aimlCate/1).

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

main_loop:-repeat,main_loop1(Atom),fail.
:-abolish(aimlCate/9).
:-dynamic(aimlCate/9).

callInteractive(Term,Var):-catch(callInteractive0(Term,Var),E,aiml_error(E)),!.

%callInteractive0(Term,_):-atom(Term),!,Term,!,writeln(called(Term)),!.
callInteractive0(Term,Var):- call(Term),writeq(Term:Var),nl,fail.
callInteractive0(Term,_):-!.

currentContext(X):-makeAimlContext(X),!.

makeAimlContext(
   aimlcontext(
      stars([_pattern|_],[_topic|_],[_that|_],[_|_guard]),
      settings([_|_graph],[_|_topic2],[_username|_],[_input|_]))).

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

atomSplit(A,B):-cyc:atomSplit(A,B).

removePMark(UCase,Atoms):-append(AtomsPre,[Last],UCase),member(Last,[?,('.'),(',')]),!,removePMark(AtomsPre,Atoms).
removePMark(Atoms,Atoms).

randomPick(List,Ele):-length(List,Len),Pick is random(Len),nth0(Pick,List,Ele),!.

all_upper_atom(X):-toUppercase(X,N),N=X.

:-dynamic(default_channel/1).
:-dynamic(default_user/1).

%default_channel( "#logicmoo").
%default_user(    "default_user").         

% say(Ctx,Say):-writeq(Say),nl.
toCodes(B,A):-cyc:stringToCodelist(B,AO),(is_list(A) -> A=AO ; string_to_list(AO,A)),!.


dumpList([]):-!.
dumpList([A|B]):-say(Ctx,A),dumpList(B),!.

dumpList(B):-say(Ctx,  dumpList(B)).


say(Ctx,X):- aiml_eval(Ctx,X,Y),!,debugFmt(Y),!.

alicebot:-repeat,
	read_line_with_nl(user,Codes,[]),
        makeAimlContext(Ctx),
        once((atom_codes(Atom,Codes),alicebotCTX(Ctx,Atom))),fail.

alicebot(Input):- currentContext(Ctx),!,alicebotCTX(Ctx,Input).

alicebotCTX(Ctx,Input):- atom(Input),catch(((atom_to_term(Input,Term,Vars),callInteractive0(Term,Vars))),_,fail),!.
alicebotCTX(Ctx,Input):- once((alicebotCTX(Ctx,Input,Resp),say(Ctx,Resp))),!.
alicebotCTX(Ctx,_):- say(Ctx,'-no response-').

alicebotCTX(Ctx,Input,Resp):- atom(Input),!,
      getWordTokens(Input,TokensO),!,Tokens=TokensO,
      alicebotCTX(Ctx,Tokens,Resp),!.

alicebotCTX(Ctx,[TOK|Tokens],Resp):- atom(TOK),atom_concat('@',_,TOK),!,systemCall(Ctx,'bot',[TOK|Tokens],Output),debugFmt(Output).

alicebotCTX(Ctx,Tokens,Resp):-
   toUppercase(Tokens,UCase),!,
   removePMark(UCase,Atoms),!,
   alicebot2(Ctx,Atoms,Resp),!.

alicebot2(Atoms,Resp):- currentContext(Ctx),!,alicebot2(Ctx,Atoms,Resp).

alicebot2(Ctx,Atoms,Resp):-	
   retractall(posibleResponse(_,_)),
   flag(a_answers,_,0),!,
   ignore((

   call_with_depth_limit(computeAnswer(Ctx,1,srai(Atoms),O,N),8000,DL),
	 ignore((nonvar(N),nonvar(O),savePosibleResponse(N,O))),flag(a_answers,X,X+1),X>3)),!,
   findall(NR-OR,posibleResponse(NR,OR),L),!,
   (format('~n-> ~w~n',[L])),
   keysort(L,S),
   dumpList(S),
   reverse(S,[Resp|RR]),
   degrade(Resp),!,
   rememberSaidIt(Resp),!.


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
% Expand Answers
% ===============================================================================================
expandVariables(Ctx,Votes,[],[],Votes):-!.
expandVariables(Ctx,Votes,[A|B],[AA|BB],Votes):-expandVar(Ctx,A,AA),!,expandVariables(Ctx,Votes,B,BB,Votes).
expandVariables(Ctx,Votes,[A|B],[A|BB],Votes):-atomic(A),!,expandVariables(Ctx,Votes,B,BB,Votes).
expandVariables(Ctx,VotesM,O,OO,VotesM):-expandVar(Ctx,O,OO),!.
expandVariables(Ctx,VotesM,O,O,VotesM):-!.


expandVar(Ctx,nick,A):-!,default_user(B),!,from_atom_codes(A,B),!.
expandVar(Ctx,person,A):-!,default_user(B),!,from_atom_codes(A,B),!.
expandVar(Ctx,botnick,'jllykifsh'):-!.
expandVar(Ctx,mynick,'jllykifsh'):-!.
expandVar(Ctx,name=name,'jllykifsh'):-!.
expandVar(Ctx,mychan,A):-!,default_channel(B),!,from_atom_codes(A,B),!.
expandVar(Ctx,Resp,Resp):-atomic(Resp),!.
expandVar(_Ctx,A,A):-atomic(A),!.


from_atom_codes(Atom,Atom):-atom(Atom),!.
from_atom_codes(Atom,Codes):-convert_to_string(Codes,Atom),!.
from_atom_codes(Atom,Codes):-atom_codes(Atom,Codes).


:-dynamic(recursiveTag/1).

recursiveTag(random).
% ===============================================================================================
% Compute Answer Probilities
% ===============================================================================================

computeAnswer(Ctx,Votes,IN,_,_):-debugFmt(computeAnswer(_,Votes,IN,_,_)),fail.

computeAnswer(Ctx,Votes,MidVote - In,Out,VotesO):- computeAnswer(Ctx,Votes,In,Out,VotesA), VotesO is VotesA * MidVote.

computeAnswer(Ctx,Votes,[],[],Votes):-!.
computeAnswer(Ctx,Votes,[A|B],[A|Resp],VotesO):-atomic(A),!,expandVariables(Ctx,Votes,B,Resp,VotesO).

computeAnswer(Ctx,Votes,['.'],[],Votes):-!.
computeAnswer(Ctx,Votes,_I,_,_):-(Votes>20;Votes<0.3),!,fail.

computeAnswer(Ctx,Votes,['*'],_,_):- !,fail.
/*
computeAnswer(Ctx,Votes,['*'],sTAR,3):- !.
computeAnswer(Ctx,Votes,['*'],_,_):- !,trace.
*/

% atomic 
computeAnswer(Ctx,Votes,randomsentence,O,VotesO):-!, choose_randomsentence(X),!,computeAnswer(Ctx,Votes,X,O,VotesO).
computeAnswer(Ctx,Votes,In,Out,Votes):-expandVar(Ctx,In,Out).
computeAnswer(Ctx,Votes,Resp,Resp,Votes):-atomic(Resp),!.

% <srai>s
computeAnswer(Ctx,Votes,srai(Input),O,VotesO):- !,flatten([Input],Flat),computeSRAI(Ctx,Votes,Flat,Mid,VotesM),debugOnFailureAiml(expandVariables(Ctx,VotesM,Mid,O,VotesO)).

computeAnswer(Ctx,Votes,li(List),AA,VotesO):-!,computeAnswer(Ctx,Votes,List,AA,VotesO).

computeAnswer(Ctx,Votes,random(List),AA,VotesO):-!,randomPick(List,Pick),computeAnswer(Ctx,Votes,Pick,AA,VotesO).
computeAnswer(Ctx,Votes,condition(List),AA,VotesO):-!,member(Pick,List),computeAnswer(Ctx,Votes,Pick,AA,VotesO).
computeAnswer(Ctx,Votes,String,Atom,Votes):-string(String),!,string_to_atom(String,Atom).
computeAnswer(Ctx,Votes,String,Atom,Votes):-is_string(String),toCodes(String,Codes),!,from_atom_codes(Atom,Codes).
computeAnswer(Ctx,Votes,'$stringCodes'(List),AA,Votes):-!,from_atom_codes(AA,List).
computeAnswer(Ctx,Votes,gossip(Thought),O,VotesO):-!,computeAnswer(Ctx,Votes,Thought,O,VotesO).
computeAnswer(Ctx,Votes,think(Thought),[],VotesO):-!,computeAnswer(Ctx,Votes,Thought,O,VotesO).

computeAnswer(Ctx,Votes,get(WHO,[X]),Resp,VotesO):- computeAnswer(Ctx,Votes,get(WHO,X),Resp,VotesO).
computeAnswer(Ctx,Votes,get(user,X),Resp,VotesO):-getAliceMem(X,E),!,computeAnswer(Ctx,Votes,E,Resp,VotesM),VotesO is VotesM * 1.1.
computeAnswer(Ctx,Votes,get(_,_),_,_):-!,fail.

% element inner reductions

computeAnswer(Ctx,Votes, element(Tag, ATTRIBS, [DO|IT]), OUT, NEWVOTE) :- recursiveTag(Tag),not(DO=(_-_)),!,         
        withAttributes(filelevel,ATTRIBS,((findall(VoteMid-Out,((member(In,[DO|IT]),computeAnswer(Ctx,Votes, In, Out, VoteMid))),INNERDONE),
         NOUT=..[Tag,ATTRIBS,INNERDONE],!,
         computeAnswer(Ctx,Votes,NOUT,OUT,NEWVOTE)))).

% element outer reductions
computeAnswer(Ctx,Votes, element(Tag, [], []), OUT, NEWVOTE) :- NOUT=TAG,!,computeAnswer(Ctx,Votes,NOUT,OUT,NEWVOTE).
computeAnswer(Ctx,Votes, element(Tag, [], DOIT), OUT, NEWVOTE) :- NOUT=..[Tag,DOIT],!,computeAnswer(Ctx,Votes,NOUT,OUT,NEWVOTE).
computeAnswer(Ctx,Votes, element(Tag, DOIT, []), OUT, NEWVOTE) :- NOUT=..[Tag,DOIT],!,computeAnswer(Ctx,Votes,NOUT,OUT,NEWVOTE).
computeAnswer(Ctx,Votes, element(Tag, ATTRIBS, DOIT), OUT, NEWVOTE) :- NOUT=..[Tag,ATTRIBS,DOIT],!,computeAnswer(Ctx,Votes,NOUT,OUT,NEWVOTE).



computeAnswer(Ctx,Votes,cycrandom(RAND),O,VotesO):-!, computeAnswer(Ctx,Votes,cyceval(RAND),RO,VotesO),randomPick(RO,O).
computeAnswer(Ctx,Votes,cyceval(RAND),O,Votes):-!,  RAND=O. %%computeAnswer(Ctx,Votes,cyceval(RAND),RO,VotesO),randomPick(RO,O).




/*

computeAnswer(Ctx,Votes,template([], DOIT), OUT, NEWVOTE) :- !,computeAnswer(Ctx,Votes,DOIT,OUT,NEWVOTE).
*/

computeAnswer(Ctx,Votes,set(X,E),Resp,VotesO):-!,computeAnswer(Ctx,Votes,E,Resp,VotesM),setAliceMem(X,Resp),!,VotesO is VotesM * 1.1.

%computeAnswer(Ctx,Votes,B,Out,VotesO):-append(BB,['.','.'|BBB],B),append(BB,['.'|BBB],RB),!,computeAnswer(Ctx,Votes,RB,Out,VotesO).

computeAnswer(Ctx,Votes,B,Out,VotesO):-append(BB,['.'|BBB],B),append(BB,BBB,RB),!,computeAnswer(Ctx,Votes,RB,Out,VotesO).

computeAnswer(Ctx,Votes,[A|L],OO,VotesO):-!,
	 computeAnswer(Ctx,Votes,A,AA,VotesM),
	 computeAnswer(Ctx,VotesM,L,LL,VotesO),
	 once(flatten([AA,LL],OO)).

computeAnswer(Ctx,Votes,Resp,Resp,Votes).





% ===============================================================================================
% Apply Input Match
% ===============================================================================================

computeSRAI(Ctx,Votes,[],_,_):-!,fail.
computeSRAI(Ctx,Votes,Input,TopicStarO,VotesO):- notrace(((computeSRAI2(Ctx,Votes,Input,TopicStarO,VotesO),TopicStarO \= [*]))).

computeSRAI2(Ctx,Votes,Input,TopicStarO,VotesO):-
	 getLastSaid(WhatSaid),
	 set_matchit(Input,MatchIt),get_aiml_what(Ctx,What,MatchIt,Out),
	 rateMatch(What,WhatSaid,What,NewTopic,TopicVote,TopicStar), 
	 rateMatch(MatchIt,Input,Out,Next,Voted,_), 
	 flatten([Next],NextO),
	 subst(NextO,topicstar,TopicStar,TopicStarO),
	 VotesO is Votes * (Voted + TopicVote).

% this next line is what it does on fallback
%computeSRAI(Ctx,Votes,[B|Flat],[B|TopicStarO],VotesO):-computeSRAI(Ctx,Votes,Flat,TopicStarO,VotesO).

set_matchit([Input|_],[Input|_]).
set_matchit([Input|_],[_,Input|_]).
set_matchit([_,Input|_],[_,Input|_]).

%get_aiml_what(Ctx,What,Match,OOut):-get_aiml_cyc(What,Match,Out),(([srai(Out)] = OOut);OOut=Out).
get_aiml_what(Ctx,What,Match,Out):-what(What, Match,Out).
get_aiml_what(Ctx,[*],Match,Out):-response(Match,Out).

%%%%aimlCate(graph,topic,that,pattern,flags,call,guard,template,userdict).
get_aiml_what(Ctx,[T|HAT],Match,Out):-aimlCate(_graph,_topic,[T|HAT],Match,_flags,_call,_guard,Out,_userdict).
get_aiml_what(Ctx,[*],Match,Out):-aimlCate(_graph,_topic,(*),Match,_flags,_call,_guard,Out,_userdict).

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

:-['cynd/logicmoo_module_aiml_loader.pl'].
:-['cynd/logicmoo_module_aiml_eval.pl'].


%:-['bootstrap.aiml.pl'].

%:-load_aiml_files.

%:-debug,run_chat_tests.

%:-main_loop.

% :- tell(listing1),listing,told.

:- guitracer.

:- do.

