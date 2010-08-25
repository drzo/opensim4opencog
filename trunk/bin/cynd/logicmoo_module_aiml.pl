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
%%:- style_check(-discontiguous).
:- style_check(-atom).
:- style_check(-string).

:-multifile(what/3).
:-multifile(response/2).
:-dynamic(lineInfoElement/4).

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

% ===================================================================
%  aimlCate database decl
% ===================================================================

:-dynamic(aimlCateSigCached/1).
aimlCateSig(X):-aimlCateSigCached(X),!.
aimlCateSig(Pred):-aimlCateOrder(List),length(List,L),functor(Pred,aimlCate,L),asserta(aimlCateSigCached(Pred)),!.

aimlCateOrder([graph,precall,topic,that,request,pattern,flags,call,guard,userdict,template,srcinfo,srcfile]).

% [graph,precall,topic,that,pattern,flags,call,guard,template,userdict]
cateMemberTags(Result):- aimlCateOrder(List), findall(E,(member(E0,List),once((E0=[E|_];E0=E))), Result).


makeAimlCateSig(Ctx,ListOfValues,Pred):-aimlCateSig(Pred),!,makeAimlCate(Ctx,ListOfValues,Pred,current_value),!.

:- aimlCateOrder(List),length(List,L),dynamic(aimlCate/L),multifile(aimlCate/L). 

:-dynamic(default_channel/1).
:-dynamic(default_user/1).

%default_channel( "#logicmoo").
%default_user(    "default_user").         

% say(Say):-writeq(Say),nl.

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
alicebotCTX(Ctx,Input):- alicebotCTX(Ctx,Input,Resp),!,say(Ctx,Resp),!.
%%alicebotCTX(Ctx,_):- trace, say(Ctx,'-no response-').


alicebotCTX(_Ctx,[],_):-debugFmt('no input'),!,fail.
alicebotCTX(Ctx,Input,Resp):- atom(Input),!,
      getWordTokens(Input,TokensO),!,Tokens=TokensO,
      alicebotCTX(Ctx,Tokens,Resp),!.
alicebotCTX(Ctx,[TOK|Tokens],Output):- atom(TOK),atom_concat('@',_,TOK),!,systemCall(Ctx,'bot',[TOK|Tokens],Output),debugFmt(Output).
alicebotCTX(Ctx,Tokens,Resp):-
   toUppercase(Tokens,UCase),!,
   removePMark(UCase,Atoms),!,
   alicebot2(Ctx,Atoms,Resp),!.
alicebotCTX(_Ctx,In,Res):- !,ignore(Res='-no response-'(In)).




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
        withAttributes(_Ctx,ATTRIBS,((findall(Out,((member(In,[DO|IT]),expandVariables(Ctx,Votes, In, Out, _VoteMid))),INNERDONE),
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
        withAttributes(_Ctx,ATTRIBS,((findall(OutVoteMid,((member(In,[DO|IT]),computeInner(Ctx,Votes, In, OutVoteMid))),INNERDONE),
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

computeAnswer(Ctx,Votes,star(pattern,[],[]),Resp,VotesO):-!,computeAnswer(Ctx,Votes,*,Resp,VotesM),VotesO is VotesM * 1.1.

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
  findall(TopicStarM=VotesM:ProofM,notrace(((computeSRAI2(Ctx,Votes,Input,TopicStarM,VotesM,ProofM)))),FOUND),
  FOUND=[_|_], !, member(TopicStarO=VotesO:Proof,FOUND),
   debugFmt(computeSRAI(Input,Proof)), TopicStarO \= [*].

computeSRAI(Ctx,Votes,Input,_,_):- debugFmt(computeSRAI2(Ctx,Votes,Input)),fail.
computeSRAI(Ctx,Votes,Input,TopicStarO,VotesO):- computeSRAI2(Ctx,Votes,Input,TopicStarO,VotesO,_Proof),TopicStarO \= [*].

% this next line is what it does on fallback
%computeSRAI(Ctx,Votes,[B|Flat],[B|TopicStarO],VotesO):-computeSRAI(Ctx,Votes,Flat,TopicStarO,VotesO).

computeSRAI2(Ctx,Votes,Input,TopicStarO,VotesO,Proof):-
	 getLastSaid(ThatSaid),
	 set_matchit(Input,MatchIt),
         trace,
         get_aiml_that(Ctx,SaidThat,MatchIt,Out,Proof),
	 rateMatch(SaidThat,ThatSaid,SaidThat,NewTopic,TopicVote,TopicStar), 
	 rateMatch(MatchIt,Input,Out,Next,Voted,_), 
	 flatten([Next],NextO),
	 subst(NextO,topicstar,TopicStar,TopicStarO),
	 VotesO is Votes * (Voted + TopicVote).

set_matchit(Input,Input).

set_matchit(A,B):-member(Mid,[[_,_,_,_],[_,_,_],[_,_],[_]]),sublistspan(Mid,A),sublistspan(Mid,B).

sublistspan(Mid,Full):-sublistspan(_Left,Mid,_Right,Full).
sublistspan(Left,Mid,Right,Full):-append(Left,MidRight,Full),append(Mid,Right,MidRight).

%set_matchit(A,B):-member(E,A),member(E,B).
/*
set_matchit([_,Input|_],[_,Input|_]).
set_matchit([Input|_],[Input|_]).
set_matchit([_,_,Input|_],[_,Input|_]).
set_matchit([_,Input|_],[_,_,Input|_]).
%%set_matchit([Input|_],[_,Input|_]).
*/

%get_aiml_that(Ctx,SaidThat,Match,OOut):-get_aiml_cyc(SaidThat,Match,Out),(([srai(Out)] = OOut);OOut=Out).
get_aiml_that(_Ctx,SaidThat,Match,Out,what(SaidThat, Match,Out)):-what(SaidThat, Match,Out).
get_aiml_that(_Ctx,[*],Match,Out,response(Match,Out)):-response(Match,Out).

get_aiml_that(_CTX,[T|HAT],MATCH,OUT,aimlCate(_GRAPH,_PRECALL,_TOPIC,[T|HAT],_INPUT,MATCH,_FLAGS,_CALL,_GUARD,_USERDICT,OUT,_LINENO,_SRCFILE)):-aimlCate(_GRAPH,_PRECALL,_TOPIC,[T|HAT],_INPUT,MATCH,_FLAGS,_CALL,_GUARD,_USERDICT,OUT,_LINENO,_SRCFILE).
get_aiml_that(_CTX,[*],MATCH,OUT,aimlCate(_GRAPH,_PRECALL,_TOPIC,(*),_INPUT,MATCH,_FLAGS,_CALL,_GUARD,_USERDICT,OUT,_LINENO,_SRCFILE)):-aimlCate(_GRAPH,_PRECALL,_TOPIC,(*),_INPUT,MATCH,_FLAGS,_CALL,_GUARD,_USERDICT,OUT,_LINENO,_SRCFILE).

%get_aiml_cyc([*],[String|ListO],[Obj,*]):-poStr(Obj,[String|List]),append(List,[*],ListO).
%get_aiml_cyc([*],[String,*],[Obj,*]):-poStr(Obj,String).


% ===============================================================================================
% Rate Match -  rateMatch(Markup,Original,InputBefore,InputAfter,Cost,Consumed)
% ===============================================================================================
rateMatch([],[],Out,Out,1,[]):-!.

rateMatch(Match,Match,Out,Out,1.3,[]):-!.

rateMatch([This|More],[This|More2],Out,OOut,Vote2,Grabbed):-
      rateMatch(More,More2,Out,OOut,Vote,Grabbed),
      Vote2 is Vote * (1.11).

rateMatch([*],More,Out,OOut,0.77,More):-!,subst(Out,*,More,OOut),!.
rateMatch(['_'],More,Out,OOut,0.87,More):-!,subst(Out,*,More,OOut),!.

rateMatch([*|Rest],More,Out,OOut,VoteO,Grabbed):-
      append(Grabbed,Rest,More),
      rateMatch(More,_More2,Out,OOut,Vote,_),
      subst(Out,*,Grabbed,OOut),
      VoteO is Vote * (0.72).

rateMatch(['_'|Rest],More,Out,OOut,VoteO,Grabbed):-
      append(Grabbed,Rest,More),
      rateMatch(More,_More2,Out,OOut,Vote,_),
      subst(Out,*,Grabbed,OOut),
      VoteO is Vote * (0.82).
            


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

:-ensure_loaded('cynd/logicmoo_module_aiml_shared.pl').
%% :-ensure_loaded('cynd/logicmoo_module_aiml_shared.pl').
:-ensure_loaded('cynd/logicmoo_module_aiml_xpath.pl').

:-traceAll.

:-ensure_loaded('cynd/logicmoo_module_aiml_loader.pl').
:-ensure_loaded('cynd/logicmoo_module_aiml_eval.pl').


%:-ensure_loaded('bootstrap.aiml.pl').

%:-load_aiml_files.

%:-debug,run_chat_tests.

%:-main_loop.

% :- tell(listing1),listing,told.

%:- guitracer.

%:- do.



