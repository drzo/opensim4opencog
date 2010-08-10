% ===================================================================
% File 'logicmoo_module_aiml_eval.pl'
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

% ===================================================================
%  Prolog-like call
% ===================================================================

aiml_call(Ctx,_ - Calls):- !,aiml_call(Ctx,Calls),!.

aiml_call(Ctx,[Atomic|Rest]):-atom(Atomic),!, aiml_eval(Ctx,[Atomic|Rest],Output),!,debugFmt(Output),!.


aiml_call(Ctx,element(Learn, ATTRIBS, Value)):-  member(Learn,[load,learn]),!,
 debugOnFailureAiml((
     attributeValue(Ctx,ATTRIBS,[graph],Graph,current_value),
     attributeValue(Ctx,ATTRIBS,[filename,uri,path,dir,file],Filename,Value),
      withAttributes(Ctx,filelevel,[filename=Filename,graph=Graph|ATTRIBS],
      load_aiml_files(Ctx,Filename)))).

aiml_call(Ctx,Call):- aiml_eval(Ctx,Call,Calls),!,callEachElement(Ctx,Calls),!.
aiml_call(Ctx,INNER_XML):-aiml_eval(Ctx,INNER_XML,Rendered),!, debugFmt(Rendered),!.

aiml_call(Ctx,element(genlmt,TOFROM,_)):-
 debugOnFailureAiml((
      attributeValue(Ctx,TOFROM,[to,name],TO,error),
      attributeValue(Ctx,TOFROM,[graph,from],FROM,current_value),
      assertz(genlMtGraph(TO,FROM)))),!.

 aiml_call(Ctx,element(Learn, ATTRIBS, Value)):- aiml_error(aiml_call(Ctx,element(Learn, ATTRIBS, Value))),!.


% ===================================================================
%  Prolog-like call
% ===================================================================

callEachElement(Ctx,[C|Calls]):-!, callEachElement(Ctx,C),callEachElement(Ctx,Calls).
callEachElement(Ctx,element(A,B,C)):- convert_element(Ctx,element(A,B,C),ELE),callEachElement(Ctx,ELE),!.
callEachElement(Ctx,C):-callInteractive(C,_).

% ===================================================================
%  render templates
% ===================================================================

render_value(template,ListOut,Render):-aiml_eval(Ctx,ListOut,Render),!.

%aiml_eval(Ctx,[ValueI],ValueO):-atom(ValueI),!,aiml_eval(Ctx,ValueI,ValueO),!.
%aiml_eval(Ctx,[Value|I],ValueO):-atom(Value),concat_atom([Value|I],' ',ValueI),!,aiml_eval(Ctx,ValueI,ValueO),!.
%aiml_eval(Ctx,ValueI,ValueO):- !,ValueI=ValueO,!.

aiml_eval(Ctx,_ - Calls):- throw(var(Calls)),!.
aiml_eval(Ctx,Num - Msg,Result):-!,aiml_eval(Ctx,Msg,Result),!.

aiml_eval(_Ctx,[],[]):-!.
aiml_eval(Ctx,[Atomic|Rest],[Atomic|Output]):-atomic(Atomic),!,aiml_eval(Ctx,Rest,Output),!.

aiml_eval(_Ctx,A,B):-atomic(A),!,B=A.


aiml_eval(Ctx,element(srai,ATTRIBS,DOIT),RETURN):-
      withAttributes(Ctx,filelevel,ATTRIBS,
       (aiml_eval(Ctx,DOIT,INNER),computeAnswer(Ctx,10,element(srai,ATTRIBS,INNER),RMID,_Votes))),
         aiml_eval(Ctx,RMID,RETURN).


aiml_eval(Ctx,[A|AA], [B|BB]):- aiml_eval(Ctx,A,B),convert_template(Ctx,AA,BB),!.
%aiml_eval(Ctx,[A|AA], [B|BB]):- convert_element(Ctx,A,B),aiml_eval(Ctx,AA,BB),!.
%%aiml_eval(Ctx,[A|AA], [B|BB]):- convert_element(Ctx,A,B),convert_template(Ctx,AA,BB),!.



% ===================================================================
%  template tag impl
% ===================================================================


%aiml_eval(Ctx,INNER_XML,[debugFmt(Rendered)]):-aiml_eval(Ctx,INNER_XML,Rendered),!.


% ===================================================================
%  MISSING tag impl
% ===================================================================
%%aiml_eval(Ctx,AIML,[debugFmt(aiml_eval_missing(AIML))]):-!.

aiml_eval(Ctx,element(Learn, ATTRIBS, Value)):- aiml_error(aiml_eval(Ctx,element(Learn, ATTRIBS, Value))),!.


aiml_eval(Ctx,INNER_XML,RESULT):-tag_eval(Ctx,INNER_XML,RESULT),!.

aiml_eval(Ctx,RESULT,RESULT):-!.



% ===================================================================
%  system tag impl
% ===================================================================

tag_eval(Ctx,element(system,ATTRIBS,INNER_XML),Output):-aiml_eval(Ctx,INNER_XML,Rendered),
         attributeValue(Ctx,ATTRIBS,[lang],Value,'bot'),
         systemCall(Ctx,Value,Rendered,Output).


systemCall(Ctx,Lang,Eval,Out):-atom(Eval),!,atomSplit(Eval,Atoms),!,systemCall(Ctx,Lang,Atoms,Out).
systemCall(Ctx,'bot',['@load',Filename],['@load',Filename]):-  load_aiml_files(Ctx,Filename),!. %%%  load_aiml_file_graph(Ctx,[],default,Filename),!.
systemCall(Ctx,'bot',['@chgraph',Graph],['@chgraph',Graph]):-  set_current_value(Ctx,graph,Graph),!.
systemCall(Ctx,Lang,Eval,writeq(evaled(Lang,Eval))):- aiml_error(evaled(Lang,Eval)).

% ===================================================================
%  learn tag impl
% ===================================================================

% 0.9 version
tag_eval(Ctx,element(Learn, ATTRIB, Value),done(new)/*NEW*/):- member(Learn,[load,learn]),
 debugOnFailureAiml((
     attributeValue(Ctx,ATTRIB,[graph],Graph,current_value),
     attributeValue(Ctx,ATTRIB,[filename,uri,path,dir,file],Filename,Value),
     load_structure(Filename,MOREXML,[dialect(xml),space(remove)]))),!,
     append(EXTRA,MOREXML,NEWXML),
     NEW = element(aiml,ATTRIB,NEWXML),
     debugOnFailureAiml((load_aiml_structure(Ctx,NEW))),!.
     

load_aiml_file_graph(Ctx,XML,Graph,Filename):-
 debugOnFailureAiml((
      withAttributes(Ctx,filelevel,[filename=Filename,graph=Graph|XML],
      load_aiml_files(Ctx,Filename)))).


% ===================================================================
% attribute searching
% ===================================================================


attributeOrTagValue(Ctx,ATTRIBS,NameS,ValueO,Else,XML):-attributeValue(Ctx,ATTRIBS,NameS,ValueO,Else),!.
attributeOrTagValue(Ctx,ATTRIBS,NameS,ValueO,Else,XML):-findTagValue(Ctx,XML,NameS,ValueO,Else),!.



attributeValue(Ctx,ATTRIBS,NameS,ValueO,Else):-member(Name,NameS),member(Name=ValueI,ATTRIBS),!,aiml_eval(Ctx,ValueI,ValueO),!.
attributeValue(Ctx,ATTRIBS,NameS,ValueO,current_value):-member(Name,NameS),current_value(Ctx,Name,ValueI),aiml_eval(Ctx,ValueI,ValueO),!.
attributeValue(Ctx,ATTRIBS,NameS,Value,Error):-error==Error,aiml_error(attributeValue(Ctx,ATTRIBS,NameS,Value,error)).
attributeValue(Ctx,ATTRIBS,_Name,ValueO,Else):-ValueI=Else,!,aiml_eval(Ctx,ValueI,ValueO).


findTagValue(Ctx,XML,NameS,ValueO,Else):-member(Name,NameS),member(element(Name,ATTRIBS,ValueI),XML),aiml_eval(Ctx,ValueI,ValueO),!.
findTagValue(Ctx,XML,[Name|_],ValueO,Else):-member(element(Name,ATTRIBS,ValueI),XML),member(_=Name,ATTRIBS),aiml_eval(Ctx,ValueI,ValueO),!.
findTagValue(Ctx,XML,NameS,ValueO,Else):-member(INNERXML,XML),findTagValue(Ctx,INNERXML,NameS,ValueO,Else),!.
findTagValue(Ctx,XML,Name,ValueO,Else):-ValueI=Else,!,aiml_eval(Ctx,ValueI,ValueO).


current_value(Ctx,Name,ValueI):-peekNameValue(Ctx,_,Name,ValueI),!.


%['name'='SomeName','Description'='some descr','Input'='error','ExpectedAnswer'='SomeAnswwer']
getAttributeOrTags(Ctx,[],ATTRIBS,INNERXML,[]):-!.


getAttributeOrTags(Ctx,[N=Default|More],ATTRIBS,INNERXML,[N=ValueO|NormalProps]):- 
      member(element(N,Atribs,Value),INNERXML) ,aiml_eval(Ctx,Value,ValueO),!,
      getAttributeOrTags(Ctx,More,ATTRIBS,INNERXML,NormalProps),!.



getAttributeOrTags(Ctx,[N=Default|More],ATTRIBS,INNERXML,[N=Found|NormalProps]):- 
      attributeOrTagValue(Ctx,ATTRIBS,[N],Found,Default,INNERXML),
      getAttributeOrTags(Ctx,More,ATTRIBS,INNERXML,NormalProps),!.
getAttributeOrTags(Ctx,[N=Default|More],ATTRIBS,INNERXML,[N=Default|NormalProps]):- 
      getAttributeOrTags(Ctx,More,ATTRIBS,INNERXML,NormalProps),!.


set_current_value(Ctx,N,V):-pushNameValue(Ctx,user,N,V).
