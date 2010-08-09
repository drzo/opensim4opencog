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

aiml_call([Atomic|Rest]):-atom(Atomic),!, aiml_eval([Atomic|Rest],Output),!,debugFmt(Output),!.

aiml_call(_ - Calls):- !,aiml_call(Calls),!.

aiml_call(element(Learn, ATTRIBS, Value)):- member(Learn,[load,learn]),!,
 debugOnFailureAiml((
     attributeValue(ATTRIBS,[graph],Graph,current_value),
     attributeValue(ATTRIBS,[filename,uri,path,dir,file],Filename,Value),
      pushAttributes(filelevel,[filename=Filename,graph=Graph|ATTRIBS]),
      load_aiml_files(Filename),
      popAttributes(filelevel,[filename=Filename,graph=Graph|ATTRIBS]))),!.



aiml_call(element(Learn, ATTRIBS, Value)):- aiml_error(aiml_call(element(Learn, ATTRIBS, Value))),!.

aiml_call(Call):- aiml_eval(Call,Calls),!,callEachElement(Calls),!.
aiml_call(INNER_XML):-render_outvalue(INNER_XML,Rendered),!, debugFmt(Rendered),!.

aiml_call(element(genlmt,TOFROM,_)):-
 debugOnFailureAiml((
      attributeValue(TOFROM,[to,name],TO,error),
      attributeValue(TOFROM,[graph,from],FROM,current_value),
      assertz(genlMtGraph(TO,FROM)))),!.


% ===================================================================
%  Prolog-like call
% ===================================================================

callEachElement([C|Calls]):-!, callEachElement(C),callEachElement(Calls).
callEachElement(element(A,B,C)):- convert_element(element(A,B,C),ELE),callEachElement(ELE),!.
callEachElement(C):-callInteractive(C,_).

% ===================================================================
%  render templates
% ===================================================================

render_value(template,ListOut,Render):-render_outvalue(ListOut,Render),!.

render_outvalue([ValueI],ValueO):-atom(ValueI),!,render_outvalue(ValueI,ValueO),!.
render_outvalue([Value|I],ValueO):-atom(Value),concat_atom([Value|I],' ',ValueI),!,render_outvalue(ValueI,ValueO),!.
render_outvalue(ValueI,ValueO):- !,ValueI=ValueO,!.

render_outvalue([],[]):-!.

aiml_eval(Num - Msg,Result):-!,aiml_eval(Msg,Result),!.

aiml_eval(A,B):-atomic(A),!,B=A.

aiml_eval(element(srai,ATTRIBS,DOIT),RETURN):-
      pushAttributes(filelevel,ATTRIBS),
      aiml_eval(DOIT,MID), computeAnswer(10,MID,RETURN,_Votes),
      popAttributes(filelevel,ATTRIBS),!.


aiml_eval([A|AA], [B|BB]):- aiml_eval(A,B),convert_template(AA,BB),!.
aiml_eval([A|AA], [B|BB]):- convert_element(A,B),aiml_eval(AA,BB),!.

% ===================================================================
%  system tag impl
% ===================================================================

aiml_eval(element(system,ATTRIBS,INNER_XML),[systemCall(Value,Rendered,Output)]):-render_outvalue(INNER_XML,Rendered),
         attributeValue(ATTRIBS,[lang],Value,'bot'),
         systemCall(Value,Rendered,Output).


systemCall(Lang,Eval,Out):-atom(Eval),!,atomSplit(Eval,Atoms),!,systemCall(Lang,Atoms,Out).
systemCall('bot',['@load',Filename],['@load',Filename]):-  load_aiml_files(Filename),!. %%%  load_aiml_file_graph([],default,Filename),!.
systemCall('bot',['@chgraph',Graph],['@chgraph',Graph]):-  set_current_value(graph,Graph),!.
systemCall(Lang,Eval,writeq(evaled(Lang,Eval))):- trace,!.



% ===================================================================
%  learn tag impl
% ===================================================================

% 0.9 version
aiml_eval(element(Learn, ATTRIB, EXTRA),NEW):- member(Learn,[load,learn]),
 debugOnFailureAiml((
     attributeValue(ATTRIB,[graph],Graph,current_value),
     attributeValue(ATTRIB,[filename,uri,path,dir,file],Filename,'/dev/null'),
     load_structure(Filename,MOREXML,[dialect(xml),space(remove)]))),!,
     append(EXTRA,MOREXML,NEWXML),
     NEW = element(aiml,ATTRIB,NEWXML),
     !,load_aiml_structure(NEW),!.
     



load_aiml_file_graph(XML,Graph,Filename):-
 debugOnFailureAiml((
      pushAttributes(filelevel,[filename=Filename,graph=Graph|XML]),
      load_aiml_files(Filename),
      popAttributes(filelevel,[filename=Filename,graph=Graph|XML]))),!.



% ===================================================================
%  template tag impl
% ===================================================================

aiml_eval(INNER_XML,[debugFmt(Rendered)]):-render_outvalue(INNER_XML,Rendered),!.


% ===================================================================
%  MISSING tag impl
% ===================================================================
%%aiml_eval(AIML,[debugFmt(aiml_eval_missing(AIML))]):-!.

% ===================================================================
% attribute searching
% ===================================================================


attributeOrTagValue(ATTRIBS,NameS,ValueO,Else,XML):-attributeValue(ATTRIBS,NameS,ValueO,Else),!.
attributeOrTagValue(ATTRIBS,NameS,ValueO,Else,XML):-findTagValue(XML,NameS,ValueO,Else),!.



attributeValue(ATTRIBS,NameS,ValueO,Else):-member(Name,NameS),member(Name=ValueI,ATTRIBS),!,render_outvalue(ValueI,ValueO),!.
attributeValue(ATTRIBS,NameS,ValueO,current_value):-member(Name,NameS),current_value(Name,ValueI),render_outvalue(ValueI,ValueO),!.
attributeValue(ATTRIBS,NameS,Value,Error):-error==Error,aiml_error(attributeValue(ATTRIBS,NameS,Value,error)).
attributeValue(ATTRIBS,Name,ValueO,Else):-ValueI=Else,!,render_outvalue(ValueI,ValueO).



findTagValue(XML,NameS,ValueO,Else):-member(Name,NameS),member(element(Name,ATTRIBS,ValueI),XML),!,render_outvalue(ValueI,ValueO).
findTagValue(XML,Name,Value,error):-!,aiml_error(findTagValue(XML,Name,Value,error)).
findTagValue(XML,Name,ValueO,Else):-ValueI=Else,!,render_outvalue(ValueI,ValueO).


current_value(Name,ValueI):-peekNameValue(_,Name,ValueI),!.


%['name'='SomeName','Description'='some descr','Input'='error','ExpectedAnswer'='SomeAnswwer']
getAttributeOrTags([],ATTRIBS,LIST,[]):-!.
getAttributeOrTags([N=Default|More],ATTRIBS,LIST,[N=Found|NormalProps]):- 
      attributeOrTagValue(ATTRIBS,[N],Found,Default,LIST),
      getAttributeOrTags(More,ATTRIBS,LIST,NormalProps),!.

set_current_value(N,V):-pushNameValue(user,N,V).
