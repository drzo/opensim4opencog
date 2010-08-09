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

eval_templateList([A|DOIT]):-eval_template(A),eval_templateList(DOIT),!.
eval_templateList([]).


eval_template(_ - Calls):- !,eval_template(Calls),!.

eval_template(element(srai,ATTRIBS,DOIT)):-
      pushAttributes(filelevel,ATTRIBS),
      eval_templateList(DOIT),
      popAttributes(filelevel,ATTRIBS),!.

eval_template(element(Learn, ATTRIBS, Value)):- member(Learn,[load,learn]),!,
 debugOnFailureAiml((
     attributeValue(ATTRIBS,[graph],Graph,current_value),
     attributeValue(ATTRIBS,[filename,uri,path,dir,file],Filename,Value),
      pushAttributes(filelevel,[filename=Filename,graph=Graph|ATTRIBS]),
      load_aiml_files(Filename),
      popAttributes(filelevel,[filename=Filename,graph=Graph|ATTRIBS]))),!.



eval_template(element(Learn, ATTRIBS, Value)):- aiml_error(eval_template(element(Learn, ATTRIBS, Value))),!.

eval_template(Call):- aiml_eval(Call,Calls),!,callEachList(Calls),!.
eval_template(INNER_XML):-render_outvalue(INNER_XML,Rendered),!, debugFmt(Rendered),!.




callEachList(C):-not(is_list(C)),!,callInteractive(C,_).
callEachList(Calls):-is_list(Calls),member(Cs,Calls),callEachList(Cs),fail.

aiml_eval(_ - Calls, Res):- !,aiml_eval(Calls, Res),!.
aiml_eval([],[]):-!.
aiml_eval([A|AA],[B|BB]):-aiml_eval(A,B),aiml_eval(AA,BB),!.

aiml_eval(element(pre, _, ListIn), writeln(Render)):- 
      convert_element(ListIn,ListOut),
      render_value(template,ListOut,Render),!.

% ===================================================================
%  render templates
% ===================================================================

render_value(template,ListOut,Render):-render_outvalue(ListOut,Render),!.

render_outvalue([ValueI],ValueO):-atom(ValueI),!,render_outvalue(ValueI,ValueO),!.
render_outvalue([Value|I],ValueO):-atom(Value),concat_atom([Value|I],' ',ValueI),!,render_outvalue(ValueI,ValueO),!.
render_outvalue(ValueI,ValueO):- !,ValueI=ValueO,!.


% ===================================================================
%  system tag impl
% ===================================================================

aiml_eval(element(system,ATTRIBS,INNER_XML),[systemCall(Value,Rendered,Output)]):-render_outvalue(INNER_XML,Rendered),
         attributeValue(ATTRIBS,[lang],Value,'bot'),
         systemCall(Value,Rendered,Output).


systemCall(Lang,Eval,Out):-atom(Eval),!,atomSplit(Eval,Atoms),!,systemCall(Lang,Atoms,Out).
systemCall('bot',['@load',Filename],['@load',Filename]):- load_aiml_file_graph([],default,Filename),!.
systemCall(Lang,Eval,writeq(evaled(Lang,Eval))):-!.



% ===================================================================
%  learn tag impl
% ===================================================================

% 0.9 version
aiml_eval(element(Learn, XML, []),[load_aiml_file_graph(XML,Graph,Filename)]):- member(Learn,[load,learn]),
 debugOnFailureAiml((
     attributeValue(XML,[graph],Graph,current_value),
     attributeValue(XML,[filename,uri,path,dir,file],Filename,error))),!.



load_aiml_file_graph(XML,Graph,Filename):-
 debugOnFailureAiml((
      pushAttributes(filelevel,[filename=Filename,graph=Graph|XML]),
      load_aiml_files(Filename),
      popAttributes(filelevel,[filename=Filename,graph=Graph|XML]))),!.



aiml_eval(element(genlmt,TOFROM,_),[genlMtGraph(TO,FROM)]):-
 debugOnFailureAiml((
      attributeValue(TOFROM,[to,name],TO,error),
      attributeValue(TOFROM,[graph,from],FROM,current_value),
      assertz(genlMtGraph(TO,FROM)))),!.

% ===================================================================
%  template tag impl
% ===================================================================

%aiml_eval(INNER_XML,[debugFmt(Rendered)]):-render_outvalue(INNER_XML,Rendered),!.


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
