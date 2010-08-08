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

eval_template(INNER_XML):-render_outvalue(INNER_XML,Rendered),!,debugFmt(Output).
eval_template(Call):- aiml_eval(Call,Calls), callEachList(Calls).

  

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

systemCall(Lang,Eval,writeq(evaled(Lang,Eval))):-!.

% ===================================================================
%  learn tag impl
% ===================================================================

% 0.9 version
aiml_eval(element(learn, XML, []),[load_aiml_graph(Graph,Filename,XML)]):-
     attributeValue(XML,graph,Graph,error),
     attributeValue(XML,filename,Filename,error),!.

load_aiml_graph(Graph,Filename,XML):-
      pushAttributes(filelevel,XML),
      load_aiml_files(Filename),
      popAttributes(filelevel,XML),!.


% ===================================================================
%  template tag impl
% ===================================================================

aiml_eval(INNER_XML,[debugFmt(Output)]):-render_outvalue(INNER_XML,Rendered),!.


% ===================================================================
%  MISSING tag impl
% ===================================================================
aiml_eval(AIML,[debugFmt(aiml_eval_missing(AIML))]):-!.

% ===================================================================
% attribute searching
% ===================================================================


attributeOrTagValue(ATTRIBS,Name,ValueO,Else,XML):-attributeValue(ATTRIBS,Name,ValueO,Else),!.
attributeOrTagValue(ATTRIBS,Name,ValueO,Else,XML):-findTagValue(XML,Name,ValueO,Else),!.


attributeValue(ATTRIBS,NameS,Value,Else):-member(Name,NameS),attributeValue(ATTRIBS,Name,Value,Else).
attributeValue(ATTRIBS,Name,ValueO,Else):-member(Name=ValueI,ATTRIBS),!,render_outvalue(ValueI,ValueO).
attributeValue(ATTRIBS,Name,Value,error):-!,aiml_error(attributeValue(ATTRIBS,Name,Value,error)).
attributeValue(ATTRIBS,Name,ValueO,Else):-ValueI=Else,!,render_outvalue(ValueI,ValueO).


findTagValue(XML,NameS,Value,Else):-member(Name,NameS),findTagValue(XML,Name,Value,Else).
findTagValue(XML,Name,ValueO,Else):-member(element(Name,ATTRIBS,ValueI),XML),!,render_outvalue(ValueI,ValueO).
findTagValue(XML,Name,Value,error):-!,aiml_error(findTagValue(XML,Name,Value,error)).
findTagValue(XML,Name,ValueO,Else):-ValueI=Else,!,render_outvalue(ValueI,ValueO).

