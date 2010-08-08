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

aiml_eval(_ - Calls, Res):- !,aiml_eval(Calls, Res),!.
aiml_eval([],[]):-!.
aiml_eval([A|AA],[B|BB]):-aiml_eval(A,B),aiml_eval(AA,BB),!.
aiml_eval(element(learn, XML, []),callEachList([load_aiml_graph(Graph,Filename,XML)])):-
     attributeValue(XML,graph,Graph,error),
     attributeValue(XML,filename,Filename,error),!.

aiml_eval(AIML,debugFmt(aiml_eval_missing(AIML))):-!.


% ===================================================================
% attribute searching
% ===================================================================
attributeValue(XML,NameS,ValueO,Else):-member(Name,NameS),member(Name=ValueI,XML),!,convert_value(ValueI,ValueO).
attributeValue(XML,Name,ValueO,Else):-member(Name=ValueI,XML),!,convert_value(ValueI,ValueO).
attributeValue(XML,Name,Value,error):-!,aiml_error(attributeValue(XML,Name,Value,error)).
attributeValue(XML,Name,ValueO,Else):-ValueI=Else,!,convert_value(ValueI,ValueO).

convert_value([ValueI],ValueO):-atom(ValueI),!,convert_value(ValueI,ValueO),!.
convert_value([Value|I],ValueO):-atom(Value),concat_atom([Value|I],' ',ValueI),!,convert_value(ValueI,ValueO),!.
convert_value(ValueI,ValueO):-!,ValueI=ValueO,!.



load_aiml_graph(Graph,Filename,XML):-
      pushAttributes(filelevel,XML),
      load_aiml_files(Filename),
      popAttributes(filelevel,XML),!.
   
