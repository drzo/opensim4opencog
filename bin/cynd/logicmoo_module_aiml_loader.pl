
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
:- style_check(-discontiguous).
:- style_check(-atom).
:- style_check(-string).

:-dynamic(attribDatabase/3).

dyn_retractall(E):- retractall(E),functor(E,F,A),dynamic(F/A),!.

pp_listing(Pred):-!. %%functor(Pred,F,A),functor(FA,F,A),listing(F),nl,findall(NV,predicate_property(FA,NV),LIST),writeq(LIST),nl,!.

%:-dyn_retractall(aimlCate(_)).

aimlDebugFmt(X):-!. %%debugFmt(X),!.

debugOnFailureAiml((A,B)):- !,debugOnFailureAiml(A),!,debugOnFailureAiml(B),!.
debugOnFailureAiml(Call):- once( catch(Call,E,aiml_error(Call:E)) ; (trace,writeq(fail(Call))) ).

% =================================================================================
% AIML Loading
% =================================================================================
load_aiml_files:-dyn_retractall(aimlCate(_)),fail.
load_aiml_files:-load_aiml_files('aiml/test_suite/*.aiml'),fail.
load_aiml_files:-load_aiml_files('*.aiml'),fail.
load_aiml_files:-pp_listing(aimlCate(_)).


load_aiml_files([]):-!.
load_aiml_files([H|INNER_XML]):-
   load_aiml_files(H),!,
   load_aiml_files(INNER_XML).
load_aiml_files(F):-atom(F),
   expand_file_name(F,[F]),!,
   load_aiml_file(F).
load_aiml_files(F):-atom(F),expand_file_name(F,FILES),!,load_aiml_files(FILES),!.

aiml_files(F,Files):-atom_concat(F,'/',WithSlashes),absolute_file_name(WithSlashes,[relative_to('./')],WithOneSlash),
                    atom_concat(WithOneSlash,'/*.aiml',Mask),expand_file_name(Mask,Files),!.

load_aiml_file(F):- exists_directory(F), aiml_files(F,Files),load_aiml_files(Files),!.
load_aiml_file(F):- exists_file(F), create_aiml_file(F),!.
load_aiml_file(F):- file_name_extension(F,'aiml',Aiml), exists_file(Aiml),create_aiml_file(Aiml),!.
load_aiml_file(F):- atom_concat('../',F,Again),load_aiml_file(Again),!.


aimlOption(rebuild_Aiml_Files,true).

time_file_safe(F,INNER_XML):-exists_file(F),time_file(F,INNER_XML).

create_aiml_file(F):-
   atom_concat(F,'.pl',PLNAME),
   time_file_safe(PLNAME,PLTime), % fails on non-existent
   time_file_safe(F,FTime),
   not(aimlOption(rebuild_Aiml_Files,true)),
   PLTime > FTime,!,
   [PLNAME],!.

ifThen(When,Do):-When->Do;true.

create_aiml_file(F):-
  debugOnFailureAiml((
     Dofile = fail,
   atom_concat(F,'.pl',PLNAME),
   ifThen(Dofile,tell(PLNAME)),
   (format(user_error,'%~w~n',[F])),
   load_structure(F,X,[dialect(xml),space(remove)]),
   ATTRIBS = [filename=F],
   pushAttributes(filelevel,ATTRIBS),
   load_aiml_structure_list(X),
   popAttributes(filelevel,ATTRIBS),
   ifThen(Dofile,(told,[PLNAME])))),!.


load_aiml_structure_list(L):-load_mapcar(load_aiml_structure,L).


load_mapcar(Pred,[X|L]):-debugOnFailureAiml(call(Pred,X)),!,debugOnFailureAiml(load_mapcar(Pred,L)),!.
load_mapcar(_Pred,[]):-!.



withAttributes(filelevel,ATTRIBS,Call):-
    debugOnFailureAiml((
     pushAttributes(filelevel,ATTRIBS),
     Call,
     popAttributes(filelevel,ATTRIBS))),!.

% ============================================
% Loading content
% ============================================

load_aiml_structure(O):-atomic(O),!,writeq(load_aiml_structure(O)),nl.

load_aiml_structure(element(aiml,ALIST,LIST)):-
   debugOnFailureAiml((
     ground(ALIST),
     replaceAttribute(name,graph,ALIST,ATTRIBS),
     pushAttributes(filelevel,ATTRIBS),
     load_aiml_structure_list(LIST),
     popAttributes(filelevel,ATTRIBS))),!.

load_aiml_structure(element(Tag,ALIST,INNER_XML)):- member(Tag,[topic,flags]),
   debugOnFailureAiml((
     replaceAttribute(name,Tag,ALIST,ATTRIBS),
     withAttributes(filelevel,ATTRIBS,load_aiml_structure_list(INNER_XML)))),!.

load_aiml_structure(element(Tag,ALIST,INNER_XML)):- cateMember(Tag),
   debugOnFailureAiml((
     replaceAttribute(name,Tag,ALIST,ATTRIBS),
     pushAttributes(filelevel,ATTRIBS),
     pushAttributes(category,[Tag=INNER_XML|ATTRIBS]))),!.

load_aiml_structure(element(Tag,ALIST,INNER_XML)):- member(Tag,[category]),
   debugOnFailureAiml((
     pushAttributes(filelevel,ALIST),
     load_category(element(Tag,ALIST,INNER_XML)),
     popAttributes(filelevel,ALIST))),!.

load_aiml_structure(element(Tag,ALIST,LIST)):- tagType(Tag,immediate),!,
       debugOnFailureAiml((
     replaceAttribute(name,Tag,ALIST,ATTRIBS),
     pushAttributes(filelevel,ATTRIBS),      
     debugFmt(call_immediate(Tag,ALIST,LIST)))),!.


% ============================================
% special dictionaries
% ============================================

% user/bot dictionaries
load_aiml_structure(element(Tag,ALIST,LIST)):- member(Tag,[predicates,vars,properties]),
   debugOnFailureAiml((
     debugOnFailureAiml(replaceAttribute(name,dictionary,ALIST,ATTRIBS)),
   debugOnFailureAiml(pushAttributes(filelevel,ATTRIBS)),
   debugOnFailureAiml(load_aiml_structure_list(LIST)),
     debugOnFailureAiml(popAttributes(filelevel,ATTRIBS)))),!.


% user/bot predicatates
load_aiml_structure(element(Tag,ALIST,LIST)):-member(Tag,[predicate]),
   debugOnFailureAiml((
     attributeValue(ALIST,[name,var],Name,error),
     attributeValue(ALIST,[default],Default,''),
     attributeValue(ALIST,[value,default],Value,LIST),
     attributeValue(ALIST,['set-return'],SetReturn,value),
   peekNameValue(filelevel,dictionary,Dict),
   load_aiml_structure(dict(Dict,Name,Value)),
     load_aiml_structure(dict(defaultValue(Dict),Name,Default)),
     load_aiml_structure(dict(setReturn(Dict),Name,SetReturn)))),!.

% user/bot dictionaries name/values
load_aiml_structure(element(Tag,ALIST,LIST)):-member(Tag,[property,var,item]),
   debugOnFailureAiml((
     attributeValue(ALIST,[name,var],Name,error),
     attributeValue(ALIST,[value,default],Value,LIST),
     peekNameValue(filelevel,dictionary,Dict),
     load_aiml_structure(dict(Dict,Name,Value)))),!.


% substitutions
load_aiml_structure(element(substitutions,ALIST,LIST)):-
   debugOnFailureAiml((
      replaceAttribute(name,graph,[dictionary=substitutions(input)|ALIST],ATTRIBS),
     pushAttributes(filelevel,ATTRIBS),
     load_mapcar(load_substs,LIST),
     popAttributes(filelevel,ATTRIBS))),!.

load_substs(element(Tag,ALIST,LIST)):- substitutionDictsName(Tag,Dict),
   debugOnFailureAiml((
      replaceAttribute(name,graph,[dictionary=substitutions(Dict)|ALIST],ATTRIBS),
     pushAttributes(filelevel,ATTRIBS),
     load_mapcar(load_substs,LIST),
     popAttributes(filelevel,ATTRIBS))),!.

load_substs(element(substitute,ATTRIBS,LIST)):-
   debugOnFailureAiml((
      peekNameValue(filelevel,dictionary,substitutions(Catalog)),
      attributeOrTagValue(ATTRIBS,[find,name,before],Find,error,LIST),
      attributeOrTagValue(ATTRIBS,[replace,value,after],Replace,error,LIST),
      debugOnFailureAiml(load_aiml_structure(dict(substitutions(Catalog),Find,Replace))))),!.

% substitutions
load_aiml_structure(element(substitute,ATTRIBS,LIST)):- load_substs(element(substitute,ATTRIBS,LIST)),!.
load_aiml_structure(element(substitution,ATTRIBS,LIST)):- load_substs(element(substitute,ATTRIBS,LIST)),!.


load_aiml_structure(substitute(Dict,Find,Replace)):-
  debugOnFailureAiml((
      convert_text(Find,F),!,convert_text(Replace,Resp),!,
      load_aiml_structure(dict(substitutions(Dict),F,Resp)))),!.

% actual assertions
load_aiml_structure(dict(Dict,Name,Value)):-
      assertz(dict(Dict,Name,Value)),!.


% ============================================
% Rewrite or Error loading
% ============================================

load_aiml_structure(element(Tag,ALIST,PATTERN)):- once(convert_ele(element(Tag,ALIST,PATTERN),NEW)),
  NEW \== element(Tag,ALIST,PATTERN), once((ground(NEW);convert_ele(element(Tag,ALIST,PATTERN),_NEXT))),!,
  debugOnFailureAiml(load_aiml_structure(NEW)),!.


load_aiml_structure(X):-aiml_error(load_aiml_structure(X)).
%      saveFAttribute(F,A),
     % 
      %(catch(X,_,fail);asserta(X)),!.


convert_text('',''):-!.
convert_text([],''):-!.
convert_text(C,D):-is_list(C),!,convert_text_list(C,D),!.
convert_text(A,O):-atom(A),!,convert_atom(A,O).
convert_text(A,''):-ignore_aiml(A),!.
convert_text(E,F):-aiml_error(convert_text(E,F)),!,E=F.


convert_text_list([],[]):-!.
convert_text_list([A],B):-!,convert_text(A,B).
convert_text_list(M,C):-delete(M,'',B), (M == B -> C=B ; convert_text_list(B,C)).
convert_text_list([A|AA],BBB):-convert_text(A,B),convert_text_list(AA,BB),!,flattem_append(B,BB,BBB0),!,BBB=BBB0.

convert_atom(A,Z):-convert_atom0(A,Y),!,Y=Z.
convert_atom(E,F):-aiml_error(convert_atom(E,F)),!,E=F.
%convert_atom(A,C):-atom_to_number(A,C),!.
convert_atom0(A,A):-concat_atom([A],' ',A).
convert_atom0(A,C):-atomSplit(A,M),!,convert_text(M,C),!.
convert_atom0(A,A).

flattem_append(A,B,BBB):-flatten([A],AA),!,flatten([B],BB),!,append(AA,BB,BBB),!.

:-dynamic(saveDFAttribute/2).
:-dynamic(replace_t/5).
:-dynamic(response_t/5).
                
saveFAttribute(F,A):-saveDFAttribute(F,A),!.
saveFAttribute(F,A):-asserta(saveDFAttribute(F,A)),dynamic(F/A).

tagType(Tag,immediate):-member(Tag,[substitute,learn,aiml,genlMt,srai,think,system,javascript,eval]).

% ===============================================================================================
%    AIML Runtime Database
% ===============================================================================================
prolog_must(Call):-Call,!.
prolog_must(Call):-!,aiml_error(Call).

pushAttributes(Scope,[N=V|L]):- pushNameValue(Scope,N,V),!,pushAttributes(Scope,L),!.
pushAttributes(_Scope,[]).


popAttributes(Scope,[N=V|L]):- popNameValue(Scope,N,V),!,popAttributes(Scope,L),!.
popAttributes(_Scope,[]).


grabAttributes([Name|SList],Scope,[Name=Value|Results]):- grabNameValue(Scope,Name,Value),!,
         grabAttributes(SList,Scope,Results),!.
grabAttributes([],_Scope,[]):-!.


grabNameValue(Scope,Name,Value):-attribDatabase(Scope,Name,Value),debugOnFailureAiml((ground(Name=Value))).
grabNameValue(Scope,Name,Value):-attribDatabase(Scope2,Name,Value),Scope\=Scope2,debugOnFailureAiml((ground(Name=Value))).


pushNameValue(Scope,N,V):-var(Scope),!,aiml_error((pushNameValue(Scope,N,V):-var(Scope))),!.
pushNameValue(Scope,Name,V):-nonvar(Name),Name==name,aimlDebugFmt(nop(pushNameValue(Scope,name,V))),!.
pushNameValue(Scope,N,V):-prolog_must(ground(attribDatabase(Scope,N,V))),
     aimlDebugFmt(pushNameValue(Scope,N,V)),
     asserta(attribDatabase(Scope,N,V)),!.

popNameValue(Scope,N,V):-var(Scope),!,aiml_error((popNameValue(Scope,N,V):-var(Scope))),!.
popNameValue(Scope,Name,V):-nonvar(Name),Name==name,aimlDebugFmt(nop(popNameValue(Scope,name,V))),!.
popNameValue(Scope,N,V):-prolog_must(ground(v(Scope,N,V))),
   ignore(attribDatabase(Scope,N,V)),
   ignore(dyn_retract(attribDatabase(Scope,N,V))),!,
   prolog_must(ground(popNameValue1(Scope,N,V))),!.

dyn_retract(attribDatabase(Scope,N,V)):-aimlDebugFmt(retract(attribDatabase(Scope,N,V))),!. %%,retract(attribDatabase(Scope,N,V)),trace.

peekNameValue(Scope,N,V):-attribDatabase(Scope,N,V),!.

% only do the first found?
replaceAttribute(Before,After,[Before=Value|ATTRIBS],[After=Value|ATTRIBS]):-!.
% comment out the line above to do all
replaceAttribute(Before,After,[Before=Value|ALIST],[After=Value|ATTRIBS]):-
   replaceAttribute(Before,After,ALIST,ATTRIBS).
% skip over BeforeValue
replaceAttribute(Before,After,[BeforeValue|ALIST],[BeforeValue|ATTRIBS]):-
   replaceAttribute(Before,After,ALIST,ATTRIBS).
% the endcase
replaceAttribute(_Before,_After,[],[]).




% ===============================================================================================
%  UTILS
% ===============================================================================================


ignore_aiml(VAR):-var(VAR),!,aiml_error(VAR).
ignore_aiml([])-!.
ignore_aiml(''):-!.
ignore_aiml(A):-atom(A),!,atom_codes(A,C),!,clean_codes(C,D),!,D=[].
ignore_aiml([A|B]):-ignore_aiml(A),!,ignore_aiml(B),!.


clean_codes(X,Y):-trim(X,Y),!.
clean_codes(X,X).

%clean_out_atom(X,Y):-atomSplit(X,C),delete(C,'',O),concat_atom(C,' ',Y).
clean_out_atom(X,Y):-atom_codes(X,C),clean_codes(C,D),!,atom_codes(X,D),!,Y=X.

aiml_classify([],[]).
aiml_classify(Find,[atom]):-atomic(Find).
aiml_classify([H|INNER_XML],Out):-
      classifySingle(H,Class),
      aiml_classify(INNER_XML,More),
      sort([Class|More],OutM),!,
      classify2(OutM,Out).
aiml_classify(_T,[unk]).

classify2([in,out|Resp],[out|Resp]).
classify2(Out,Out).

classifySingle('_',var('_')).
classifySingle(*,var('*')).
classifySingle(Atom,in):-atom(Atom),all_upper_atom(Atom).
classifySingle(Atom,out):-atom(Atom).
classifySingle(Atom,spec(F)):-compound(Atom),functor(Atom,F,_).
classifySingle(_Atom,unknown).

                                        
      
varize(Find,Replace,FindO,ReplaceO):-
      subst((Find,Replace),'_','$VAR'(0),(FindM,ReplaceM)),
      subst((FindM,ReplaceM),'*','$VAR'(0),(FindO,ReplaceO)),!.


aiml_error(E):-debugFmt('~q~n',[error(E)]),trace,!.


% ===============================================================================================
%  Save Categories
% ===============================================================================================

assertCate(Cate):-
     convert_template(Cate,Assert),!,
      prolog_must(ground(Assert)),
      assertz(aimlCate(Assert)).

% ===============================================================================================
%  Load Categories
% ===============================================================================================

load_category(O):- clearCateStack,load_category0(O),!.

load_category0([A|L]):-!,
   pushCateStack([A|L]),
   makeCategories,!.

load_category0(A):-!,
   pushCateElement(A),
   makeCategories,!.

makeCategories:-
   popCateElements(Cate),!,
   assertCate(Cate),!,
   pp_listing(attribDatabase(category,_,_)),!.

pushCateStack([A|L]):-pushCateElement(A),pushCateStack(L).
pushCateStack([]).

%ignore((member(element(justbeforethat,[],[]),L),(IA= [index='1,2'],IB=[]))),
%pushCateElement([]):-!.
%pushCateElement(E):-ignore_aiml(E),!.

pushCateElement(element(Tag,[],PATTERN)):- cateMember(Tag),!,
  convert_template(PATTERN,DCG),
  pushAttributes(category,[Tag=DCG]),!.


pushCateElement(element(Tag,ALIST,PATTERN)):- cateMember(Tag),
      replaceAttribute(name,Tag,ALIST,ATTRIBS),!,
      convert_template(PATTERN,DCG),
      pushAttributes(category,ATTRIBS),
      pushAttributes(category,[Tag=DCG]),!.

pushCateElement(element(Tag,ATTRIBS,INNER_XML)):-member(Tag,[outerctx,category]),
   convert_attributes(ATTRIBS,CONV),
   pushAttributes(category,CONV),
   pushCateStack(INNER_XML). %%popAttributes(category,CONV).

load_aiml_structure(element(Tag,ALIST,INNER_XML)):- member(Tag,[topic]),
   replaceAttribute(name,Tag,ALIST,ATTRIBS),
      debugOnFailureAiml(withAttributes(filelevel,ATTRIBS,pushCateStack(INNER_XML))).



pushCateElement(element(Tag,ALIST,INNER_XML)):- cateMember(Tag),
   replaceAttribute(name,Tag,ALIST,ATTRIBS),
   convert_attributes(ATTRIBS,CONV),
   pushAttributes(category,CONV),
   pushCateStack(INNER_XML).   


pushCateElement(E):-aiml_error(pushCateElement(E)).


% ===============================================================================================
%  Popping when Building categories
% ===============================================================================================

clearCateStack:-dyn_retractall(attribDatabase(category,_,_)).

popCateElements(CateO):- cateMemberTags(CATETAGS), debugOnFailureAiml((grabAttributes(CATETAGS,category,Cate),!,CateO=Cate)).

popCateElements(CateO):-fail,
    popCateElements1(Cate1),popCateElements2(Cate2),append(Cate1,Cate2,Cate),!,CateO=Cate.
popCateElements1(CateO):- findall(Tag=DCG,cateNodes1(category,Tag,DCG),Cate),!,CateO=Cate.
popCateElements2(CateO):- findall(Tag=DCG,cateNodes2(category,Tag,DCG),Cate),!,CateO=Cate.


cateNodes1(Scope,Tag,DCGO):-member(Tag,[pattern,template]),once(cateNodes1a(Scope,Tag,TEMPLATE)),once(convert_template(TEMPLATE,DCG)),!,DCG=DCGO.

cateNodes1a(Scope,Tag,DCGO):-peekNameValue(Scope,Tag,DCG),popNameValue(Scope,Tag,DCG),!,DCG=DCGO.
cateNodes1a(Scope,Tag,DCGO):-listing(attribDatabase),aiml_error(peekNameValue(Scope,Tag,DCG)),!,DCG=DCGO.
cateNodes1a(Scope,Tag,DCGO):-peekNameValue(Other,Tag,DCG),Other\==Scope,!,DCG=DCGO.


cateNodes2(Scope,Tag,DCGO):-member(Tag,[that,guard,topic]),once(cateNodes2a(Scope,Tag,TEMPLATE)),once(convert_template(TEMPLATE,DCG)),!,DCG=DCGO.

cateNodes2a(Scope,Tag,DCGO):-peekNameValue(Other,Tag,DCG),Other\==Scope,!,DCG=DCGO.
cateNodes2a(Scope,Tag,DCGO):-aiml_error(peekNameValue(Scope,Tag,DCG)),!,DCG=DCGO.


% ===============================================================================================
%  PATTERN/TEMPLATE normalization
% ===============================================================================================
convert_template(X,Y):-nonvar(Y),throw(nonvar(Y)).
convert_template([],[])-!.
convert_template([I|P],L):- ignore_aiml(I),!,convert_template(P,L),!.
convert_template([I|P],[O|L]):- convert_element(I,O),!,convert_template(P,L),!.
convert_template(P,PO):-convert_element(P,PO).


convert_element(Input,Out):-atomic(Input),!,Out=Input.
convert_element(Input,Out):-convert_ele(Input,M),!,convert_ele(M,OutO),!,OutO=Out.


      
nameOrValue(ALIST, _VALUE, NORV, 0):-member(name=NORV,ALIST),!.
nameOrValue(ALIST, _VALUE, NORV, 0):-member(var=NORV,ALIST),!.
nameOrValue(_XATS, VALUE, NORV, 1):- NORV = VALUE.

convert_ele(X,Y):-nonvar(Y),throw(nonvar(Y)).
convert_ele(In,_In):-not(ground(In)),aiml_error(not(ground(In))),!,fail.

convert_ele(li(A),li(AA)):-convert_template(A,AA).

% bot/get/set
convert_ele(element(bot, ALIST, VALUE),get(bot,NAME)):-nameOrValue(ALIST,VALUE,NORV,_),convert_template(NORV,NAME).
convert_ele(element(get, ALIST, VALUE),get(user,NAME)):-nameOrValue(ALIST,VALUE,NORV,_),convert_template(NORV,NAME).
convert_ele(element(set, ALIST, VALUE),set(user,NAME,VALUEO)):-nameOrValue(ALIST,VALUE,NORV,0),
      convert_template(NORV,NAME),
      convert_template(VALUE,VALUEO).

% get_xxx/set_xxx
convert_ele(element(BOT_ATOM, ALIST, V),element(get,[name=N|ALIST],VV)):-atom_concat('get_',N,BOT_ATOM),convert_template(V,VV).
convert_ele(element(BOT_ATOM, ALIST, V),element(set,[name=N|ALIST],VV)):-atom_concat('set_',N,BOT_ATOM),convert_template(V,VV).

% bot_xxx/botxxx
convert_ele(element(BOT_ATOM, ALIST, V),element(bot,[name=N|ALIST],VV)):-atom_concat('bot_',N,BOT_ATOM),convert_template(V,VV).
convert_ele(element(BOT_ATOM, ALIST, V),element(bot,[name=N|ALIST],VV)):-atom_concat('bot',N,BOT_ATOM),lengthAtLeast(N,2),convert_template(V,VV).

% getXXX
convert_ele(element(BOT_ATOM, ALIST, V),element(get,[name=N|ALIST],VV)):-atom_concat('get',N,BOT_ATOM),lengthAtLeast(N,2),convert_template(V,VV).

lengthAtLeast(N,GE):-atom(N),atom_length(N,L),L>=GE.


%DELAY convert_ele(element(srai, [], B),srai(BB)):-convert_template(B,BB).
convert_ele(element(random, [], B),random(BB)):-convert_template(B,BB).
convert_ele(element(li, [], B),li(BB)):-convert_template(B,BB).
%DELAY convert_ele(element(star, [], []),(*)).
convert_ele(element(a, [Target, Link], Name),A):-sformat(S,'<a ~q ~q>~w</a>',[Target, Link, Name]),string_to_atom(S,A).
convert_ele(element(a, [Link], Name),A):-sformat(S,'<a ~q>~w</a>',[Link, Name]),string_to_atom(S,A).

convert_ele(element(get, [name=Var], []),get(Var)):-!.
convert_ele(element(learn, filename=F),load_any_file(F)):-!.
convert_ele(element(sr,ALIST,MORE),element(srai,ALIST,[element(star,ALIST,MORE)])):-!.
convert_ele(element(star,ALIST,MORE),star(pattern,XLAT2,MORE2)):-!,starIndex(star,pattern,ALIST,MORE,XLAT2,MORE2).
  starIndex(_Tag,_Star,ALIST,MORE,XLAT2,MORE2):-convert_attributes(ALIST,XLAT2),convert_template(MORE,MORE2),!.

convert_ele(element(Tag,ALIST,MORE),star(Star,XLAT2,MORE2)):-starType(Tag,Star),!,starIndex(Tag,Star,ALIST,MORE,XLAT2,MORE2).
   starType(Tag,Star):-member(Tag=Star,[star=pattern,topicstar=topic,gruardstar=guard,inputstar=pattern,thatstar=that]),!.
   starType(Tag,Star):-atom_concat(Star,'_star',Tag),!.
   starType(Tag,Star):-atom_concat(Star,'star',Tag),!.

convert_ele(element(Tag, ALIST , INNER_XML), RESULT):-
      transform_aiml_structure(Tag,NewTag,ALIST,NewProps,INNER_XML,NEWPATTERN),
      convert_ele(element(NewTag, NewProps, NEWPATTERN),RESULT),!.

convert_ele(L,LO):-is_list(L),flatten(L,M),!,
	    (L==M -> LO=M ; convert_template(M,LO)).

%convert_ele(A,B):-atom(A),atom_to_number(A,B).

convert_ele(A,W):-atom(A),atomSplit(A,B),!,convert_text(B,W).

convert_ele(element(A, B, C),INNER_XML):-tagType(A, immediate),!,
      convert_name(A,AA),
      convert_attributes(B,BB),
      convert_template(C,CC),!,
   (element(A, B, C) == element(AA, BB, CC) ->  INNER_XML=..[element,AA,BB,CC] ; convert_element(element(AA, BB, CC),INNER_XML)),!.

convert_ele(element(A, B, C),INNER_XML):-
      convert_name(A,AA),
      convert_attributes(B,BB),
      convert_template(C,CC),!, 
   (element(A, B, C) == element(AA, BB, CC) ->  INNER_XML=..[element,AA,BB,CC] ; convert_element(element(AA, BB, CC),INNER_XML)),!.

convert_ele(O,O).


convert_attributes([B|A],[BB|AA]):-convert_attribute(B,BB),convert_attributes(A,AA).
convert_attributes([],[]).

convert_attribute(A=B,AA=BB):-convert_name(A,AA),convert_template(B,BB).

convert_name(A,AAA):-convert_name0(A,AA), (A==AA -> AAA=AA ; convert_name(AA,AAA)).

convert_name0(A,AA):-toLowercase(A,AA).
convert_name0(var,name).
convert_name0(file,uri).
convert_name0(path,uri).
convert_name0(dir,uri).
convert_name0(filename,uri).

defaultPredicates(N,V):-member(N,[username,botname]),V='*'.



tagType(Tag,pushable):-cateFallback(LIST),member([Tag=_],LIST).
tagType(Tag,insideCate):-cateMember(Tag).

tagType(Tag,requiredCate):-member(Tag,[pattern,template]).
tagType(Tag,optionalCate):-cateMember(Tag),not(tagType(Tag,requiredCate)).


cateMember(Tag):-cateMemberTags(List),member(Tag,List).
cateMemberTags([graph,topic,that,pattern,flags,call,guard,template,userdict]).

cateFallback([topic='*',call='true',flags='*',that='*',
       dictionary='userdict',
       userdict='user',graph='default',pattern='ERROR PATTERN',guard='*',template='ERROR TEMPLATE'|MORE]):-findall(N=V,defaultPredicates(N,V),MORE).


transform_aiml_structure(catagory,category,OldProps,OldProps,NEWPATTERN,NEWPATTERN).
transform_aiml_structure(alice,aiml,OldProps,OldProps,NEWPATTERN,NEWPATTERN).
transform_aiml_structure('name','bot',OldProps,[name=['name']|OldProps],NEWPATTERN,NEWPATTERN).
transform_aiml_structure(OldName,NewName,OldProps,NewProps,NEWPATTERN,NEWPATTERN):-
      specialIndex(OldName,NewName,AddProps),append(AddProps,OldProps,NewProps).

specialIndex(justbeforethat,that,[index=(2:1)]).
specialIndex(justthat ,input,[index=2]).
specialIndex(beforethat,input,[index=3]).

specialIndex(load,learn,[]).
specialIndex(set_female,set,[name=gender,value=female]).

specialIndex(getname,name,[name=[name]]).
specialIndex(gettopic,name,[name=[name]]).

specialIndex(personf,formatter,[type=url_encode]).
specialIndex(Name,formatter,[type=Method]):-formatterMethod(Name,Method).




formatterMethod(NamedMethod,NamedMethod):-substitutionDicts(NamedMethod).

evaluatorsDicts(Dict):-member(Dict,[system,javascript,eval,cycquery,cycsystem,cycassert]).

substitutionDicts(Dict):-member(Dict,[formal,uppercase,lowercase,sentence,gossip,think,(format)]).


%substitutionDictsName(input,pattern).
substitutionDictsName(N,N):-substitutionDicts(N).

substitutionDicts(input).
substitutionDicts(output).
substitutionDicts(gender).
substitutionDicts(person).
substitutionDicts(person2).
substitutionDicts(person3).
%substitutionDicts(Dict):-evaluatorsDicts(Dict).

%%:-abolish(attribDatabase/3).
:-dyn_retractall(attribDatabase(_,_,_)).
:-cateFallback(ATTRIBS),popAttributes(filelevel,ATTRIBS).
:-cateFallback(ATTRIBS),pushAttributes(filelevel,ATTRIBS).
:-pp_listing(attribDatabase(_,_,_)).


:-dyn_retractall(aimlCate(_)).


saveaimlCate:-tell(aimlCate),listing(aimlCate),told.

do:-load_aiml_files,saveaimlCate,alicebot.




