
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

:-dynamic(dict/3).
:-dynamic(dict/3).

%%%retractall(E):- retractall(E),functor(E,F,A),dynamic(F/A),!.

pp_listing(_Pred):-!. %%functor(Pred,F,A),functor(FA,F,A),listing(F),nl,findall(NV,predicate_property(FA,NV),LIST),writeq(LIST),nl,!.


aimlDebugFmt(X):-debugFmt(X),!.

debugOnFailureAiml((A,B)):- !,debugOnFailureAiml(A),!,debugOnFailureAiml(B),!.
debugOnFailureAiml(Call):- once( catch(Call,E,(debugFmt(E),fail))),!.
debugOnFailureAiml(Call):- trace,Call.


% =================================================================================
% AIML Loading
% =================================================================================
%%load_aiml_files:-retractall(aimlCate(_)),fail.
load_aiml_files:-load_aiml_files(Ctx,'aiml/test_suite/*.aiml'),fail.
load_aiml_files:-load_aiml_files(Ctx,'*.aiml'),fail.
%load_aiml_files:-pp_listing(aimlCate).
load_aiml_files.

%%tell(f5),load_aiml_files('part5/*.aiml'),told.


load_aiml_files(File):-currentContext(Ctx),load_aiml_files(Ctx,Files).
load_aiml_files(Ctx,[]):-!.
load_aiml_files(Ctx,[H|INNER_XML]):-
   load_aiml_files(Ctx,H),!,
   load_aiml_files(Ctx,INNER_XML).
load_aiml_files(Ctx,F):-atom(F),
   expand_file_name(F,[F]),!,
   load_aiml_file(Ctx,F).
load_aiml_files(Ctx,F):-atom(F),expand_file_name(F,FILES),!,load_aiml_files(Ctx,FILES),!.

aiml_files(F,Files):-atom_concat(F,'/',WithSlashes),absolute_file_name(WithSlashes,[relative_to('./')],WithOneSlash),
                    atom_concat(WithOneSlash,'/*.aiml',Mask),expand_file_name(Mask,Files),!.

load_aiml_file(F):-currentContext(Ctx),load_aiml_file(Ctx,F).
load_aiml_file(Ctx,F):- exists_directory(F), aiml_files(F,Files),load_aiml_files(Ctx,Files),!.
load_aiml_file(Ctx,F):- exists_file(F), create_aiml_file(F),!.
load_aiml_file(Ctx,F):- file_name_extension(F,'aiml',Aiml), exists_file(Aiml),create_aiml_file(Aiml),!.



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
   ATTRIBS = [filename=F],!,
   pushAttributes(Ctx,filelevel,ATTRIBS),
   load_aiml_structure_list(Ctx,X),
   popAttributes(Ctx,filelevel,ATTRIBS),
   ifThen(Dofile,(told,[PLNAME])))),!.


load_aiml_structure_list(Ctx,L):-load_mapcar(Ctx,load_aiml_structure,L).


load_mapcar(Ctx,Pred,[X|L]):-!,call(Pred,Ctx,X) ,!, load_mapcar(Ctx,Pred,L),!.
load_mapcar(Ctx,_Pred,[]):-!.
load_mapcar(Ctx,Pred,X):-debugOnFailureAiml(call(Pred,Ctx,X)),!.


withAttributes(_Ctx,_filelevel,[],Call):-!,debugOnFailureAiml(Call).
withAttributes(Ctx,filelevel,ATTRIBS,Call):-
    debugOnFailureAiml((
     pushAttributes(Ctx,filelevel,ATTRIBS),
     Call,
     popAttributes(Ctx,filelevel,ATTRIBS))),!.

% ============================================
% Loading content
% ============================================
% <aiml>
load_aiml_structure(Ctx,[A|B]):-!,load_aiml_structure_list(Ctx,[A|B]).

%catagory
load_aiml_structure(Ctx,element(catagory,ALIST,LIST)):-load_aiml_structure(Ctx,element(category,ALIST,LIST)),!.



load_aiml_structure(Ctx,element(aiml,ALIST,LIST)):-
     replaceAttribute(Ctx,name,graph,ALIST,ATTRIBS),
     withAttributes(Ctx,filelevel,ATTRIBS,load_aiml_structure_list(Ctx,LIST)).

% \n\n\n
load_aiml_structure(Ctx,O):-atomic(O),!,aimlDebugFmt(load_aiml_structure(Ctx,O)),!.

% that/topic
load_aiml_structure(Ctx,element(Tag,ALIST,INNER_XML)):- member(Tag,[topic,flags,that]),
     replaceAttribute(Ctx,name,Tag,ALIST,ATTRIBS),
         withAttributes(Ctx,filelevel,ATTRIBS, load_category(Ctx,element(Tag,ALIST,INNER_XML))),!.

%category
load_aiml_structure(Ctx,element(category,ALIST,INNER_XML)):- !,
     withAttributes(Ctx,filelevel,ALIST, load_category(Ctx,element(Tag,ALIST,INNER_XML))).


% error of pattern
load_aiml_structure(Ctx,element(Tag,ALIST,INNER_XML)):- cateMember(Tag), aiml_error(element(Tag,ALIST,INNER_XML)),
     replaceAttribute(Ctx,name,Tag,ALIST,ATTRIBS),
         withAttributes(Ctx,filelevel,ATTRIBS, load_category(Ctx,element(Tag,ALIST,INNER_XML))),!.


% substitute,learn,aiml,genlMt,srai,think,system,javascript,eval
load_aiml_structure(Ctx,element(Tag,ALIST,LIST)):- tagType(Tag,immediate),!,
     replaceAttribute(Ctx,name,Tag,ALIST,ATTRIBS),
     pushAttributes(Ctx,filelevel,ATTRIBS),      
     debugFmt(call_immediate(Tag,ALIST,LIST)).



% ============================================
% Test Suite 
% ============================================

load_aiml_structure(Ctx,element('TestSuite',ATTRIBS,LIST)):-
   debugOnFailureAiml((
     withAttributes(Ctx,filelevel,ATTRIBS,
        load_aiml_structure_list(Ctx,LIST)))).
   
load_aiml_structure(Ctx,element(TC,ATTRIBS,LIST)):-member(TC,['testcase','TestCase']),
     withAttributes(Ctx,filelevel,ATTRIBS,((
     getAttributeOrTags(Ctx,['name'='SomeName'],ATTRIBS,LIST,NormalProps1),
     getAttributeOrTags(Ctx,['Description'='ERROR descr'],ATTRIBS,LIST,NormalProps2),
     getAttributeOrTags(Ctx,['Input'='ERROR Input'],ATTRIBS,LIST,NormalProps3),
     getAttributeOrTags(Ctx,['ExpectedAnswer'='ERROR ExpectedAnswer'],ATTRIBS,LIST,NormalProps4),
     %%,'Description'='some descr','Input'='UKN','ExpectedAnswer'='SomeAnswwer'
     debugFmt(testIt([NormalProps1,NormalProps2,NormalProps3,NormalProps4]))))),!.


% ============================================
% special dictionaries
% ============================================

% user/bot dictionaries
load_aiml_structure(Ctx,element(Tag,ALIST,LIST)):- member(Tag,[predicates,vars,properties]),
   debugOnFailureAiml((
     replaceAttribute(Ctx,name,dictionary,ALIST,ATTRIBS),
     withAttributes(Ctx,filelevel,ATTRIBS,
      load_aiml_structure_list(Ctx,LIST)))).

% user/bot predicatates
load_aiml_structure(Ctx,element(Tag,ALIST,LIST)):-member(Tag,[predicate]),
   debugOnFailureAiml((
     attributeValue(Ctx,ALIST,[name,var],Name,error),
     attributeValue(Ctx,ALIST,[default],Default,''),
     attributeValue(Ctx,ALIST,[value,default],Value,LIST),
     attributeValue(Ctx,ALIST,['set-return'],SetReturn,value),
   peekNameValue(Ctx,filelevel,dictionary,Dict),
   load_aiml_structure(Ctx,dict(Dict,Name,Value)),
     load_aiml_structure(Ctx,dict(defaultValue(Dict),Name,Default)),
     load_aiml_structure(Ctx,dict(setReturn(Dict),Name,SetReturn)))),!.

% user/bot dictionaries name/values
load_aiml_structure(Ctx,element(Tag,ALIST,LIST)):-member(Tag,[property,var,item]),
   debugOnFailureAiml((
     attributeValue(Ctx,ALIST,[name,var],Name,error),
     attributeValue(Ctx,ALIST,[value,default],Value,LIST),
     peekNameValue(Ctx,filelevel,dictionary,Dict),
     load_aiml_structure(Ctx,dict(Dict,Name,Value)))),!.



% ============================================
% special substitution dictionaries
% ============================================
% substitutions
load_aiml_structure(Ctx,element(substitutions,ALIST,LIST)):-
   debugOnFailureAiml((
      replaceAttribute(Ctx,name,graph,[dictionary=substitutions(input)|ALIST],ATTRIBS),
     withAttributes(Ctx,filelevel,ATTRIBS,
     load_mapcar(Ctx,load_substs,LIST)))).

load_substs(Ctx,element(Tag,ALIST,LIST)):- substitutionDictsName(Tag,Dict),
   debugOnFailureAiml((
      replaceAttribute(Ctx,name,graph,[dictionary=substitutions(Dict)|ALIST],ATTRIBS),
     withAttributes(Ctx,filelevel,ATTRIBS,
     load_mapcar(Ctx,load_substs,LIST)))).


load_substs(Ctx,element(substitute,ATTRIBS,LIST)):-
   debugOnFailureAiml((
      peekNameValue(Ctx,filelevel,dictionary,substitutions(Catalog)),
      attributeOrTagValue(Ctx,ATTRIBS,[find,name,before],Find,error,LIST),
      attributeOrTagValue(Ctx,ATTRIBS,[replace,value,after],Replace,error,LIST),
      debugOnFailureAiml(load_aiml_structure(Ctx,dict(substitutions(Catalog),Find,Replace))))),!.

% substitutions
load_aiml_structure(Ctx,element(substitute,ATTRIBS,LIST)):- load_substs(Ctx,element(substitute,ATTRIBS,LIST)),!.
load_aiml_structure(Ctx,element(substitution,ATTRIBS,LIST)):- load_substs(Ctx,element(substitute,ATTRIBS,LIST)),!.


load_aiml_structure(Ctx,substitute(Dict,Find,Replace)):-
  debugOnFailureAiml((
      convert_text(Find,F),!,convert_text(Replace,Resp),!,
      load_aiml_structure(Ctx,dict(substitutions(Dict),F,Resp)))),!.

% actual assertions
load_aiml_structure(Ctx,dict(Dict,Name,Value)):-
      assertz(dict(Dict,Name,Value)),!.

load_aiml_structure(Ctx,element(A, B, C),INNER_XML):-tagType(A, immediate),!,
      convert_name(A,AA),
      convert_attributes(Ctx,B,BB),
      convert_template(Ctx,C,CC),!,
   (element(A, B, C) \== element(AA, BB, CC) ->  load_aiml_structure(Ctx,element(AA, BB, CC),INNER_XML)),!.  

% ============================================
% Rewrite or Error loading
% ============================================

load_aiml_structure(Ctx,element(Tag,ALIST,PATTERN)):-
     convert_element(Ctx,element(Tag,ALIST,PATTERN),NEW),
     load_aiml_structure_diff(Ctx,element(Tag,ALIST,PATTERN),NEW),!.


load_aiml_structure_diff(Ctx,BEFORE,AFTER):- BEFORE\==AFTER,!, load_aiml_structure(Ctx,AFTER).

%%load_aiml_structure_diff(Ctx,BEFORE,AFTER):- aiml_error(load_aiml_structure(Ctx,BEFORE)),!.

load_aiml_structure(Ctx,X):- aiml_error(load_aiml_structure(Ctx,X)).
%      saveFAttribute(Ctx,F,A),
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

:-dynamic(saveDFAttribute/3).
:-dynamic(replace_t/5).
:-dynamic(response_t/5).
                
saveFAttribute(Ctx,F,A):-saveDFAttribute(Ctx,F,A),!.
saveFAttribute(Ctx,F,A):-asserta(saveDFAttribute(Ctx,F,A)),dynamic(F/A).

tagType(Tag,immediate):-member(Tag,[substitute,learn,aiml,genlMt,srai,think,system,javascript,eval]).

% ===============================================================================================
%    AIML Runtime Database
% ===============================================================================================
prolog_must(Call):-Call,!.
prolog_must(Call):-!,aiml_error(Call).

pushAttributes(Ctx,_Scope,[]).
pushAttributes(Ctx,Scope,[N=V|L]):-not(ground((N,V))).
pushAttributes(Ctx,Scope,[N=V|L]):- pushNameValue(Ctx,Scope,N,V),!,pushAttributes(Ctx,Scope,L),!.



popAttributes(Ctx,Scope,[N=V|L]):- popNameValue(Ctx,Scope,N,V),!,popAttributes(Ctx,Scope,L),!.
popAttributes(Ctx,_Scope,[]).



pushNameValue(Ctx,Scope,N,V):-prolog_must(ground(dict(a,Scope,N,V))),asserta(dict(Scope,N,V)),!.

popNameValue(Ctx,Scope,N,V):-
   dict(Scope,N,V),prolog_must(ground((N,V))),ignore(retract(dict(Scope,N,V))),!,
   prolog_must(ground(popNameValue1(Scope,N,V))),!.

dyn_retract(dict(Scope,N,V)):-(retract(dict(Scope,N,V))),!.

peekNameValue(Ctx,Scope,N,V):-dict(Scope,N,V),!.

replaceAttribute(Ctx,_Before,_After,[],[]).
% only do the first found?
replaceAttribute(Ctx,Before,After,[Before=Value|ATTRIBS],[After=Value|ATTRIBS]):-!.
% comment out the line above to do all
replaceAttribute(Ctx,Before,After,[Before=Value|ALIST],[After=Value|ATTRIBS]):-
   replaceAttribute(Ctx,Before,After,ALIST,ATTRIBS).
% skip over BeforeValue
replaceAttribute(Ctx,Before,After,[BeforeValue|ALIST],[BeforeValue|ATTRIBS]):-
   replaceAttribute(Ctx,Before,After,ALIST,ATTRIBS).
% the endcase
replaceAttribute(Ctx,_Before,_After,B,B).




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


aiml_error(E):-  trace,debugFmt('~q~n',[error(E)]),!.


% ===============================================================================================
%  Save Categories
% ===============================================================================================
%%aimlCate(graph,topic,that,pattern,flags,call,guard,template,userdict).

assertCate(Ctx,Cate):- convert_template(Ctx,Cate,Assert),!,assertCate2(Ctx,Assert).
assertCate2(Ctx,[_=Graph,_=Topic,_=That,_=Pattern,_=Flags,_=Call,_=Guard,_=Template,_=Userdict]):-!,
      NEW = aimlCate(Graph,Topic,That,Pattern,Flags,Call,Guard,Template,Userdict),
      once((retract(NEW),asserta(NEW)) ; (asserta(NEW),(format('~q.~n',NEW)))),!.
      
/*

assertCate(Ctx,Cate):-
     convert_template(Ctx,Cate,Assert),!,
     %% prolog_must(ground(Assert)),
     %% retractall(aimlCate(Assert)),
      %%asserta([aimlCate(Assert)])
      (format('~q.~n',aimlCate(Assert))).
*/
% ===============================================================================================
%  Load Categories
% ===============================================================================================

load_category(Ctx,O):- 
      clearCateStack(Ctx),
      load_category0(Ctx,O),!.

load_category0(Ctx,A):-
   pushCateElement(Ctx,A),
   makeCategories(Ctx),!.


makeCategories(Ctx):-
   peekCateElements(Ctx,Cate),!,
   assertCate(Ctx,Cate),!.


% flag/topic
pushCateElement(Ctx,element(Tag,ALIST,INNER_XML)):- member(Tag,[topic,flag]),
  replaceAttribute(Ctx,name,Tag,ALIST,ATTRIBS),
  withAttributes(Ctx,filelevel,ATTRIBS,
     load_mapcar(Ctx,load_category0,INNER_XML)).

%catagory
pushCateElement(Ctx,element(catagory, A, B)):-pushCateElement(Ctx,element(category, A, B)).

% remove <that>s from <pattern>s
pushCateElement(Ctx,element(pattern,ATTRIBS,INNER_XML)):-
    findall(element(that,ALIST,LIST),member(element(that,ALIST,LIST),INNER_XML),ListT),
    takeout(element(that,_,_),INNER_XML,NOTHATS),
    pushCateElement(Ctx,element(pattern,ATTRIBS,NOTHATS)),!,
    load_mapcar(Ctx,each_that,ListT),
    makeCategories(Ctx).

% remove <template>s from <category>s
pushCateElement(Ctx,element(Tag,ATTRIBS,INNER_XML)):-member(Tag,[outerctx,category]),
   convert_attributes(Ctx,ATTRIBS,CONV),
   pushAttributes(Ctx,category,CONV),
   findall(element(template,ALIST,LIST),member(element(template,ALIST,LIST),INNER_XML),ListT),
   takeout(element(template,_,_),INNER_XML,NOTEMPLATES),
   load_mapcar(Ctx,pushCateElement,NOTEMPLATES).
  % < each subtemplate is incharge of runing the finalizer >
   load_mapcar(Ctx,each_template,ListT).


% handle <guard><call>
pushCateElement(Ctx,element(Tag,ALIST,PATTERN)):- cateMember(Tag),
      replaceAttribute(Ctx,name,Tag,ALIST,ATTRIBS),!,
      convert_template(Ctx,PATTERN,DCG),
      pushAttributes(Ctx,category,ATTRIBS),
      pushAttributes(Ctx,category,[Tag=DCG]),!.

% remove <that>s from <templates>s 
each_template(Ctx,element(template,Attribs,INNER_XML)):-
    findall(element(that,ALIST,LIST),member(element(that,ALIST,LIST),INNER_XML),ListT),
    takeout(element(that,_,_),INNER_XML,NOTHATS),
    % then push the template and call the <that> finalizer
    pushCateElement(Ctx,element(template,Attribs,NOTHATS)),
    pushAttributes(Ctx,category,[Tag=NOTHATS]),
    load_mapcar(Ctx,each_that,ListT),
    makeCategories(Ctx).

% the <that> finalizer
each_that(Ctx,element(that,ALIST,INNER_XML)):-
      pushCateElement(Ctx,element(that,ALIST,INNER_XML)),!,
      makeCategories(Ctx).

takeout(X,[X|R],R).
takeout(X,[F|R],[F|S]) :- takeout(X,R,S).

pushCateElement(Ctx,E):-aiml_error(pushCateElement(Ctx,E)).
pushCateElement(Ctx,element(Tag,[],PATTERN)):- cateMember(Tag),!,
  convert_template(Ctx,PATTERN,DCG),
  pushAttributes(Ctx,category,[Tag=DCG]),!.


% ===============================================================================================
%  Popping when Building categories
% ===============================================================================================

clearCateStack(Ctx):-retractall(dict(category,_,_)).

popCateElements(Ctx,Cate):- cateMemberTags(CATETAGS), peekAttributes(Ctx,CATETAGS,category,Cate),!.


peekCateElements(Ctx,Cate):- cateMemberTags(CATETAGS), peekAttributes(Ctx,CATETAGS,category,Cate),!.

peekAttributes(Ctx,[Name|SList],Scope,[Name=Value|Results]):- grabNameValue(Ctx,Scope,Name,Value),peekAttributes(Ctx,SList,Scope,Results),!.
peekAttributes(_Ctx,[],_Scope,[]):-!.

grabNameValue(Ctx,Scope,Name,Value):-dict(Scope,Name,Value),ground(Name=Value),!.
grabNameValue(Ctx,Scope,Name,Value):-dict(Scope2,Name,Value),Scope\=Scope2,ground(Name=Value),!,checkValue(Value).
grabNameValue(Ctx,Scope,Name,Value):-Value=(*),!.

checkValue(V):-term_to_atom(V,A),!. %%concat_atom([_,_|_],'ERROR',A) -> trace ; true. 


popCateElements(Ctx,CateO):- popCateElements1(Ctx,Cate1),popCateElements2(Ctx,Cate2),append(Cate1,Cate2,Cate),!,CateO=Cate.
popCateElements1(Ctx,CateO):- findall(Tag=DCG,cateNodes1(Ctx,category,Tag,DCG),Cate),!,CateO=Cate.
popCateElements2(Ctx,CateO):- findall(Tag=DCG,cateNodes2(Ctx,category,Tag,DCG),Cate),!,CateO=Cate.


cateNodes1(Ctx,Scope,Tag,DCGO):-member(Tag,[pattern,template]),once(cateNodes1a(Ctx,Scope,Tag,TEMPLATE)),once(convert_template(Ctx,TEMPLATE,DCG)),!,DCG=DCGO.

cateNodes1a(Ctx,Scope,Tag,DCGO):-peekNameValue(Ctx,Scope,Tag,DCG),popNameValue(Ctx,Scope,Tag,DCG),!,DCG=DCGO.
cateNodes1a(Ctx,Scope,Tag,DCGO):-listing(dict),aiml_error(peekNameValue(Ctx,Scope,Tag,DCG)),!,DCG=DCGO.
cateNodes1a(Ctx,Scope,Tag,DCGO):-peekNameValue(Ctx,Other,Tag,DCG),Other\==Scope,!,DCG=DCGO.


cateNodes2(Scope,Tag,DCGO):-member(Tag,[that,guard,topic]),once(cateNodes2a(Scope,Tag,TEMPLATE)),once(convert_template(Ctx,TEMPLATE,DCG)),!,DCG=DCGO.

cateNodes2a(Scope,Tag,DCGO):-peekNameValue(Ctx,Other,Tag,DCG),Other\==Scope,!,DCG=DCGO.
cateNodes2a(Scope,Tag,DCGO):-aiml_error(peekNameValue(Ctx,Scope,Tag,DCG)),!,DCG=DCGO.


% ===============================================================================================
%  PATTERN/TEMPLATE normalization
% ===============================================================================================
convert_template(Ctx,X,_Y):-var(X),throw(var(X)).
convert_template(Ctx,_X,Y):-nonvar(Y),throw(nonvar(Y)).
convert_template(Ctx,[],[]):-!.
convert_template(Ctx,[ATOM],O):-atom(ATOM),!,atomSplit(ATOM,LIST),!,toAtomList(LIST,O),!.
convert_template(Ctx,[I|P],GOOD):- atom(I),atomSplit(I,LIST),!,toAtomList(LIST,O),!,convert_element(Ctx,P,L),!,flatten([O,L],GOOD),!.
convert_template(Ctx,[I|P],L):- ignore_aiml(I),!,convert_template(Ctx,P,L),!.
convert_template(Ctx,[I|P],[O|L]):- convert_element(Ctx,I,O),!,convert_template(Ctx,P,L),!.
convert_template(Ctx,P,PO):-convert_element(Ctx,P,PO).


toAtomList(A,O):-delete(A,'',O),!.

convert_element(Ctx,Input,Out):-atomic(Input),!,Out=Input.

convert_element(Ctx,Input,Out):-convert_ele(Ctx,Input,M),!,M=Out,!.
%%%,convert_ele(Ctx,M,OutO),!,OutO=Out.


      
nameOrValue(ALIST, _VALUE, NORV, 0):-member(name=NORV,ALIST),!.
nameOrValue(ALIST, _VALUE, NORV, 0):-member(var=NORV,ALIST),!.
nameOrValue(_XATS, VALUE, NORV, 1):- NORV = VALUE.

convert_ele(Ctx,_X,Y):-nonvar(Y),throw(nonvar(Y)).
convert_ele(Ctx,In,_In):-not(ground(In)),aiml_error(not(ground(In))),!,fail.

convert_ele(Ctx,li(A),li(AA)):-convert_template(Ctx,A,AA).
convert_ele(Ctx,element(NSLocal,A,B),Out):- concat_atom([NS,Local],':',NSLocal),!,convert_ele(Ctx,element(Local,A,B),Out),!.
convert_ele(Ctx,element(NS:Local,A,B),Out):- !,convert_ele(Ctx,element(Local,A,B),Out),!.
convert_ele(Ctx,element(html:TAG,A,B),Out):-!,convert_ele(Ctx,element(TAG,A,B),Out),!.
convert_ele(Ctx,element(br,[],[]),'<br/>').
convert_ele(Ctx,element(p,[],[]),'<p/>').
convert_ele(Ctx,element(pre,[],B),B):-!,convert_template(Ctx,B,BB).

convert_ele(Ctx,element(catagory, A, B),Out):-convert_ele(Ctx,element(category, A, B),Out).
%%convert_ele(Ctx,element(Tag, A, B),BB):- member(Tag,[category,srai]), convert_template(Ctx,element(Tag, A, B),BB).

% bot/get/set
convert_ele(Ctx,element(bot, ALIST, VALUE),get(bot,NAME)):-nameOrValue(ALIST,VALUE,NORV,_),convert_template(Ctx,NORV,NAME).
convert_ele(Ctx,element(get, ALIST, VALUE),get(user,NAME)):-nameOrValue(ALIST,VALUE,NORV,_),convert_template(Ctx,NORV,NAME).
convert_ele(Ctx,element(set, ALIST, VALUE),set(user,NAME,VALUEO)):-nameOrValue(ALIST,VALUE,NORV,0),convert_template(Ctx,NORV,NAME),
      convert_template(Ctx,VALUE,VALUEO),!.

% get_xxx/set_xxx
convert_ele(Ctx,element(BOT_ATOM, ALIST, V),element(get,[name=N|ALIST],VV)):-atom_concat('get_',N,BOT_ATOM),convert_template(Ctx,V,VV).
convert_ele(Ctx,element(BOT_ATOM, ALIST, V),element(set,[name=N|ALIST],VV)):-atom_concat('set_',N,BOT_ATOM),convert_template(Ctx,V,VV).

% bot_xxx/botxxx
convert_ele(Ctx,element(BOT_ATOM, ALIST, V),element(bot,[name=N|ALIST],VV)):-atom_concat('bot_',N,BOT_ATOM),convert_template(Ctx,V,VV).
convert_ele(Ctx,element(BOT_ATOM, ALIST, V),element(bot,[name=N|ALIST],VV)):-atom_concat('bot',N,BOT_ATOM),lengthAtLeast(N,2),convert_template(Ctx,V,VV),!.

% getXXX
convert_ele(Ctx,element(BOT_ATOM, ALIST, V),element(get,[name=N|ALIST],VV)):-atom_concat('get',N,BOT_ATOM),lengthAtLeast(N,2),convert_template(Ctx,V,VV),!.

lengthAtLeast(N,GE):-atom(N),atom_length(N,L),L>=GE.




convert_ele(Ctx,element(random, [], B),random(BB)):-convert_template(Ctx,B,BB).
convert_ele(Ctx,element(li, [], B),li(BB)):-convert_template(Ctx,B,BB).
%DELAY convert_ele(Ctx,element(star, [], []),(*)).
convert_ele(Ctx,element(a, [Target, Link], Name),A):-sformat(S,'<a ~q ~q>~w</a>',[Target, Link, Name]),string_to_atom(S,A).
convert_ele(Ctx,element(a, [Link], Name),A):-sformat(S,'<a ~q>~w</a>',[Link, Name]),string_to_atom(S,A).

convert_ele(Ctx,element(get, [name=Var], []),get(Var)):-!.
convert_ele(Ctx,element(learn, filename=F),load_any_file(F)):-!.
convert_ele(Ctx,element(sr,ALIST,MORE),element(srai,ALIST,[element(star,ALIST,MORE)])):-!.
convert_ele(Ctx,element(star,ALIST,MORE),star(pattern,XLAT2,MORE2)):-!,starIndex(star,pattern,ALIST,MORE,XLAT2,MORE2).
  starIndex(_Tag,_Star,ALIST,MORE,XLAT2,MORE2):-convert_attributes(Ctx,ALIST,XLAT2),convert_template(Ctx,MORE,MORE2),!.

convert_ele(Ctx,element(Tag,ALIST,MORE),star(Star,XLAT2,MORE2)):-starType(Tag,Star),!,starIndex(Tag,Star,ALIST,MORE,XLAT2,MORE2).
   starType(Tag,Star):-member(Tag=Star,[star=pattern,topicstar=topic,gruardstar=guard,inputstar=pattern,thatstar=that]),!.
   starType(Tag,Star):-atom_concat(Star,'_star',Tag),!.
   starType(Tag,Star):-atom_concat(Star,'star',Tag),!.

convert_ele(Ctx,element(Tag, ALIST , INNER_XML), RESULT):-
      transform_aiml_structure(Tag,NewTag,ALIST,NewProps,INNER_XML,NEWPATTERN),
      convert_ele(Ctx,element(NewTag, NewProps, NEWPATTERN),RESULT),!.

convert_ele(Ctx,L,LO):-is_list(L),flatten(L,M),!,
	    (L==M -> LO=M ; convert_template(Ctx,M,LO)).

%convert_ele(Ctx,A,B):-atom(A),atom_to_number(A,B).

convert_ele(Ctx,A,W):-atom(A),atomSplit(A,B),!,convert_text(B,W).

convert_ele(Ctx,element(A, B, C),INNER_XML):-tagType(A, immediate),!,
      convert_name(A,AA),
      convert_attributes(Ctx,B,BB),
      convert_template(Ctx,C,CC),!,
   (element(A, B, C) == element(AA, BB, CC) ->  INNER_XML=..[element,AA,BB,CC] ; convert_element(Ctx,element(AA, BB, CC),INNER_XML)),!.

convert_ele(Ctx,element(A, B, C),INNER_XML):-
      convert_name(A,AA),
      convert_attributes(Ctx,B,BB),
      convert_template(Ctx,C,CC),!, 
   (element(A, B, C) == element(AA, BB, CC) ->  INNER_XML=..[element,AA,BB,CC] ; convert_element(Ctx,element(AA, BB, CC),INNER_XML)),!.

convert_ele(Ctx,O,O).


convert_attributes(Ctx,[B|A],[BB|AA]):-convert_attribute(B,BB),convert_attributes(Ctx,A,AA).
convert_attributes(Ctx,[],[]).

convert_attribute(A=B,AA=BB):-convert_name(A,AA),convert_template(Ctx,B,BB).

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

cateFallback([
       topic='*',
       call='true',
       flags='*',
       that='*',
       dictionary='userdict',
       userdict='user',
       substitutions='input',
       graph='default',
       pattern='ERROR PATTERN',
       guard='*',
       template='ERROR TEMPLATE'|MORE]):-findall(N=V,defaultPredicates(N,V),MORE).


transform_aiml_structure(catagory,Ctx,category,OldProps,OldProps,NEWPATTERN,NEWPATTERN).
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


formatterProc(Dict):-member(Dict,[formal,uppercase,lowercase,sentence,gossip,think,(format)]).
formatterMethod(NamedMethod,NamedMethod):-formatterProc(NamedMethod).


evaluatorsDicts(Dict):-member(Dict,[system,javascript,eval,cycquery,cycsystem,cycassert]).


%substitutionDictsName(input,pattern).
substitutionDictsName(N,N):-substitutionDicts(N).

substitutionDicts(input).
substitutionDicts(output).
substitutionDicts(gender).
substitutionDicts(person).
substitutionDicts(person2).
substitutionDicts(person3).
%substitutionDicts(Dict):-evaluatorsDicts(Dict).

%%:-abolish(dict/3).

:-retractall(dict(_,_,_)).
/*
:- cateFallback(ATTRIBS), pushAttributes(Ctx,filelevel,ATTRIBS).
:- cateFallback(ATTRIBS), popAttributes(Ctx,filelevel,ATTRIBS). 
:- cateFallback(ATTRIBS), pushAttributes(Ctx,filelevel,ATTRIBS).
*/
:-pp_listing(dict(_,_,_,_,_,_,_,_,_)).


%%:-retractall(aimlCate(_)).


save:-tell(aimlCate),
   listing(aimlCate),
   listing(dict),
   told.


dt:-load_aiml_files('aiml/chomskyAIML/*.aiml').

do:-load_aiml_files,alicebot.




