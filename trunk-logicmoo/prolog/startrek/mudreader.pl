
:-catch(guitracer,E,writeq(E)),nl.
:-set_prolog_flag(double_quotes,string).

:-[runtest].

end_of_file.

:-[createInstanceFile].

end_of_file.

culispify(X,Z,Feat):-culispify1(X,Y,Feat1),culispify1(Y,Z,Feat2),append(Feat1,Feat2,Feat).

culispify1(X,Y,Feat):-cyc:s2p(user:culispifyExtra,X,Y,Feat).

culispifyExtra(X,X,[]):- (var(X);number(X);is_string(X)),!.
culispifyExtra(quoted(X),Y,Feat):-!,culispify(X,Y,Feat).
culispifyExtra('#$'(S),Y,Feat):-!,string_to_atom(S,A),culispify(A,Y,Feat).

mr:-tell(tttst),open('startrek/startrek.lisp',read,O),catch(cyc:lisp_read(O,_,Lisp),_,fail),culispify(Lisp,PLisp,V),(format('~q.~n',[PLisp:V])),fail.
mr:-told.


%%osimAssert(O).
osimAssert(O,MT):-osimMtR(O,MT),!.
osimAssert(O,MT):-assertz(osimMtR(O,MT)).

:-abolish(osimMtR/2).
:-dynamic(osimMtR/2).

osimMt(Call,Mt):-osimMtR(Call,Mt).
osimMt(Call,Mt):-compound(Call),Call=..[N,A,VV],not(member(N,[relationAllInstance,isa,genls])),osimMt(isa(A,C),_),osimMt(relationAllInstance(N,C,VV),Mt).

:-['startrek.pl'].


location(X):-osimMt(isa(X,'BPVLocation'),_).
pathBetween(X,D,Y):-osimMt(pathBetween(['BoundsOfDirectionFn',X,D],X,Y),_).
pathName(X,D,S):-osimMt(nameString(['BoundsOfDirectionFn',X,D],string(S)),_).
description(AS,D):-something(AS),findall(S,
  ((isagenls(AS,A);A=AS),
   ddesc(A,S)),D).

ddesc(A,S):- nameString(A,S).
ddesc(A,S):- (osimMt(definiteDescriptions(A,string(S)),_);osimMt(personalIndentifyingCharacteristic(A,string(S)),_)).
%%ddesc(A,S):- (osimMt(relationAllInstance(definiteDescriptions,A,string(S)),_);osimMt(relationAllInstance(personalIndentifyingCharacteristic,A,string(S)),_)).
ddesc(A,S):- osimMt(relationAllInstance(_,A,string(S)),_).

isagenls(A,B):-isagenls0(A,B).
isagenls(A,C):-isagenls0(A,B),isagenls0(B,C).
isagenls(A,D):-isagenls0(A,B),isagenls0(B,C),isagenls0(C,D).

isagenls0(A,C):-osimMt(isa(A,C),_).
isagenls0(A,C):-osimMt(genls(A,C),_).

something(A):-actor(A);item(A);location(A).
somethingIsa(A,T):-something(A),findall(S,isagenls(A,S),T).

nameString(X,S):-osimMt(nameString(X,string(S)),_).
nameString(X,S):-osimMt(termStrings(X,string(S)),_).

actor(A):- osimMt(isa(C,'BPVAgentType'),_),osimMt(isa(A,C),_).
item(A):- osimMt(isa(C,'BPVArtifactType'),_),osimMt(isa(A,C),_).
possesses(A,I):-osimMt(possesses(A,I),_).
wearing(A,I):-osimMt(wornOn(I,[_The,[_BodyPartCollectionFn,A,_Torso]]),_).
atloc(A,L):-osimMt('in-ContCompletely'(A,L),_).

at(A,L):-atloc(A,L).
at(A,L):-possesses(L,A).
at(A,L):-wearing(L,A).

ina(A,L):-at(A,L).
ina(A,B):-at(A,L),at(L,B).

actorAttrib(A,N,V):-osimMt(isa(C,'BPVAgentType'),_),osimMt(isa(A,C),_),osimMt(relationAllInstance(N,C,VV),_),functor(VV,_,AN),arg(AN,VV,V).
itemAttrib(A,N,V):-osimMt(isa(C,'BPVArtifactType'),_),osimMt(isa(A,C),_),osimMt(relationAllInstance(N,C,VV),_),functor(VV,_,AN),arg(AN,VV,V).


createInstanceFile:-
 tell(createInstanceFile),
 unify_listing(actor/1),
 unify_listing(wearing/2),
 unify_listing(possesses/2),
 unify_listing(item/1),
 unify_listing(atloc/2),
 unify_listing(location/1),
 unify_listing(somethingIsa/2),
 unify_listing(nameString/2),
   unify_listing(description/2),
   unify_listing(pathBetween/3),
   unify_listing(pathName/3),
 told.

