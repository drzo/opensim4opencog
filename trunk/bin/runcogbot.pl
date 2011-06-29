%------------------------------------------------------------------------------
%
%  runcobot.pl
%
%     Example module for use of Prolog in SecondLife!!!
%
% You'd simply run this from swipl, it launches cogbot.
%
% Cogbot is usually in this mode
% set_prolog_flag(double_quotes,string).
%
%------------------------------------------------------------------------------

%% add to search paths

assertIfNew(Gaf):-catch(call(Gaf),_,fail),!.
assertIfNew(Gaf):-assert(Gaf).

:- assertIfNew(user:file_search_path(foreign, '.')).
:- assertIfNew(user:file_search_path(jpl_examples, 'examples/prolog')).
:- assertIfNew(user:file_search_path(jar, '.')).
:- assertIfNew(user:file_search_path(library, '.')).

%------------------------------------------------------------------------------

%% load needed modules

:-use_module(library(clipl)).
%%:-use_module(library(jpl)).

%------------------------------------------------------------------------------

%% load the cogbot assembly
:- current_prolog_flag(address_bits,32) -> cliLoadAssembly('Cogbot32.exe') ; cliLoadAssembly('Cogbot.exe').

%% cache the type names
% prevents us having to use long names for things like SimAvatar
%
cacheShortNames:-
  cliMembers('cogbot.TheOpenSims.SimAvatar',_),
  cliMembers('cogbot.Listeners.WorldObjects',_),
  cliMembers('OpenMetaverse.Primitive',_).

:-cacheShortNames.
%------------------------------------------------------------------------------
% some type layout conversions (to make cleaner code)
%
%  Layouts are records - it's a field layout
%  cliToFromLayout is a way to register an automagic conversion type
%  cliAddLayout adds a conversion between C# type and Prolog type
%------------------------------------------------------------------------------

addLayouts:-
  cliAddLayout('Vector3',v3(x,y,z)),
  cliAddLayout('Vector3d',v3d('X','Y','Z')),
  cliAddLayout('Vector4',v4('X','Y','Z','W')),
  cliAddLayout('Quaternion',quat('X','Y','Z','W')),
 %%  cliAddLayout('UUID',uuid('_Guid')),
 cliToFromLayout('UUID',uuid('ToString'),'UUIDFromString'),
 %%  cliAddLayout('Guid',guid(string)),
  !.

:-addLayouts.




%------------------------------------------------------------------------------
% object getter functions
%
% cliGet is a field accessor that says use the property, if you can't
% find that use the raw field and return.
%
% So this is 'get the GridMaster property from the static
% cogbot.Listeners.WorldObjects and return in Sys'
%
% ------------------------------------------------------------------------------
%
%

worldSystem(Sys):-cliGet('cogbot.Listeners.WorldObjects','GridMaster',Sys).

% gets some property of the GridMaster
%  worldSystem(+Field, -Value)
%
worldSystem(Field,Value):-worldSystem(Sys),cliGet(Sys,Field,Value).

%% get each SimObject
% this is every primitive, linked or not, as a complex term
% it's a partially marshalled object from the simulator
% cliCollection iterates thru elements
simObject(Ele):-worldSystem('SimObjects',Objs),cliCollection(Objs,Ele).

% the above is simpler form of:   simObject(Ele):-worldSystem(Sys),cliGet(Sys,'SimObjects',Obj),cliCollection(Obj,Ele).

%% get each SimAvatar
%
% this is the set of av's that are known to the simulator, they are only
% actually present if they have a prim
%
% simObject and simAvatar handle the complexity of sim crossings
% there's a simRegion predicate with simParcels (cli not done yet)
%
simAvatar(Ele):-cliGet('cogbot.Listeners.WorldObjects','SimAvatars',Obj),cliCollection(Obj,Ele).

%% get the clientManager Instance
%%
%  A class that holds all the static singletons
%  A client is a logged on acct in this context
%
%  clientmanager binds radegast to the client
%  botconfig is run from clientmanager
clientManager(SingleInstance):-cliGet('cogbot.ClientManager','SingleInstance',SingleInstance).

%% get the botClient Instance
% this only unifies once, someday there will be a botClients
botClient(Obj):-clientManager(Man),cliGet(Man,'LastRefBotClient',Obj).

% given an object and a property returns value for the avatar
%
% walks down property tree
% botClient([name,length,X)
% botClient([name,length],X)
% botClient([position,z],X)
%
% a.b.c.d
% botClient([a.b.c.d],X).
% botClient([a,b,c,d],X).
% botClient([a,b,c,d],X). = mybot.a.b.c.d
% prolog botClient([a,b,c,d],X). = c# object X = mybotClient.a.b.c.d
% botClient(['Inventory','Store',rootfolder,name],Y).
% Y = "My Inventory".
%
% botClient(['Inventory','Store',rootnode,nodes,values],Y),
%	findall(S,(cliCollection(Y,Z),cliToString(Z,S)),L),writeq(L).
%	["Scripts","Photo Album","*MD* Brown Leather Hat w/Bling",
%	"Body Parts","Notecards","Objects","Clothing","Landmarks","Textures",
%     "Gestures","boxed fem_talk","Calling Cards","Animations","Sounds",
%	"Trash","Lost And Found"]
%       Y = @'C#720558400'
%
%       finds all grandchildren
%       botClient(['Inventory','Store',rootnode,nodes,values],Y),
%	     findall(S,(cliCollection(Y,Z),cliGet(Z,'children',GC),
%	     cliCollecton(GC,'children',GCReal),cliToString(GCReal,S)),L),writeq(L).
%

botClient([P|N],Value):-!,botClient(Obj),cliGet(Obj,[P|N],Value).
botClient(Property,Value):-botClient(Obj),cliGet(Obj,Property,Value),!.

% a way to call a method on c#
% cliCall('System',printf(32),Y).
botClientCall(Call):-botClientCall(Call,Res),cliWriteln(Res).
botClientCall([P|N],Value):-!,botClient(Obj),cliGet(Obj,P,Mid),cliGet(Mid,N,Value).
botClientCall(Property,Value):-botClient(Obj),cliCall(Obj,Property,Value).

% wrappered execute command in a convenience pred
% botClientCmd(say("hi"))
%
botClientCmd([C|Cmd]):-!,concat_atom([C|Cmd],' ',Str),botClientCall(executeCommand(Str)).
botClientCmd(C):-compound(C),!,C=..[F|A],listifyFlat(A,FL),!,botClientCmd([F|FL]).
% this form expects a term of the form methodname(arg, arg,arg)
botClientCmd(Str):-botClientCall(executeCommand(Str)).

% helper pred for botClientCmd
listifyFlat([],[]):-!.
listifyFlat([H|T],HT):-!,listifyFlat(H,HL),listifyFlat(T,TL),!,append(HL,TL,HT).
listifyFlat(C,FA):-functor(C,F,1),!,C=..[F,A],!,listifyFlat(A,FA).
listifyFlat(C,FA):-compound(C),!,C=..[F|A],!,listifyFlat([F|A],FA).
listifyFlat(C,[C]).

%% get the gridClient Instance
%  libOMV's version of gridClient, in case you want the direct one
gridClient(Obj):-botClient(BC),cliGet(BC,'gridClient',Obj).

%------------------------------------------------------------------------------
% listing functions
%------------------------------------------------------------------------------

listS(P):-call(P,A),cliToString(A,S),writeq(S),fail.
listS(P):-writeq('done'(P)),nl.

listAvatars:-listS(simAvatar).
listPrims:-listS(simObject).


%------------------------------------------------------------------------------
% event handler functions
%------------------------------------------------------------------------------

%% print some events
onSimEvent(_A,_B,_C):-!. % comment out this first line to print them
onSimEvent(A,B,C):-!,assertz(wasSimEvent(A,B,C)).
onSimEvent(_A,_B,C):-cliToString(onSimEvent(C),AS),writeq(AS),nl.

%% on first bot client created register the global event handler
onFirstBotClient(A,B):-
 botClient(Obj),
  % uncomment the next line if you want all commands to run thru the universal event handler
   %%cliAddEventHandler(Obj,'EachSimEvent',onSimEvent(_,_,_)),
   cliToString(onFirstBotClient(A-B-Obj),Objs),writeq(Objs),nl.

%% register onFirstBotClient
:- cliAddEventHandler('cogbot.ClientManager','BotClientCreated',onFirstBotClient(_,_)).


%------------------------------------------------------------------------------
% start Radegast!
%------------------------------------------------------------------------------
:-dynamic(ranSL).

runSL:-ranSL,!.
% this is so you can reconsult this file without restarting radegast
runSL:-asserta(ranSL),!,cliCall('ABuildStartup.Program','Main',[],_).

:-runSL.

% assertIfNew is assert a new grounded atomic fact only if the predicate
% was previously undefined

%:-retractall(cliSubProperty(_,_)).
:-assertIfNew(cliSubProperty('cogbot.TheOpenSims.SimAvatar','ProfileProperties')).
:-assertIfNew(cliSubProperty('cogbot.TheOpenSims.SimAvatar','AvatarInterests')).
:-assertIfNew(cliSubProperty('cogbot.TheOpenSims.SimAvatar','FriendshipInfo')).
:-assertIfNew(cliSubProperty('cogbot.TheOpenSims.SimObject','Prim')).
:-assertIfNew(cliSubProperty('cogbot.TheOpenSims.SimObject','Properties')).

%------------------------------------------------------------------------------
% CLR Introspection of event handlers
%------------------------------------------------------------------------------

gridClientEvents(E):-cliMemb('OpenMetaverse.GridClient',f,M),arg(3,M,Type),cliMemb(Type,e,E).


listMembs:-cliNew('System.Collections.Generic.List'(string),[int],[10],O),cliMembers(O,M),member(E,M),writeq(E),nl,fail.
listMembs:-gridClientEvents(E),writeq(E),nl,fail.
listMembs. % so pred doesnt fail

:-listMembs.

% coerces anything to avatar object
resolveAvatar(Name,Name):-cliIsObject(Name),cliIsType(Name,'SimAvatar'),!.
resolveAvatar(Name,Object):-cliIsObject(Name),cliToString(Name,String),!,resolveAvatar(String,Object).
resolveAvatar(Name,Object):-cliCall('cogbot.Listeners.WorldObjects','TryGetSimAvatarFromName'(string),[Name],Object).

resolveObjectByName(Name,Object):-cliCall('cogbot.Listeners.WorldObjects','TryGetSimAvatarFromName'(string),[Name],Object).

sayTo(Speaker,ToWho,What):-resolveAvatar(ToWho,Listener),cliCall(Speaker,talkto('SimAvatar',string),[Listener,What],_O).

% gives you a list of all the properties
simObject(X,OE):-simObject(X),cliGet(X,infoMap,Y),cliCol(Y,PE),cliUnify(OE,PE).

:-set_prolog_flag(double_quotes,string).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
simDistance(V1,V2,D):-cliCall(V1,distance(V1,V2),D).

simAvDistance(A,C,E):-var(A),nonvar(C),!,simAvDistance(C,A,E).
simAvDistance(A,C,E):-simAvatar(A),cliGet(A,globalposition,B),simAvatar(C),A\=C,cliGet(C,globalposition,D),simDistance(B,D,E).
simObjDistance(A,C,E):-var(A),nonvar(C),!,simObjDistance(C,A,E).
simObjDistance(A,C,E):-simObject(A),cliGet(A,globalposition,B),simObject(C),A\=C,cliGet(C,globalposition,D),simDistance(B,D,E).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
end_of_file.
%------------------------------------------------------------------------------
 6 ?- botClient('Inventory',Y),cliMemb(Y,p,Z).
X = @'C#589147000',
Y = @'C#742328240',
Z = p(0, 'Store', 'Inventory', [], [], 'CanRead'(true), 'CanWrite'(false), decl(static(false), 'InventoryManager'), access_pafv(true, false, false, false)) ;
false.

14 ?- botClientCmd(say("hi there!")).
"Success say"
true.

15 ?- botClientCmd(shout("hi there!")).
"Success shout"

cliGet('cogbot.TheOpenSims.SimTypeSystem','objectTypes',O),cliGetType(O,T),cliTypeSpec(T,S).

3 ?- botClientCall(executeCommand("jump"),X),cliWriteln(X).
"Success Jump"
X = @'C#186521916'.

 22 ?- botClientCall(talk("hello world"),O).
 O = @null.


  botClientCall(talk("hello world from prolog!",0,enum('OpenMetaverse.ChatType','Shout')),O).

  botClientCall([name,indexOf(char('D'))],O).
  cliToTagged(sbyte(-1),O),cliWriteln(O).

 simAvatar(A),cliGet(A,'isonline',B),cliGet(A,'globalposition',C),cliWriteln(A-B-C).
 simObject(X,struct('NamedParam',A,'FirstName')).

simObject(X,struct('NamedParam',A,B)).
%------------------------------------------------------------------------------
% Inventory examples
%------------------------------------------------------------------------------
15 ?- botClient(['Inventory','Store',rootfolder,name],Y).
Y = "My Inventory".

2 ?- botClient(['Inventory','Store',rootnode,nodes,values],Y),cliToString(Y,Z).
Y = @'C#718980688',
Z = "System.Collections.Generic.Dictionary`2+ValueCollection[OpenMetaverse.UUID,OpenMetaverse.InventoryNode]".

4 ?- botClient(['Inventory','Store',rootnode,nodes,values],Y),findall(S,(cliCollection(Y,Z),cliToString(Z,S)),L),writeq(L).
["Scripts","Photo Album","*MD* Brown Leather Hat w/Bling","Body Parts","Notecards","Objects","Clothing","Landmarks","Textures","Gestures","boxed fem_talk","Calling Cards","Animations","Sounds","Trash","Lost And Found"]
Y = @'C#720558400',
L = ["Scripts", "Photo Album", "*MD* Brown Leather Hat w/Bling", "Body Parts", "Notecards", "Objects", "Clothing", "Landmarks", "Textures"|...].


%------------------------------------------------------------------------------
% simAvatar examples
%------------------------------------------------------------------------------
[1] 58 ?- simAvatar(X),cliGet(X,hasprim,@(true)),cliToString(X,S).
X = @'C#638101232',
S = "BinaBot Daxeline" ;
X = @'C#638101728',
S = "Nephrael Rajesh" ;
X = @'C#638111960',
S = "Trollerblades Wasp" ;
false.



v

[debug] 11 ?- simAvatar(X),cliGet(X,'DebugInfo',Y).
X = @'C#723685664',
Y = "BinaBot Daxeline 6d808b3b-990b-474e-a37c-6cf88a9ffb02 Belphegor/152.6617/44.31475/63.0686@59.7028\n Avatar 6d808b3b-990b-474e-a37c-6cf88a9ffb02 (localID 98440636)(childs 2)(PrimFlagsTrue Physics, ObjectModify, ObjectCopy, ObjectYouOwner, ObjectMove, ObjectTransfer)[Avatar](!IsPassable) -NoActions- " ;
X = @'C#723739264',
Y = "Jess Riederer 6ce37e11-17d5-4a4f-a225-b1d214ff322a HEADING: Jess Riederer\n UNATTACHED_PRIM 6ce37e11-17d5-4a4f-a225-b1d214ff322a -NoActions- " ;
X = @'C#723740080',
Y = "VankHalon2 Resident 1fdb943b-1502-42fa-bc52-21812d836cec HEADING: VankHalon2 Resident\n UNATTACHED_PRIM 1fdb943b-1502-42fa-bc52-21812d836cec -NoActions- " ;
X = @'C#723739864',
Y = "TeegZaas Resident 31feab04-79d0-40db-b6a6-4284af659811 HEADING: TeegZaas Resident\n UNATTACHED_PRIM 31feab04-79d0-40db-b6a6-4284af659811 -NoActions- " ;
X = @'C#723739664',
Y = "Nephrael Rajesh 8f92ed5d-9883-4a25-8b82-87fc2c2e1a85 HEADING: Nephrael Rajesh\n UNATTACHED_PRIM 8f92ed5d-9883-4a25-8b82-87fc2c2e1a85 -NoActions- " ;
X = @'C#723740432',
Y = "Nephrael Rae 9d01e386-7aa1-4b7f-ae24-fddfb97fd506 HEADING: Nephrael Rae\n UNATTACHED_PRIM 9d01e386-7aa1-4b7f-ae24-fddfb97fd506 -NoActions- " ;
X = @'C#723740200',
Y = "Annie Obscure 9eda1cfc-e0c4-41ed-b2d2-e62bb70366df HEADING: Annie Obscure\n UNATTACHED_PRIM 9eda1cfc-e0c4-41ed-b2d2-e62bb70366df -NoActions- " ;
X = @'C#723740656',
Y = "Nocco Oldrich 3b26fd9c-59d6-48d0-ac99-4cff578c4ab6 HEADING: Nocco Oldrich\n UNATTACHED_PRIM 3b26fd9c-59d6-48d0-ac99-4cff578c4ab6 -NoActions- " ;
X = @'C#723743632',
Y = "Satir DeCuir 9702bd45-9880-48ab-a516-e96e40731a13 HEADING: Satir DeCuir\n UNATTACHED_PRIM 9702bd45-9880-48ab-a516-e96e40731a13 -NoActions- " ;
X = @'C#723744208',
Y = "Draven Littlebird d191e38e-32f9-4f83-9289-6b580eb804ad HEADING: Draven Littlebird\n UNATTACHED_PRIM d191e38e-32f9-4f83-9289-6b580eb804ad\n EffectType-Beam-Once: \"{doneBy=Draven Littlebird,Choices=NIL,info=NIL}\" \"{objectActedOn=UNATTACHED_PRIM 7939a07b-e2e3-9d7b-445f-ff603e760396,Choices=NIL,info=NIL}\" \"{eventPartiallyOccursAt=?/0/0/0@90,Choices=NIL,info=NIL}\" \"{simDuration=1,Choices=NIL,info=NIL}\" \"{id=f8dae11b-b6a6-fe43-7281-c62f1313367b,Choices=NIL,info=NIL}\" 634437706401961059\n LookAtType-Focus-Once: \"{doneBy=Draven Littlebird,Choices=NIL,info=NIL}\" \"{objectActedOn=UNATTACHED_PRIM 7939a07b-e2e3-9d7b-445f-ff603e760396,Choices=NIL,info=NIL}\" \"{eventPartiallyOccursAt=HEADING: UNATTACHED_PRIM 7939a07b-e2e3-9d7b-445f-ff603e760396,Choices=NIL,info=NIL}\" \"{simDuration=1.701412E+38,Choices=NIL,info=NIL}\" \"{id=1d7c699c-b42a-3e7e-b825-3e9e6d13ed30,Choices=NIL,info=NIL}\" 634437706401961061\n EffectType-Beam-Once: \"{doneBy=Draven Littlebird,Choices=NIL,info=NIL}\" \"{objectActedOn=UNATTACHED_PRIM 7939a07b-e2e3-9d7b-445f-ff603e760396,Choices=NIL,info=NIL}\" \"{eventPartiallyOccursAt=?/0/0/0@90,Choices=NIL,info=NIL}\" \"{simDuration=1,Choices=NIL,info=NIL}\" \"{id=362bf818-1fb3-0289-ca40-2812cf9c5662,Choices=NIL,info=NIL}\" 634437706401961801\n LookAtType-Focus-Once: \"{doneBy=Draven Littlebird,Choices=NIL,info=NIL}\" \"{objectActedOn=UNATTACHED_PRIM 7939a07b-e2e3-9d7b-445f-ff603e760396,Choices=NIL,info=NIL}\" \"{eventPartiallyOccursAt=HEADING: UNATTACHED_PRIM 7939a07b-e2e3-9d7b-445f-ff603e760396,Choices=NIL,info=NIL}\" \"{simDuration=1.701412E+38,Choices=NIL,info=NIL}\" \"{id=1d7c699c-b42a-3e7e-b825-3e9e6d13ed30,Choices=NIL,info=NIL}\" 634437706401962759\n EffectType-Beam-Once: \"{doneBy=Draven Littlebird,Choices=NIL,info=NIL}\" \"{objectActedOn=UNATTACHED_PRIM 7939a07b-e2e3-9d7b-445f-ff603e760396,Choices=NIL,info=NIL}\" \"{eventPartiallyOccursAt=?/0/0/0@90,Choices=NIL,info=NIL}\" \"{simDuration=1,Choices=NIL,info=NIL}\" \"{id=25bea9ae-0427-5ece-cab4-31ed14317d2e,Choices=NIL,info=NIL}\" 634437706401963078\n EffectType-Beam-Once: \"{doneBy=Draven Littlebird,Choices=NIL,info=NIL}\" \"{objectActedOn=UNATTACHED_PRIM 7939a07b-e2e3-9d7b-445f-ff603e760396,Choices=NIL,info=NIL}\" \"{eventPartiallyOccursAt=?/0/0/0@90,Choices=NIL,info=NIL}\" \"{simDuration=1,Choices=NIL,info=NIL}\" \"{id=324dc944-f207-ab79-68e3-884c935b70fc,Choices=NIL,info=NIL}\" 634437706401964540\n LookAtType-Focus-Once: \"{doneBy=Draven Littlebird,Choices=NIL,info=NIL}\" \"{objectActedOn=UNATTACHED_PRIM 7939a07b-e2e3-9d7b-445f-ff603e760396,Choices=NIL,info=NIL}\" \"{eventPartiallyOccursAt=HEADING: UNATTACHED_PRIM 7939a07b-e2e3-9d7b-445f-ff603e760396,Choices=NIL,info=NIL}\" \"{simDuration=1.701412E+38,Choices=NIL,info=NIL}\" \"{id=1d7c699c-b42a-3e7e-b825-3e9e6d13ed30,Choices=NIL,info=NIL}\" 634437706401964582\n LookAtType-Focus-Once: \"{doneBy=Draven Littlebird,Choices=NIL,info=NIL}\" \"{objectActedOn=UNATTACHED_PRIM 7939a07b-e2e3-9d7b-445f-ff603e760396,Choices=NIL,info=NIL}\" \"{eventPartiallyOccursAt=HEADING: UNATTACHED_PRIM 7939a07b-e2e3-9d7b-445f-ff603e760396,Choices=NIL,info=NIL}\" \"{simDuration=1.701412E+38,Choices=NIL,info=NIL}\" \"{id=1d7c699c-b42a-3e7e-b825-3e9e6d13ed30,Choices=NIL,info=NIL}\" 634437706401964664\n EffectType-Beam-Once: \"{doneBy=Draven Littlebird,Choices=NIL,info=NIL}\" \"{objectActedOn=UNATTACHED_PRIM 7939a07b-e2e3-9d7b-445f-ff603e760396,Choices=NIL,info=NIL}\" \"{eventPartiallyOccursAt=?/0/0/0@90,Choices=NIL,info=NIL}\" \"{simDuration=1,Choices=NIL,info=NIL}\" \"{id=140c32c1-911d-96b3-6885-af10d639ff99,Choices=NIL,info=NIL}\" 634437706401964959\n EffectType-Beam-Once: \"{doneBy=Draven Littlebird,Choices=NIL,info=NIL}\" \"{objectActedOn=UNATTACHED_PRIM 7939a07b-e2e3-9d7b-445f-ff603e760396,Choices=NIL,info=NIL}\" \"{eventPartiallyOccursAt=?/0/0/0@90,Choices=NIL,info=NIL}\" \"{simDuration=1,Choices=NIL,info=NIL}\" \"{id=6cbd2457-f2e1-30d2-dcc5-1749461be3f1,Choices=NIL,info=NIL}\" 634437706401965809" .


15 ?- set_prolog_flag(double_quotes,chars).
true.

16 ?- X="sadfsdf".
X = [s, a, d, f, s, d, f].

17 ?- set_prolog_flag(double_quotes,codes).
true.

18 ?- X="sadfsdf".
X = [115, 97, 100, 102, 115, 100, 102].

19 ?- set_prolog_flag(double_quotes,atom).
true.

20 ?- X="sadfsdf".
X = sadfsdf.

21 ?- set_prolog_flag(double_quotes,string).
true.

22 ?- X="sadfsdf".
X = "sadfsdf".

[debug] 14 ?- simAvatar(X),cliGet(X,'ActionEventQueue',Y),cliCollection(Y,Z).
X = @'C#723744208',
Y = @'C#728007880',
Z = event('SimObjectEvent', struct('DateTime', 5246123754218764857), "LookAtType-FreeLook", @'C#728025192', enum('SimEventType', 'EFFECT'), enum('SimEventStatus', 'Once'), enum('SimEventClass', 'REGIONAL')) ;
X = @'C#723744208',
Y = @'C#728007880',
Z = event('SimObjectEvent', struct('DateTime', 5246123754234604700), "LookAtType-FreeLook", @'C#728029648', enum('SimEventType', 'EFFECT'), enum('SimEventStatus', 'Once'), enum('SimEventClass', 'REGIONAL')) ;
X = @'C#723744208',
Y = @'C#728007880',
Z = event('SimObjectEvent', struct('DateTime', 5246123754290346888), "LookAtType-FreeLook", @'C#728029304', enum('SimEventType', 'EFFECT'), enum('SimEventStatus', 'Once'), enum('SimEventClass', 'REGIONAL')) ;
X = @'C#723744208',
Y = @'C#728007880',
Z = event('SimObjectEvent', struct('DateTime', 5246123754295014857), "LookAtType-FreeLook", @'C#728030112', enum('SimEventType', 'EFFECT'), enum('SimEventStatus', 'Once'), enum('SimEventClass', 'REGIONAL')) ;

[1] 65 ?- simAvatar(X),cliGet(X,hasprim,@(true)),cliToString(X,S),cliGet(X,'ActionEventQueue',AEQ),cliCol(AEQ,EE),cliToDataTerm(EE,DATA).


[debug] 3 ?- cliNew('System.Collections.Generic.List'(string),[int],[10],O),cliMembers(O,M),!,member(E,M),writeq(E),nl,fail.
f(0,'_items'(arrayOf('String')))
f(1,'_size'('Int32'))
f(2,'_version'('Int32'))
f(3,'_syncRoot'('Object'))
f(4,'_emptyArray'(arrayOf('String')))
f(5,'_defaultCapacity'('Int32'))
p(0,'Capacity'('Int32'))
p(1,'Count'('Int32'))
p(2,'System.Collections.IList.IsFixedSize'('Boolean'))
p(3,'System.Collections.Generic.ICollection<T>.IsReadOnly'('Boolean'))
p(4,'System.Collections.IList.IsReadOnly'('Boolean'))
p(5,'System.Collections.ICollection.IsSynchronized'('Boolean'))
p(6,'System.Collections.ICollection.SyncRoot'('Object'))
p(7,'Item'('String'))
p(8,'System.Collections.IList.Item'('Object'))
m(0,'ConvertAll'('Converter'('String',<)))
m(1,get_Capacity)
m(2,set_Capacity('Int32'))
m(3,get_Count)
m(4,'System.Collections.IList.get_IsFixedSize')
m(5,'System.Collections.Generic.ICollection<T>.get_IsReadOnly')
m(6,'System.Collections.IList.get_IsReadOnly')
m(7,'System.Collections.ICollection.get_IsSynchronized')
m(8,'System.Collections.ICollection.get_SyncRoot')
m(9,get_Item('Int32'))
m(10,set_Item('Int32','String'))
m(11,'IsCompatibleObject'('Object'))
m(12,'VerifyValueType'('Object'))
m(13,'System.Collections.IList.get_Item'('Int32'))
m(14,'System.Collections.IList.set_Item'('Int32','Object'))
m(15,'Add'('String'))
m(16,'System.Collections.IList.Add'('Object'))
m(17,'AddRange'('System.Collections.Generic.IEnumerable'('String')))
m(18,'AsReadOnly')
m(19,'BinarySearch'('Int32','Int32','String','System.Collections.Generic.IComparer'('String')))
m(20,'BinarySearch'('String'))
m(21,'BinarySearch'('String','System.Collections.Generic.IComparer'('String')))
m(22,'Clear')
m(23,'Contains'('String'))
m(24,'System.Collections.IList.Contains'('Object'))
m(25,'CopyTo'(arrayOf('String')))
m(26,'System.Collections.ICollection.CopyTo'('Array','Int32'))
m(27,'CopyTo'('Int32',arrayOf('String'),'Int32','Int32'))
m(28,'CopyTo'(arrayOf('String'),'Int32'))
m(29,'EnsureCapacity'('Int32'))
m(30,'Exists'('System.Predicate'('String')))
m(31,'Find'('System.Predicate'('String')))
m(32,'FindAll'('System.Predicate'('String')))
m(33,'FindIndex'('System.Predicate'('String')))
m(34,'FindIndex'('Int32','System.Predicate'('String')))
m(35,'FindIndex'('Int32','Int32','System.Predicate'('String')))
m(36,'FindLast'('System.Predicate'('String')))
m(37,'FindLastIndex'('System.Predicate'('String')))
m(38,'FindLastIndex'('Int32','System.Predicate'('String')))
m(39,'FindLastIndex'('Int32','Int32','System.Predicate'('String')))
m(40,'ForEach'('System.Action'('String')))
m(41,'GetEnumerator')
m(42,'System.Collections.Generic.IEnumerable<T>.GetEnumerator')
m(43,'System.Collections.IEnumerable.GetEnumerator')
m(44,'GetRange'('Int32','Int32'))
m(45,'IndexOf'('String'))
m(46,'System.Collections.IList.IndexOf'('Object'))
m(47,'IndexOf'('String','Int32'))
m(48,'IndexOf'('String','Int32','Int32'))
m(49,'Insert'('Int32','String'))
m(50,'System.Collections.IList.Insert'('Int32','Object'))
m(51,'InsertRange'('Int32','System.Collections.Generic.IEnumerable'('String')))
m(52,'LastIndexOf'('String'))
m(53,'LastIndexOf'('String','Int32'))
m(54,'LastIndexOf'('String','Int32','Int32'))
m(55,'Remove'('String'))
m(56,'System.Collections.IList.Remove'('Object'))
m(57,'RemoveAll'('System.Predicate'('String')))
m(58,'RemoveAt'('Int32'))
m(59,'RemoveRange'('Int32','Int32'))
m(60,'Reverse')
m(61,'Reverse'('Int32','Int32'))
m(62,'Sort')
m(63,'Sort'('System.Collections.Generic.IComparer'('String')))
m(64,'Sort'('Int32','Int32','System.Collections.Generic.IComparer'('String')))
m(65,'Sort'('System.Comparison'('String')))
m(66,'ToArray')
m(67,'TrimExcess')
m(68,'TrueForAll'('System.Predicate'('String')))
m(69,'ToString')
m(70,'Equals'('Object'))
m(71,'GetHashCode')
m(72,'GetType')
m(73,'Finalize')
m(74,'MemberwiseClone')
c(0,'List`1')
c(1,'List`1'('Int32'))
c(2,'List`1'('System.Collections.Generic.IEnumerable'('String')))
c(3,'List`1')

2 ?- botClient([self,simposition],X).
X = struct('Vector3', 153.20449829101562, 44.02702713012695, 63.06859588623047).

cliGetType(struct('Vector3', 153.20449829101562, 44.02702713012695, 63.06859588623047),T),cliWriteln(T).

cliWriteln(struct('Vector3', 153.20449829101562, 44.02702713012695, 63.06859588623047)).

cliTypeToSpec/2

cliSpecToType/2

 Just file storage now...

 [debug] 8 ?- cliNew('System.Collections.Generic.Dictionary'(string,string),[],[],O),cliGet(O,count,C)

 cliNew('System.Collections.Generic.List'(string),[int],[10],O),cliGetType(O,T),cliWriteln(T).

 cliNew('System.Collections.Generic.List'(string),[int],[10],O),cliMembers(O,M)

 ERROR: findField IsVar _G929 on type System.Collections.Generic.Dictionary`2[System.String,System.String]
ERROR: findProperty IsVar _G929 on type System.Collections.Generic.Dictionary`2[System.String,System.String]
   Call: (9) message_to_string('Only possible for compound or atoms', _G1079) ? leap

cliShortType(dict,'System.Collections.Generic.Dictionary`2').

cliFindType(dict(string,string),Found).

12 ?- cliFindClass('System.Collections.Generic.Dictionary'('int','string'),X),cliToString(X,Y).
X = @'C#592691552',
Y = "class cli.System.Collections.Generic.Dictionary$$00602_$$$_I_$$_Ljava__lang__String_$$$$_".

13 ?- cliFindType('System.Collections.Generic.Dictionary'('int','string'),X),cliToString(X,Y).
X = @'C#592687600',
Y = "System.Collections.Generic.Dictionary`2[System.Int32,System.String]".


?- cliFindType('System.Collections.Generic.Dictionary'(string,string),NewObj).

?- cliNew('System.Collections.Generic.Dictionary'(string,string),NewObj).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
5 ?- simObject(O),cliGet(O,simregion,X),cliToString(X,S).
O = @'C#701938432',
X = @'C#702256808',
S = "Belphegor (216.82.46.79:13005)" .

4 ?- simObject(O),cliGet(O,name,X),cliToString(X,S).
O = @'C#701938432',
X = S, S = "BinaBot Daxeline" .

5 ?- simObject(O),cliGet(O,simulator,X),cliToString(X,S).
O = @'C#701938432',
X = @'C#701938424',
S = "Belphegor (216.82.46.79:13005)" .

1 ?- simObject(O),cliGet(O,'Type',X),cliToString(X,S).
O = @'C#679175768',
X = @'C#679175760',
S = "cogbot.TheOpenSims.SimAvatarImpl" .

7 ?- cliGet('cogbot.Listeners.WorldObjects','SimObjects',Objs),cliGet(Objs,'count',X).
Objs = @'C#585755456',
X = 5905.


22 ?- botClient(X),cliMembers(X,M),cliToString(M,S).
X = @'C#585755312',
M = [m('GetFolderItems'('String')), m('GetFolderItems'('UUID')), m('SetRadegastLoginOptions'), m('GetGridIndex'('String', 'Int32&')), m('SetRadegastLoginForm'('LoginConsole', 'LoginOptions')), m('GetLoginOptionsFromRadegast'), m('ShowTab'('String')), m('AddTab'(..., ..., ..., ...)), m(...)|...],
S = "[m(GetFolderItems(String)),m(GetFolderItems(UUID)),m(SetRadegastLoginOptions),m(GetGridIndex(String,Int32&)),m(SetRadegastLoginForm(LoginConsole,LoginOptions)),m(GetLoginOptionsFromRadegast),m(ShowTab(String)),m(AddTab(String,String,UserControl,EventHandler)),m(InvokeThread(String,ThreadStart)),m(InvokeGUI(Control,ThreadStart)),m(InvokeGUI(ThreadStart)),m(GetSecurityLevel(UUID)),m(ExecuteTask(String,TextReader,OutputDelegate)),m(DoHttpGet(String)),m(DoHttpPost(arrayOf(Object))),m(ExecuteXmlCommand(String,OutputDelegate)),m(XmlTalk(String,OutputDelegate)),m(DoAnimation(String)),m(GetAnimationOrGesture(String)),m(Talk(String)),m(Talk(String,Int32,ChatType)),m(InstantMessage(UUID,String,UUID)),m(NameKey),m(get_EventsEnabled),m(set_EventsEnabled(Boolean)),m(<.ctor>b__0),m(<.ctor>b__1),m(<.ctor>b__2),m(<.ctor>b__3),m(<.ctor>b__4),m(<DoAnimation>b__34),m(op_Implicit(BotClient)),m(get_Network),m(get_Settings),m(get_Parcels),m(get_Self),m(get_Avatars),m(get_Friends),m(get_Grid),m(get_Objects),m(get_Groups),m(get_Assets),m(get_Estate),m(get_Appearance),m(get_Inventory),m(get_Directory),m(get_Terrain),m(get_Sound),m(get_Throttle),m(OnEachSimEvent(SimObjectEvent)),m(add_EachSimEvent(EventHandler`1)),m(remove_EachSimEvent(EventHandler`1)),m(get_IsLoggedInAndReady),m(Login),m(Login(Boolean)),m(LogException(String,Exception)),m(get_BotLoginParams),m(GetBotCommandThreads),m(AddThread(Thread)),m(RemoveThread(Thread)),m(getPosterBoard(Object)),m(get_MasterName),m(set_MasterName(String)),m(get_MasterKey),m(set_MasterKey(UUID)),m(get_AllowObjectMaster),m(get_IsRegionMaster),m(get_TheRadegastInstance),m(set_TheRadegastInstance(RadegastInstance)),m(IMSent(Object,InstantMessageSentEventArgs)),m(add_OnInstantMessageSent(InstantMessageSentArgs)),m(remove_OnInstantMessageSent(InstantMessageSentArgs)),m(SetLoginName(String,String)),m(SetLoginAcct(LoginDetails)),m(LoadTaskInterpreter),m(StartupClientLisp),m(RunOnLogin),m(SendResponseIM(GridClient,UUID,OutputDelegate,String)),m(updateTimer_Elapsed(Object,ElapsedEventArgs)),m(AgentDataUpdateHandler(Object,PacketReceivedEventArgs)),m(GroupMembersHandler(Object,GroupMembersReplyEventArgs)),m(AvatarAppearanceHandler(Object,PacketReceivedEventArgs)),m(AlertMessageHandler(Object,PacketReceivedEventArgs)),m(ReloadGroupsCache),m(GroupName2UUID(String)),m(Groups_OnCurrentGroups(Object,CurrentGroupsEventArgs)),m(Self_OnTeleport(Object,TeleportEventArgs)),m(Self_OnChat(Object,ChatEventArgs)),m(Self_OnInstantMessage(Object,InstantMessageEventArgs)),m(DisplayNotificationInChat(String)),m(Inventory_OnInventoryObjectReceived(Object,InventoryObjectOfferedEventArgs)),m(Network_OnDisconnected(Object,DisconnectedEventArgs)),m(EnsureConnectedCheck(DisconnectType)),m(Network_OnConnected(Object)),m(Network_OnSimDisconnected(Object,SimDisconnectedEventArgs)),m(client_OnLogMessage(Object,LogLevel)),m(Network_OnEventQueueRunning(Object,EventQueueRunningEventArgs)),m(Network_OnSimConnected(Object,SimConnectedEventArgs)),m(Network_OnSimConnecting(Simulator)),m(Network_OnLogoutReply(Object,LoggedOutEventArgs)),m(UseInventoryItem(String,String)),m(ListObjectsFolder),m(wearFolder(String)),m(PrintInventoryAll),m(findInventoryItem(String)),m(logout),m(WriteLine(String)),m(DebugWriteLine(String,arrayOf(Object))),m(WriteLine(String,arrayOf(Object))),m(output(String)),m(describeAll(Boolean,OutputDelegate)),m(describeSituation(OutputDelegate)),m(describeLocation(Boolean,OutputDelegate)),m(describePeople(Boolean,OutputDelegate)),m(describeObjects(Boolean,OutputDelegate)),m(describeBuildings(Boolean,OutputDelegate)),m(get_LispTaskInterperter),m(enqueueLispTask(Object)),m(evalLispReader(TextReader)),m(evalLispReaderString(TextReader)),m(evalXMLString(TextReader)),m(XML2Lisp2(String,String)),m(XML2Lisp(String)),m(evalLispString(String)),m(evalLispCode(Object)),m(ToString),m(Network_OnLogin(Object,LoginProgressEventArgs)),m(InvokeAssembly(Assembly,String,OutputDelegate)),m(ConstructType(Assembly,Type,String,System.Predicate`1[[System.Type, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]],System.Action`1[[System.Type, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]])),m(LoadAssembly(Assembly)),m(RegisterCommand(String,Command)),m(RegisterCommand(Command)),m(DoCommandAll(String,UUID,OutputDelegate)),m(GetVoiceManager),m(Dispose),m(AddBotMessageSubscriber(SimEventSubscriber)),m(RemoveBotMessageSubscriber(SimEventSubscriber)),m(SendNetworkEvent(String,arrayOf(Object))),m(SendPersonalEvent(SimEventType,String,arrayOf(Object))),m(SendPipelineEvent(SimObjectEvent)),m(argsListString(IEnumerable)),m(argString(Object)),m(ExecuteCommand(String)),m(InvokeJoin(String)),m(InvokeJoin(String,Int32)),m(InvokeJoin(String,Int32,ThreadStart,ThreadStart)),m(InvokeNext(String,ThreadStart)),m(ExecuteCommand(String,OutputDelegate)),m(ExecuteBotCommand(String,OutputDelegate)),m(DoCmdAct(Command,String,String,OutputDelegate)),m(GetName),m(cogbot.Listeners.SimEventSubscriber.OnEvent(SimObjectEvent)),m(cogbot.Listeners.SimEventSubscriber.Dispose),m(TalkExact(String)),m(Intern(String,Object)),m(InternType(Type)),m(RegisterListener(Listener)),m(RegisterType(Type)),m(GetAvatar),m(FakeEvent(Object,String,arrayOf(Object))),m(Equals(Object)),m(GetHashCode),m(GetType),m(Finalize),m(MemberwiseClone),c(BotClient),c(BotClient(ClientManager,GridClient)),p(Network(NetworkManager)),p(Settings(Settings)),p(Parcels(ParcelManager)),p(Self(AgentManager)),p(Avatars(AvatarManager)),p(Friends(FriendsManager)),p(Grid(GridManager)),p(Objects(ObjectManager)),p(Groups(GroupManager)),p(Assets(AssetManager)),p(Estate(EstateTools)),p(Appearance(AppearanceManager)),p(Inventory(InventoryManager)),p(Directory(DirectoryManager)),p(Terrain(TerrainManager)),p(Sound(SoundManager)),p(Throttle(AgentThrottle)),p(IsLoggedInAndReady(Boolean)),p(BotLoginParams(LoginDetails)),p(MasterName(String)),p(MasterKey(UUID)),p(AllowObjectMaster(Boolean)),p(IsRegionMaster(Boolean)),p(TheRadegastInstance(RadegastInstance)),p(LispTaskInterperter(ScriptInterpreter)),p(EventsEnabled(Boolean)),e(EachSimEvent(Object,SimObjectEvent)),e(OnInstantMessageSent(Object,IMessageSentEventArgs)),f(m_EachSimEvent(EventHandler`1)),f(m_EachSimEventLock(Object)),f(OneAtATimeQueue(TaskQueueHandler)),f(gridClient(GridClient)),f(LoginRetriesFresh(Int32)),f(LoginRetries(Int32)),f(ExpectConnected(Boolean)),f(thisTcpPort(Int32)),f(_BotLoginParams(LoginDetails)),f(botPipeline(SimEventPublisher)),f(botCommandThreads(IList`1)),f(XmlInterp(XmlScriptInterpreter)),f(GroupID(UUID)),f(GroupMembers(System.Collections.Generic.Dictionary`2[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null],[OpenMetaverse.GroupMember, OpenMetaverse, Version=0.0.0.26031, Culture=neutral, PublicKeyToken=null]])),f(Appearances(System.Collections.Generic.Dictionary`2[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null],[OpenMetaverse.Packets.AvatarAppearancePacket, OpenMetaverse, Version=0.0.0.26031, Culture=neutral, PublicKeyToken=null]])),f(Running(Boolean)),f(GroupCommands(Boolean)),f(_masterName(String)),f(PosterBoard(Hashtable)),f(SecurityLevels(System.Collections.Generic.Dictionary`2[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null],[cogbot.BotPermissions, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(_masterKey(UUID)),f(__TheRadegastInstance(RadegastInstance)),f(OnInstantMessageSent(InstantMessageSentArgs)),f(VoiceManager(VoiceManager)),f(CurrentDirectory(InventoryFolder)),f(bodyRotation(Quaternion)),f(forward(Vector3)),f(left(Vector3)),f(up(Vector3)),f(updateTimer(System.Timers.Timer)),f(WorldSystem(WorldObjects)),f(GetTextures(Boolean)),f(describers(System.Collections.Generic.Dictionary`2[[System.String, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[cogbot.DescribeDelegate, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(listeners(System.Collections.Generic.Dictionary`2[[System.String, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[cogbot.Listeners.Listener, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(Commands(SortedDictionary`2)),f(tutorials(System.Collections.Generic.Dictionary`2[[System.String, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[cogbot.Tutorials.Tutorial, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(describeNext(Boolean)),f(describePos(Int32)),f(currTutorial(String)),f(BoringNamesCount(Int32)),f(GoodNamesCount(Int32)),f(RunningMode(Int32)),f(AnimationFolder(UUID)),f(searcher(BotInventoryEval)),f(taskInterperterType(String)),f(scriptEventListener(ScriptEventListener)),f(ClientManager(ClientManager)),f(muteList(System.Collections.Generic.List`1[[System.String, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]])),f(muted(Boolean)),f(GroupMembersRequestID(UUID)),f(GroupsCache(System.Collections.Generic.Dictionary`2[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null],[OpenMetaverse.Group, OpenMetaverse, Version=0.0.0.26031, Culture=neutral, PublicKeyToken=null]])),f(GroupsEvent(ManualResetEvent)),f(CatchUpInterns(MethodInvoker)),f(useLispEventProducer(Boolean)),f(lispEventProducer(LispEventProducer)),f(RunStartupClientLisp(Boolean)),f(RunStartupClientLisplock(Object)),f(_LispTaskInterperter(ScriptInterpreter)),f(LispTaskInterperterLock(Object)),f(registeredTypes(System.Collections.Generic.List`1[[System.Type, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]])),f(KnownAssembies(System.Collections.Generic.Dictionary`2[[System.Reflection.Assembly, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[System.Collections.Generic.List`1[[cogbot.Listeners.Listener, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]], mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]])),f(AssemblyListeners(System.Collections.Generic.Dictionary`2[[System.Reflection.Assembly, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[System.Collections.Generic.List`1[[cogbot.Listeners.Listener, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]], mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]])),f(TalkingAllowed(Boolean)),f(IsEnsuredRunning(Boolean)),f(EnsuredRadegastRunning(Boolean)),f(InvokedMakeRunning(Boolean)),f(AddingTypesToBotClientNow(Boolean)),f(NeedRunOnLogin(Boolean)),f(debugLevel(Int32)),f(CS$<>9__CachedAnonymousMethodDelegate5(MethodInvoker)),f(CS$<>9__CachedAnonymousMethodDelegate35(ThreadStart))]".

24 ?- simAvatar(X),cliMembers(X,M),cliToString(M,S).
X = @'C#585739064',
S = "[m(get_SelectedBeam),m(set_SelectedBeam(Boolean)),m(cogbot.TheOpenSims.SimActor.GetSelectedObjects),m(SelectedRemove(SimPosition)),m(SelectedAdd(SimPosition)),m(get_ProfileProperties),m(set_ProfileProperties(AvatarProperties)),m(get_AvatarInterests),m(set_AvatarInterests(Interests)),m(get_AvatarGroups),m(set_AvatarGroups(System.Collections.Generic.List`1[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null]])),m(set_IsKilled(Boolean)),m(ThreadJump),m(WithAnim(UUID,ThreadStart)),m(OpenNearbyClosedPassages),m(OnMoverStateChange(SimMoverState)),m(DebugInfo),m(LogEvent(SimObjectEvent)),m(AddCanBeTargetOf(Int32,SimObjectEvent)),m(get_theAvatar),m(get_SightRange),m(set_SightRange(Double)),m(GetKnownObjects),m(GetNearByObjects(Double,Boolean)),m(get_LastAction),m(set_LastAction(BotAction)),m(get_CurrentAction),m(set_CurrentAction(BotAction)),m(makeActionThread(BotAction)),m(MakeEnterable(SimMover)),m(RestoreEnterable(SimMover)),m(get_IsRoot),m(get_IsSitting),m(set_IsSitting(Boolean)),m(get_HasPrim),m(get_IsControllable),m(GetSimulator),m(get_GlobalPosition),m(GetSimRegion),m(get_SimRotation),m(Do(SimTypeUsage,SimObject)),m(TakeObject(SimObject)),m(AttachToSelf(SimObject)),m(WearItem(InventoryItem)),m(ScanNewObjects(Int32,Double,Boolean)),m(AddKnowns(System.Collections.Generic.IEnumerable`1[[cogbot.TheOpenSims.SimObject, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),m(ResetRegion(UInt64)),m(GetSizeDistance),m(GetGridClient),m(AddGoupRoles(System.Collections.Generic.List`1[[OpenMetaverse.AvatarGroup, OpenMetaverse, Version=0.0.0.26031, Culture=neutral, PublicKeyToken=null]])),m(TalkTo(SimAvatar,String)),m(TalkTo(SimAvatar,BotMentalAspect)),m(Debug(String,arrayOf(Object))),m(Eat(SimObject)),m(WithSitOn(SimObject,ThreadStart)),m(StopAllAnimations),m(WithGrabAt(SimObject,ThreadStart)),m(WithAnim(SimAsset,ThreadStart)),m(ExecuteLisp(SimObjectUsage,Object)),m(get_Flying),m(set_Flying(Boolean)),m(KilledPrim(Primitive,Simulator)),m(ResetPrim(Primitive,BotClient,Simulator)),m(SetFirstPrim(Primitive)),m(GetName),m(ToString),m(SetClient(BotClient)),m(FindSimObject(SimObjectType,Double,Double)),m(Matches(String)),m(StandUp),m(UpdateObject(ObjectMovementUpdate,ObjectMovementUpdate)),m(Touch(SimObject)),m(RemoveObject(SimObject)),m(StopMoving),m(Approach(SimObject,Double)),m(TrackerLoop),m(MoveTo(Vector3d,Double,Single)),m(Write(String)),m(GotoTarget(SimPosition)),m(SendUpdate(Int32)),m(TeleportTo(SimRegion,Vector3)),m(SetMoveTarget(SimPosition,Double)),m(OnlyMoveOnThisThread),m(SetMoveTarget(Vector3d)),m(EnsureTrackerRunning),m(get_ApproachPosition),m(set_ApproachPosition(SimPosition)),m(get_ApproachVector3D),m(set_ApproachVector3D(Vector3d)),m(get_KnownTypeUsages),m(SitOn(SimObject)),m(SitOnGround),m(SetObjectRotation(Quaternion)),m(TurnToward(Vector3)),m(TurnToward0(Vector3)),m(get_IsDrivingVehical),m(UpdateOccupied),m(get_IsWalking),m(get_IsFlying),m(get_IsStanding),m(get_IsSleeping),m(get_DebugLevel),m(set_DebugLevel(Int32)),m(GetCurrentAnims),m(GetAnimUUIDs(List`1)),m(GetBeforeUUIDs(List`1,Int32)),m(GetAfterUUIDs(List`1,Int32)),m(GetDurringUUIDs(List`1,Int32)),m(GetCurrentAnimDict),m(OnAvatarAnimations(List`1)),m(AnimEvent(UUID,SimEventStatus,Int32)),m(get_GroupRoles),m(set_GroupRoles(Dictionary`2)),m(SetPosture(SimObjectEvent)),m(GetSequenceNumbers(System.Collections.Generic.IEnumerable`1[[System.Collections.Generic.KeyValuePair`2[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null],[System.Int32, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]], mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]],Int32&,Int32&)),m(StartOrStopAnimEvent(IDictionary`2,IDictionary`2,String,System.Collections.Generic.IList`1[[cogbot.TheOpenSims.SimObjectEvent, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),m(Overlaps(System.Collections.Generic.IEnumerable`1[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null]],System.Collections.Generic.IEnumerable`1[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null]])),m(GetLastEvent(String,Int32)),m(<ThreadJump>b__13),m(get_ConfirmedObject),m(set_ConfirmedObject(Boolean)),m(get_UsePosition),m(get_ZHeading),m(GetHeading),m(CanShoot(SimPosition)),m(AddInfoMap(Object,String)),m(get__propertiesCache),m(set__propertiesCache(ObjectProperties)),m(GetObject(String)),m(get_RegionHandle),m(set_RegionHandle(UInt64)),m(get_ID),m(set_ID(UUID)),m(get_Properties),m(set_Properties(ObjectProperties)),m(GetCubicMeters),m(GetGroupLeader),m(GetTerm),m(get_PathStore),m(TurnToward(SimPosition)),m(IndicateTarget(SimPosition,Boolean)),m(FollowPathTo(SimPosition,Double)),m(TeleportTo(SimPosition)),m(SetObjectPosition(Vector3d)),m(SetObjectPosition(Vector3)),m(TurnToward(Vector3d)),m(get_OuterBox),m(get_LocalID),m(get_ParentID),m(GetInfoMap),m(SetInfoMap(String,MemberInfo,Object)),m(AddInfoMapItem(NamedParam)),m(PollForPrim(WorldObjects,Simulator)),m(get_IsTouchDefined),m(get_IsSitDefined),m(get_IsSculpted),m(get_IsPassable),m(set_IsPassable(Boolean)),m(get_IsPhantom),m(set_IsPhantom(Boolean)),m(get_IsPhysical),m(set_IsPhysical(Boolean)),m(get_InventoryEmpty),m(get_Sandbox),m(get_Temporary),m(get_AnimSource),m(get_AllowInventoryDrop),m(get_IsAvatar),m(Distance(SimPosition)),m(get_Prim),m(get_ObjectType),m(set_ObjectType(SimObjectType)),m(get_NeedsUpdate),m(get_IsKilled),m(RemoveCollisions),m(IsTypeOf(SimObjectType)),m(get_Children),m(get_HasChildren),m(get_Parent),m(set_Parent(SimObject)),m(AddChild(SimObject)),m(get_IsTyped),m(RateIt(BotNeeds)),m(GetTypeUsages),m(GetUsages),m(GetMenu(SimAvatar)),m(IsParentAccruate(Primitive)),m(UpdateOccupied0),m(UpdateOccupied1),m(UpdateOccupied2),m(AddSuperTypes(System.Collections.Generic.IList`1[[cogbot.TheOpenSims.SimObjectType, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),m(SuperTypeString),m(get_IsRegionAttached),m(GetSimScale),m(TryGetGlobalPosition(Vector3d&)),m(UpdatePosition(UInt64,Vector3)),m(TryGetGlobalPosition(Vector3d&,OutputDelegate)),m(TryGetSimPosition(Vector3&)),m(TryGetSimPosition(Vector3&,OutputDelegate)),m(get_SimPosition),m(set_SimPosition(Vector3)),m(GetParentPrim(Primitive,OutputDelegate)),m(GetParentPrim0(Primitive,OutputDelegate)),m(EnsureParentRequested(Simulator)),m(get_ParentGrabber),m(BadLocation(Vector3)),m(GetActualUpdate(String)),m(GetBestUse(BotNeeds)),m(GetProposedUpdate(String)),m(ToGlobal(UInt64,Vector3)),m(HasFlag(Object)),m(Error(String,arrayOf(Object))),m(SortByDistance(System.Collections.Generic.List`1[[cogbot.TheOpenSims.SimObject, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),m(CompareDistance(SimObject,SimObject)),m(CompareDistance(Vector3d,Vector3d)),m(DistanceVectorString(SimPosition)),m(DistanceVectorString(Vector3d)),m(DistanceVectorString(Vector3)),m(get_Mesh),m(BottemArea),m(GetGlobalLeftPos(Int32,Double)),m(IsInside(Vector3)),m(get_ActionEventQueue),m(set_ActionEventQueue(Queue`1)),m(get_ShouldEventSource),m(GetSimVerb),m(get_SitName),m(get_TouchName),m(get_IsAttachment),m(get_AttachPoint),m(get_IsAttachable),m(get_IsChild),m(set_IsChild(Boolean)),m(OnSound(UUID,Single)),m(OnEffect(String,Object,Object,Single,UUID)),m(get_IsSolid),m(set_IsSolid(Boolean)),m(get_IsUseable),m(set_IsUseable(Boolean)),m(DebugColor),m(get_IsDebugging),m(set_IsDebugging(Boolean)),m(get_IsMeshed),m(set_IsMeshed(Boolean)),m(get_Item(String)),m(set_Item(String,Object)),m(Equals(Object)),m(GetHashCode),m(GetType),m(Finalize),m(MemberwiseClone),c(SimAvatarImpl(UUID,WorldObjects,Simulator)),c(SimAvatarImpl),p(SelectedBeam(Boolean)),p(ProfileProperties(AvatarProperties)),p(AvatarInterests(Interests)),p(AvatarGroups(System.Collections.Generic.List`1[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null]])),p(IsKilled(Boolean)),p(theAvatar(Avatar)),p(SightRange(Double)),p(LastAction(BotAction)),p(CurrentAction(BotAction)),p(IsRoot(Boolean)),p(IsSitting(Boolean)),p(HasPrim(Boolean)),p(IsControllable(Boolean)),p(GlobalPosition(Vector3d)),p(SimRotation(Quaternion)),p(Flying(Boolean)),p(ApproachPosition(SimPosition)),p(ApproachVector3D(Vector3d)),p(KnownTypeUsages(IEnumerable`1)),p(IsDrivingVehical(Boolean)),p(IsWalking(Boolean)),p(IsFlying(Boolean)),p(IsStanding(Boolean)),p(IsSleeping(Boolean)),p(DebugLevel(Int32)),p(GroupRoles(Dictionary`2)),p(ConfirmedObject(Boolean)),p(UsePosition(SimPosition)),p(ZHeading(Single)),p(_propertiesCache(ObjectProperties)),p(RegionHandle(UInt64)),p(ID(UUID)),p(Properties(ObjectProperties)),p(PathStore(SimPathStore)),p(OuterBox(Box3Fill)),p(LocalID(UInt32)),p(ParentID(UInt32)),p(IsTouchDefined(Boolean)),p(IsSitDefined(Boolean)),p(IsSculpted(Boolean)),p(IsPassable(Boolean)),p(IsPhantom(Boolean)),p(IsPhysical(Boolean)),p(InventoryEmpty(Boolean)),p(Sandbox(Boolean)),p(Temporary(Boolean)),p(AnimSource(Boolean)),p(AllowInventoryDrop(Boolean)),p(IsAvatar(Boolean)),p(Prim(Primitive)),p(ObjectType(SimObjectType)),p(NeedsUpdate(Boolean)),p(Children(ListAsSet`1)),p(HasChildren(Boolean)),p(Parent(SimObject)),p(IsTyped(Boolean)),p(IsRegionAttached(Boolean)),p(SimPosition(Vector3)),p(ParentGrabber(TaskQueueHandler)),p(Mesh(SimMesh)),p(ActionEventQueue(Queue`1)),p(ShouldEventSource(Boolean)),p(SitName(String)),p(TouchName(String)),p(IsAttachment(Boolean)),p(AttachPoint(AttachmentPoint)),p(IsAttachable(Boolean)),p(IsChild(Boolean)),p(IsSolid(Boolean)),p(IsUseable(Boolean)),p(IsDebugging(Boolean)),p(IsMeshed(Boolean)),p(Item(Object)),f(BeamInfos(MushDLR223.Utilities.ListAsSet`1[[cogbot.TheOpenSims.EffectBeamInfo, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(SelectedObjects(MushDLR223.Utilities.ListAsSet`1[[PathSystem3D.Navigation.SimPosition, PathSystem3D, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(_debugLevel(Int32)),f(_SelectedBeam(Boolean)),f(_profileProperties(AvatarProperties)),f(_AvatarInterests(Interests)),f(_AvatarGroups(System.Collections.Generic.List`1[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(old(SimMoverState)),f(_SightRange(Double)),f(KnownSimObjects(ListAsSet`1)),f(_knownTypeUsages(MushDLR223.Utilities.ListAsSet`1[[cogbot.TheOpenSims.SimTypeUsage, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(_currentAction(BotAction)),f(actionThread(Thread)),f(actionLock(Object)),f(AspectName(String)),f(Client(BotClient)),f(TrackerLoopLock(Object)),f(IsBlocked(Boolean)),f(lastDistance(Double)),f(MoveToMovementProceedure(MovementProceedure)),f(GotoMovementProceedure(MovementProceedure)),f(MovementConsumer(Thread)),f(ApproachDistance(Double)),f(ApproachThread(Thread)),f(ExpectedCurrentAnims(InternalDictionary`2)),f(CurrentAnimSequenceNumber(Int32)),f(PostureType(String)),f(LastPostureEvent(SimObjectEvent)),f(postureLock(Object)),f(IsProfile(Boolean)),f(<LastAction>k__BackingField(BotAction)),f(<ApproachPosition>k__BackingField(SimPosition)),f(<ApproachVector3D>k__BackingField(Vector3d)),f(<GroupRoles>k__BackingField(Dictionary`2)),f(InTurn(Int32)),f(mergeEvents(Boolean)),f(UseTeleportFallback(Boolean)),f(ObjectMovementUpdateValue(ObjectMovementUpdate)),f(_Prim0(Primitive)),f(WorldSystem(WorldObjects)),f(WasKilled(Boolean)),f(_children(ListAsSet`1)),f(scaleOnNeeds(Single)),f(_Parent(SimObject)),f(RequestedParent(Boolean)),f(LastKnownSimPos(Vector3)),f(lastEvent(SimObjectEvent)),f(LastEventByName(System.Collections.Generic.Dictionary`2[[System.String, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[cogbot.TheOpenSims.SimObjectEvent, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(HasPrimLock(Object)),f(toStringNeedsUpdate(Boolean))]"


6 ?- cliFindType('ulong',X),cliToString(X,S).
X = @'C#40644616',
S = "System.UInt64".

6 ?- cliFindType('ulong&',X),cliToString(X,S).
X = @'C#40644616',
S = "System.UInt64".


3 ?- cliFindType('int',X),cliToString(X,S).
X = @'C#40644880',
S = "System.Int32".

4 ?- cliFindClass('int',X),cliToString(X,S).
X = @'C#40644792',
S = "int".

5 ?- cliFindClass('ulong',X),cliToString(X,S).
X = @'C#40644704',
S = "class cli.System.UInt64".

6 ?- cliFindType('ulong',X),cliToString(X,S).
X = @'C#40644616',
S = "System.UInt64".

7 ?- cliFindType('java.lang.String',X),cliToString(X,S).
X = @'C#40644608',
S = "System.String".

8 ?- cliFindClass('java.lang.String',X),cliToString(X,S).
X = @'C#40643064',
S = "class java.lang.String".

9 ?- cliFindType('System.String',X),cliToString(X,S).
X = @'C#40644608',
S = "System.String".

10 ?- cliFindClass('System.String',X),cliToString(X,S).
X = @'C#40643064',
S = "class java.lang.String".

11 ?- cliFindClass('cli.System.String',X),cliToString(X,S).
X = @'C#40643064',
S = "class java.lang.String".

cliFindClass('Dictionary'('int','string'),X)

8 ?- cliGet('System.UInt64','MaxValue',X),cliToString(X,S).
X = @'C#33826112',
S = "18446744073709551615".

cliToString(18446744073709551615,S).
cliToString(18446744073709551616,S).
cliToString(18446744073709551617,S).

1 ?- cliGetType(X,Y).
Y = @'C#8398216'.

2 ?- cliGetType(X,Y),cliToString(Y,W).
Y = @'C#8398216',
W = "SbsSW.SwiPlCs.PlTerm".

3 ?- cliGetType(1,Y),cliToString(Y,W).
Y = @'C#8398208',
W = "System.Int32".

4 ?- cliGetType(1.1,Y),cliToString(Y,W).
Y = @'C#8398200',
W = "System.Double".

5 ?- cliGetType(1.1,Y),cliToString(Y,W).
Y = @'C#8398200',
W = "System.Double".

6 ?- cliGetClass(1.1,Y),cliToString(Y,W).
Y = @'C#8398192',
W = "class cli.System.Double".

7 ?- cliGetClass(f,Y),cliToString(Y,W).
Y = @'C#8398184',
W = "class java.lang.String".

8 ?- cliGetClass('f',Y),cliToString(Y,W).
Y = @'C#8398184',
W = "class java.lang.String".


9 ?- cliGetType(1,Y),cliToString(Y,W).
Y = @'C#8398208',
W = "System.Int32".


9 ?- cliGetType(c(a),Y),cliToString(Y,W).

cliGetType('ABuildStartup.Program',Y),cliToString(Y,W).

cliLoadAssembly('Cogbot.exe'),cliCall('ABuildStartup.Program','Main',[],Y),cliToString(Y,W).
cliFindType('ABuildStartup.Program',Y),cliToString(Y,W).


System.InvalidOperationException occurred
  Message="DragDrop registration did not succeed."
  Source="System.Windows.Forms"
  StackTrace:
       at System.Windows.Forms.Control.SetAcceptDrops(Boolean accept)
  InnerException: System.Threading.ThreadStateException
       Message="Current thread must be set to single thread apartment (STA) mode before OLE calls can be made. Ensure that your Main function has STAThreadAttribute marked on it."
       Source="System.Windows.Forms"
       StackTrace:
            at System.Windows.Forms.Control.SetAcceptDrops(Boolean accept)
       InnerException:



MSP430F247TPM

onFriendsRightsUpdated(FriendInfo):-writeq([updated,FriendInfo]).

%% cliGet('System.UInt16','MaxValue',V).
%% cliGet('System.UInt32','MaxValue',V).
%% cliGet('System.UInt64','MaxValue',V).
%% cliGet('System.Char','MaxValue',V).   <- this crashes it :(  unmappable char!

%%% jpl_get('java.lang.Integer','MAX_VALUE',Out).
%%% jni_func(6, 'java.lang.Integer', Class).
%%% jni_func(6, 'java/lang/String', Class)

/*
jpl_versions_demo :-
	cliCall( 'jpl.JPL', version_string, [], Vj),
	jpl:jpl_c_lib_version( Vc),
	jpl_pl_lib_version( Vp),

	nl,
	write( 'prolog library version: '), write( Vp), nl,
	write( '  java library version: '), write( Vj), nl, %% this one returns a "string"
	write( '     c library version: '), write( Vc), nl,
	(	Vp == Vj,
		Vj == Vc
	->	write( 'BINGO! you appear to have the same version of each library installed'), nl
	;	write( 'WHOOPS! you appear not to have the same version of each library installed'), nl
	),
	nl.
*/

% this directive runs the above demo

%%:- jpl_versions_demo.



System.ArgumentException occurred

  Source="mscorlib"
  StackTrace:

  InnerException:

?- gridClient(Obj), cliGet(Obj,'Friends',NM), cliAddEventHandler(NM,'FriendRightsUpdate',onFriendsRightsUpdated(_)).

?- gridClient(Obj), cliGet(Obj,'Objects',NM), cliAddEventHandler(NM,'ObjectUpdate',objectUpdated(_)).


%% RegisteringFor IMs and Chat


   ?- botClient(Obj), cliAddEventHandler(Obj,'EachSimEvent',onSimEvent(_,_,_)).

, cliAddEventHandler(NM,'ObjectUpdate',objectUpdated(_)).

cliLoadAssembly('Cogbot.exe'),

java.util.zip.ZipException was unhandled
Message: error in opening zip file




:- use_module(library(jpl)).


 ?- cliVectorToArray(int(10,10,10),X),cliGetType(X,Y),cliToString(Y,Z).


 6 ?- cliVectorToArray(int(10,10,10),X),cliArrayToVector(X,Y).
 X = @'C#33904824',
 Y = 'System.Int32'(10, 10, 10).



jpl_jlist_demo :-
	jpl_new( 'javax.swing.JFrame', ['modules'], F),
	jpl_new( 'javax.swing.DefaultListModel', [], DLM),
	jpl_new( 'javax.swing.JList', [DLM], L),
	jpl_call( F, getContentPane, [], CP),
	jpl_call( CP, add, [L], _),
	(	current_module( M),
		jpl_call( DLM, addElement, [M], _),
		fail
	;	true
	),
	jpl_call( F, pack, [], _),
	jpl_call( F, getHeight, [], H),
	jpl_call( F, setSize, [150,H], _),
	jpl_call( F, setVisible, [@(true)], _).


% this directive runs the above demo

:- jpl_jlist_demo.



:- module(test_jpl,
	  [ run_tests/0,
	    run_tests/1
	  ]).
% ensure we get the local copies

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(jpl_examples, 'examples/prolog')).
:- asserta(user:file_search_path(jar, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(library(jpl)).
:- use_module(library(plunit)).

:- jpl:add_search_path('CLASSPATH', 'jpltest.jar').

:- begin_tests(jpl).

test(
	ancestor_types_1,
	[	true(
			Ts == [class([jpl],['Compound']),class([jpl],['Term']),class([java,lang],['Object'])]
		)
	]
) :-
	jpl:jpl_type_to_ancestor_types( class([jpl],['Atom']), Ts).

test(
	call_array_equals_1,
	[	setup((
			jpl_new( array(byte), [4,5,6], A1),
			jpl_new( array(byte), [4,5,6], A2)
		))
	]
) :-
	jpl_call( A1, equals, [A2], @(false)).

test(
	call_array_equals_2,
	[	setup((
			jpl_new( array(byte), [4,5,6], A1)
		))
	]
) :-
	jpl_call( A1, equals, [A1], @(true)).

test(
	call_array_hashcode_1,
	[	setup((
			jpl_new( array(byte), [4,5,6], A)
		)),
		true((
			integer( H)
		))
	]
) :-
	jpl_call( A, hashCode, [], H).

test(
	call_array_hashcode_2,
	[	setup((
			jpl_new( array(byte), [4,5,6], A1),
			jpl_new( array(byte), [4,5,6], A2)
		)),
		true((
			H1 \== H2
		))
	]
) :-
	jpl_call( A1, hashCode, [], H1),
	jpl_call( A2, hashCode, [], H2).

test(
	call_array_to_string_1,
	[	setup((
			jpl_new( array(byte), [4,5,6], A)
		)),
		true((
			atom_codes( S, [0'[, 0'B | _])
		))
	]
) :-
	jpl_call( A, toString, [], S).

test(
	call_instance_param_cyclic_term_1,
	[	setup((
			T = f(T),
			jpl_new( 'jpl.test.Test', [], Test)
		)),
		throws(
			error(type_error(acyclic,T),context(jpl_call/4,_))
		)
	]
) :-
	jpl_call( Test, methodInstanceTerm, [{T}], @(true)).

testX(
	call_instance_param_cyclic_term_2,
	[	setup((
			T = f(T),
			jpl_new( 'jpl.test.Test', [], Test)
		)),
		throws(
			error(type_error(acyclic,_),context(jpl_call/4,_))
		)
	]
) :-
	jpl_call( Test, methodInstanceTerm, [{T}], @(true)).

test(
	call_method_static_array_1,
	[	setup((
			jpl_new( array(int), [3,4,5], IntArray)
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticArray, [IntArray], 'int[]').

test(
	call_method_static_array_2,
	[	setup((
			jpl_new( array(byte), [3,4,5], ByteArray)
		)),
		throws(
			error(
				type_error(method_params,[ByteArray]),
				context(jpl_call/4,_)
			)
		)
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticArray, [ByteArray], _).

test(
	call_static_param_cyclic_term_1,
	[	setup((
			T = f(T)
		)),
		throws(
			error(type_error(acyclic,T),context(jpl_call/4,_))
		)
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticTerm, [{T}], @(true)).

test(
	call_class_get_name_1,
	[	setup((
			ClassName = 'java.lang.Integer',
			jpl_classname_to_class( ClassName, ClassObject)
		)),
		true((
			ClassName == ClassName2
		))
	]
) :-
	jpl_call( ClassObject, getName, [], ClassName2).

test(
	call_get_array_bad_field_name_1,
	[	setup((
			jpl_new( array(byte), 5, A),
			FieldName = colour
		)),
		throws(
			error(domain_error(array_field_name,FieldName),context(jpl_get/3,_))
		)
	]
) :-
	jpl_get( A, FieldName, _).

test(
	call_get_array_bad_fspec_1,
	[	setup((
			jpl_new( array(byte), 5, A),
			Fspec = poo(77)
		)),
		throws(
			error(type_error(array_lookup_spec,Fspec),context(jpl_get/3,_))
		)
	]
) :-
	jpl_get( A, Fspec, _).

test(
	call_get_array_bad_index_range_1,
	[	setup((
			jpl_new( array(byte), 5, A)
		)),
		throws(
			error(domain_error(array_index_range,(-1)-2),context(jpl_get/3,_))
		)
	]
) :-
	jpl_get( A, (-1)-2, _).

test(
	call_get_array_bad_index_range_2,
	[	setup((
			jpl_new( array(byte), 5, A)
		)),
		throws(
			error(domain_error(array_index_range,10-12),context(jpl_get/3,_))
		)
	]
) :-
	jpl_get( A, 10-12, _).

test(
	call_get_array_bad_index_range_3,
	[	setup((
			jpl_new( array(byte), 5, A)
		)),
		throws(
			error(domain_error(array_index_range,3-33),context(jpl_get/3,_))
		)
	]
) :-
	jpl_get( A, 3-33, _).

test(
	call_get_array_bad_index_range_4,
	[	setup((
			jpl_new( array(byte), 5, A)
		)),
		throws(
			error(type_error(array_index_range,this-that),context(jpl_get/3,_))
		)
	]
) :-
	jpl_get( A, this-that, _).

test(
	get_array_element_1,
	[	setup((
			jpl_new( array(byte), [4,5,6,7,8], A)
		)),
		true((
			7 == V
		))
	]
) :-
	jpl_get( A, 3, V). % should bind V = 7 i.e. a[3] i.e. the fourth array element counting from zero

test(
	get_array_elements_1,
	[	setup((
			jpl_new( array(byte), [4,5,6,7,8], A)
		)),
		true((
			[5,6] == V
		))
	]
) :-
	jpl_get( A, 1-2, V). % should bind V = [5,6] i.e. a[1-2] i.e. the 2nd to 3rd array elements counting from zero

test(
	get_array_length_1,
	[	setup((
			Len1 is 5,
			jpl_new( array(byte), Len1, A)
		)),
		true((
			Len1 == Len2
		))
	]
) :-
	jpl_get( A, length, Len2). % should bind Len2 to the (integer) value of Len1

test(
	get_array_negative_index_1,
	[	setup((
			BadIndex is -1,
			jpl_new( array(byte), 5, A)
		)),
		throws(
			error(domain_error(array_index,BadIndex), context(jpl_get/3,_))
		)
	]
) :-
	jpl_get( A, BadIndex, _).

test(
	get_array_unbound_fspec_1,
	[	setup((
			jpl_new( array(byte), 5, A)
		)),
		throws(
			error(instantiation_error,context(jpl_get/3,_))
		)
	]
) :-
	jpl_get( A, _, _).

test(
	get_field_static_boolean_1,
	[	true((
			V == @(false)
		))
	]
) :-
	jpl_get( 'jpl.test.Test', fieldStaticBoolean1, V).

test(
	get_field_static_boolean_2,
	[	true((
			V == @(true)
		))
	]
) :-
	jpl_get( 'jpl.test.Test', fieldStaticBoolean2, V).

test(
	get_field_static_char_1,
	[	true((
			V == 0
		))
	]
) :-
	jpl_get( 'jpl.test.Test', fieldStaticChar1, V).

test(
	get_field_static_char_2,
	[	true((
			V == 65535
		))
	]
) :-
	jpl_get( 'jpl.test.Test', fieldStaticChar2, V).

test(
	get_field_instance_byte_2,
	[	setup((
			jpl_new( 'jpl.test.Test', [], Test)
		)),
		true((
			V == -1
		))
	]
) :-
	jpl_get( Test, fieldInstanceByte2, V).

test(
	list_to_array_1,
	[	true((
			Type == array(byte)
		))
	]
) :-
	jpl_list_to_array( [1,2,3], A),
	jpl_object_to_type( A, Type).

test(
	method_static_byte_1,
	[	throws(
			error(
				type_error(method_params,[-129]),
				context(jpl_call/4,_)
			)
		)
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoByte, [-129], _).

test(
	method_static_echo_boolean_1,
	[	setup((
			jpl_false( V1)
		)),
		true((
			V1 == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoBoolean, [V1], V2).

test(
	method_static_echo_boolean_2,
	[	setup((
			jpl_true( V1)
		)),
		true((
			V1 == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoBoolean, [V1], V2).

test(
	method_static_echo_char_1,
	[	setup((
			V1 = 0
		)),
		true((
			V1 == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoChar, [V1], V2).

test(
	method_static_echo_char_2,
	[	setup((
			V1 = 65535
		)),
		true((
			V1 == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoChar, [V1], V2).

test(
	method_static_char_3,
	[	setup((
			V1 = -1
		)),
		throws(
			error(
				type_error(method_params,[V1]),
				context(jpl_call/4,_)
			)
		)
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoChar, [V1], _).

test(
	method_static_char_4,
	[	setup((
			V1 = 1.0
		)),
		throws(
			error(
				type_error(method_params,[V1]),
				context(jpl_call/4,_)
			)
		)
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoChar, [V1], _).

test(
	method_static_char_5,
	[	setup((
			V1 = a
		)),
		throws(
			error(
				type_error(method_params,[V1]),
				context(jpl_call/4,_)
			)
		)
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoChar, [V1], _).

test(
	method_static_echo_double_1,
	[	setup((
			V1 = 1.5
		)),
		true((
			V1 == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoDouble, [V1], V2).

test(
	method_static_echo_double_2,
	[	setup((
			V1 = 2
		)),
		true((
			V2 =:= float(V1)
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoDouble, [V1], V2).

test(
	method_static_echo_double_3,
	[	setup((
			(   current_prolog_flag( bounded, true)
		    ->  current_prolog_flag( max_integer, V1)
		    ;   V1 is 2**63-1
		    ),
			V2b is float(V1)
		)),
		true((
			V2 == V2b
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoDouble, [V1], V2).

test(
	method_static_echo_float_1,
	[	setup((
			V1 = 1.5
		)),
		true((
			V1 == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoFloat, [V1], V2).

test(
	method_static_echo_float_2,
	[	setup((
			V1 is 2,
			V2b is float(V1)
		)),
		true((
			V2 == V2b
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoFloat, [V1], V2).

test(
	method_static_echo_float_3,
	[	setup((
			(   current_prolog_flag( bounded, true)
		    ->  current_prolog_flag( max_integer, V1)
		    ;   V1 is 2**63-1 % was 2**99
		    ),
			V2b is float(V1)
		)),
		true((
			V2 == V2b
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoFloat, [V1], V2).

test(
	method_static_echo_float_4,
	[	blocked('we do not yet widen unbounded integers to floats or doubles'),
		setup((
			(   current_prolog_flag( bounded, true)
		    ->  current_prolog_flag( max_integer, V1)
		    ;   V1 is 2**99		% an unbounded integer
		    ),
			V2b is float(V1)
		)),
		true((
			V2 == V2b
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoFloat, [V1], V2).

test(
	new_abstract_class_1,
	[	setup((
			Classname = 'java.util.Dictionary'
		)),
		throws(
			error(
				type_error(concrete_class,Classname),
				context(jpl_new/3,_)
			)
		)
	]
) :-
	jpl_new( Classname, [], _).

test(
	new_array_boolean_from_val_1,
	[	setup((
			jpl_false( V)
		)),
		true((
			V == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', newArrayBooleanFromValue, [V], A),
	jpl_get( A, 0, V2).

test(
	new_array_double_from_val_1,
	[	setup((
			V is 1.5
		)),
		true((
			V == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', newArrayDoubleFromValue, [V], A),
	jpl_get( A, 0, V2).

test(
	new_array_float_from_val_1,
	[	setup((
			V is 1.5
		)),
		true((
			V == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', newArrayFloatFromValue, [V], A),
	jpl_get( A, 0, V2).

test(
	new_interface_1,
	[	setup((
			Classname = 'java.util.Enumeration'
		)),
		throws(
			error(
				type_error(concrete_class,Classname),
				context(jpl_new/3,_)
			)
		)
	]
) :-
	jpl_new( Classname, [], _).

test(
	new_param_cyclic_term_1,
	[	setup((
			T = f(T)
		)),
		throws(
			error(
				type_error(acyclic,T),
				context(jpl_new/3,_)
			)
		)
	]
) :-
	jpl_new( 'jpl.test.Test', [{T}], _).

test(
	prolog_calls_java_calls_prolog_1,
	[	true((
			V == @(true)
		))
	]
) :-
	jpl_new( 'jpl.Query', ['4 is 2+2'], Q),
	jpl_call( Q, hasSolution, [], V).

test(
	set_array_element_cyclic_term_1,
	[	setup((
			T = f(T),
			jpl_new( array(class([jpl,test],['Test'])), 5, A)
		)),
		throws(
			error(
				type_error(acyclic,T),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( A, 0, {T}).

test(
	set_array_elements_bad_type_1,
	[	setup((
			jpl_new( array(byte), 3, A)
		)),
		throws(
			error(
				type_error(array(byte),[128]),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( A, 0, 128).

test(
	set_array_length_1,
	[	setup((
			jpl_new( array(byte), 6, A)
		)),
		throws(
			error(
				permission_error(modify,final_field,length),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( A, length, 13).

test(
	set_field_bad_field_spec_1,
	[	setup((
			BadFieldName = 3.7
		)),
		throws(
			error(
				type_error(field_name,BadFieldName),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( 'jpl.test.Test', BadFieldName, a).

test(
	set_field_instance_cyclic_term_1,
	[	setup((
			T = f(T),
			jpl_new( 'jpl.test.Test', [], Test)
		)),
		throws(
			error(
				type_error(acyclic,T),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( Test, instanceTerm, {T}).

test(
	set_field_long_array_1,
	[	setup((
			jpl_new( array(long), [1,2,3], LongArray)
		))
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticLongArray, LongArray).

test(
	set_field_long_array_2,
	[	setup((
			jpl_new( array(int), [1,2,3], IntArray)
		)),
		throws(
			error(
				type_error('[J',IntArray),	% NB '[J' is *not* how the type was specified in the failing goal
				context(
					jpl_set/3,
					'the value is not assignable to the named field of the class'
				)
			)
		)
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticLongArray, IntArray).

test(
	set_field_object_array_1,
	[	setup((
			jpl_new( 'java.util.Date', [], Date),
			jpl_new( array(class([java,lang],['Object'])), [Date,Date], ObjArray)
		))
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticObjectArray, ObjArray).

test(
	set_field_static_bad_type_1,
	[	setup((
			BadVal = 27
		)),
		throws(
			error(
				type_error(boolean,BadVal),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticBoolean, BadVal).

test(
	set_field_static_boolean_1,
	[	setup((
			jpl_true( V)
		))
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticBoolean, V).

test(
	set_field_static_boolean_2,
	[	setup((
			jpl_false( V)
		))
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticBoolean, V).

test(
	set_field_static_boolean_bad_1,
	[	setup((
			BadVal = foo(bar)
		)),
		throws(
			error(
				type_error(field_value,BadVal),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticBoolean, BadVal).

test(
	set_field_static_cyclic_term_1,
	[	setup((
			T = f(T)
		)),
		throws(
			error(
				type_error(acyclic,T),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( 'jpl.test.Test', staticTerm, {T}).

test(
	set_field_static_final_int_1,
	[	setup((
			FieldName = fieldStaticFinalInt,
			Value = 6
		)),
		throws(
			error(
				permission_error(modify,final_field,FieldName),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( 'jpl.test.Test', FieldName, Value).

test(
	set_field_static_shadow_1,
	[	blocked('we do not yet resolve same-named shadowed fields')
	]
) :-
	jpl_set( 'jpl.test.ShadowB', fieldStaticInt, 3).

test(
	set_field_static_term_1,
	[	setup((
			T1 = foo(bar,33),
			T2 = bar(77,bing)
		)),
		true((
			T1 == T1a,
			T2 == T2a
		))
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticTerm, {T1}),
	jpl_get( 'jpl.test.Test', fieldStaticTerm, {T1a}),
	jpl_set( 'jpl.test.Test', fieldStaticTerm, {T2}),
	jpl_get( 'jpl.test.Test', fieldStaticTerm, {T2a}).

test(
	set_field_static_term_2,
	[	setup((
			T1 = foo(bar,33),
			T2 = bar(77,bing)
		))
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticTerm, {T1}),
	jpl_get( 'jpl.test.Test', fieldStaticTerm, {T1}),
	jpl_set( 'jpl.test.Test', fieldStaticTerm, {T2}),
	jpl_get( 'jpl.test.Test', fieldStaticTerm, {T2}).

test(
	set_get_array_element_boolean_1,
	[	setup((
			jpl_new( array(boolean), 3, A),
			V = @(false)
		)),
		true((
			V == Vr
		))
	]
) :-
	jpl_set( A, 2, V),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_boolean_2,
	[	setup((
			jpl_new( array(boolean), 3, A),
			V = @(true)
		)),
		true((
			V == Vr
		))
	]
) :-
	jpl_set( A, 2, V),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_boolean_3,
	[	setup((
			jpl_new( array(boolean), 3, A),
			V = bogus
		)),
		throws(
			error(
				type_error(array(boolean),[V]),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( A, 2, V).

test(
	set_get_array_element_byte_1,
	[	setup((
			jpl_new( array(byte), 3, A),
			V = 33
		)),
		true((
			V == Vr
		))
	]
) :-
	jpl_set( A, 2, V),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_byte_2,
	[	setup((
			jpl_new( array(byte), 3, A),
			V = 128
		)),
		throws(
			error(
				type_error(array(byte),[V]),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( A, 2, V).

test(
	set_get_array_element_char_1,
	[	setup((
			jpl_new( array(char), 3, A),
			V = 65535
		)),
		true((
			V == Vr
		))
	]
) :-
	jpl_set( A, 2, V),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_double_1,
	[	setup((
			jpl_new( array(double), 3, A),
			V = 2.5
		)),
		true((
			V == Vr
		))
	]
) :-
	jpl_set( A, 2, V),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_float_1,
	[	setup((
			jpl_new( array(float), 3, A),
			V = 7.5
		)),
		true((
			V == Vr
		))
	]
) :-
	jpl_set( A, 2, V),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_float_2,
	[	setup((
			jpl_new( array(float), 3, A),
			V is 2,
			VrX is float(V)
		)),
		true((
			VrX == Vr
		))
	]
) :-
	jpl_set( A, 2, V),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_float_3,
	[	setup((
			jpl_new( array(float), 3, A),
			(	current_prolog_flag( bounded, true)
			->	current_prolog_flag( max_integer, Imax)
			;	Imax is 2**63-1
			),
			VrX is float(Imax)
		)),
		true((
			VrX == Vr
		))
	]
) :-
	jpl_set( A, 2, Imax),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_long_1,
	[	setup((
			jpl_new( array(long), 3, A),
			(	current_prolog_flag( bounded, true)
			->	current_prolog_flag( max_integer, V)
			;	V is 2**63-1
			)
		)),
		true((
			V == Vr
		))
	]
) :-
	jpl_set( A, 2, V),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_long_2,
	[	setup((
			jpl_new( array(long), 3, A),
			(	current_prolog_flag( bounded, true)
			->	current_prolog_flag( max_integer, V)
			;	V is 2**63
			)
		)),
		throws(
			error(
				type_error(array(long),[V]),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( A, 2, V).

test(
	set_get_array_elements_boolean_1,
	[	setup((
			jpl_new( array(boolean), 3, A),
			Vf = @(false),
			Vt = @(true)
		)),
		true((
			Vf+Vt+Vf == Vr0+Vr1+Vr2
		))
	]
) :-
	jpl_set( A, 0, Vf),
	jpl_set( A, 1, Vt),
	jpl_set( A, 2, Vf),
	jpl_get( A, 0, Vr0),
	jpl_get( A, 1, Vr1),
	jpl_get( A, 2, Vr2).

test(
	set_get_field_static_long_1,
	[	setup((
			(   current_prolog_flag( bounded, true)
		    ->  current_prolog_flag( max_integer, V)
		    ;   V is 2**63-1
		    )
		)),
		true((
			V == V2
		))
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticLong, V),
	jpl_get( 'jpl.test.Test', fieldStaticLong, V2).

test(
	set_non_accessible_field_1,
	[	throws(
			error(
				existence_error(field,gagaga),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( 'jpl.test.Test', gagaga, 4).

test(
	terms_to_array_1,
	[]
) :-
	jpl_terms_to_array( [foo(bar)], A),
	jpl_object_to_type( A, array(class([jpl],['Term']))),
	jpl_get( A, length, 1),
	jpl_get( A, 0, T),
	jpl_call( T, toString, [], 'foo(bar)').

test(
	throw_java_exception_1,
	[	blocked('part of the error term is nondeterministic: we need to match with _'),
		throws(
			error(
				java_exception(@(_)),
				'java.lang.NumberFormatException'
			)
		)
	]
) :-
	jpl_call( 'java.lang.Integer', decode, [q], _).

test(
	versions_1,
	[	true((
			Vpl == Vc,
			Vc == Vjava
		))
	]
) :-
	jpl_pl_lib_version(Vpl),
	jpl_c_lib_version(Vc),
	jpl_call( 'jpl.JPL', version_string, [], Vjava).

%	JW: Mutual recursion check.  Moved from jpl.pl to here.  As the
%	callback is in module user, we define it there.

user:jpl_test_fac( N, F) :-
	(	N == 1
	->	F = 1
	;	N > 1
	->	N2 is N-1,
		jpl_call( 'jpl.test.Test', fac, [N2], F2),	% call its Java counterpart, which does vice versa
		F is N*F2
	;	F = 0
	).

test(fac10,
     [ true(N==3628800)
     ]) :-
     user:jpl_test_fac(10, N).

test(threads1,
	[	true((
			thread_create(jpl_call('java.lang.System', currentTimeMillis, [], _), ThreadId, []),
			thread_join(ThreadId, true)
		))
	]
) :-
	jpl_call('java.lang.System', currentTimeMillis, [], _).

test(threads2, true(X==true)) :-
	jpl_call('java.lang.System', currentTimeMillis, [], _),
	thread_create(jpl_call('java.lang.System', currentTimeMillis, [], _), ThreadId, []),
	thread_join(ThreadId, X).

test(threads3,
	[	true((
			length(Ss, 1000),
			sort(Ss, [true])
		))
	]
) :-
	jpl_call('java.lang.System', currentTimeMillis, [], _),
	findall(
		Status,
		(	between(1, 1000, _),
			thread_create(jpl_call('java.lang.System', currentTimeMillis, [], _), ThreadId, []),
			thread_join(ThreadId, Status)
		),
		Ss
	).

test(jref1,
	[	true((
			Term1 \== Term2,
			Term1 =@= Term2
		))
	]
) :-
	length(Term1, 5),
	jpl:jni_term_to_jref(Term1, JRef),
	jpl:jni_jref_to_term(JRef, Term2).

:- end_tests(jpl).
