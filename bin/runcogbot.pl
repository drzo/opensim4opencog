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

assertIfNewRC(Gaf):-catch(call(Gaf),_,fail),!.
assertIfNewRC(Gaf):-asserta(Gaf).

:- assertIfNewRC(user:file_search_path(foreign, '.')).
:- assertIfNewRC(user:file_search_path(jpl_examples, 'examples/prolog')).
:- assertIfNewRC(user:file_search_path(jar, '.')).
:- assertIfNewRC(user:file_search_path(library, '.')).
:- assertIfNewRC(user:file_search_path(library, '../test')).
:- assertIfNewRC(user:file_search_path(test, '../test')).
:- assertIfNewRC(user:file_search_path(test, '../../test')).
:- assertIfNewRC(user:file_search_path(cogbot, './prolog/simulator')).
:- assertIfNewRC(user:file_search_path(cogbot, './prolog')).
:- assertIfNewRC(user:file_search_path(library, './prolog')).

:- use_module(cogbot(cogrobot)).
:- use_module(test(testpathfind)).

:-runSL.

end_of_file.
%------------------------------------------------------------------------------
 6 ?- botget('Inventory',Y),cli_memb(Y,p,Z).
X = @'C#589147000',
Y = @'C#742328240',
Z = p(0, 'Store', 'Inventory', [], [], 'CanRead'(true), 'CanWrite'(false), decl(static(false), 'InventoryManager'), access_pafv(true, false, false, false)) ;
false.

14 ?- botdo(say("hi there!")).
"Success say"
true.

15 ?- botdo(shout("hi there!")).
"Success shout"

cli_get('cogbot.TheOpenSims.SimTypeSystem','objectTypes',O),cli_get_type(O,T),cli_typespec(T,S).

3 ?- botcall(executeCommand("jump"),X),cli_writeln(X).
"Success Jump"
X = @'C#186521916'.

 22 ?- botcall(talk("hello world"),O).
 O = @null.


  botcall(talk("hello world from prolog!",0,enum('OpenMetaverse.ChatType','Shout')),O).

  botcall([name,indexOf(char('D'))],O).
  cli_to_tagged(sbyte(-1),O),cli_writeln(O).

 world_avatar(A),cli_get(A,'isonline',B),cli_get(A,'globalposition',C),cli_writeln(A-B-C).
 grid_object(X,struct('NamedParam',A,'FirstName')).

grid_object(X,struct('NamedParam',A,B)).
%------------------------------------------------------------------------------
% Inventory examples
%------------------------------------------------------------------------------
15 ?- botget(['Inventory','Store',rootfolder,name],Y).
Y = "My Inventory".

2 ?- botget(['Inventory','Store',rootnode,nodes,values],Y),cli_to_str(Y,Z).
Y = @'C#718980688',
Z = "System.Collections.Generic.Dictionary`2+ValueCollection[OpenMetaverse.UUID,OpenMetaverse.InventoryNode]".

4 ?- botget(['Inventory','Store',rootnode,nodes,values],Y),findall(S,(cli_col(Y,Z),cli_to_str(Z,S)),L),writeq(L).
["Scripts","Photo Album","*MD* Brown Leather Hat w/Bling","Body Parts","Notecards","Objects","Clothing","Landmarks","Textures","Gestures","boxed fem_talk","Calling Cards","Animations","Sounds","Trash","Lost And Found"]
Y = @'C#720558400',
L = ["Scripts", "Photo Album", "*MD* Brown Leather Hat w/Bling", "Body Parts", "Notecards", "Objects", "Clothing", "Landmarks", "Textures"|...].


%------------------------------------------------------------------------------
% world_avatar examples
%------------------------------------------------------------------------------
[1] 58 ?- world_avatar(X),cli_get(X,hasprim,@(true)),cli_to_str(X,S).
X = @'C#638101232',
S = "BinaBot Daxeline" ;
X = @'C#638101728',
S = "Nephrael Rajesh" ;
X = @'C#638111960',
S = "Trollerblades Wasp" ;
false.



v

[debug] 11 ?- world_avatar(X),cli_get(X,'DebugInfo',Y).
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

[debug] 14 ?- world_avatar(X),cli_get(X,'ActionEventQueue',Y),cli_col(Y,Z).
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

[1] 65 ?- world_avatar(X),cli_get(X,hasprim,@(true)),cli_to_str(X,S),cli_get(X,'ActionEventQueue',AEQ),cli_col(AEQ,EE),cli_to_dataTerm(EE,DATA).


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

%% cli_get('System.UInt16','MaxValue',V).
%% cli_get('System.UInt32','MaxValue',V).
%% cli_get('System.UInt64','MaxValue',V).
%% cli_get('System.Char','MaxValue',V).   <- this crashes it :(  unmappable char!

%%% jpl_get('java.lang.Integer','MAX_VALUE',Out).
%%% jni_func(6, 'java.lang.Integer', Class).
%%% jni_func(6, 'java/lang/String', Class)

/*
jpl_versions_demo :-
	cli_call( 'jpl.JPL', version_string, [], Vj),
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

?- gridclient_ref(Obj), cli_get(Obj,'Friends',NM), cli_add_event_handler(NM,'FriendRightsUpdate',onFriendsRightsUpdated(_)).

?- gridclient_ref(Obj), cli_get(Obj,'Objects',NM), cli_add_event_handler(NM,'ObjectUpdate',objectUpdated(_)).


%% RegisteringFor IMs and Chat


   ?- current_bot(Obj), cli_add_event_handler(Obj,'EachSimEvent',on_sim_event(_,_,_)).

, cli_add_event_handler(NM,'ObjectUpdate',objectUpdated(_)).

cli_load_assembly('Cogbot.exe'),

java.util.zip.ZipException was unhandled
Message: error in opening zip file




:- use_module(library(jpl)).


 ?- cli_VectorToArray(int(10,10,10),X),cli_get_type(X,Y),cli_to_str(Y,Z).


 6 ?- cli_VectorToArray(int(10,10,10),X),cli_ArrayToVector(X,Y).
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
	call_instance_param_cycli_c_term_1,
	[	setup((
			T = f(T),
			jpl_new( 'jpl.test.Test', [], Test)
		)),
		throws(
			error(type_error(acycli_c,T),context(jpl_call/4,_))
		)
	]
) :-
	jpl_call( Test, methodInstanceTerm, [{T}], @(true)).

testX(
	call_instance_param_cycli_c_term_2,
	[	setup((
			T = f(T),
			jpl_new( 'jpl.test.Test', [], Test)
		)),
		throws(
			error(type_error(acycli_c,_),context(jpl_call/4,_))
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
	call_static_param_cycli_c_term_1,
	[	setup((
			T = f(T)
		)),
		throws(
			error(type_error(acycli_c,T),context(jpl_call/4,_))
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
	new_array_boolean_From_val_1,
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
	new_array_double_From_val_1,
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
	new_array_float_From_val_1,
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
	new_param_cycli_c_term_1,
	[	setup((
			T = f(T)
		)),
		throws(
			error(
				type_error(acycli_c,T),
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
	set_array_element_cycli_c_term_1,
	[	setup((
			T = f(T),
			jpl_new( array(class([jpl,test],['Test'])), 5, A)
		)),
		throws(
			error(
				type_error(acycli_c,T),
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
	set_field_instance_cycli_c_term_1,
	[	setup((
			T = f(T),
			jpl_new( 'jpl.test.Test', [], Test)
		)),
		throws(
			error(
				type_error(acycli_c,T),
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
	set_field_static_cycli_c_term_1,
	[	setup((
			T = f(T)
		)),
		throws(
			error(
				type_error(acycli_c,T),
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


