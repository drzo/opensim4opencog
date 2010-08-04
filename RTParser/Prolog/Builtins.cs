#if VISUAL_STUDIO
#define debugging
#define arg1index
#define mswindows
#define newor
#endif

/*-----------------------------------------------------------------------------------------

  C#Prolog -- Copyright (C) 2007-2009 John Pool -- j.pool@ision.nl
                   Contributions 2009 by Lars Iwer -- lars.iwer@inf.tu-dresden.de

  This library is free software; you can redistribute it and/or modify it under the terms of
  the GNU General Public License as published by the Free Software Foundation; either version
  2 of the License, or any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for details, or enter 'license.' at the command prompt.

-------------------------------------------------------------------------------------------*/

namespace RTParser.Prolog
{
    public enum BI
    {
        none, abolish, abort, arg, append, assert, asserta, assertz, atom_, atom_chars,
        atomic, call, clause, clearall, compound, consult, copy_term, current_op_init, cut,
        dayname, dayofweek, dayofyear, debug, display, environment, eq_num, eq_str, expand_term,
        fail, float_, functor, ge_num, ge_ord, gensym, get, get0, ground, gt_num, gt_ord, halt,
        inc_counter, integer, is_, isdatetime, istimespan, le_num, le_ord, leapyear, length,
        list, listing, listing0, listing0X, listing0XN, listingX, listingXN, lt_num, lt_ord,
        match_regex, member, name, ne_num, ne_str, ne_uni, next_op, nl, nodebug, nonvar,
        nospy, nospyall, notrace, noverbose, now, number, numbervars, numcols, or, persistent,
        persistent_info, persistent_uncache, pp_defines, predicatePN, predicateX, put,
        query_timeout, quit, read, retract, retract_where, retractall, see, seek, seen,
        setof_add, setof_exit, setof_init, shell, silent, sort, spy, spypoints, statistics,
        string_, tab, tell, today, told, trace, undef_pred_action, undefineds, unifiable,
        univ, username, userroles, validdate, validtime, var, verbose, version, weekno,
        whereXY, write, writef, writef0, writeln, writeq, xml_term, xmltrace, jcall0, jpred0,
    }

    public class Builtins
    {
        public static string Predicates =
          @"&builtin
       :- op( 900,  fy, [\+, not, once]).
       :- op( 700, xfx, [=, \=, ==, \==, is, :=, =:, =:=, =\=, <, >, =<, >=, @<, @>, @=<, @>=, =.., ?=]).
       :- op( 600, xfy, :).
       :- op( 500, yfx, [+, -, #, xor, /\, \/]).
       :- op( 400, yfx, [*, /, //, <<, >>]).
       :- op( 300, xfx, [mod]).
       :- op( 200,  fy, [+, -, \]).
       :- op( 200, xfx, **).
       :- op( 200, xfy, ^).

       :- op( 500,  fx, [listing, listing0, spy, nospy]).

       :- op( 950, xfx, where).      % for persistent predicates
       :- op( 949, yfx, or).         % ...
       :- op( 948, yfx, and).        % ...
       :- op( 947, xfx, like).       % ...
       :- op( 947, xfx, between).    % ...
       :- op( 947, xfx, in).         % ...
       :- op( 947, fx,  containing). % ...
       :- op( 700, xfx, <>).         % ...

       license :- shell( write, '""GNU GENERAL PUBLIC LICENSE.doc""').

       !                          :== cut.
       call( X)                   :== call.
       '$metacall'( X)            :== call.
       fail                       :== fail.


       jcall0(ObjOrClz,MemberName,ArgsList,Result)         :== jcall0.
       jcall0(ObjOrClz,MemberName_ArgsList,Result)         :== jcall0.       
       jset0(ObjOrClz,MemberName,ArgsList,InValue)         :== jcall0.
       jget0(ObjOrClz,MemberName,ArgsList,OutValue)         :== jcall0.
       jpred(ObjOrClz,MemberName,ArgsList,Value)         :== jpred0.

       jcall_ext(MemberName,ArgsList,Result) :- jcall0(static('RTParser.Prolog.Ext'),MemberName,ArgsList,Result).
       jnew(ClassName,ArgsList,Result) :- jcall_ext('jnew',[ClassName,ArgsList],Result).

       X = X.
       true.

       \+( X) :- X, !, fail.
       \+( X).

       not( X) :- X, !, fail.
       not( X).

/*
       A naive implementation for the ; (or) operator would be:

       X ; Y :- call( X).
       X ; Y :- call( Y).

       This definition is incorrect. If X is compound and ends with a cut, Y will be selected on
       backtracking anyway, since the effect of a cut in a call argument is limited to the call itself
       (and according to ISO does not extend to the ';'. If it did, the second clause would not be tried).

       This has been solved by handling ; directly in ExecuteGoalList()

       The if-then-else below works correctly but could be treated similarly (i.e. in ExecuteGoalList())
*/

       X ; Y                      :== or. % just for preventing redefinition by the user

       C -> X ; Y :- C, !, X.
       C -> X ; Y :- !, Y.
       C -> X :- C, !, X.

       once( X) :- X, !.

       repeat.
       repeat :- repeat.

       consult( X)                :== consult.

%       asserta( X) :-
%         nonvar (X),
%         X = (P-->Q),  %%% this is not handled correctly. Is on the TODO-list
%         !,
%         expand_term( X, R),
%         asserta( R).
       asserta( X)                :== asserta.

%       assert( X) :-
%         nonvar (X),
%         X = (P-->Q),
%         !,
%         expand_term( X, R),
%         assert( R).
       assert( X)                 :== assert.

%       assertz( (P-->Q)) :-
%         nonvar (X),
%         X = (P-->Q),
%         !,
%         expand_term( X, R),
%         assertz( R).
       assertz( X)                :== assertz.

       retract( X) where Y        :== retract_where. % == where( retract( X), Y)
       X where Y                  :== whereXY.
       retract( X)                :== retract.
       retract( X) :- retract( X).

       retractall( X)             :== retractall.
       spy( X)                    :== spy.
       spy( X, [_|_])             :== spy.
       nospy( X)                  :== spy.
       nospyall                   :== nospyall.
       verbose                    :== verbose.
       noverbose                  :== noverbose.
       silent                     :== silent.
       trace                      :== trace.
       notrace                    :== notrace.
       debug                      :== debug.
       nodebug                    :== nodebug.

       setof( X, P, L) :-
         '$setof_init'( S),
         ( call( P),
           '$setof_add'( S, X),
           fail
         ;
           true
         ),
         '$setof_exit'( S, L).

       '$setof_init'( S)          :== setof_init.
       '$setof_add'( S, X)        :== setof_add.
       '$setof_exit'( S, L)       :== setof_exit.

       findall( X, P, L) :-
         !,
         setof( X, P, L).

       current_op( P, F, N) :-
         '$current_op_init'( P, F, N, S, I),
         '$next_op'( S, I, P, F, N).

       '$current_op_init'( P, F, N, S, I)
                                  :== current_op_init.

       '$next_op'( S, I, P, F, N) :== next_op.
       '$next_op'( S, I, P, F, N) :-
         I >= 0, % counter I is decreased at each call
         '$next_op'( S, I, P, F, N).

       version( V, R)             :== version.
       halt                       :== halt.
       quit                       :== quit.
       abort                      :== abort.

       length( L, N)              :== length.
       sort( L, S)                :== sort.
       functor( T, F, N)          :== functor.
       arg( N, T, A)              :== arg.
       abolish( X/N)              :== abolish.
       gensym( X)                 :== gensym.
       clause( H, B)              :== clause.

       var( V)                    :== var.
       nonvar( V)                 :== nonvar.
       atom( A)                   :== atom_.
       atomic( A)                 :== atomic.
       integer( V)                :== integer.
       float( V)                  :== float_.
       number( V)                 :== number.
       compound( V)               :== compound.
       list( L)                   :== list.
       string( V)                 :== string_.
       isdatetime( V)             :== isdatetime.
       istimespan( V)             :== istimespan.

       X is Y                     :== is_.
       X \= Y                     :== ne_uni.
       X == Y                     :== eq_str.
       X \== Y                    :== ne_str.
       X =:= Y                    :== eq_num.
       X =\= Y                    :== ne_num.
       X < Y                      :== lt_num.
       X =< Y                     :== le_num.
       X > Y                      :== gt_num.
       X >= Y                     :== ge_num.
       X @< Y                     :== lt_ord.
       X @=< Y                    :== le_ord.
       X @> Y                     :== gt_ord.
       X @>= Y                    :== ge_ord.
       X =.. Y                    :== univ.
       unifiable( X, Y)           :== unifiable.

%       X ?= Y :-
%         (A == B ; A \= B), !. % SWI-Prolog

       % I/O-predicates.

       see( F)                    :== see.
       read( X)                   :== read.
       get0( C)                   :== get0.
       get( C)                    :== get.
       seek( N, C)                :== seek.
       seen                       :== seen.
       tell( F)                   :== tell.
       write( X)                  :== write.
       writeq( X)                 :== write. %!!!!!!!! writeq is not yet correct
       writeln( X)                :== writeln.
       put( C)                    :== put.
       nl                         :== nl.
       tab( N)                    :== tab.
       display( X)                :== display.
       told                       :== told.
       ttyflush.

       % misc

       name( A, L)                :== name.
       atom_chars( A, L)          :== atom_chars.
       expand_term( (P-->Q), R)   :== expand_term.
       numbervars( X, B, E)       :== numbervars.
       'WRITEF' ( S, L)           :== writef0.
       writef( S, X) :- !, 'WRITEF'(S, [X]).
       writef( S, X1, X2) :- !, 'WRITEF'(S, [X1, X2]).
       writef( S, X1, X2, X3) :- !, 'WRITEF'(S, [X1, X2, X3]).
       writef( S, X1, X2, X3, X4) :- !, 'WRITEF'(S, [X1, X2, X3, X4]).
       writef( S)                 :== writef.
       username( X)               :== username.
       shell( X)                  :== shell.
       shell( X, Args)            :== shell.
       predicate( P/N)            :== predicatePN.
       predicate( T)              :== predicateX.
       persistent( P/N)           :== persistent.
       ground( X)                 :== ground.

       now( H, M, S)              :== now.
       validtime( H, M, S)        :== validtime.
       today( Y, M, D)            :== today.
       validdate( Y, M, D)        :== validdate.
       dayname( Y, M, D, N)       :== dayname.
       dayofweek( Y, M, D, N)     :== dayofweek.
       dayofyear( Y, M, D, N)     :== dayofyear.
       leapyear( Y)               :== leapyear.
       weekno( Y, M, D, N)        :== weekno.
       weekno( N)                 :== weekno.

       % Persistent predicates

       persistent_info( X/N, L)   :== persistent_info.
       persistent_uncache( X/N)   :== persistent_uncache.

       % XML (PLXML -- Google ""PLXML Prolog"")

       xml_term( X, P)            :== xml_term.
       xml_term( C, X, P)         :== xml_term. % controls: see below

       /* XMLPL 'CONTROLS' TO BE IMPLEMENTED:
        * At this release, the Controls applying to in-bound ( Chars -> Document)
        * parsing are:
        *
        *  extended_characters( <bool>)        : Use the extended character
        *                                     : entities for XHTML ( default true)
        *
        *  format( <bool>)                     : Strip layouts when no character data
        *                                     : appears between elements.
        *                                     : ( default true)
        *
        *  remove_attribute_prefixes(  <bool>) : Remove namespace prefixes from
        *                                     : attributes when it's the same as the
        *                                     : prefix of the parent element
        *                                     : ( default false).
        *
        *  [<bool> is one of 'true' or 'false']
        *
        * For out-bound ( Document -> Chars) parsing, the only available option is:
        *
        *  format( <Bool>)                     : Indent the element content
        *
        *  NEW ( JPO)
        *
        *  encoding(  <Encoding type> )        : Encoding (int32) to appear in XML-declaration, e.g. 65001 = 'utf-8'
        *                                        For a complete list, cf. Encodings.pl

        *  check_dtd(  <bool> )                : Read the referenced DTD and raise an
        *                                        exception + fail if it is not found.
        *                                        NOTE: only in the .NET Framework v2.0
        */

       /* xml_subterm(  +XMLTerm, ?Subterm ) unifies Subterm with a sub-term of Term.
        * Note that XMLTerm is a sub-term of itself.
        */
       xml_subterm( Term, Term).
       xml_subterm( xml(_Attributes,Content), Term) :-
         xml_subterm(  Content, Term).
       xml_subterm( [H|T], Term) :-
         (  xml_subterm( H, Term)
         ; xml_subterm( T, Term)
         ).
       xml_subterm( element(_Name,_Attributes,Content), Term) :-
         xml_subterm( Content, Term).
       xml_subterm( namespace(_URI,_Prefix,Content), Term) :-
         xml_subterm( Content, Term).

       % list functions

       listing                    :== listing.
       listing( X/N)              :== listingXN.
       listing( [X|Rest]) :-
         listing( X),
         !,
         listing( Rest).
       listing( []).
       listing( X)                :== listingX.

       % same, but for predefined predicates ( mainly for testing & debugging)

       listing0                   :== listing0.
       listing0( X/N)             :== listing0XN.
       listing0( [X|Rest]) :-
         listing0( X),
         !,
         listing0( Rest).
       listing0( []).
       listing0( X)               :== listing0X.

       pp_defines( X)             :== pp_defines. % preprocessor symbol definitions
       undefineds                 :== undefineds.
       copy_term( X, Y)           :== copy_term.
       undef_pred_action( X/N, A) :== undef_pred_action.
       clearall                   :== clearall.
       spypoints                  :== spypoints.

       memberchk( X, [Y|Rest]) :- nonvar( X), member( X, [Y|Rest]).
       % Actually member/2 given below is memberchk/2.
       % As member/2, it is not completely correct, e.g. it should backtrack on member( 1, [1,1,1]) !!!
       member( X, L)              :== member. % fails for unbound X or L. Disables backtracking upon success
       member( X, [X|_]).
       member( X, [_|L]) :- member( X, L).

%       append( X, Y, L)           :== append. % fails for non-list X or Y. Disables backtracking upon success
       append( [], X, X).
       append( [X|Y], U, [X|V]):-
         append(  Y, U, V).

       match_regex( S, P, L)      :== match_regex. % Find all occurances of match pattern P in string S

       xmltrace( X)               :== xmltrace.
       xmltrace( X, N)            :== xmltrace.
       numcols( N)                :== numcols. % number of columns in the DOS-box

       userroles ( X)             :== userroles.
       statistics( T, [MSec,_])   :== statistics.
       environment( X, Y)         :== environment.

       inc_counter( N)            :== inc_counter. % N is increased at each execution and not unbound in backtracking

       query_timeout( MSecs)      :== query_timeout. % must be entered as a separate query *before* the query you want to limit.

       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TEMP (samples, testing & experimental)

       :- op( 199, xfy, :).

       age(peter, 7).
       age(ann, 5).
       age(ann, 6).
       age(pat,8).
       age(tom, 5).


% Ivan Bratko, ""PROLOG Programming for Artificial Intelligence"", example at p.560
       
       sentence(Number) --> (noun_phrase(Number), verb_phrase(Number)).
       verb_phrase(Number) --> verb(Number), noun_phrase(Number).
       noun_phrase(Number) --> determiner(Number), noun(Number).
       determiner(singular) --> [a].
       determiner(singular) --> [the].
       determiner(plural) --> [the].
       noun(singular) --> [cat].
       noun(singular) --> [mouse].
       noun(plural) --> [cats].
       noun(plural) --> [mice].
       verb(singular) --> [scares].
       verb(singular) --> [hates].
       verb(plural) --> [scare].
       verb(plural) --> [hate].
       verb(plural) --> [hate];[love].

%       % Simplified example
%       sentence --> noun_phrase, verb_phrase.
%       verb_phrase --> verb, noun_phrase.
%       noun_phrase --> determiner, noun.
%       determiner --> [a].
%       determiner --> [the].
%       noun --> [cat].
%       noun --> [mouse].
%       noun --> [cats].
%       noun --> [mice].
%       verb --> [scares].
%       verb --> [hates].
%       verb --> [scare].
%       verb --> [hate];[love].

%       n --> [dog];[cat].
%       m --> [mouse].
%       e --> [].
%       ln :- listing0( n/2).
%       lm :- listing0( m/2).
%       le :- listing0( e/2).


% TALK (Fernando Pereira, Stuart Shieber), ""Prolog and Natural Language Analysis"", pp. 149+

       % A PDF-version of this book (obtained from Internet after googling
       % ""Fernando Pereira TALK-program"") can be found in the TALK directory.

       talk :- ['TALK\talk'].

       % Start the program by entering 'go.', end by entering an empty line.
       %
       % Sample dialog (TALK-responses not shown) (notice: no terminating dots):
       %
       % >> principia is a book
       % >> bertrand wrote every book
       % >> what did bertrand write
       

% CHAT-80 (Fernando Pereira) (http://www.cis.upenn.edu/~pereira/oldies.html)

       % See demo.txt in the CHAT-directory for examples.
       % Start CHAT by entering 'hi.', end by entering 'bye.'
       % The CHAT-software is copyrighted !!! (although I do not think F.Pereira still cares)

       chat :- consult([
        'chat-80\xgrun',     % XG (eXtended Grammar)
        'chat-80\newg',      % clone + lex
        'chat-80\clotab',    % attachment tables
        'chat-80\slots1',    % fits arguments into predicates
        'chat-80\scopes',    % quantification and scoping
        'chat-80\qplan',     % query planning
        'chat-80\talkr',     % query evaluation
        'chat-80\readin',    % sentence input
        'chat-80\ptree',     % print trees
        'chat-80\aggreg',    % aggregation operators
        'chat-80\templa',    % semantic dictionary
        'chat-80\slots2',    % fits arguments into predicates
        'chat-80\newdict',   % syntactic dictionary
        'chat-80\world',     % geographical data base (dating from 25 years ago!)
        'chat-80\rivers',    % ...
        'chat-80\cities',    % ...
        'chat-80\countries', % ...
        'chat-80\contain',   % ...
        'chat-80\borders',   % ...
        'chat-80\ndtabl',    % relation info
        'chat-80\newtop']).  % top level

       keysort(L, S) :- !, sort (L, S).

       conc([], L, L).
       conc([X|L1], L2, [X|L3]) :- conc(L1, L2, L3).

       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END of TEMP (testing)


reverse([], []):-!.
reverse([Head|Tail], Result):- reverse(Tail, Reduced),append(Reduced, [Head], Result),!.


       ";
    }
}
