:- multifile sccs_id/1.
:- dynamic sccs_id/1.

sccs_id('@(#)init.pl	1.12 04/17/07').

:- use_module(library(date), [
	datime/1
   ]).

:- use_module(library(environ), [
	environ/2
   ]).

:- use_module(library(files), [
	file_exists/1
   ]).

:- use_module(library(lists), [
	append/2
   ]).

:- use_module(library(strings), [
	concat_atom/2
   ]).

get_home_dir(HOME) :-
	( environ('MYHOME', HOME) ->
	  true
        ; environ('HOME', HOME)
	).

:- op(700, fy, bcl).       % BASE CLAUSE LISTING: allows   | ?- bcl <predicate>.
:- op(700, fy, bcc).       % BASE CLAUSE COUNT:   allows   | ?- bcc <predicate>. 
:- op(700, fy, ls).        % LISTING:             allows   | ?- l <predicate>. 
:- op(700, fy, u).         % run a Unix command
:- op(700, xfx, =?).       % testing equality

:- [messages].

:- [utils].

:- [startup].

:- ensure_loaded(library(emacsdebug)).
:- window_format(bindings,  _, [quoted(true),portrayed(true),max_depth(0)]).
:- window_format(source,    _, [quoted(true),portrayed(true),max_depth(0)]).
:- window_format(ancestors, _, [quoted(true),portrayed(true),max_depth(0)]).
:- emacs_debugger(_, on).

call_or_assertz(Term) :-
	( call(Term) ->
	  true
        ; assertz(Term)
        ).

call_or_asserta(Term) :-
	( call(Term) ->
	  true
        ; asserta(Term)
        ).

:- environ('SKR', SKR),
   concat_atom([SKR, '/prolog/'], SKRPrologUtilsDir),
   get_home_dir(HOME),
   concat_atom([HOME, '/specialist/SKR/prolog/'], HomePrologUtilsDir),
   call_or_asserta(file_search_path(prolog_utils, SKRPrologUtilsDir)),
   call_or_asserta(file_search_path(prolog_utils, HomePrologUtilsDir)).

:- ensure_loaded(prolog_utils(compilation)),
   absolute_file_name(prolog_utils(compilation), CompilationAbsoluteFileName),
   retractall(compile_time(CompilationAbsoluteFileName, _)),
   datime(TimeStamp),
   assert(compile_time(CompilationAbsoluteFileName, TimeStamp)).

:- environ('PWD', PWD),
   sub_atom(usemrep, PWD),
   { prolog_utils(semrep) }
 ; true.

% :- current_predicate('SAVED STATE!!', 'SAVED STATE!!'), retract_access_status ; true.

% defined in prolog_utils(startup)
:- init_application_environment.

:- absolute_file_name(prolog_utils(init), InitAbsoluteFileName),
   retractall(compile_time(InitAbsoluteFileName, _)),
   datime(TimeStamp),
   assert(compile_time(InitAbsoluteFileName, TimeStamp)).
