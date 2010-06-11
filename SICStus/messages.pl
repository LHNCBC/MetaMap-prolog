%%% Code provided by Per Mildner via e-mail 07/09/2009
%%% to store and re-play warning and error messages
%%% generated during compilation.

:- dynamic collecting_messages/0.
:- dynamic collected/3.

%% initially true
collecting_messages.

:- multifile user:message_hook/3.

%% Records messages for later re-play
user:message_hook(Severity, Message, Lines) :-
	collecting_messages,
	memberchk(Severity, [warning, error]),
	assertz(collected(Severity, Message, Lines)),
	%% also do default processing
	fail.
  
%% This will be called by top-level on each prompt.
user:message_hook(informational,prompt(_,_,_,_,_),_) :-
	%% Ensure we only print collected
	%% messages at first prompt.
	collecting_messages,
	retractall(collecting_messages),
	print_collected_messages,
	%% let default processing handle the prompt
	fail.

print_collected_messages :-
	( collected(_, _, _) -> % >= 1 messages
	  format(user_error, '~N### COLLECTED ERRORS~n',[]),
	  ( collected(S, M, L),
	    print_collected_message(S, M, L),
	    nl(user_error),
	    fail
	  ; retractall(collected(_, _, _))
	  )
	; true
	).

print_collected_message(Severity, _Message, Lines) :-
	print_message_lines(user_error, Severity, Lines).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Allow non-interactive tracing of specific predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_message_hook_term(ValidPredicates, MessageHookTerm) :-
	( ValidPredicates == [] ->
	  format(user_output, '~nNo valid predicates to trace.~n', []),
	  !,
	  fail
   	; MessageHookTerm = ((
	    user:message_hook(help, debugger(_,_,_,_,_,_,_,Goal,_,_),_) :-
		    ( Goal = _Module:Term ->
		      functor(Term, F, A),
		      \+ memberchk(F/A, ValidPredicates)
		    ; functor(Goal, F, A),
		    \+ memberchk(F/A, ValidPredicates)
		    ),
		    !))
	).

write_message_hook_file(MessageHookTerm, MessageHookFile) :-
	MessageHookFile = '/var/tmp/message_hook_$$.pl',
	open(MessageHookFile, write, OutputStream),
	set_output(OutputStream),
	% portray_clause((:- abolish(message_hook/3, [force(true)]))),
	% nl(OutputStream),
	portray_clause((:- multifile user:message_hook/3)),
	nl(OutputStream),
	% portray_clause((:- set_prolog_flag(single_var_warnings, off))),
	% nl(OutputStream),
	portray_clause(MessageHookTerm),
	nl(OutputStream),
	% portray_clause((:- set_prolog_flag(single_var_warnings, on))),
	% nl(OutputStream),
	% portray_clause((:- emacs_debugger(_, off), leash([]), trace)),
	% nl(OutputStream),
	close(OutputStream).
	
notrace_predicate :- no_trace_predicate.

no_trace_predicate :-
	notrace,
	leash(full),
	% emacs_debugger(_, on),
	abolish(message_hook/3, [force(true)]).

trace_predicate(Predicate, Arity) :-
	no_trace_predicate,
	trace_all_predicates([Predicate/Arity]).

trace_predicate(Predicates) :-
	no_trace_predicate,
	( atom(Predicates) ->
	  expand_one_predicate(Predicates, ExpandedPredicateList)
	; Predicates = _Functor/_Arity ->
	  ExpandedPredicateList = [Predicates]
	; expand_predicates(Predicates, ExpandedPredicateList)
	),
	trace_all_predicates(ExpandedPredicateList).

trace_all_predicates(Predicates) :-
	validate_predicates(Predicates, ValidPredicates),
	nl(user_output),
	announce_traced_predicates(ValidPredicates),
	nl(user_output),
	create_message_hook_term(ValidPredicates, MessageHookTerm),
	write_message_hook_file(MessageHookTerm, MessageHookFile),
	compile(MessageHookFile).
	
expand_predicates([], []).
expand_predicates([H|T], ExpandedPredicates) :-
	expand_one_predicate(H, ExpandedH),
	expand_predicates(T, ExpandedT),
	append(ExpandedH, ExpandedT, ExpandedPredicates).

expand_one_predicate(Functor/Arity, [Functor/Arity]) :- !.
expand_one_predicate(PredicateName, ExpandedPredicates) :-
	atom(PredicateName),
	( setof(Functor/Arity,
		Module^Skeletal^(current_predicate(PredicateName, Module:Skeletal),
			functor(Skeletal, Functor, Arity)),
	        ExpandedPredicates) ->
	  true
	; ExpandedPredicates = []
	).

validate_predicates([], []).
validate_predicates([H|T], ValidatedPredicates) :-
	H = Functor/Arity,
	( current_predicate(Functor, _Module:SkeletalSpecification),
	  functor(SkeletalSpecification, Functor, Arity) ->
	  ValidatedPredicates = [H|RestValidatedPredicates]
	; current_predicate(Functor, _SkeletalSpecification) ->
	  ValidatedPredicates = [H|RestValidatedPredicates]
	; format('~n~q/~w is not a known predicate~n', [Functor,Arity]),
	  ValidatedPredicates = RestValidatedPredicates
	),
	validate_predicates(T, RestValidatedPredicates).

announce_traced_predicates([]).
announce_traced_predicates([H|T]) :-
	H = Functor/Arity,
	announce_one_traced_predicate(Functor, Arity),
	announce_traced_predicates(T).

announce_one_traced_predicate(Functor, Arity) :-
	format(user_output, 'Tracing ~q/~w~n', [Functor, Arity]).
