:- use_module(library(directory), [
	file_property/3
   ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% COMPILATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic compile_time/2.

file_has_been_updated(File, Force) :-
	% Force the file to be re-loaded regardless.
	( Force =:= 1 -> true
        ; absolute_file_name(File, [access(exist), file_type(prolog)], AbsoluteFileName),
	  ( \+ source_file(AbsoluteFileName) ->
	    true
	  ; compile_time(AbsoluteFileName, CompileTime),
	    file_property(AbsoluteFileName, modify_time, ModifyTime),
	    ModifyTime @> CompileTime ->
	    true
	  )
        ).

{A,B} :- !,
	reload_file(A, 0),
	{B}.
{B} :- reload_file(B, 0).

force_reload_file(FileName) :-
	reload_file(FileName, 1).

reload_file(FileName, Force) :-
	% absolute_file_name(FileName, AbsoluteFileName),
	% current_stream(AbsoluteFileName, _Mode, _Stream),
	( file_has_been_updated(FileName, Force) ->
	  format('ABOLISHING ALL PREDICATES DEFINED IN ~q.~n', [FileName]),
	  abolish_all(FileName),
	  compile(FileName),
	  absolute_file_name(FileName, AbsoluteFileName),
	  datime(TimeStamp),
	  retractall(compile_time(AbsoluteFileName, _)),
	  assert(compile_time(AbsoluteFileName, TimeStamp)),
	  check_for_multiple_arities(AbsoluteFileName)
        ; true).

abolish_all(FileName) :- abolish_all_1(FileName, 0).

abolish_all_1(FileName, Suffix) :-
	( Suffix == 1 ->
	    ( atom_chars(FileName, FileNameString),
	      rev(FileNameString, ReversedFileNameString),
	      ReversedFileNameString = [108, 112, 46|_] ->
	      FileNameWithSuffix = FileName
	    ; concat_atom([FileName, '.pl'], FileNameWithSuffix)
	    )
	; FileNameWithSuffix = FileName
	),
	absolute_file_name(FileNameWithSuffix, AbsoluteFileNameWithSuffix),
	source_file(Module:Pred, AbsoluteFileNameWithSuffix),
        functor(Pred, Functor, Arity),
        abolish(Module:Functor, Arity),
        format('Abolished ~q in file ~w~n', [Functor/Arity,FileNameWithSuffix]),
	fail
      ; format('DONE ABOLISHING PREDICATES in file ~w.~n', [FileName]),
	nl.

check_for_multiple_arities(FileName) :-
	setof(Pred:Arities,
	      multiple_arity_predicate(FileName, Pred, Arities),
	      MultipleArityPredicates),
	!,
	nl,
	warn_multiple_arity_predicates(MultipleArityPredicates, FileName),
	nl.
check_for_multiple_arities(_).

multiple_arity_predicate(AbsoluteFileName, PredicateName, Arities) :-
	gen_pred_name(PredicateName, AbsoluteFileName, Predicate),
	functor(Predicate, PredicateName, _Arity),
	setof(Term, gen_pred_with_known_functor(PredicateName, AbsoluteFileName, Term), PredList),
	map_preds_to_arities(PredList, MultipleArities),
	sort(MultipleArities, Arities),
	Arities = [_,_|_].

gen_pred_name(PredicateName, FileName, SkeletalSpecification) :-
	source_file(SkeletalSpecification, FileName),
	functor(SkeletalSpecification, PredicateName, _).
gen_pred_name(PredicateName, FileName, SkeletalSpecification) :-
	source_file(_Module:SkeletalSpecification, FileName),
	functor(SkeletalSpecification, PredicateName, _).

gen_pred_with_known_functor(PredicateName, FileName, SkeletalSpecification) :-
	current_predicate(PredicateName,  SkeletalSpecification),
	source_file(SkeletalSpecification, FileName).
gen_pred_with_known_functor(PredicateName, FileName, SkeletalSpecification) :-
	current_predicate(PredicateName,  _Module:SkeletalSpecification),
	source_file(_Module:SkeletalSpecification, FileName).

map_preds_to_arities([], []).
map_preds_to_arities([Pred1|RestPreds], [Arity1|RestArities]) :-
	functor(Pred1, _Functor, Arity1),
	map_preds_to_arities(RestPreds, RestArities).
	
warn_multiple_arity_predicates([], _FileName).
warn_multiple_arity_predicates([Pred:ArityList|Rest], FileName) :-
	format('~NWARNING: Predicate "~q" in file~n~w has multiple arities: ~w~n',
	       [Pred, FileName, ArityList]),
	warn_multiple_arity_predicates(Rest, FileName).
