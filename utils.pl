:- use_module(library(basics), [
	member/2,
	memberchk/2
   ]).

:- use_module(library(caseconv), [
	lower/2
   ]).

:- use_module(library(environ), [
	environ/2
   ]).


A =? B :-
	format(user_output,
	       '~2n~*c~2nATTTEMPTING TO UNIFY~2n~q~2nAND~2n~q~2n', [80,35,A,B]),
	( A = B ->
	  format(user_output, 'UNIFICATION SUCCEEDED~2n~*c~n', [80,35])
        ; format(user_output, 'UNIFICATION FAILED.~2n~*c~n', [80,35]),
	  !,
	  fail
        ).

h    :- halt.

ini  :- force_reload_file(myhome('prolog.ini')).
pcc  :-
	environ('HOME', HOME),
	concat_atom([HOME, '/specialist/bin/pcc.pl'], PCCFile),
	compile(PCCFile).

show_all_preds :-
	source_file(Pred, File),
	functor(Pred, Func, Arity),
	format(user_output, '~a/~d:~40|~a~n', [Func,Arity,File]),
	fail
      ; true.

d   :- debugging.
n   :- nospyall, nodebug, current_spypoint(S), remove_spypoint(S), fail ; true.

sas :- show_all_streams.
% show all streams
show_all_streams :-
	write_all(FileName-StreamType-StreamID, current_stream(FileName,StreamType,StreamID)).

% close all streams
cas :-
	current_stream(FileName, StreamType, StreamID),
	format(user_output, '~w~n', [FileName-StreamType-StreamID]),
	close(StreamID),
	fail
      ; true.

% file search path
fsp :- listing(file_search_path).

% show current modules
scm :- write_all(M, current_module(M)).

ins :- {'~/prolog/instrument.pl'}.


%%% :- abolish(unknown_predicate_handler,3).

%%% :- prolog_flag(character_escapes, _, on).

%%% unknown_predicate_handler(_, _, _).

unknown_predicate_handler(Goal, Module, fail) :-
	functor(Goal, Functor, Arity),
	\+ predicate_property(Goal, (dynamic)),
	format_to_all('~3n~10c ERROR!!! ~60c~n', [35,35]),
	warn_unknown_module(Module),
	format_to_all('Predicate ~q/~q is UNKNOWN in module ~w~n~n', [Functor,Arity,Module]),
        ( setof(Functor/OtherArity,
	        Property^OtherArity^(predicate_property(Module:Predicate, Property),
	                             functor(Predicate, Functor, OtherArity),
			             OtherArity \== Arity),
	  	PredicateList) ->
	  format_to_all('Predicate(s) with same name but different arity in the ~w module: ~n',
		 [Module]),
	  write_list_with_format_to_all(PredicateList, '* ~w~n'),
	  nl
	; setof(OtherModule:Functor/OtherArity,
	        Predicate^Property^(predicate_property(OtherModule:Predicate, Property),
	                             functor(Predicate, Functor, OtherArity)),
	  	PredicateList) ->
	  format_to_all('Predicate(s) with same name in other modules:~n', []),
	  write_list_with_format_to_all(PredicateList, '* ~w~n')
        ; true
        ),
	format_to_all('~80c~n', [35]),
	flush_all,
	abort.

warn_unknown_module(Module) :-
	( current_module(Module) ->
	  true
      	; format_to_all('Module ~w is UNKNOWN~n', [Module])
    	).

% format output to all output streams, if any, and then to user_output
format_to_all(FormatString, Args) :-
	current_stream(_File, write, OutputStream),
	format(OutputStream, FormatString, Args),
	fail.
format_to_all(FormatString, Args) :-
	format(user_output, FormatString, Args).

flush_all :- 
	current_stream(_File, write, OutputStream),
	flush_output(OutputStream),
	fail.
flush_all :- flush_output(user_output).

base_clause_count(Predicate, Arity, Num) :-
	( Predicate = _M:Pred ->
	  P = Pred
        ; P = Predicate
        ),
	current_predicate(P, Module:Skeletal),
	functor(Skeletal, P, Arity),
	bagof(Skeletal, call(Module:Skeletal), List),
	length(List, Num).

%% can be used for compiled, as well as dynamic, predicates;
%% the vanilla listing/1 built-in cannot be used for compiled predicates.
base_clause_listing(Predicate) :-
	current_predicate(Predicate, Module:Skeletal),
	!,
	base_clause_listing_1(Predicate, Module, Skeletal).
base_clause_listing(Predicate) :-
	format(user_output, 'No predicate ~w found~n', [Predicate]).

base_clause_listing_1(Predicate, Module, Skeletal) :-
	functor(Skeletal, Predicate, Arity),
	format(user_output, '~n~q:~q/~q~n', [Module,Predicate,Arity]),
	setof(Skeletal, Module^Skeletal^call(Module:Skeletal), SolutionList),
	write_list_with_format(SolutionList, '~q~n'),
	fail
      ; true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bcl Predicate 	    :- base_clause_listing(Predicate).
ls Predicate   	    :- listing(Predicate).
lsa           	    :- listing.

% ms  Predicate 	    :- myspy(Predicate).
% s   Predicate 	    :- spy Predicate.
u Command     	    :- unix(system(Command)).

% assert control options
aco(Option) :-
	var(Option),
	!,
	format(user_output, '~nCannot assert a variable as a control option!~n', []),
	fail.

aco(Option) :-
	program_name(ProgramName),
	aco_1(Option, ProgramName).
	
aco_1([H|T], ProgramName) :- assert_control_options([H|T], ProgramName).
aco_1(Option, ProgramName) :-
	atom(Option),
	assert_one_control_option(Option, ProgramName).

assert_control_options([], _ProgramName).
assert_control_options([ShortControlOption|RestShortControlOptions], ProgramName) :-
	assert_one_control_option(ShortControlOption, ProgramName),
	assert_control_options(RestShortControlOptions, ProgramName).

assert_one_control_option(ShortControlOption, ProgramName) :-
	( nls_system:is_control_option(usemrep, ShortControlOption, ControlOption, _, _) ->
	      ( nls_system:control_option(ControlOption) ->
		format(user_output, '~NControl option ~w (-~w) is already in effect~n',
		       [ControlOption,ShortControlOption])
	      ; ProgramName:override_control_option(assert, ControlOption) ->
		format(user_output, '~NControl option ~w (-~w) has already been overridden~n',
		       [ControlOption,ShortControlOption])
	      ; assert(nls_system:control_option(ControlOption)),
	        assert(ProgramName:override_control_option(assert, ControlOption)),
	        format(user_output, '~NASSERTED control option ~w (-~w)~n',
		       [ControlOption,ShortControlOption])
	      )
	 ; format(user_output, '~NERROR: ~q is not a known usemrep control option.~n',
		   [ShortControlOption])
	 ).

% delete control options

dco(Option) :-
	program_name(ProgramName),
	dco_1(Option, ProgramName).

dco_1(Option, ProgramName) :-
	( var(Option) ->
	  true
	; atom(Option) ->
	  true
	),
	!,
	delete_one_control_option(Option, ProgramName).

dco_1([H|T],  ProgramName) :- !, delete_control_options([H|T],  ProgramName).
dco_1(Option, ProgramName) :- delete_one_control_option(Option, ProgramName).

delete_control_options([], _ProgramName).
delete_control_options([ShortControlOption|RestShortControlOptions], ProgramName) :-
	delete_one_control_option(ShortControlOption, ProgramName),
	delete_control_options(RestShortControlOptions, ProgramName).

delete_one_control_option(ShortControlOption, ProgramName) :-
	var(ShortControlOption),
	!,
	retract(nls_system:control_option(ControlOption)),
	assert(ProgramName:override_control_option(retract, ControlOption)),
	nls_system:is_control_option(usemrep, ShortControlOption, ControlOption, _, _),
	format(user_output, '~NRETRACTED control option ~w (-~w)~n',
	       [ControlOption,ShortControlOption]).

delete_one_control_option(ShortControlOption, ProgramName) :-
	( nls_system:is_control_option(usemrep, ShortControlOption, ControlOption, _, _) ->
	     ( nls_system:control_option(ControlOption) ->
	       retract(nls_system:control_option(ControlOption)),
	       assert(ProgramName:override_control_option(retract, ControlOption)),
	       format(user_output, '~NRETRACTED control option ~w (-~w)~n',
		      [ControlOption,ShortControlOption])
	       ; format(user_output, '~NERROR: ~q (~w) is not currently asserted as a usemrep control option.~n',
			 [ShortControlOption,ControlOption])
		)
	; format(user_output, '~NERROR: ~q is not a known usemrep control option.~n',
		 [ShortControlOption])
	).


% override files
orf :-
	program_abbreviation(Abbrev),
	program_name(Name),
	program_version(Version),
	format(user_output, 'New input file: ', []),
	read(NewInputFile),
	retractall(Name:override_file(infile, read, _)),
	retractall(Name:override_file(outfile, write, _)),
	( NewInputFile == end_of_file -> % type Control-C Control-D to prompt
	  true
	; assert(Name:override_file(infile, read, NewInputFile)),
	  concat_atom([NewInputFile, '.', Abbrev, '.', Version], NewOutputFile),
	  assert(Name:override_file(outfile, write, NewOutputFile))
	 ).

saved_state_name('~/SAW/USemrep/usemrep_main/save_usemrep').

sav   :-
	usemrep:shutdown_usemrep,
	assert('SAVED STATE!!'),
	saved_state_name(SavedStateName),
	save_program(SavedStateName).

rsav  :-
	saved_state_name(SavedStateName),
	restore(SavedStateName).
	
% bcc Predicate/Arity :- base_clause_count(Predicate, Arity, Num), write(Predicate/Arity-Num), nl, fail ; true.
bcc Predicate :-
	( Predicate = Pred/_Arity ->
	  P = Pred
        ; Predicate = _Module:Pred/_Arity ->
	  P = Pred
        ; P = Predicate
        ),
	base_clause_count(P, Arity, Num),
	format(user_output, '~w~n', [P/Arity-Num]),
	fail
      ; true.


find_source_file(String) :-
	source_file(FileName),
	% satisfy this goal at most once!!
	( sub_atom(String, FileName) -> true ),
	format(user_output, '~w~n', [FileName]),
	fail
      ; true.

interesting_file(F) :-
	source_file(F),	
	\+ sub_atom(quintus, F),
	\+ sub_atom(qplib, F),
	\+ sub_atom('nls/specialist', F),
	\+ sub_atom('nls/skr', F).
	
n0b(N) :- bagof(Pred/Prop, predicate_property(Pred,Prop), List), length(List, N).
n1b(N) :- bagof(Pred/Prop, no_module_predicate_property(Pred, _Mod, Prop), List), length(List, N).

n0s(N) :- setof(Pred/Prop, predicate_property(Pred,Prop), List), length(List, N).
n1s(N) :- setof(Pred/Prop, no_module_predicate_property(Pred, _Mod, Prop), List), length(List, N).

n1sp :- setof(Pred/Prop, no_module_predicate_property(Pred, _Mod, Prop), List),
	sort(List, SortedList),
	length(List, N),
	format(user_output, '~w~n', [N]),
	write_list_with_format(SortedList, '~w~n').

show_user_defined_preds :-
	setof(Specification, user_defined_predicate(Specification), List),
	write_list_with_format(List, '~w~n').

user_defined_predicate(Functor/Arity) :-
	no_module_predicate_property(SkeletalSpecification, _Module, _Prop),
	functor(SkeletalSpecification, Functor, Arity),
	\+ no_module_predicate_property(SkeletalSpecification, _Module, built_in).

% This should backtrack through all predicates that are either
%  * visible in the current module, or
%  * built in
no_module_predicate_property(Specification, user, Property) :-
	predicate_property(Specification, Property).

no_module_predicate_property(Specification, Module, Property) :-
	predicate_property(Module:Specification, Property),
	\+ predicate_property(Module:Specification, built_in).

find_predicate(0, Atom, Predicate, Arity, SourceFileOrPropertyList) :-
	predicate_property(SkeletalSpecification, _Property),
	functor(SkeletalSpecification, Predicate, Arity),
	sub_atom_ignore_case(Atom, Predicate),
	setof(SourceFileOrProperty,
	      source_file_or_predicate_property(SkeletalSpecification, SourceFileOrProperty),
	      SourceFileOrPropertyList).

find_predicate(1, Atom, Predicate, Arity, Module) :-
	% no_module_predicate_property(SkeletalSpecification, Module, _Property),
	% source_file(Module:SkeletalSpecification, _File),
	predicate_property(Module:SkeletalSpecification, Property),
	Property \== built_in,
	functor(SkeletalSpecification, Predicate, Arity),
	sub_atom_ignore_case(Atom, Predicate).

source_file_or_predicate_property(SkeletalSpecification, FileOrProperty) :-
	source_file(SkeletalSpecification, FileOrProperty)
      ; predicate_property(SkeletalSpecification, FileOrProperty).

% returns a list of elements of the form
% index/3-['/net/nls3/export/home/quintus/quintus3.5/generic/qplib3.5/library/strings.pl',
%          compiled,(locked),imported_from(strings)]
find_all_predicates(0, Atom, PredicateList) :-
	setof(PredicateName/Arity-SourceFileOrPropertyList,
              find_predicate(0, Atom, PredicateName, Arity, SourceFileOrPropertyList),
	      PredicateList).

% returns a list of elements of the form
% domain_processing:add_msu_index/3
find_all_predicates(1, Atom, PredicateList) :-
	setof(PredicateName/Arity,
              Module^find_predicate(1, Atom, PredicateName, Arity, Module),
	      PredicateList).

write_all_predicate_lists([]).
write_all_predicate_lists([H|T]) :-
	write_one_predicate_from_list(H),
	write_all_predicate_lists(T).

write_one_predicate_from_list(PredicateName/Arity) :-
	!,
	write_one_predicate_from_list(_Module:PredicateName/Arity).

write_one_predicate_from_list(Module:PredicateName/Arity) :-
	!,
	functor(SkeletalSpecification, PredicateName, Arity),
	setof(SourceFileOrProperty,
	      Module^source_file_or_predicate_property(Module:SkeletalSpecification, 
	      					SourceFileOrProperty),
	      SourceFileOrPropertyList),
	write_one_predicate(PredicateName/Arity-SourceFileOrPropertyList),
	nl,
	ttyflush.
write_one_predicate_from_list(PredicateName) :-
	write_one_predicate_from_list(_Module:PredicateName/_Arity).

%	setof(PredicateName/Arity:SourceFileOrProperty,
%              find_predicate(Private, Atom, PredicateName, Arity, SourceFileOrProperty),
%	      PredicateList),
%	      write_predicate_list_nl(PredicateList).

find_all_predicates2(Atom1, Atom2) :-
	Atom1 \== Atom2,
	setof(PredicateName/Arity,
	      Module^find_predicate(1, Atom1, PredicateName, Arity, Module),
	      PredicateList),
	matching_predicates(PredicateList, Atom2, MatchingPredicates),
	write_all_predicate_lists(MatchingPredicates).

matching_predicates([], _Atom2, []).
matching_predicates([First|Rest], Atom2, Matching) :-
	First = PredicateName/_Arity,
	( sub_atom(Atom2, PredicateName) ->
	  Matching = [First|RestMatching]
	; Matching = RestMatching
	),
	matching_predicates(Rest, Atom2, RestMatching).


remove_module(Specification, SkeletalSpecification) :-
	( Specification = ModuleName:PossibleSkeletalSpecification ->
	  ( var(ModuleName),
	    var(PossibleSkeletalSpecification) ->
	    SkeletalSpecification = ':'
           ; current_module(ModuleName) ->
	     SkeletalSpecification = PossibleSkeletalSpecification
	   )
        ; SkeletalSpecification = Specification
        ).

write_predicate_list_nl([]).
% all list elements should be of the form PredName/Arity:PropertyList
write_predicate_list_nl([First|Rest]) :-
	write_one_predicate(First),
	nl,
	write_predicate_list_nl(Rest).

write_one_predicate(Module:PredName/Arity-List) :-
	format(user_output, '~w~n', [Module:PredName/Arity]),
	write_list_with_format(List, '   *~w~n').
write_one_predicate(PredName/Arity-List) :-
	format(user_output, '~w~n', [PredName/Arity]),
	write_list_with_format(List, '   *~w~n').

find_pred_source_file(Functor, SourceFile) :-
	current_predicate(Functor, SkeletalSpecification),
	source_file(SkeletalSpecification, SourceFile).


mult :-
	source_file(Module1:Predicate, File1),
	source_file(Module2:Predicate, File2),
 	\+ predicate_property(Predicate, multifile),
	File1 @< File2,
	functor(Predicate, Functor, Arity),
	format(user_output, '#### Predicate ~a/~d is defined in modules/files:~n~5c* ~a:~a~n~7cand~n~5c* ~a:~a~n~n',
	       [Functor,Arity,0' ,Module1,File1,0' ,0' ,Module2,File2]),
	fail
      ; true.

spy_all(PartialPredicateName, PredicateList) :-
	find_all_predicates(1, PartialPredicateName, PredicateList),
	spy(PredicateList).

% find public and private predicates
% must work on this to prevent duplications
fap(PartialPredicateName) :- fap_1(PartialPredicateName, _PredicateList).

fap_1(PartialPredicateName, PredicateList1) :-
	% ( find_all_predicates(0, PartialPredicateName, PredicateList0),
	%   write_predicate_list_nl(PredicateList0),
	%   write('-------------'), nl,
	%   fail
	find_all_predicates(1, PartialPredicateName, PredicateList1),
	write_all_predicate_lists(PredicateList1).

fa1(PartialPredicateName) :-
	find_all_predicates(0, PartialPredicateName, PredicateList0),
	write_predicate_list_nl(PredicateList0).

fa2(PartialPredicateName) :-
	find_all_predicates(1, PartialPredicateName, PredicateList1),
	write_all_predicate_lists(PredicateList1).

fap2(PredName1, PredName2) :-
	find_all_predicates2(PredName1, PredName2).

sf :-
	source_file(F),
	format(user_output, '~w~n', [F]),
	fail
      ; true.

fsf(FileName) :-
	find_source_file(FileName).

myspy(Atom) :-
	setof(PredicateName/Arity,
              Module^find_predicate(1, Atom, PredicateName, Arity, Module),
	      PredicateList),
	write_numbered_list(PredicateList),
	read(Choice),
	select_chosen_predicate(PredicateList, Choice, 1, ChosenPredicate/Arity),
	spy ChosenPredicate/Arity.
	
select_chosen_predicate([Predicate1|_RestPredicates], Choice, Choice, Predicate1) :- !.
select_chosen_predicate([_Predicate1|RestPredicates], Choice, Num, ChosenPredicate) :-
	Num1 is Num + 1,
	select_chosen_predicate(RestPredicates, Choice, Num1, ChosenPredicate).
	
write_numbered_list(List) :- write_numbered_list_1(List, 1).

write_numbered_list_1([], _).
write_numbered_list_1([H|T], Num) :-
	format(user_output, '~w: ~w~n', [Num,H]),
	Num1 is Num + 1,
	write_numbered_list_1(T, Num1).

partial_listing(PredicateName, Number) :-
	current_predicate(PredicateName, PredicateSkeletalSpecification),
	bagof(PredicateSkeletalSpecification, PredicateSkeletalSpecification, List),
	print_first_n_elements(Number, 0, List).

print_first_n_elements(Number, Number, _List) :- !.
print_first_n_elements(Number, Temp, [H|T]) :-
	format(user_output, '~w~n', [H]),
	NextTemp is Temp + 1,
	print_first_n_elements(Number, NextTemp, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sub_atom(SubAtom, FullAtom) :-
	atom_chars(SubAtom, SubList),
	atom_chars(FullAtom, FullList),
	append([_L1, SubList, _L2], FullList).

sub_atom_ignore_case(SubAtom, FullAtom) :-
	lower(SubAtom, LowerSubAtom),
	lower(FullAtom, LowerFullAtom),
	sub_atom(LowerSubAtom, LowerFullAtom).

write_list_with_format([], _Format).
write_list_with_format([H|T], Format) :-
	format(Format, H),
	write_list_with_format(T, Format).


write_list_with_format_to_all([], _Format).
write_list_with_format_to_all([H|T], Format) :-
	format_to_all(Format, H),
	write_list_with_format_to_all(T, Format).

write_all_to_file(Variable, Goal, File) :-
	open(File, write, OutputStream),
	set_output(OutputStream),
	write_all(Variable, Goal),
	close(OutputStream).

write_all(Variable, Goal) :-
	call(Goal),
	format(user_output, '~w~n', [Variable]),
	fail
      ; true.

writeq_all(Variable, Goal) :-
	call(Goal),
	format(user_output, '~q~n', [Variable]),
	fail
      ; true.

filename_without_extension(FileName, FileNameWithoutExtension) :-
	atom_chars(FileName, FileNameName),
	chars_until_last(FileNameName, 46, FileNameWithoutExtensionName), % 47 = ASCII('.')
	atom_chars(FileNameWithoutExtension, FileNameWithoutExtensionName).

basename(AbsoluteFileNameAtom, BaseNameAtom) :-
	atom_chars(AbsoluteFileNameAtom, AbsoluteFileNameString),
	chars_after_last(AbsoluteFileNameString, 47, _Prefix, BaseName), % 47 = ASCII('/')
	atom_chars(BaseNameAtom, BaseName).

filename_and_extension(AbsoluteFileNameAtom, FileName, Extension) :-
	atom_chars(AbsoluteFileNameAtom, AbsoluteFileNameString),
	chars_after_last(AbsoluteFileNameString, 47, _Prefix1, BaseNameString), % 47 = ASCII('/')
	split_string(BaseNameString, ".", FileNameString, ExtensionString),
	atom_chars(FileName, FileNameString),
	atom_chars(Extension, ExtensionString).

dirname(AbsoluteFileNameAtom, DirNameAtom) :-
	atom_chars(AbsoluteFileNameAtom, AbsoluteFileNameString),
	chars_until_last(AbsoluteFileNameString, 47, DirName), % 47 = ASCII('/')
	atom_chars(DirNameAtom, DirName).

chars_after_last(String, Char, Prefix, Suffix) :-
	append(Prefix, [Char|Suffix], String),
	\+ memberchk(Char, Suffix),
	!.

chars_until_last(String, Char, Prefix) :-
	append([Prefix, [Char], Suffix], String),
%	last(Char, Prefix),
	\+ member(Char, Suffix),
	!.

chars_until_first(String, Char, Prefix) :-
	append([Prefix, [Char], _Suffix], String),
	!.

count(Var, Goal, List) :-
	setof(Var, Goal^Goal, Solutions),
	remove_vars(Solutions, SolutionsWithoutVars),
	setof(Solution-Count,
	      Var^Goal^SolutionsWithoutVars^
	         solution_count(Var, Goal, SolutionsWithoutVars, Solution, Count),
	      List).

solution_count(Var, Goal, SolutionList, Solution, Count) :-
	member(Solution, ['$#$#$#$#$'|SolutionList]),
	Var = Solution,
	setof(Goal, Goal, SpecificSolutions),
	length(SpecificSolutions, Count).


remove_vars([], []).
remove_vars([H|T], NoVars) :-
	var(H),
	!,
	remove_vars(T, NoVars).
remove_vars([H|T], [H|RestNoVars]) :-
	remove_vars(T, RestNoVars).
