:- use_module(library(lists), [
	last/2
   ]).

:- use_module(library(system), [
	environ/2
   ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% STARTUP STUFF %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

l :- init_application_environment.

a :- environ('PWD', PWD), show_init_data(PWD).

init_application_environment :-
	environ('HOME', HOME),
	environ('PWD',  PWD),
	determine_environment(HOME, PWD, Env),
 	init_application(PWD, Env),
	nl,
	show_init_data(PWD).

% If the user's home directory is a sub_atom of the current working directory,
% then the environment is DEVELOPMENT; otherwise, it's PRODUCTION.
determine_environment(HOME, PWD, Environment) :-
	( sub_atom('SICS', PWD) ->
	  Environment = sics
	; sub_atom(HOME, PWD) ->
	  Environment = devl
	; Environment = prod
	).

init_application(PWD, Env) :-
	determine_application(PWD, App),
	init_application_paths(App, Env),
	% temp05(App),
	compile_application(PWD)
	% announce_temp05(App)
      ; true.	

compile_application(PWD) :-
	concat_atoms([PWD, '/', 'loader.pl'], File),
	( file_exists(File) ->
	  compile(File)
        ; format(user_output, 'No loader.pl found in ~w.~n', [PWD])
	).

show_init_data(PWD) :-
	format(user_output, 'PWD:  ~w~n', [PWD]),
	possibly_environ('EXTRA_SICSTUS_ARGS', Args),
	format(user_output, 'ARGS: ~w~n', [Args]),
	format(user_output, '~nOpen Streams:~n', []),
	show_all_streams.
	% format('~nControl Options:~n', []).
	% format('~nFile Overrides:~n', []),
	% bcl override_file,
	% format('~nControl Option Overrides:~n', []),
	% bcl override_control_option.

possibly_environ(EnvironVar, Value) :-
	( environ(EnvironVar, Value) ->
	  true
	; Value = []
	).

% If 'USemrep' is a sub_atom of the current working directory,
% then the area is USEMREP; otherwise, it's ORIGINAL_SYSTEMS.
determine_application(PWD, Area) :-
	( sub_atom('SKR', PWD) ->
	  Area = skr
	; sub_atom('public_mm', PWD) ->
	  Area = skr
	; environ('USER', USER),
	  ( USER == 'flang' ->
	    Area = skr
	  ; USER == 'alan' ->
	    Area = skr
	  ; format(user_output,
		   '~n~nNot in application directory; application env not initiated.~n~n', []),
	    fail
	  )
	).

% system_path_alias(demo).
% system_path_alias(helpsys).
% system_path_alias(language).
% system_path_alias(library).
% system_path_alias(messages).
% system_path_alias(package).
% system_path_alias(qplib).
% system_path_alias(quintus).
% system_path_alias(runtime).
% system_path_alias(system).
% system_path_alias(tutorial).
% 
% retract_user_defined_paths :-
% 	file_search_path(PathAlias, DirSpec),
% 	\+ system_path_alias(PathAlias),
% 	PathAlias \== prolog_utils,
% 	retract(file_search_path(PathAlias, DirSpec)),
% 	fail
%       ; true.

iap :- init_application_paths(skr, devl).

init_skr :- init_application_paths(skr, devl).

init_application_paths(App, Env) :-
	format(user_output, '~nInitiating paths for ~w in ~w environment....~n~n', [App, Env]),
	max_area_length(MaxAreaLength),
	% retract_user_defined_paths,
	define_path(Area, Data),
	% translate_path(Data, App, Env, Path),
	% format(user_output, '~nTranslating ~w ~w ~w ~w.~n', [Data, App, Env, Path]),
	translate_path_test(Data, App, Env, Path),
	atom_codes(Area, AreaString),
	length(AreaString, AreaLength),
	Padding is MaxAreaLength + 3 - AreaLength,
	format(user_output, '~*c~w : ~w~n', [Padding, 32, Area, Path]),
	assertz(file_search_path(Area, Path)),
        fail
      ; nl.

max_area_length(MaxAreaLength) :-
	setof(Length,
	      Area^Data^AreaString^(define_path(Area, Data),
				    name(Area, AreaString),
				    length(AreaString,Length)),
	      AllLengths),
        last(AllLengths, MaxAreaLength).

make_list(Term, List) :-
	( Term = [_|_] ->
	  List = Term
	; List = [Term]
	).

translate_path_test(Data, App, Env, Path) :-
	( translate_path(Data, App, Env, Path) ->
	  true
	; format(user_output, '~nERROR: Translation of ~w ~w ~w failed!!~n', [Data, App, Env]),
	  abort
	).

translate_path(Path, App, Env, TranslatedPath) :-
 	translate_path_1(Path, App, Env, TranslatedPath, []).

translate_path_1(Path, App, Env, TranslatedPath, RestPath) :-
	( Path = [H|T] ->
	  true
	; H = Path,
	  T = []
	),
	translate_path_2(T, H, App, Env, TempTranslatedPath, RestPath),
	% RestPath = [],
	concat_atoms(TempTranslatedPath, TranslatedPath).

translate_path_2([], Last, App, Env, TranslatedLast, Rest) :-
	translate_one_path_element(Last, App, Env, TranslatedLast, Rest).
translate_path_2([Next|T], H, App, Env, TranslatedH, TranslatedT) :-
	translate_one_path_element(H, App, Env, TranslatedH, TranslatedNext),
	translate_path_2(T, Next, App, Env, TranslatedNext, TranslatedT).

translate_one_path_element(path(Component), App, Env, Path, RestPath) :-
	!,
	static_path_data(Component, ComponentPath),
	make_list(ComponentPath, [H|T]),
	translate_path_2(T, H, App, Env, Path, RestPath).
translate_one_path_element(env(Component), _App, _Env, [Path,'/'|RestPath], RestPath) :-
	!,
	environ(Component, Path).
translate_one_path_element(Component, _App, _Env, [Component,'/'|RestPath], RestPath).
static_path_data(abgene,		[path(saw_prod),	abgene]).

static_path_data(home,			env('HOME')).
static_path_data(nls,			env('NLS')).
static_path_data(specialist,            [env('NLS'), 		specialist]).
static_path_data(specialist_devl,       [path(home), 		specialist]).
static_path_data(specialist_prod,       [path(nls), 		specialist]).
static_path_data(skr_src_home,          env('SKR_SRC_HOME')).

% These path definitions are the ones used in use_module declarations
define_path(home,               env('HOME')).
define_path(lexicon,            [path(skr_src_home), lexicon, lexicon]).
% define_path(lexicon,            [path(skr_src_home), .., lexicon, lexicon]).
% define_path(lexicon,            [path(skr_src_home), .., .., lexicon, lexicon]).
define_path(metamap,            [path(skr_src_home), metamap]).
% define_path(metamap,            [path(skr_src_home), .., metamap]).
% define_path(metamap,            [path(skr_src_home), .., .., metamap]).
define_path(morph,		[path(skr_src_home), lexicon, morph]).
% define_path(morph,		[path(skr_src_home), .., lexicon, morph]).
% define_path(morph,		[path(skr_src_home), .., .., lexicon, morph]).
define_path(mmi,                [path(skr_src_home), mmi]).
% define_path(mmi,                [path(skr_src_home), .., mmi]).
% define_path(mmi,                [path(skr_src_home), .., .., mmi]).
define_path(skr,                [path(skr_src_home), skr]).
% define_path(skr,                [path(skr_src_home), .., skr]).
% define_path(skr,                [path(skr_src_home), .., .., skr]).
define_path(skr_db,             [path(skr_src_home), db]).
% define_path(skr_db,             [path(skr_src_home), .., db]).
% define_path(skr_db,             [path(skr_src_home), .., .., db]).
define_path(skr_lib,            [path(skr_src_home), lib]).
% define_path(skr_lib,            [path(skr_src_home), .., lib]).
% define_path(skr_lib,            [path(skr_src_home), .., .., lib]).
define_path(tagger,             [path(skr_src_home), tagger]).
% define_path(tagger,             [path(skr_src_home), .., tagger]).
% define_path(tagger,             [path(skr_src_home), .., .., tagger]).
define_path(text,	    	[path(skr_src_home), text]).
% define_path(text,	    	[path(skr_src_home), .., text]).
% define_path(text,	    	[path(skr_src_home), .., .., text]).
define_path(wsd,                [path(skr_src_home), 'WSD/WSD']).
% define_path(wsd,                [path(skr_src_home), .., 'WSD/WSD']).
% define_path(wsd,                [path(skr_src_home), .., .., 'WSD/WSD']).
define_path(mm_tools_lib,	[path(skr_src_home), '../tools/lib']).

