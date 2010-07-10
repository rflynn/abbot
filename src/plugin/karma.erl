% ex: set ts=2 noet:

%
% track the "karma", or popularity, of terms via the ++/-- postfix operator
% example: i love foo! foo++
%

% "++":
%
% nick: nick++ -> nick - 1
% nick: foo++ -> foo + 1
% nick: foo++ foo++ -> foo + 1
% nick: ++++ -> "++" + 1
% nick: ()++ -> "" + 1
% nick: (())++ -> "()" + 1
% nick: (foo)++ -> foo + 1
% nick: (foo bar)++ -> "foo bar" + 1
% nick: foo++ bar++ -> foo + 1, bar + 1
%
% "--" same syntax as ++, opposite effect
% nick: C++-- -> "C++" - 1
%

-module(karma).
-author("pizza@parseerror.com").
-export(
	[
		test/0,
		loop/0,
		karma_terms/1
	]).

-include_lib("../irc.hrl").
-import(test).
-import(irc).
-import(ircutil).
-import(util).

%
% Implementation:
%
% karma is stored by channel, and aliases are allowed. conceptually...
%
% Karma[Dst][aliasTo] dict Key => AliasedTo
% Karma[Dst][aliasFrom] dict Key => dict(allAliases)
% Karma[Dst][karma] dict Key => {KeyOriginalCase, KarmaScore}
%
% karma_score(Term)
% 	get all aliases for Term
%		add all karma for all aliases and Term
% 
% alias(ToDict, FromDict, To, From)
% 	if ToDict[To][From]:
% 		To already aliases From
% 	else if ToDict[From][To]:
% 		From already aliases To (Loop)
% 	else
% 		ToDict[To] add [From]
% 		FromDict[From] add [To]
%
% aliases(ToDict, FromDict, Term)
% 	
% 	
% 	
% 	
%

test() ->
	test:unit(karma,
		[
			{ karma_terms,		test_karma_terms()				}
		]).

loop() ->
	receive
		{act, Pid, Irc, Msg, Dst, Nick, ["karma", "topten"]} ->
			karma_list(Pid, Irc, Msg, Dst, Nick, fun(A,B) -> karma_sorthi(A,B) end),
			loop();
		{act, Pid, Irc, Msg, Dst, Nick, ["karma", "bottomten"]} ->
			karma_list(Pid, Irc, Msg, Dst, Nick, fun(A,B) -> karma_sortlo(A,B) end),
			loop();
		{act, Pid, Irc, Msg, Dst, Nick, ["karma", "fight" | Terms]} ->
			io:format("fuck you A~n"),
			karma_fight(Pid, Irc, Msg, Dst, Nick, Terms, 1),
			io:format("fuck you B~n"),
			loop();
		{act, Pid, Irc, Msg, Dst, Nick, ["antikarma", "fight" | Terms]} ->
			karma_fight(Pid, Irc, Msg, Dst, Nick, Terms, -1),
			loop();
		{act, Pid, Irc, Msg, Dst, Nick, ["karma", "alias" | Terms]} ->
			karma_alias(Pid, Irc, Msg, Dst, Nick, Terms),
			loop();
		{act, Pid, Irc, Msg, Dst, Nick, ["karma", "aliases" | Term]} ->
			karma_aliases(Pid, Irc, Msg, Dst, Nick, Term),
			loop();
		{act, Pid, _Irc, _Msg, Dst, Nick, ["karma", "chameleon"]} ->
			Pid ! {q, irc:resp(Dst, Nick, "Karma karma karma karma karma chameleon.") },
			loop();
		{act, Pid, Irc, Msg, Dst, Nick, ["karma" | Term]} ->
			karma_get(Pid, Irc, Msg, Dst, Nick, Term, 1),
			loop();
		{act, Pid, Irc, Msg, Dst, Nick, ["antikarma" | Term]} ->
			% trollboy feeds on antikarma
			karma_get(Pid, Irc, Msg, Dst, Nick, Term, -1),
			loop();
		% scan all lines for possible karma modifications
		{act, Pid, Irc, Msg, Dst, Nick, Txt} ->
			karma_scan(Pid, Irc, Msg, Dst, Nick, Txt),
			loop();
		% help
		{help, Pid, Dst, Nick} ->
			Pid ! { q, 
				[ irc:resp(Dst, Nick, Nick ++ ": " ++ Txt) || Txt <-
					[
						"[\"karma\" | Term] -> print karma for Term",
						"[\"karma\", \"fight\" | Terms] -> compare karma",
						"[\"karma\", \"topten\"] -> best karma scores",
						"[\"karma\", \"bottomten\"] -> worst karma scores",
						%"[\"karma\", \"alias\" | Terms] -> alias Term2 to Term1",
						"otherwise Foo++ -> Foo+1, Bar-- -> Bar-1, MyNick++ -> MyNick-1, (Foo Bar)++ -> \"Foo Bar\"+1"
					]
				] },
			loop()
	end.

% given a white-space separated list of strings, return a dict in the form of
% {Key, IntChange}
% example: karma_terms(["foo++", "foo++", "foo--", "(Foo)++", "bar", "(bar" "baz", "quux)++"]) -> [{"foo",1}, {"bar baz quux",1}]
karma_terms(Terms) ->
	karma_terms(util:j(Terms), dict:new()).

karma_terms([], Dict) ->
	% return deterministic output for testing purposes
	lists:sort(fun({A,_},{B,_}) -> A =< B end, dict:to_list(Dict));
karma_terms(Str, Dict) ->
	case re:run(Str, "([(](.{0,128})[)]|[^\s]{1,128})([+][+]|[-][-])(?=\s|$)") of
		nomatch -> karma_terms([], Dict);
		% single match
		{match, [{FullPos,FullLen},{TermPos,TermLen},{ParenTermPos,ParenTermLen},{ModPos,ModLen}]} ->
			% extract parenthesized or non-parenthesized term
			Term =
				case ParenTermPos of
					-1 -> util:trim(string:substr(Str, TermPos+1, TermLen));
					_ -> util:trim(string:substr(Str, ParenTermPos+1, ParenTermLen))
				end,
			TermNormal = string:to_lower(Term),
			Mod = string:substr(Str, ModPos+1, ModLen),
			Adj =
				case Mod of
					"++" -> +1;
					"--" -> -1
				end,
			% find the original case and previous adjustment, if any
			AdjPrev =
				case dict:find(TermNormal, Dict) of
					error -> 0;
					{ok, Val} -> Val
				end,
			% normalize karma adjustment; can't multi-increment in one line
			AdjNorm = 
				case Adj+AdjPrev of
					2 -> 1;
					-2 -> -1;
					N -> N
				end,
			% search the rest of the string
			karma_terms(
				string:substr(Str, FullPos+1+FullLen),
				dict:store(TermNormal, AdjNorm, Dict))
		end.

% karma_terms karma-parsing unit test
test_karma_terms() ->
	[
	  % In       								Out
		{ [[ "" ]],								[] },
		{ [[ "++" ]],							[] },
		{ [[ "--" ]],							[] },
		{ [[ "+-" ]],							[] },
		{ [[ "-+" ]],							[] },
		{ [[ "-+" ]],							[] },
		{ [[ "a" ]],							[] },
		{ [[ "a b c" ]],					[] },
		{ [[ "a+" ]],							[] },
		{ [[ "a-" ]],							[] },
		{ [[ "()" ]],							[] },
		{ [[ "(())" ]],						[] },
		{ [[ "(()" ]],						[] },
		{ [[ "(++)" ]],						[] },
		{ [[ "(++--)" ]],					[] },
		{ [[ "+-+-" ]],						[] },
		{ [[ "a++b" ]],						[] },
		{ [[ "a++" ]],						[{"a",1}] },
		{ [[ "A++" ]],						[{"a",1}] },
		{ [[ "a++ a++" ]],				[{"a",1}] },
		{ [[ "a++ a--" ]],				[{"a",0}] },
		{ [[ "a++ a++ a++" ]],		[{"a",1}] },
		{ [[ "a++ a-- a++" ]],		[{"a",1}] },
		{ [[ "a++ b++" ]],				[{"a",1},{"b",1}] },
		{ [[ "b++ a++" ]],				[{"a",1},{"b",1}] },
		{ [[ "b-- a++" ]],				[{"a",1},{"b",-1}] },
		{ [[ "(a)++" ]],					[{"a",1}] },
		{ [[ "(a )++" ]],					[{"a",1}] },
		{ [[ "( a )++" ]],				[{"a",1}] },
		{ [[ "( )++" ]],					[{"",1}] },
		{ [[ "(a b)++" ]],				[{"a b",1}] },
		{ [[ "C++" ]],						[{"c",1}] },
		{ [[ "C+++" ]],						[{"c+",1}] },
		{ [[ "C++++" ]],					[{"c++",1}] },
		{ [[ "C++--" ]],					[{"c++",-1}] },
		{ [[ "(C++)++" ]],				[{"c++",1}] }
	].

karma_scan(Pid, Irc, Msg, Dst, Nick, Txt) ->
	Terms = karma_terms(Txt),
	if
		Terms == [] -> nil;
		true -> 
			KDst = karma_dst(Irc, Dst),
			karma_set(Pid, Irc, Msg, Dst, Nick, KDst, Terms)
	end.

% adjust karma for Dst with one or more Terms
karma_set(Pid, Irc, _Msg, Dst, _Nick, KDst, []) ->
	karma_save(Pid, Irc, Dst, KDst);
karma_set(Pid, Irc, Msg, Dst, Nick, KDst, [{Key,Adj}|Terms]) ->
	Adj2 =
		case string:to_lower(Nick) == Key of % voting yourself up does not work
			false -> Adj;
			true -> -1
		end,
	% KDst contains aliasTo, aliasFrom and karma dicts
	KarmaDict = dict:fetch(karma, KDst),
	OldScore =
		case dict:find(Key, KarmaDict) of
			error -> 0;
			{ok, Score} -> Score
		end,
	NewScore = OldScore + Adj2,
	% update score
	KarmaDict2 = dict:store(Key, NewScore, KarmaDict),
	% update KDst
	KDst2 = dict:store(karma, KarmaDict2, KDst),
	% process next terms
	karma_set(Pid, Irc, Msg, Dst, Nick, KDst2, Terms).

% locate or create the per-Dst karma dictionary from Irc state
karma_dst(Irc, Dst) ->
	K = irc:state(Irc, karma, dict:new()),
	KDst = 
			case dict:find(Dst, K) of
				error -> % build skeletal per-chan karma dict
					dict:store(aliasTo, dict:new(),
						dict:store(aliasFrom, dict:new(),
							dict:store(karma, dict:new(), dict:new())));
				{ok, D} -> D
			end,
	KDst.

karma_save(Pid, Irc, Dst, KDst) ->
	K = irc:state(Irc, karma, dict:new()),
	K2 = dict:store(Dst, KDst, K),
	Pid ! {setstate, karma, K2},
	Pid ! save. % write to disk

karma_alias(Pid, Irc, Msg, Dst, Nick, Txt) ->
	Terms = terms(util:j(Txt)),
	case length(Terms) of
		0 -> karma_get(Pid, Irc, Msg, Dst, Nick, ["alias"], 1);
		1 -> nil;
		_ ->
			KDst = karma_dst(Irc, Dst),
			DictTo = dict:fetch(aliasTo, KDst),
			[Alias, Target | _] = Terms,
			PrevTarget = util:find_or(Alias, DictTo, nil),
			Answer =
				case PrevTarget == Target of
					true -> lists:flatten(io_lib:format("~s is already aliased to ~s", [Alias, Target]));
					false ->
						% FIXME: TODO: detect is Target already aliases Alias
						DictFrom = dict:fetch(aliasFrom, KDst), % pull reverse-mapping dict
						% Alias does not already point to Target; if it points to something else, de-alias it
						DictFrom2 =
							case util:find_or(PrevTarget, DictTo, nil) of
								nil -> dict:new();
								PrevTargetFrom -> % remove Alias from PrevTarget's existing reverse-mapping
									PrevTargetFrom2 = dict:erase(Alias, PrevTargetFrom),
									dict:store(PrevTarget, PrevTargetFrom2, DictFrom)
								end,
						DictTo2 = dict:store(Alias, Target, DictTo),
						% add Target -> Alias reverse-mapping
						TargetFrom = util:find_or(Target, DictFrom2, dict:new()), % 
						DictFrom3 = dict:store(Target, dict:store(Alias, nil, TargetFrom), DictFrom2),
						KDst2 = dict:store(aliasTo, DictTo2, KDst),
						KDst3 = dict:store(aliasFrom, DictFrom3, KDst2),
						karma_save(Pid, Irc, Dst, KDst3),
						lists:flatten(io_lib:format("~s aliased to ~s", [Alias, Target]))
					end,
			Resp = Nick ++ ": " ++ Answer,
			Pid ! {pipe, Msg, irc:resp(Dst, Nick, Resp)}
	end.

karma_aliases(Pid, Irc, Msg, Dst, Nick, Txt) ->
	Terms = terms(util:j(Txt)),
	Term = % command with no terms looks up self
		case length(Terms) of
			0 -> Nick;
			_ -> hd(Terms)
		end,
	KDst = karma_dst(Irc, Dst),
	DictFrom = dict:fetch(aliasFrom, KDst),
	AliasDict = util:find_or(Term, DictFrom, dict:new()),
	Aliases = [ Key || {Key,_} <- dict:to_list(AliasDict) ],
	Answer =
		case length(Aliases) of
			0 -> lists:flatten(io_lib:format("~s has no aliases", [Term]));
			N -> lists:flatten(io_lib:format("~s has ~B alias~s: ~s",
				[Term, N, if N == 1 -> ""; true -> "es" end, ircutil:dotdotdot(util:join(",", Aliases), 200)]))
		end,
	Resp = Nick ++ ": " ++ Answer,
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Resp)}.

terms(Str) -> terms(Str, []).
terms([], T) -> T;
terms(Str, T) ->
	case re:run(Str, "(?:[(](.{0,128})[)]|[^\s]{1,128})(?=\s|$)") of
		nomatch -> T;
		{match, [{Pos,Len}]} ->
			Term = util:trim(string:substr(Str, Pos+1, Len)),
			terms(string:substr(Str, Pos+1+Len), T ++ [string:to_lower(Term)]);
		{match, [{FullPos,FullLen},{Pos,Len}]} ->
			Term = util:trim(string:substr(Str, Pos+1, Len)),
			terms(string:substr(Str, FullPos+1+FullLen), T ++ [string:to_lower(Term)])
		end.

karma_sum_aliases(KarmaDict, Aliases) ->
	karma_sum_aliases(KarmaDict, dict:to_list(Aliases), 0).

karma_sum_aliases(_, [], N) -> N;
karma_sum_aliases(KarmaDict, [{Alias,_}|Aliases], N) ->
	karma_sum_aliases(KarmaDict, Aliases, N + util:find_or(Alias, KarmaDict, 0)).

karma_get(Pid, Irc, Msg, Dst, Nick, Txt, Mult) ->
	Terms = terms(util:j(Txt)),
	Term = % command with no terms looks up self
		case length(Terms) of
			0 -> Nick;
			_ -> hd(Terms)
		end,
	KDst = karma_dst(Irc, Dst),
	KarmaDict = dict:fetch(karma, KDst),
	KarmaLabel =
		if
			Mult == -1 -> "antikarma";
			true -> "karma"
		end,
	DictFrom = dict:fetch(aliasFrom, KDst),
	Aliases = util:find_or(Term, DictFrom, dict:new()),
	Answer =
		case (dict:is_key(Term, KarmaDict) or (dict:size(Aliases) > 0)) of
			false -> % no karma entry, no aliases
				lists:flatten(io_lib:format("~s has no ~s", [Term, KarmaLabel]));
			true -> % a karma entry and/or aliases
				Score = util:find_or(Term, KarmaDict, 0),
				Score2 = Score + karma_sum_aliases(KarmaDict, Aliases),
				lists:flatten(io_lib:format("~s for ~s is ~s~B",
					[KarmaLabel, Term, if Score2 * Mult > 0 -> "+"; true -> "" end, Score2 * Mult]))
			end,
	Resp = Nick ++ ": " ++ Answer,
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Resp)}.

is_plural(X) ->
		(X == "you") or
		lists:suffix("es", X) or
		lists:suffix("s", X).

has(X) ->
	case is_plural(X) of
		true -> "have";
		false -> "has"
	end.

karma_fight(Pid, Irc, Msg, Dst, Nick, Txt, Mult) ->
	Terms = terms(util:j(Txt)),
	case length(Terms) of
		0 -> karma_get(Pid, Irc, Msg, Dst, Nick, ["fight"], Mult);
		1 -> nil;
		_ ->
			KDst = karma_dst(Irc, Dst),
			KarmaDict = dict:fetch(karma, KDst),
			[X, Y | _] = Terms,
			XScore = util:find_or(X, KarmaDict, 0) * Mult,
			YScore = util:find_or(Y, KarmaDict, 0) * Mult,
			Answer =
				if
					XScore > YScore -> lists:flatten(io_lib:format("~s ~s beaten ~s (+~B)", [X, has(X), Y, XScore-YScore]));
					XScore < YScore -> lists:flatten(io_lib:format("~s ~s beaten ~s (+~B)", [Y, has(Y), X, YScore-XScore]));
					XScore == YScore -> lists:flatten(io_lib:format("~s has tied ~s", [X, Y]))
				end,
			Resp = Nick ++ ": " ++ Answer,
			Pid ! {pipe, Msg, irc:resp(Dst, Nick, Resp)}
	end.

karma_sorthi({KA,NA}, {KB,NB}) -> if NA > NB -> true; true -> KA =< KB end.
karma_sortlo({KA,NA}, {KB,NB}) -> if NA > NB -> false; true -> KA > KB end.

% topten/bottomten
% sort, trim, number
karma_list(Pid, Irc, Msg, Dst, Nick, SortFn) ->
	KDst = karma_dst(Irc, Dst),
	KarmaDict = dict:fetch(karma, KDst),
	LSorted = lists:sort(SortFn, dict:to_list(KarmaDict)),
	Answer =
		case length(LSorted) of
			0 -> "no karma";
			Len ->
				LEnum = lists:zip(lists:seq(1, Len), LSorted),
				LTrim = lists:takewhile(fun({N,_}) -> N =< 10 end, LEnum),
				LTxt = [ lists:flatten(io_lib:format("#~B ~s (~s~B)",
					[N, Key, if Score > 0 -> "+"; true -> "" end, Score]))
						|| {N,{Key,Score}} <- LTrim ],
				util:j(LTxt)
		end,
	Resp = Nick ++ ": " ++ Answer,
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Resp)}.

