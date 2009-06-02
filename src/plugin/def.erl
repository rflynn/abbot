% ex: set ts=2 noet:
%  

-module(def).
-author("pizza@parseerror.com").
-export(
	[
		test/0,
		loop/0
	]).

-include_lib("../irc.hrl").
-import(test).
-import(irc).

test() ->
	% TODO: actual, you know, tests.
	true.

loop() ->
	receive
		% dict get
		{act, Pid, Irc, _, Dst, Nick, ["how", "is" | Term]} ->
			dict_get(Pid, Irc, Dst, Nick, "is", Term),
			loop();
		{act, Pid, Irc, _, Dst, Nick, ["what", "is" | Term]} ->
			dict_get(Pid, Irc, Dst, Nick, "is", Term),
			loop();
		{act, Pid, Irc, _, Dst, Nick, ["who", "is" | Term]} ->
			dict_get(Pid, Irc, Dst, Nick, "is", Term),
			loop();
		{act, Pid, Irc, _, Dst, Nick, ["how", "are" | Term]} ->
			dict_get(Pid, Irc, Dst, Nick, "are", Term),
			loop();
		{act, Pid, Irc, _, Dst, Nick, ["what", "are" | Term]} ->
			dict_get(Pid, Irc, Dst, Nick, "are", Term),
			loop();
		{act, Pid, Irc, _, Dst, Nick, ["who", "are" | Term]} ->
			dict_get(Pid, Irc, Dst, Nick, "is", Term),
			loop();
		{act, Pid, Irc, _, Dst, Nick, ["what", "am" | Term]} ->
			dict_get(Pid, Irc, Dst, Nick, "are", Term),
			loop();
		{act, Pid, Irc, _, Dst, Nick, ["who", "am" | Term]} ->
			dict_get(Pid, Irc, Dst, Nick, "is", Term),
			loop();
		% dict store
		{act, Pid, Irc, _, _, Nick, [Term, "is" | Rest]} ->
			dict_set(Pid, Irc, Nick, Term, Rest),
			loop();
		{act, Pid, Irc, _, _, Nick, [Term, "are" | Rest]} ->
			dict_set(Pid, Irc, Nick, Term, Rest),
			loop();
		{act, Pid, Irc, _, _, Nick, [Term, "am" | Rest]} ->
			dict_set(Pid, Irc, Nick, Term, Rest),
			loop();
		% dict forget
		{act, Pid, Irc, _, Dst, Nick, ["forget" | Term]} ->
			dict_forget(Pid, Irc, Dst, Nick, Term),
			loop();
		{act, _, _, _, _, _, _} ->
			loop();
		% help
		{help, Pid, Dst, Nick} ->
			Pid ! { q, 
				[ irc:resp(Dst, Nick, Nick ++ ": " ++ Txt) || Txt <-
					[
						"[Term, \"is\" | Def]   -> set definition Def for Term",
						"[\"what\", \"is\", Term] -> get definition for Term"
					]
				] },
			loop()
	end.

% store Key -> Val mapping in dictionary
% NOTE: translations:
%		"you" -> "i" ("what are you?" -> "i am ...")
%		"i" -> Nick ("i am tired" -> Nick ++ " is tired")
dict_set(Pid, Irc, _Nick, "you", Rest) ->
	dict_set_(Pid, Irc, "i", Rest);
dict_set(Pid, Irc, Nick, "i", Rest) ->
	dict_set_(Pid, Irc, Nick, Rest);
dict_set(Pid, Irc, _Nick, Term, Rest) ->
	dict_set_(Pid, Irc, Term, Rest).

dict_set_(Pid, Irc, Term, Rest) ->
	Is = irc:state(Irc, is, dict:new()),
	Term2 = dict_term([Term]),
	Val = util:j(Rest),
	io:format("dict_set Term2=~p Val=~p~n", [Term2, Val]),
	Is2 = dict:store(Term2, Val, Is),
	Pid ! {setstate, is, Is2}.

% retrieve Key -> Val mapping in dictionary
% NOTE: translate between "you" <-> "i", i.e. "what are you?" -> "i am ..."
dict_get(Pid, Irc, Dst, Nick, _Connect, ["you?"]) ->
	dict_get(Pid, Irc, Dst, Nick, "am", ["i"]);
dict_get(Pid, Irc, Dst, Nick, _Connect, ["you"]) ->
	dict_get(Pid, Irc, Dst, Nick, "am", ["i"]);
dict_get(Pid, Irc, Dst, Nick, Connect, Term) ->
	Is = irc:state(Irc, is, dict:new()),
	Term2 = dict_term(Term),
	case dict:find(Term2, Is) of
		error -> nil;
		{ok, X} ->
			Answer = Term2 ++ " " ++ Connect ++ " " ++ X,
			io:format("dict_get Term2=~p Answer=~p~n",
				[Term2, Answer]),
			Pid ! { q, irc:resp(Dst, Nick, Nick ++ ": " ++ Answer) }
	end.

% 
dict_forget(Pid, Irc, Dst, Nick, Term) ->
	Is = irc:state(Irc, is, dict:new()),
	Term2 = dict_term(Term),
	io:format("dict_forget Term=~p Term2=~p~n", [Term, Term2]),
	Is2 = dict:erase(Term2, Is),
	Pid ! {setstate, is, Is2},
	Pid ! {q, irc:resp(Dst, Nick, Nick ++ ": forgotten.") }.

% normalize a dictionary Term i.e. key
dict_term(Term) ->
	J = util:join("", Term),
	Deq = lists:flatten(ircutil:dequestion([J])),
	io:format("dict_term(~p) -> ~p~n", [Term, Deq]),
	Deq.

